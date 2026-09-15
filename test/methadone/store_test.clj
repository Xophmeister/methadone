; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.store-test
  "The log on disk, and the session lifecycle over it."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :as t]
            [methadone.fixtures :refer [*dir* bb bb-eval day dead-pid hour log-path logged minute
                                        own-pid recently session store with-temp-dir]]
            [methadone.store :as store]))

(t/use-fixtures :each with-temp-dir)

(t/deftest round-tripping
  (t/testing "a missing file reads as an empty log"
    (t/is (= (logged {}) (store/read-log (log-path)))))

  (t/testing "a log survives a write and read unchanged"
    (let [before (logged {"claude" [{:id (random-uuid) :pid 1 :start (recently hour) :end (recently minute)}
                                    {:id (random-uuid) :pid 2 :start (recently minute)}]})]
      (store/write-log! before (log-path))
      (t/is (= before (store/read-log (log-path))))))

  (t/testing "timestamps are stored as #inst, so the file stays legible"
    (store/write-log! (logged {"claude" [{:id :a :pid 1 :start (recently hour)}]}) (log-path))
    (t/is (re-find #"#inst" (slurp (log-path)))))

  (t/testing "a running session is not given an :end by the round trip"
    (store/write-log! (logged {"claude" [{:id :a :pid 1 :start (recently hour)}]}) (log-path))
    (t/is (not (contains? (session (:sessions (store/read-log (log-path))) :a) :end))))

  (t/testing "a heartbeat is stored as an #inst too, not left as a raw count"
    (store/write-log! (logged {"claude" [{:id :a :pid 1 :start (recently hour) :seen (recently minute)}]})
                      (log-path))
    (t/is (inst? (:seen (session (:sessions (edn/read-string (slurp (log-path)))) :a)))))

  (t/testing "writing leaves no temporary files behind"
    (store/write-log! (logged {"claude" []}) (log-path))
    (t/is (empty? (fs/glob *dir* "*.tmp"))))

  (t/testing "the format's version is written, not merely assumed"
    ; Deliberately the literal rather than the constant: something has
    ; to pin the number, or every assertion about it moves with it.
    (store/write-log! (logged {}) (log-path))
    (t/is (= 1 (:version (edn/read-string (slurp (log-path)))))))

  (t/testing "history is written in date order, so the file stays legible"
    ; A map of a few hundred dates prints in hash order, which would
    ; churn the whole file on every write and make it unreadable.
    (store/write-log! (assoc (logged {})
                             :history {"claude" {"2026-09-15" {:count 1 :duration 0}
                                                 "2026-09-13" {:count 1 :duration 0}
                                                 "2026-09-14" {:count 1 :duration 0}}})
                      (log-path))

    (let [written (slurp (log-path))]
      (t/is (< (str/index-of written "2026-09-13")
               (str/index-of written "2026-09-14")
               (str/index-of written "2026-09-15"))))))

(t/deftest reading-an-older-log
  ; The shape written before the envelope existed. Lifting it matters
  ; more than compatibility usually does: the log is the friction, so
  ; failing to read one would hand its owner a clean slate they had not
  ; earned -- and the next write would then destroy the evidence.
  (let [moment #inst "2026-09-14T10:00:00.000-00:00"]
    (spit (fs/file (log-path))
          (pr-str {"claude" [{:id :a :pid 1 :start moment}]}))

    (let [lifted (store/read-log (log-path))]
      (t/testing "it is wrapped in the envelope, at the current version"
        (t/is (= @#'store/log-version (:version lifted)))
        (t/is (= {} (:history lifted))))

      (t/testing "with its sessions where they now belong"
        (t/is (some? (session (:sessions lifted) :a))))

      (t/testing "and its timestamps decoded, as any other log's would be"
        (t/is (= (inst-ms moment) (:start (session (:sessions lifted) :a))))))))

(t/deftest reading-a-current-log
  ; The legacy branch discards history, so a log that comes back with
  ; its history intact is proof the versioned branch was taken. Without
  ; this, fixtures that quietly wrote unversioned logs would leave the
  ; current-version branch with no coverage at all.
  (let [before (assoc (logged {})
                      :history {"claude" {"2026-09-14" {:count 2 :duration 60000}}})]

    (store/write-log! before (log-path))

    (t/testing "a versioned log is read as it stands, not lifted as an old one"
      (t/is (= (:history before) (:history (store/read-log (log-path))))))))

(t/deftest refusing-a-log-from-the-future
  ; read-log dies rather than guess at a shape it does not know, so this
  ; needs a process of its own: System/exit would otherwise take the
  ; test runner down with it.
  (spit (fs/file (log-path))
        (pr-str {:version (inc @#'store/log-version) :sessions {} :history {}}))

  (let [{:keys [exit err]} (bb-eval {"HOME" (str *dir*)}
                                    (str "(require '[methadone.store :as store])"
                                         "(store/read-log \"" (log-path) "\")"))]

    (t/testing "it gives up rather than reading what it cannot understand"
      (t/is (= 1 exit)))

    (t/testing "and says so, rather than dying silently"
      (t/is (re-find #"version" err)))))

(t/deftest transactions
  (t/testing "the function is applied, and the resulting log returned"
    (t/is (= (logged {"claude" []}) (store/update-log! (store) #(assoc % "claude" [])))))

  (t/testing "the result is what a subsequent read sees"
    (store/update-log! (store) #(assoc % "claude" []))
    (t/is (= (logged {"claude" []}) (store/read-log (log-path)))))

  (t/testing "the lock lives beside the log, not in it"
    (store/update-log! (store) identity)
    (t/is (fs/exists? (str (log-path) ".lock")))))

(t/deftest self-healing
  (t/testing "a session abandoned by a dead process is reaped in passing"
    (let [start (recently (* 10 minute))]
      (store/write-log! (logged {"claude" [{:id :orphan :pid (dead-pid) :start start}]}) (log-path))
      (t/is (= start (:end (session (:sessions (store/update-log! (store) identity)) :orphan))))))

  (t/testing "a session whose process still lives is left running"
    (store/write-log! (logged {"claude" [{:id :live :pid (own-pid) :start (recently minute)}]}) (log-path))
    (t/is (nil? (:end (session (:sessions (store/update-log! (store) identity)) :live)))))

  (t/testing "a session that ended beyond the retention period is pruned in passing"
    (store/write-log! (logged {"claude" [{:id    :ancient
                                          :pid   1
                                          :start (recently (* 40 day))
                                          :end   (recently (* 31 day))}]})
                      (log-path))
    (t/is (nil? (session (:sessions (store/update-log! (store) identity)) :ancient)))))

(t/deftest session-lifecycle
  (let [started (recently (* 5 minute))
        id      (store/open-session! (store) "claude" started)]

    (t/testing "opening returns an ID that identifies a session in the log"
      (t/is (= started (:start (session (:sessions (store/read-log (log-path))) id)))))

    (t/testing "the new session is running"
      (t/is (nil? (:end (session (:sessions (store/read-log (log-path))) id)))))

    (t/testing "the PID recorded is Methadone's own, so that we can be reaped"
      (t/is (= (own-pid) (:pid (session (:sessions (store/read-log (log-path))) id)))))

    (t/testing "closing ends the session"
      (store/close-session! (store) id (recently minute))
      (t/is (some? (:end (session (:sessions (store/read-log (log-path))) id)))))

    (t/testing "closing twice does not move the end, as the shutdown hook may repeat it"
      (let [ended (:end (session (:sessions (store/read-log (log-path))) id))]
        (store/close-session! (store) id (System/currentTimeMillis))
        (t/is (= ended (:end (session (:sessions (store/read-log (log-path))) id))))))))

(t/deftest heartbeating
  (let [id   (store/open-session! (store) "claude" (recently (* 5 minute)))
        beat (recently (* 3 minute))]

    (t/testing "a heartbeat is recorded against the running session"
      (store/touch-session! (store) id beat)
      (t/is (= beat (:seen (session (:sessions (store/read-log (log-path))) id)))))

    (t/testing "a heartbeat arriving after the close cannot reopen the session"
      (store/close-session! (store) id (recently minute))
      (let [closed (session (:sessions (store/read-log (log-path))) id)]
        (store/touch-session! (store) id (System/currentTimeMillis))
        (t/is (= closed (session (:sessions (store/read-log (log-path))) id)))))))

(t/deftest thread-safety
  (t/testing "simultaneous transactions in one process do not collide"
    ; The heartbeat runs on a thread of its own, so update-log! must
    ; tolerate two callers in the same process. A file lock belongs to
    ; the process rather than to the thread that took it, so without a
    ; monitor beneath it the second caller gets an
    ; OverlappingFileLockException rather than its turn.
    ; The configuration is built here rather than in the workers because
    ; binding is thread-local: a bare Thread sees *dir*'s root value,
    ; and fs/path quietly turns a nil parent into a relative path.
    (let [cfg     (store)
          threads 4
          each    25
          failed  (atom [])
          workers (doall (for [_ (range threads)]
                           (Thread.
                            (fn []
                              (dotimes [_ each]
                                (try (store/open-session! cfg "claude"
                                                          (System/currentTimeMillis))
                                     (catch Exception e (swap! failed conj (class e)))))))))]

      (run! #(.start %) workers)
      (run! #(.join %) workers)

      (t/is (empty? @failed) "every transaction succeeded")
      (t/is (= (* threads each) (count (get-in (store/read-log (log-path)) [:sessions "claude"])))))))

(t/deftest concurrency
  (t/testing "simultaneous transactions do not lose one another's updates"
    ; This is the only test that justifies the lock existing: the failure
    ; it guards against cannot be reached from a single process.
    (let [n     8
          code  (str "(require '[methadone.config :as config] "
                     "         '[methadone.store :as store]) "
                     "(store/open-session! (assoc config/defaults :log \"" (log-path) "\") "
                     "\"claude\" (System/currentTimeMillis))")
          procs (doall (repeatedly n #(p/process [bb "--classpath" (str (fs/absolutize "src"))
                                                  "-e" code])))
          exits (mapv (comp :exit deref) procs)]

      (t/is (every? zero? exits) "every appending process exited cleanly")
      (t/is (= n (count (get-in (store/read-log (log-path)) [:sessions "claude"])))))))
