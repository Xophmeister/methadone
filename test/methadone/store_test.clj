; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.store-test
  "The log on disk, and the session lifecycle over it."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.test :as t]
            [methadone.fixtures :refer [*dir* bb day dead-pid hour log-path minute own-pid recently session store with-temp-dir]]
            [methadone.store :as store]))

(t/use-fixtures :each with-temp-dir)

(t/deftest round-tripping
  (t/testing "a missing file reads as an empty log"
    (t/is (= {} (store/read-log (log-path)))))

  (t/testing "a log survives a write and read unchanged"
    (let [before {"claude" [{:id (random-uuid) :pid 1 :start (recently hour) :end (recently minute)}
                            {:id (random-uuid) :pid 2 :start (recently minute)}]}]
      (store/write-log! before (log-path))
      (t/is (= before (store/read-log (log-path))))))

  (t/testing "timestamps are stored as #inst, so the file stays legible"
    (store/write-log! {"claude" [{:id :a :pid 1 :start (recently hour)}]} (log-path))
    (t/is (re-find #"#inst" (slurp (log-path)))))

  (t/testing "a running session is not given an :end by the round trip"
    (store/write-log! {"claude" [{:id :a :pid 1 :start (recently hour)}]} (log-path))
    (t/is (not (contains? (session (store/read-log (log-path)) :a) :end))))

  (t/testing "a heartbeat is stored as an #inst too, not left as a raw count"
    (store/write-log! {"claude" [{:id :a :pid 1 :start (recently hour) :seen (recently minute)}]}
                      (log-path))
    (t/is (inst? (:seen (session (edn/read-string (slurp (log-path))) :a)))))

  (t/testing "writing leaves no temporary files behind"
    (store/write-log! {"claude" []} (log-path))
    (t/is (empty? (fs/glob *dir* "*.tmp")))))

(t/deftest transactions
  (t/testing "the function is applied, and the resulting log returned"
    (t/is (= {"claude" []} (store/update-log! (store) #(assoc % "claude" [])))))

  (t/testing "the result is what a subsequent read sees"
    (store/update-log! (store) #(assoc % "claude" []))
    (t/is (= {"claude" []} (store/read-log (log-path)))))

  (t/testing "the lock lives beside the log, not in it"
    (store/update-log! (store) identity)
    (t/is (fs/exists? (str (log-path) ".lock")))))

(t/deftest self-healing
  (t/testing "a session abandoned by a dead process is reaped in passing"
    (let [start (recently (* 10 minute))]
      (store/write-log! {"claude" [{:id :orphan :pid (dead-pid) :start start}]} (log-path))
      (t/is (= start (:end (session (store/update-log! (store) identity) :orphan))))))

  (t/testing "a session whose process still lives is left running"
    (store/write-log! {"claude" [{:id :live :pid (own-pid) :start (recently minute)}]} (log-path))
    (t/is (nil? (:end (session (store/update-log! (store) identity) :live)))))

  (t/testing "a session that ended beyond the retention period is pruned in passing"
    (store/write-log! {"claude" [{:id    :ancient
                                  :pid   1
                                  :start (recently (* 40 day))
                                  :end   (recently (* 31 day))}]}
                      (log-path))
    (t/is (nil? (session (store/update-log! (store) identity) :ancient)))))

(t/deftest session-lifecycle
  (let [started (recently (* 5 minute))
        id      (store/open-session! (store) "claude" started)]

    (t/testing "opening returns an ID that identifies a session in the log"
      (t/is (= started (:start (session (store/read-log (log-path)) id)))))

    (t/testing "the new session is running"
      (t/is (nil? (:end (session (store/read-log (log-path)) id)))))

    (t/testing "the PID recorded is Methadone's own, so that we can be reaped"
      (t/is (= (own-pid) (:pid (session (store/read-log (log-path)) id)))))

    (t/testing "closing ends the session"
      (store/close-session! (store) id (recently minute))
      (t/is (some? (:end (session (store/read-log (log-path)) id)))))

    (t/testing "closing twice does not move the end, as the shutdown hook may repeat it"
      (let [ended (:end (session (store/read-log (log-path)) id))]
        (store/close-session! (store) id (System/currentTimeMillis))
        (t/is (= ended (:end (session (store/read-log (log-path)) id))))))))

(t/deftest heartbeating
  (let [id   (store/open-session! (store) "claude" (recently (* 5 minute)))
        beat (recently (* 3 minute))]

    (t/testing "a heartbeat is recorded against the running session"
      (store/touch-session! (store) id beat)
      (t/is (= beat (:seen (session (store/read-log (log-path)) id)))))

    (t/testing "a heartbeat arriving after the close cannot reopen the session"
      (store/close-session! (store) id (recently minute))
      (let [closed (session (store/read-log (log-path)) id)]
        (store/touch-session! (store) id (System/currentTimeMillis))
        (t/is (= closed (session (store/read-log (log-path)) id)))))))

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
      (t/is (= (* threads each) (count (get (store/read-log (log-path)) "claude")))))))

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
      (t/is (= n (count (get (store/read-log (log-path)) "claude")))))))
