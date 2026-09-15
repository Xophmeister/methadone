; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

; Log format ($XDG_STATE_HOME/methadone/log.edn):
;
; ```edn
; {:version  <INT>                             ; log format version
;
;  :sessions {"<STR: BINARY>"                  ; keyed by binary basename
;             [{:id    <UUID>                  ; unique session ID
;               :pid   <INT: METHADONE PID>    ; the Methadone process
;               :start <INST>                  ; when the session began
;               :end   <INST>                  ; when the session ended (optional)
;               :seen  <INST>}                 ; last heartbeat of a running session
;              ...]
;             ...}
;
;  :history {"<STR: BINARY>"                   ; keyed by binary basename
;             {<STR: YYYY-MM-DD>               ; keyed by (local) date of the sessions
;              {:count    <INT>                ; number of sessions on that date
;               :duration <INT: MILLISECONDS>  ; total duration of sessions on that date
;              ...}
;            ...}}
; ```

(ns methadone.store
  "The log on disk: reading it, writing it atomically and mutating it
  under a lock, plus the session lifecycle expressed in those terms."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [methadone.log :as log]
            [methadone.util :refer [die]]))

(def ^:const ^:private log-version 1)

(def ^:private timestamps [:start :end :seen])

(defn- convert-session [f session]
  (reduce (fn [session key] (cond-> session (session key) (update key f)))
          session
          timestamps))

(defn- convert [f log]
  (log/map-sessions (partial convert-session f) log))

; Timestamps are milliseconds in memory but #inst on disk, so that the
; log stays legible. These are the only two places that bridge the two.
(def ^:private decode (partial convert java.util.Date/.getTime))
(def ^:private encode (partial convert java.util.Date/new))

(defn pid-alive?
  "Whether the process with the given PID is still running. PIDs are
  recycled, so a false positive is possible; that leaves a dead session
  open and thus errs towards more friction, which is the safe direction."
  [pid]

  (when pid
    (let [handle (java.lang.ProcessHandle/of pid)]
      (and (.isPresent handle) (.isAlive (.get handle))))))

(defn read-log
  "Read the log from the given file, returning an empty log if it doesn't
  exist."
  [path]

  (if (fs/exists? path)
    (let [log (edn/read-string (slurp (fs/file path)))]
      (cond
        ; Legacy logs have no version, so upgrade
        (not (contains? log :version)) {:version  log-version
                                        :sessions (decode log)
                                        :history  {}}

        ; Current log version
        (= log-version (:version log)) (update log :sessions decode)

        ; Unsupported log version
        :else (die (str "Log version " (:version log) " is not supported"))))

    ; Empty log as fallback
    {:version  log-version
     :sessions {}
     :history  {}}))

(defn write-log!
  "Encode the log and atomically write it to the given file.

  NOTE The existence of the parent directory is the caller's
  responsibility."
  [log path]

  (let [tmp  (fs/create-temp-file {:dir    (fs/parent path)
                                   :prefix "log"
                                   :suffix ".tmp"})
        data (with-out-str (pp/pprint (-> log
                                          (update :sessions encode)
                                          (update :history #(update-vals % (partial into (sorted-map)))))))]

    (spit (fs/file tmp) data)
    (fs/move tmp path {:replace-existing true
                       :atomic-move      true})))

(def ^:private monitor (Object.))

(defn with-lock*
  "Acquire an exclusive lock on the given file, then apply f to no
  arguments.

  The lock belongs to the open file descriptor rather than to the file
  itself, so it is released whenever the channel closes -- including when
  the process dies without unwinding. There is therefore no stale lock to
  recover from. The lock file is left on disk deliberately: it must name
  a stable inode that nothing ever renames, which is exactly why it
  cannot be the log file.

  A lock is held by the process rather than by the thread that took it,
  so a second thread asking for an overlapping region gets an
  OverlappingFileLockException rather than waiting its turn. The monitor
  therefore serialises threads, before the file lock serialises
  processes."
  [lockfile f]

  (locking monitor
    (with-open [raf (java.io.RandomAccessFile. (fs/file lockfile) "rw")
                ch  (.getChannel raf)]

      ; The returned FileLock is discarded: Babashka's reflection
      ; allowlist blocks its .release, .close and .isValid methods and
      ; closing the channel releases the lock in any case.
      (.lock ch)
      (f))))

(defmacro with-lock
  "Evaluate the body under an exclusive lock on the given file."
  [lockfile & body]

  `(with-lock* ~lockfile (fn [] ~@body)))

(defn update-log!
  "Apply f to the configured log, under an exclusive lock, writing the
  result back and returning it. Orphaned sessions are reaped and expired
  ones pruned on the way through, so that the log self-heals on every
  transaction."
  [{:keys [log retention]} f]

  (with-lock (str log ".lock")
    (let [updated (update (read-log log) :sessions #(-> %
                                                        (log/reap pid-alive?)
                                                        (log/prune (System/currentTimeMillis) retention)
                                                        f))]

      (write-log! updated log)
      updated)))

(defn open-session!
  "Record the start of a session for the given binary in the log held in
  the given file, returning the new session's ID so that it may be closed
  later.

  The PID recorded is Methadone's own, not the agent's: methadone
  supervises the session, so it is Methadone's death that leaves one
  unclosed and its PID that tells a later transaction whether the session
  was abandoned.

  NOTE Call this only once the nag has elapsed, so that a countdown the
  user abandons leaves no trace."
  [config binary now]

  (let [id (random-uuid)]
    (update-log! config #(log/open % binary {:id    id
                                             :pid   (.pid (java.lang.ProcessHandle/current))
                                             :start now}))
    id))

(defn touch-session!
  "Record that the session with the given ID is still running. Sessions
  that have already ended are ignored, so a heartbeat arriving after the
  close cannot reopen one."
  [config id now]

  (update-log! config #(log/touch % id now)))

(defn close-session!
  "Record the end of the session with the given ID.

  Safe to call more than once, so the ordinary exit path and a shutdown
  hook may both invoke it without the second inflating the recorded
  duration. A session that is no longer in the log is ignored."
  [config id now]

  (update-log! config #(log/close % id now)))

(defn start-heartbeat!
  "Periodically record that the session with the given ID is still
  running, so that a reap can charge it up to its last heartbeat rather
  than guessing.

  A daemon thread, so it can never hold Methadone open past the session
  it is tracking and silent: Methadone's stderr is the agent's terminal,
  so a stack trace here would land in the middle of the agent's display."
  [config id]

  (doto (Thread. (fn []
                   (loop []
                     (Thread/sleep (:heartbeat config))
                     (try (touch-session! config id (System/currentTimeMillis))
                          (catch Exception _ nil))
                     (recur))))

    (.setDaemon true)
    (.start)))
