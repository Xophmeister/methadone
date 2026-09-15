; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.log
  "Pure operations over the whole log. Anything needing the outside
  world is injected, so that these stay testable against fixtures.")

(defn map-sessions [f log] (update-vals log #(mapv f %)))
(defn- filter-sessions [pred log] (update-vals log #(filterv pred %)))

(defn- ended
  "Mark a session as having ended at the given time. The heartbeat is
  dropped: once :end is known, :seen is redundant."
  [session now]

  (-> session (assoc :end now) (dissoc :seen)))

(defn reap
  "Reap sessions -- that is, mark as ended any whose process is no longer
  alive -- wherever they appear in the log.

  An abandoned session's true end time is unknowable, so it is charged up
  to the last moment there is evidence it was alive: its most recent
  heartbeat, or its start if it never lived long enough to record one."
  [log alive?]

  (map-sessions (fn [session]
                  (cond-> session
                    (and (not (:end session)) (not (alive? (:pid session))))
                    (ended (or (:seen session) (:start session)))))

                log))

(defn- session-date
  "Return the local date of the session's start time, in ISO-8601 format
  (YYYY-MM-DD, the default for `java.time.LocalDate.toString`), relative
  to the given time zone. One's 'day' usually starts before midnight,
  but can end either side of it, hence using the start time for
  accounting.

  NOTE tz ought to be a java.time.ZoneId; this is the responsibility of
  the caller."
  [{:keys [start]} tz]

  (-> start
      java.time.Instant/ofEpochMilli
      (.atZone tz)
      .toLocalDate
      .toString))

(defn- current?
  "Current sessions have not ended, or have ended within the retention
  period."
  [session now retention]

  (or (nil? (:end session))
      (> (:end session) (- now retention))))

(defn prune
  "Prune the log to just those sessions that are current."
  [log now retention] (filter-sessions #(current? % now retention) log))

(defn expired
  "Return the sessions that have expired, so that they can be aggregated."
  [log now retention] (filter-sessions #(not (current? % now retention)) log))

(defn tally
  "Reduce sessions to one bucket per binary per local day, holding how
  many there were and how long they ran altogether.

  Notes:
  - This makes session history cheap enough to keep indefinitely for
    aggregation purposes.
  - A session is charged to the day on which it began, regardless of when
    it ended.
  - Still-running-sessions are measured up to now."
  [sessions now tz]

  (into {} (remove (comp empty? val))
        (update-vals sessions #(apply merge-with
                                      (partial merge-with +)
                                      {}
                                      (mapv (fn [session]
                                              {(session-date session tz)
                                               {:count    1
                                                :duration (max 0 (- (or (:end session) now)
                                                                    (:start session)))}})
                                            %)))))

(defn open
  "Add a session to the log, under the given binary."
  [log binary session]

  (update log binary (fnil conj []) session))

(defn- alter-session
  "Apply f to the open session with the given ID, wherever in the log it
  appears. Sessions that have already ended are left alone."
  [log id f]

  (map-sessions #(cond-> %
                   (and (= id (:id %)) (nil? (:end %)))
                   f)

                log))

(defn touch
  "Record that the session with the given ID was still running at the
  given time, so that a reap can charge it up to its last heartbeat
  rather than guessing."
  [log id now]

  (alter-session log id #(assoc % :seen now)))

(defn close
  "Mark the session with the given ID as having ended now, wherever in
  the log it appears. Sessions are identified by UUID, so the binary need
  not be known.

  The first close wins: a session that has already ended is left alone
  and an ID that is no longer in the log is ignored."
  [log id now]

  (alter-session log id #(ended % now)))
