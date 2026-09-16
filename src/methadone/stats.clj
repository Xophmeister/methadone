; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.stats
  "Generate usage statistics and trends from the log to present to the
  user."
  (:require [clojure.string :as str]
            [methadone.log :as log]
            [methadone.policy :as policy]
            [methadone.store :as store]
            [methadone.tty :as tty]
            [methadone.util :as util]))

(defn- record
  "The log records current sessions and historical usage, per binary,
  which have no intersection. The statistics we generate are derived
  from both, so we need to munge the current sessions into the historical
  form."
  [log now tz]

  (log/accumulate-history (:history log)
                          (log/tally (:sessions log) now tz)))

(defn- date-keys
  "Generate the set of date keys (YYYY-MM-DD) from the given Unix epoch
  (in milliseconds) to a specified delta days in the past."
  [when delta tz]

  (let [t-0  (util/epoch->date when tz)
        days (map #(-> t-0 (.minusDays %)) (reverse (range 0 delta)))]

    (map str days)))

(defn- aggregate
  "Aggregate daily {:count :duration} log records over a collection of
  dates."
  [days dates]

  (apply merge-with + {:count 0 :duration 0} (keep days dates)))

(defn standings
  "Where each binary stands: what it was reached for over the last so
  many days, and what that has left it costing now.

  The two halves answer to different clocks, deliberately. The launches
  and the time are a plain count over whole days, which a reader can
  check against their own memory of the week; the score and the wait are
  decayed by age, and are the figures that actually governed the last
  nag. The first alone would say nothing about the friction, and the
  second alone would be a number nobody could verify."
  [config sessions by-day now tz days]

  (for [binary (sort (keys by-day))
        :let   [recent (aggregate (get by-day binary {}) (date-keys now days tz))
                used   (-> sessions
                           (policy/sessions-for binary)
                           (policy/usage now (:window config)))]]

    {:binary   binary
     :launches (:count recent)
     :duration (:duration recent)
     :score    (Math/round (double (policy/score config used)))
     :wait     (policy/friction config used)}))

(defn trend
  "The score earned in each of the last so many weeks, oldest first,
  across every binary at once.

  Summed by week rather than decayed, because these figures exist to be
  compared with one another and with the rows above them. A decayed
  score taken at some past instant would be neither comparable nor
  checkable, and the whole point of a trend is that it can be read."
  [config by-day now tz weeks]

  (let [every-day (apply log/accumulate-days {} (vals by-day))]
    (for [week (partition 7 (date-keys now (* 7 weeks) tz))]
      (Math/round (double (policy/score config (aggregate every-day week)))))))

(defn tabulate
  "The standings as aligned lines, a heading first. The binary column is
  widened to fit the longest name, the rest being of known width."
  [rows]

  (let [width (apply max (count "binary") (map (comp count :binary) rows))
        line  (fn [& cells]
                (apply format (str "  %-" width "s  %8s  %9s  %6s  %6s") cells))]

    (cons (line "binary" "launches" "running" "score" "wait")
          (for [{:keys [binary launches duration score wait]} rows]
            (line binary
                  launches
                  (util/spoken duration)
                  score
                  (util/spoken (* 1000 wait)))))))

(defn trending
  "The weekly scores as a single line, oldest first.

  Labelled at both ends because a bare run of numbers gives a reader no
  way to tell which way time is running, and reading it backwards
  reverses the conclusion."
  [scores]

  (str "  earlier " (str/join " " (map #(format "%5d" %) scores)) "   now"))

(defn report
  "Print a human-readable report of usage and trends.

  Sessions are reaped first, so that one abandoned by a crash is charged
  up to its last sign of life rather than counted as having run ever
  since. Nothing is written back: a report anyone might run out of idle
  curiosity should not be able to change what it is reporting on."
  [config]

  (let [now    (System/currentTimeMillis)
        tz     (java.time.ZoneId/systemDefault)
        log    (-> (:log config)
                   store/read-log
                   (update :sessions log/reap store/pid-alive?))
        by-day (record log now tz)]

    (if (empty? by-day)
      (println "Nothing logged yet.")

      (do
        ; The two halves of the table are counted differently, and a
        ; reader given "5 launches" beside "score 7" with no explanation
        ; will reasonably take one of them for a mistake.
        (println (str (tty/ansi :bold) "The last 7 days, and what it costs you now:" (tty/ansi :reset)))
        (run! println (tabulate (standings config (:sessions log) by-day now tz 7)))

        (println)
        (println (str (tty/ansi :bold) "Scored by week:" (tty/ansi :reset)))
        (println (trending (trend config by-day now tz 4)))))))
