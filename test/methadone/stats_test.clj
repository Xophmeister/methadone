; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.stats-test
  "Reading the log back: what it says about a habit, rather than what it
  charges for one."
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [methadone.fixtures :refer [config hour minute]]
            [methadone.stats :as stats]))

; A fixed moment and an explicit zone, so that every date below is a
; date rather than whatever the machine happens to think today is.
; London runs an hour ahead of UTC in September.
(def london (java.time.ZoneId/of "Europe/London"))
(def now (.toEpochMilli (java.time.Instant/parse "2026-09-17T12:00:00Z")))

(def by-day
  "A record of days, as record returns one: two binaries, three of the
  days inside the last week and one well outside it."
  {"claude"  {"2026-09-17" {:count 2 :duration (* 2 hour)}
              "2026-09-14" {:count 1 :duration hour}
              "2026-09-05" {:count 3 :duration (* 3 hour)}}
   "copilot" {"2026-09-16" {:count 1 :duration (* 30 minute)}}})

;; The record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest recording
  (let [at  #(.toEpochMilli (java.time.Instant/parse %))
        ran (fn [id from to] {:id id :start (at from) :end (at to)})

        log {:version  1
             :history  {"claude"  {"2026-09-14" {:count 2 :duration (* 2 hour)}}
                        "copilot" {"2026-09-13" {:count 1 :duration (* 10 minute)}}}
             :sessions {"claude" [(ran :same-day "2026-09-14T09:00:00Z" "2026-09-14T10:00:00Z")
                                  (ran :later    "2026-09-16T09:00:00Z" "2026-09-16T09:30:00Z")]}}

        made (@#'stats/record log now london)]

    (t/testing "the archived half is there"
      (t/is (= {:count 1 :duration (* 10 minute)} (get-in made ["copilot" "2026-09-13"]))))

    (t/testing "and so is the live half, tallied into the same shape"
      (t/is (= {:count 1 :duration (* 30 minute)} (get-in made ["claude" "2026-09-16"]))))

    ; Reachable in practice: sessions begun on one day end at different
    ; times, so around the retention boundary some of that day are
    ; archived while the rest are still held in full. A merge shallower
    ; than accumulate-history would let the live ones stand in place of
    ; the archived, and the day would quietly under-report.
    (t/testing "a day present in both halves is summed, not replaced"
      (t/is (= {:count 3 :duration (* 3 hour)} (get-in made ["claude" "2026-09-14"]))))

    (t/testing "a log with nothing in it records nothing"
      (t/is (= {} (@#'stats/record {:version 1 :history {} :sessions {}} now london))))))

;; Windowing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest dating

  (t/testing "a day is asked for per day wanted"
    (t/is (= 7 (count (@#'stats/date-keys now 7 london)))))

  (t/testing "running oldest to newest, ending today"
    ; The trend partitions these into weeks and prints them in the order
    ; they arrive, so reversing them would reverse its conclusion.
    (t/is (= ["2026-09-11" "2026-09-12" "2026-09-13" "2026-09-14"
              "2026-09-15" "2026-09-16" "2026-09-17"]
             (vec (@#'stats/date-keys now 7 london)))))

  (t/testing "in the zone given, not in UTC"
    ; 23:30Z is already tomorrow in London, so a zone-blind reading
    ; would end this run a day short.
    (let [late (.toEpochMilli (java.time.Instant/parse "2026-09-17T23:30:00Z"))]
      (t/is (= "2026-09-18" (last (@#'stats/date-keys late 3 london))))
      (t/is (= "2026-09-17" (last (@#'stats/date-keys late 3 (java.time.ZoneId/of "UTC"))))))))

(t/deftest aggregating
  (let [days (get by-day "claude")]

    (t/testing "the days named are summed"
      (t/is (= {:count 3 :duration (* 3 hour)}
               (@#'stats/aggregate days ["2026-09-14" "2026-09-17"]))))

    (t/testing "and the days not named are left out"
      (t/is (= {:count 2 :duration (* 2 hour)}
               (@#'stats/aggregate days ["2026-09-17"]))))

    (t/testing "a day with nothing in it contributes nothing"
      ; Quiet days are absent from the record rather than zero, which is
      ; why they need no special case here.
      (t/is (= {:count 2 :duration (* 2 hour)}
               (@#'stats/aggregate days ["2026-09-17" "2026-09-12" "2026-09-13"]))))

    (t/testing "and asking for none at all still gives a total, not nothing"
      ; Otherwise every caller would have to cope with nil before it
      ; could score what came back.
      (t/is (= {:count 0 :duration 0} (@#'stats/aggregate days []))))

    (t/testing "the dates need be neither contiguous nor in order"
      (t/is (= (@#'stats/aggregate days ["2026-09-14" "2026-09-17"])
               (@#'stats/aggregate days ["2026-09-17" "2026-09-14"]))))))

;; The report's figures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest standing
  ; The two halves of a row are counted differently on purpose: the
  ; launches and the time are a plain tally of whole days, the score and
  ; the wait are decayed. These sessions make them disagree, so that a
  ; row quietly reporting one where it means the other would show.
  (let [sessions {"claude" [{:id :live :start (- now (* 3 hour)) :end (- now hour)}]}
        rows     (stats/standings config sessions by-day now london 7)
        row      (fn [binary] (first (filter #(= binary (:binary %)) rows)))]

    (t/testing "one row per binary in the record"
      (t/is (= ["claude" "copilot"] (mapv :binary rows))))

    (t/testing "sorted, so that the same log reports the same way twice"
      (t/is (= (mapv :binary rows) (sort (mapv :binary rows)))))

    (t/testing "the launches are a plain count over the days asked for"
      ; Two on the 17th and one on the 14th; the three on the 5th fall
      ; outside the week and are not counted.
      (t/is (= 3 (:launches (row "claude")))))

    (t/testing "and the time likewise"
      (t/is (= (* 3 hour) (:duration (row "claude")))))

    (t/testing "while the score comes from the live sessions, decayed"
      ; One session of two hours: a launch, plus four session-equivalents
      ; of running. Nothing to do with the three launches beside it.
      (t/is (= 5 (:score (row "claude")))))

    (t/testing "and the wait is what that score currently earns"
      (t/is (= 7 (:wait (row "claude")))))

    (t/testing "a binary with a history but nothing running scores nothing"
      ; The halves are independent: copilot was used this week and is
      ; still reported, but owes nothing at this moment.
      (t/is (= 1 (:launches (row "copilot"))))
      (t/is (= 0 (:score (row "copilot")))))

    (t/testing "and every figure is whole, being meant to be read"
      (t/is (every? integer? (mapcat (juxt :launches :duration :score :wait) rows))))))

(t/deftest trending-by-week
  (let [weeks (stats/trend config by-day now london 4)]

    (t/testing "a score per week asked for"
      (t/is (= 4 (count weeks))))

    (t/testing "oldest first"
      ; The last of them covers the days the rows above are drawn from,
      ; so it is the one a reader can check by eye.
      (t/is (= [0 0 9 11] (vec weeks))))

    (t/testing "a week with nothing in it scores nothing"
      (t/is (= [0 0] (vec (take 2 weeks)))))

    (t/testing "and the binaries are collapsed, not reported apart"
      ; The last week holds claude's 3 launches over 3 hours (a score of
      ; nine) and copilot's one over half an hour (a score of two).
      (t/is (= 11 (last weeks))))

    (t/testing "asking for fewer weeks asks about a shorter span"
      (t/is (= [9 11] (vec (stats/trend config by-day now london 2)))))))

;; Presentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest tabulating
  (let [rows  [{:binary "claude" :launches 3 :duration (* 3 hour) :score 5 :wait 7}
               {:binary "a-very-long-binary-name" :launches 1 :duration minute :score 0 :wait 5}]
        lines (stats/tabulate rows)]

    (t/testing "a heading, then a line per row"
      (t/is (= 3 (count lines)))
      (t/is (str/includes? (first lines) "launches")))

    (t/testing "the longest name still fits its column"
      ; Otherwise the columns shear apart at exactly the moment somebody
      ; wraps a tool with a long name.
      (t/is (str/includes? (nth lines 2) "a-very-long-binary-name")))

    (t/testing "and every line is ruled to the same width"
      (t/is (= 1 (count (set (map count lines))))))

    (t/testing "durations are spoken, not left as milliseconds"
      (t/is (str/includes? (nth lines 1) "3h 0m"))
      (t/is (not (str/includes? (nth lines 1) (str (* 3 hour))))))

    (t/testing "and so is the wait, which arrives in seconds rather than in milliseconds"
      ; spoken takes milliseconds; friction gives seconds. Handing it
      ; the one for the other reports every wait as none at all.
      (t/is (str/includes? (nth lines 1) "7s")))

    (t/testing "no rows at all still gives a heading rather than a failure"
      ; The width comes from the longest name, and there is no longest
      ; name here.
      (t/is (= 1 (count (stats/tabulate [])))))))

(t/deftest trending-aloud
  (let [line (stats/trending [5 0 0 23])]

    (t/testing "every score is there, in the order given"
      (t/is (= [5 0 0 23]
               (mapv parse-long (re-seq #"\d+" line)))))

    (t/testing "and which end is which is said, not left to be guessed"
      ; Read the wrong way round, a falling trend is a rising one.
      (t/is (str/includes? line "earlier"))
      (t/is (str/includes? line "now"))
      (t/is (< (str/index-of line "earlier") (str/index-of line "now"))))))
