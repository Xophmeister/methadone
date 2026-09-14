; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.nag-test
  "What Methadone says, and how it says it."
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [methadone.fixtures :refer [config hour minute with-temp-dir]]
            [methadone.nag :as nag]))

(t/use-fixtures :each with-temp-dir)

(t/deftest spoken-spans
  (t/testing "seconds, while that is all there is"
    (t/is (= "45s" (nag/spoken (* 45 1000)))))

  (t/testing "minutes, once there are any"
    (t/is (= "20m" (nag/spoken (* 20 minute)))))

  (t/testing "hours and minutes together"
    (t/is (= "9h 11m" (nag/spoken (+ (* 9 hour) (* 11 minute))))))

  (t/testing "nothing at all still reads as a span"
    (t/is (= "0s" (nag/spoken 0)))))

(t/deftest accounting-for-the-wait
  ; Time spent is charged the next time round, so a nag that does not
  ; say what earned it teaches nothing: the one thing the user needs to
  ; connect is the session they left open to the wait they are sitting
  ; through now.
  (let [line (nag/summary config {:count 6.3 :duration (+ (* 9 hour) (* 11 minute))})]

    (t/testing "the launches are reported, rounded"
      (t/is (re-find #"6 launches" line)))

    (t/testing "and counted, a lone one not being 1 launches"
      (t/is (re-find #"1 launch," (nag/summary config {:count 1.0 :duration 0}))))

    (t/testing "so is the time spent, which is the whole point"
      (t/is (re-find #"9h 11m running" line)))

    (t/testing "and the score, it being what the wait is drawn from"
      (t/is (re-find #"score of 25" line)))))

(t/deftest climbing-the-ladder
  ; The rung is arrived at by arithmetic on an unbounded score, which is
  ; a thing to keep pinned: a wrong index is silent where it isn't fatal,
  ; and the worst of it only shows at the extremes nobody reaches by
  ; hand. Sweeping the range is cheaper than reasoning about it.
  (let [ladder  @#'nag/consternations
        rung    (zipmap ladder (range))
        chosen  #(nag/scorn config {:count (double %) :duration 0})
        climbed (mapv (comp rung chosen) (range 0 400 0.5))]

    (t/testing "every score lands on the ladder, none of them beside it"
      (t/is (every? some? climbed)))

    (t/testing "which is climbed, and never descended"
      (t/is (= climbed (sort climbed))))

    (t/testing "every rung of it, or the harshest words are never said"
      (t/is (= (set (range (count ladder))) (set climbed))))

    (t/testing "a clean slate is met with the mildest"
      (t/is (= (first ladder) (chosen 0))))

    (t/testing "and an absurd score with the sternest, not an index off the end"
      (t/is (= (last ladder) (chosen 1e6))))

    ; The anchors exist to tell these two apart, so the words must too:
    ; scolding both alike is what keying the rung to the wait would have
    ; done, the logistic being nearly flat across everyday usage.
    (t/testing "a light week and a heavy one are not scolded alike"
      (t/is (not= (chosen 8) (chosen 47))))

    (t/testing "and what is chosen is what the nag says"
      (t/is (str/includes? (with-out-str (nag/nag config {:count 8.0 :duration 0} 0))
                           (chosen 8))))))

(t/deftest commending-the-change-of-heart
  ; applaud draws one of these at the moment it fires and exits on the
  ; next line, so an empty list would throw inside a signal handler,
  ; during a wait, with nothing left to report it.
  (let [praise @#'nag/commendations]
    (t/testing "there is something to say"
      (t/is (seq praise)))

    (t/testing "and all of it is worth saying"
      (t/is (every? #(and (string? %) (seq (str/trim %))) praise)))))

(t/deftest drawing-only-on-a-terminal
  ; The countdown rewrites one line with cursor control. Redirected,
  ; that lands as escape sequences in somebody's log file, so the wait
  ; is taken in silence instead.
  (t/testing "given a terminal, the line is cleared even at zero seconds"
    (t/is (seq (with-out-str (#'nag/countdown 0 true)))))

  (t/testing "without one, nothing is written at all"
    (t/is (= "" (with-out-str (#'nag/countdown 0 false))))))
