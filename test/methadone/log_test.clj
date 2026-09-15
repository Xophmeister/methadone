; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.log-test
  "Pure operations over the log."
  (:require [clojure.set :as set]
            [clojure.test :as t]
            [methadone.fixtures :refer [ago hour minute now session with-temp-dir]]
            [methadone.log :as log]))

(t/use-fixtures :each with-temp-dir)

(t/deftest reaping
  ; alive? is injected precisely so that this needs no real processes: a
  ; set of live PIDs is a perfectly good liveness predicate.
  (let [before {"claude"  [{:id :live :pid 1 :start (ago hour)}
                           {:id :gone :pid 2 :start (ago hour)}
                           {:id :beat :pid 3 :start (ago hour) :seen (ago (* 10 minute))}
                           {:id :ended :pid 4 :start (ago hour) :end (ago (* 30 minute))}]
                "copilot" [{:id :nopid :start (ago hour)}]}
        after  (log/reap before #{1})]

    (t/testing "a session whose process is alive is left running"
      (t/is (nil? (:end (session after :live)))))

    (t/testing "a session whose process is gone is closed at its own start"
      (t/is (= (ago hour) (:end (session after :gone)))))

    (t/testing "a session with a heartbeat is charged up to its last beat"
      (t/is (= (ago (* 10 minute)) (:end (session after :beat)))))

    (t/testing "the heartbeat is dropped once an end is known, being redundant"
      (t/is (not (contains? (session after :beat) :seen))))

    (t/testing "a session that already ended is untouched"
      (t/is (= (ago (* 30 minute)) (:end (session after :ended)))))

    (t/testing "a session with no PID at all is treated as gone"
      (t/is (some? (:end (session after :nopid)))))

    (t/testing "reaping is idempotent"
      (t/is (= after (log/reap after #{1}))))))

(t/deftest dating-by-tz
  (let [london          (java.time.ZoneId/of "Europe/London")
        newyork         (java.time.ZoneId/of "America/New_York")
        before-midnight (-> "2026-09-14T23:30:00Z" java.time.Instant/parse .toEpochMilli)
        after-midnight  (-> "2026-09-14T00:30:00Z" java.time.Instant/parse .toEpochMilli)]

    (t/testing "a session that started before midnight is dated the next day in London"
      (t/is (= "2026-09-15" (@#'log/session-date {:start before-midnight} london))))

    (t/testing "a session that started before midnight is dated the same day in New York"
      (t/is (= "2026-09-14" (@#'log/session-date {:start before-midnight} newyork))))

    (t/testing "a session that started after midnight is dated the same day in London"
      (t/is (= "2026-09-14" (@#'log/session-date {:start after-midnight} london))))

    (t/testing "a session that started after midnight is dated the previous day in New York"
      (t/is (= "2026-09-13" (@#'log/session-date {:start after-midnight} newyork))))))

(t/deftest tallying
  (let [london (java.time.ZoneId/of "Europe/London")
        at     #(-> % java.time.Instant/parse .toEpochMilli)
        noon   (at "2026-09-15T12:00:00Z")

        ran (fn [id from to]
              (cond-> {:id id :start (at from)}
                to (assoc :end (at to))))

        ; London runs an hour ahead of UTC in September, so 22:30Z is
        ; half past eleven at night and 23:30Z is half past midnight.
        sessions {"claude"  [(ran :morning   "2026-09-14T09:00:00Z" "2026-09-14T10:00:00Z")
                             (ran :afternoon "2026-09-14T14:00:00Z" "2026-09-14T14:30:00Z")
                             (ran :overnight "2026-09-14T22:30:00Z" "2026-09-14T23:30:00Z")
                             (ran :running   "2026-09-15T08:00:00Z" nil)]
                  "copilot" [(ran :other     "2026-09-14T12:00:00Z" "2026-09-14T13:00:00Z")]}

        tallied (log/tally sessions noon london)]

    (t/testing "sessions on the same day are counted together"
      (t/is (= 3 (get-in tallied ["claude" "2026-09-14" :count]))))

    (t/testing "and their durations summed"
      ; An hour, a half-hour and an hour.
      (t/is (= (* 150 minute) (get-in tallied ["claude" "2026-09-14" :duration]))))

    (t/testing "a different day is a different bucket"
      (t/is (= 1 (get-in tallied ["claude" "2026-09-15" :count]))))

    (t/testing "and a different binary is kept apart from the first"
      (t/is (= {"2026-09-14" {:count 1 :duration hour}} (get tallied "copilot"))))

    (t/testing "a session that spans midnight belongs wholly to the day it began"
      ; It ran from half past eleven to half past midnight, so charging
      ; it to the day it ended would move an hour from the 14th to the
      ; 15th and count a session nobody started that day.
      (t/is (= 3 (get-in tallied ["claude" "2026-09-14" :count])))
      (t/is (= 1 (get-in tallied ["claude" "2026-09-15" :count]))))

    (t/testing "a session still running is measured up to now"
      ; Four hours so far, and no :end to read it from -- the case that
      ; makes now a parameter rather than something tally fetches.
      (t/is (= (* 4 hour) (get-in tallied ["claude" "2026-09-15" :duration]))))

    (t/testing "the zone given is the zone used, not UTC and not the machine's"
      ; Both instants are the 14th in UTC, so a tally that ignored its
      ; zone would agree with both of these and be wrong about each.
      (let [on (fn [iso zone]
                 (keys (get (log/tally {"claude" [{:start (at iso) :end (+ (at iso) minute)}]}
                                       noon
                                       (java.time.ZoneId/of zone))
                            "claude")))]

        (t/is (= ["2026-09-15"] (on "2026-09-14T23:30:00Z" "Europe/London")))
        (t/is (= ["2026-09-13"] (on "2026-09-14T02:00:00Z" "America/New_York")))))

    (t/testing "nothing in gives nothing out"
      (t/is (= {} (log/tally {} noon london))))

    (t/testing "and a binary with nothing left to show is dropped entirely"
      ; Otherwise the history slowly fills with binaries holding no days.
      (t/is (= {} (log/tally {"claude" []} noon london))))))

(t/deftest pruning
  (let [before {"claude"  [{:id :recent :start (ago (* 2 hour)) :end (ago (* 30 minute))}
                           {:id :expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}
                           {:id :old-run :start (ago (* 9 hour))}]
                "copilot" [{:id :also-expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}]}
        after  (log/prune before now hour)]

    (t/testing "a session that ended inside the retention period is kept"
      (t/is (some? (session after :recent))))

    (t/testing "a session that ended before it is dropped"
      (t/is (nil? (session after :expired))))

    (t/testing "a running session is kept however old, since it has yet to end"
      (t/is (some? (session after :old-run))))

    (t/testing "every binary is pruned, not just the first"
      (t/is (nil? (session after :also-expired))))))

(t/deftest expiring
  ; The complement of pruning, and the reason pruning no longer simply
  ; destroys what it drops: what falls out here is what gets tallied
  ; into the history before it goes.
  (let [before {"claude"  [{:id :recent :start (ago (* 2 hour)) :end (ago (* 30 minute))}
                           {:id :expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}
                           {:id :old-run :start (ago (* 9 hour))}]
                "copilot" [{:id :also-expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}]}

        gone (log/expired before now hour)
        ids  (fn [log] (set (map :id (mapcat val log))))]

    (t/testing "a session that ended before the retention period is returned"
      (t/is (some? (session gone :expired))))

    (t/testing "one that ended inside it is not"
      (t/is (nil? (session gone :recent))))

    (t/testing "and a running session never is, however old, having yet to end"
      ; It has no end to have expired at, and it is still accruing time.
      (t/is (nil? (session gone :old-run))))

    (t/testing "every binary is considered, not just the first"
      (t/is (some? (session gone :also-expired))))

    ; The invariant that makes the pair safe. Were the two ever to
    ; disagree, a session could fall through both -- deleted by the
    ; prune without ever having been counted by the tally -- and the
    ; only evidence would be a history quietly missing a day's work.
    ;
    ; Swept across retentions rather than asserted at one, because a
    ; single value only exercises the disagreement if a session happens
    ; to fall in the disputed window, and a fixture cannot promise that
    ; for a drift nobody has introduced yet.
    (t/testing "every session is either kept or expired, at any retention"
      (doseq [retention [(* 15 minute) (* 45 minute) hour
                         (* 3 hour) (* 6 hour) (* 24 hour)]]

        (let [k (ids (log/prune before now retention))
              g (ids (log/expired before now retention))]

          (t/is (= (ids before) (into k g)) "none lost between the two")
          (t/is (empty? (set/intersection k g)) "and none counted by both"))))

    (t/testing "expiring nothing leaves nothing to expire"
      (t/is (= {"claude" [] "copilot" []} (log/expired before now (* 24 hour)))))))

(t/deftest opening
  (t/testing "the first session for a binary creates a vector, not a list"
    (t/is (vector? (get (log/open {} "claude" {:id :a}) "claude"))))

  (t/testing "further sessions are appended in order"
    (t/is (= [{:id :a} {:id :b}]
             (get (-> {} (log/open "claude" {:id :a}) (log/open "claude" {:id :b})) "claude"))))

  (t/testing "other binaries are left alone"
    (t/is (= [{:id :c}]
             (get (log/open {"copilot" [{:id :c}]} "claude" {:id :a}) "copilot")))))

(t/deftest touching
  (let [before {"claude"  [{:id :a :start (ago hour)}
                           {:id :b :start (ago hour)}
                           {:id :done :start (ago hour) :end (ago (* 30 minute))}]
                "copilot" [{:id :c :start (ago hour)}]}]

    (t/testing "the matching session gains a heartbeat"
      (t/is (= now (:seen (session (log/touch before :a now) :a)))))

    (t/testing "no other session is touched"
      (t/is (nil? (:seen (session (log/touch before :a now) :b)))))

    (t/testing "sessions are found under any binary"
      (t/is (= now (:seen (session (log/touch before :c now) :c)))))

    (t/testing "a later heartbeat replaces an earlier one"
      (t/is (= now (:seen (session (-> before
                                       (log/touch :a (ago minute))
                                       (log/touch :a now))
                                   :a)))))

    (t/testing "a session that has already ended gains no heartbeat"
      (t/is (nil? (:seen (session (log/touch before :done now) :done)))))

    (t/testing "an unknown ID is ignored"
      (t/is (= before (log/touch before :nonesuch now))))))

(t/deftest closing
  (let [before {"claude"  [{:id :a :start (ago hour) :seen (ago minute)}
                           {:id :b :start (ago hour)}]
                "copilot" [{:id :c :start (ago hour)}]}]

    (t/testing "the matching session gains an :end"
      (t/is (= now (:end (session (log/close before :a now) :a)))))

    (t/testing "no other session is touched"
      (t/is (nil? (:end (session (log/close before :a now) :b)))))

    (t/testing "sessions are found under any binary"
      (t/is (= now (:end (session (log/close before :c now) :c)))))

    (t/testing "the first close wins, so a second cannot inflate the duration"
      (t/is (= now (:end (session (-> before (log/close :a now) (log/close :a (+ now hour))) :a)))))

    (t/testing "closing drops the heartbeat, which the :end supersedes"
      (t/is (not (contains? (session (log/close before :a now) :a) :seen))))

    (t/testing "an unknown ID is ignored"
      (t/is (= before (log/close before :nonesuch now))))))
