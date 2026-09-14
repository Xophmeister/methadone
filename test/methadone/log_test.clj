; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.log-test
  "Pure operations over the log."
  (:require [clojure.test :as t]
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
