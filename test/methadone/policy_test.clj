; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.policy-test
  "Decay, accumulation and the friction that falls out of them."
  (:require [clojure.test :as t]
            [methadone.fixtures :refer [ago close? config hour ids integrate log minute now u with-temp-dir]]
            [methadone.log :as log]
            [methadone.policy :as policy]))

(t/use-fixtures :each with-temp-dir)

(t/deftest binary-selection
  (t/testing "other binaries are not considered"
    (t/is (not (contains? (ids) :other))))

  (t/testing "a binary that has never run has no sessions"
    (t/is (= [] (policy/sessions-for log "cursor"))))

  (t/testing "every session is offered, however old"
    ; Age is a weight now rather than a cutoff, so there is nothing left
    ; to filter on: even :ancient is handed over, to be discounted later.
    (t/is (= #{:brief :lengthy :running :long-run :ancient} (ids)))))

(t/deftest decay-weighting
  (t/testing "a launch this instant counts in full"
    (t/is (close? 1.0 (:count (policy/usage [{:start now}] now hour)))))

  (t/testing "a launch one window ago counts 1/e"
    ; The window is the mean lifetime of the decay, not a cutoff.
    (t/is (close? (/ 1 Math/E) (:count (policy/usage [{:start (ago hour)}] now hour)))))

  (t/testing "every further window divides the weight by e again"
    (t/is (close? (/ 1 (* Math/E Math/E))
                  (:count (policy/usage [{:start (ago (* 2 hour))}] now hour)))))

  (t/testing "the half-life falls at window * ln 2, as documented"
    ; Loosely, because the half-life is not a whole number of
    ; milliseconds and truncating it moves the answer more than the code
    ; ever would.
    (t/is (close? 0.5 (:count (policy/usage [{:start (ago (long (* hour (Math/log 2))))}]
                                            now hour))
                  1e-6))))

(t/deftest decayed-duration
  (t/testing "time spent is the weight integrated across the session"
    (let [start (ago (* 90 minute))
          end   (ago (* 30 minute))]
      (t/is (close? (integrate start end hour 10000)
                    (:duration (policy/usage [{:start start :end end}] now hour))
                    1e-6))))

  (t/testing "a session running up to now keeps 1 - 1/e of its last window"
    (t/is (close? (* hour (- 1 (/ 1 Math/E)))
                  (:duration (policy/usage [{:start (ago hour)}] now hour)))))

  (t/testing "a session left running forever converges on one window's worth"
    ; Its older end decays exactly as fast as its newer end accrues,
    ; which is what bounds the cost of never closing one.
    (t/is (close? (double hour) (:duration (policy/usage [{:start 0}] now hour)))))

  (t/testing "the same session is worth 1/e as much a window later"
    (let [session {:start (ago (* 90 minute)) :end (ago (* 30 minute))}
          spent   (fn [t] (:duration (policy/usage [session] t hour)))]

      (t/is (close? (/ (spent now) Math/E) (spent (+ now hour)))))))

(t/deftest accumulation
  (t/testing "both terms sum across every session for the binary"
    (let [apart (map #(policy/usage [%] now hour) (policy/sessions-for log "claude"))]
      (t/is (close? (reduce + (map :count apart)) (:count (u))))
      (t/is (close? (reduce + (map :duration apart)) (:duration (u))))))

  (t/testing "an ancient session still contributes, though barely"
    ; Five windows back is e^-5 of a launch: present, and negligible.
    (let [sessions (policy/sessions-for log "claude")
          without  (:count (policy/usage (remove #(= :ancient (:id %)) sessions) now hour))]

      (t/is (< without (:count (u))))
      (t/is (< (- (:count (u)) without) 0.01)))))

(t/deftest orphan-accounting
  ; A session abandoned by SIGKILL keeps :end nil, so an unreaped log
  ; still reads it as running and charges it right up to now. Nothing
  ; else bounds the claim, which is why -main reaps before it measures.
  (let [spent  (fn [log] (:duration (policy/usage (policy/sessions-for log "claude") now hour)))
        launch (fn [log] (:count (policy/usage (policy/sessions-for log "claude") now hour)))

        ; Orphaned: a dead PID, no :end, and a heartbeat two hours stale.
        orphan {"claude" [{:id    :orphan
                           :pid   2
                           :start (ago (* 3 hour))
                           :seen  (ago (* 2 hour))}]}
        reaped (log/reap orphan #{})]

    (t/testing "an unreaped orphan is charged as though it were still running"
      (t/is (close? (:duration (policy/usage [{:start (ago (* 3 hour))}] now hour))
                    (spent orphan))))

    (t/testing "reaping charges it only up to its last heartbeat"
      (t/is (close? (:duration (policy/usage [{:start (ago (* 3 hour))
                                               :end   (ago (* 2 hour))}] now hour))
                    (spent reaped))))

    (t/testing "which is far less, there being no window to fall out of"
      ; The decay discounts the dead hours rather than excluding them,
      ; so the saving is a ratio now rather than all or nothing.
      (t/is (< (* 5 (spent reaped)) (spent orphan))))

    (t/testing "the count is unmoved either way, depending only on :start"
      (t/is (close? (launch orphan) (launch reaped))))))

(t/deftest degenerate-cases
  (t/testing "empty log"
    (t/is (= {:count 0.0 :duration 0.0} (policy/usage (policy/sessions-for {} "claude") now hour))))

  (t/testing "a clock-skewed session starting in the future never goes negative"
    (t/is (= {:count 1.0 :duration 0.0} (policy/usage [{:start (+ now (* 5 minute))}] now hour))))

  (t/testing "an end before its start never goes negative either"
    (t/is (= 0.0 (:duration (policy/usage [{:start (ago minute) :end (ago (* 5 minute))}]
                                          now hour))))))

(t/deftest exchange-rate
  (t/testing "an unused window scores nothing"
    (t/is (close? 0.0 (policy/score config {:count 0.0 :duration 0.0}))))

  (t/testing "a session-equivalent of runtime counts as another launch"
    (t/is (close? (policy/score config {:count 2.0 :duration 0.0})
                  (policy/score config {:count 0.0 :duration (* 2 (:session-equivalent config))}))))

  (t/testing "the two terms simply add"
    (t/is (close? 3.5 (policy/score config {:count 3.0 :duration (/ (:session-equivalent config) 2)}))))

  (t/testing "leaving one session open outweighs launching another"
    ; The whole point of the change: an eight-hour session is sixteen
    ; session-equivalents, against the one it cost to start.
    (t/is (< (policy/score config {:count 2.0 :duration 0.0})
             (policy/score config {:count 1.0 :duration (* 8 hour)})))))

(t/deftest friction-curve
  (t/testing "the anchors are met exactly"
    ; They are the two opinions the curve is solved from -- what a light
    ; week and a heavy one ought to cost -- so they must come back whole.
    (doseq [[usage seconds] (:anchors config)]
      (t/is (= seconds (policy/friction config {:count usage :duration 0})))))

  (t/testing "an unused window still costs a few seconds"
    (t/is (= 5 (policy/friction config {:count 0 :duration 0}))))

  (t/testing "the wait never falls as usage rises"
    (let [waits (mapv #(policy/friction config {:count % :duration 0}) (range 0 200 5))]
      (t/is (= waits (vec (sort waits))))
      (t/is (< (first waits) (last waits)))))

  (t/testing "the wait levels off at max-friction, however absurd the usage"
    ; Bounded on purpose: a wait long enough to be worth circumventing
    ; buys no deterrence at all, the bypass being a single rm.
    (t/is (= (:max-friction config) (policy/friction config {:count 1000 :duration 0})))
    (t/is (= (:max-friction config) (policy/friction config {:count 1e9 :duration 0}))))

  (t/testing "time spent tells, where once it was ignored"
    (t/is (< (policy/friction config {:count 1 :duration 0})
             (policy/friction config {:count 1 :duration (* 5 hour)}))))

  (t/testing "the leave-it-open habit is punished hardest"
    ; A week of each, at steady state: light use, many short sessions,
    ; and one long session a day. The last is what the duration term
    ; exists to catch, and it must come out worst.
    (let [light   (policy/friction config {:count 5 :duration (* 5 20 minute)})
          churner (policy/friction config {:count 40 :duration (* 40 5 minute)})
          gamer   (policy/friction config {:count 7 :duration (* 7 8 hour)})]

      (t/is (< light churner gamer))))

  (t/testing "the wait is integral, being both formatted and slept on"
    ; (format "%2d" 5.0) throws
    (t/is (integer? (policy/friction config {:count 3 :duration 0})))))
