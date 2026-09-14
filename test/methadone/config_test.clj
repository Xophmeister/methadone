; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.config-test
  "Settings, their vetting and where they are read from."
  (:require [clojure.test :as t]
            [methadone.config :as config]
            [methadone.fixtures :refer [config day minute with-temp-dir]]
            [methadone.policy :as policy]))

(t/use-fixtures :each with-temp-dir)

(t/deftest span-parsing
  (t/testing "a bare number is already milliseconds"
    (t/is (= 500 (config/span 500))))

  (t/testing "an [n unit] pair is converted"
    (t/is (= (* 7 day) (config/span [7 :days])))
    (t/is (= (* 30 minute) (config/span [30 :minutes])))
    (t/is (= 250 (config/span [250 :ms]))))

  (t/testing "anything else is nothing, for the vetting to complain about"
    (t/is (nil? (config/span [7 :fortnights])))
    (t/is (nil? (config/span "7 days")))
    (t/is (nil? (config/span [7])))
    (t/is (nil? (config/span nil)))))

(t/deftest configuration-precedence
  (t/testing "with nothing found, the defaults stand"
    (t/is (= config/defaults (config/configure []))))

  (t/testing "a partial file overrides only what it names"
    (let [merged (config/configure [{:window [1 :days]}])]
      (t/is (= [1 :days] (:window merged)))
      (t/is (= (:max-friction config/defaults) (:max-friction merged)))))

  (t/testing "the earlier file wins, being the more particular"
    ; config-files lists the user's own first, so a system-wide policy
    ; is something to be overruled rather than something that overrules.
    (t/is (= [1 :days] (:window (config/configure [{:window [1 :days]}
                                                   {:window [2 :days]}])))))

  (t/testing "spans are reduced once the configuration has settled"
    (t/is (= (* 7 day)
             (:window (config/resolved (config/configure [{:window [7 :days]}])))))))

(t/deftest configuration-search-path
  (t/testing "the user's own configuration comes first"
    (t/is (= ["/home/me/.config/methadone.edn" "/etc/xdg/methadone.edn"]
             (mapv str (config/config-files "/home/me/.config" "/etc/xdg")))))

  (t/testing "every XDG_CONFIG_DIRS entry follows, in the order given"
    (t/is (= ["/home/me/.config/methadone.edn"
              "/etc/xdg/methadone.edn"
              "/nix/etc/xdg/methadone.edn"]
             (mapv str (config/config-files "/home/me/.config"
                                            "/etc/xdg:/nix/etc/xdg")))))

  (t/testing "an unset XDG_CONFIG_DIRS falls back to the specified default"
    (t/is (= ["/home/me/.config/methadone.edn" "/etc/xdg/methadone.edn"]
             (mapv str (config/config-files "/home/me/.config" nil))))))

(t/deftest configuration-guards
  (t/testing "the defaults are themselves acceptable"
    ; Not a tautology: the vetting is quite capable of disagreeing with
    ; the values shipped beside it.
    (t/is (empty? (config/problems config/defaults))))

  (t/testing "a misspelled setting is caught rather than quietly ignored"
    (t/is (seq (config/problems (config/configure [{:windwo (* 7 day)}])))))

  (t/testing "a span must parse, and must be positive"
    (t/is (seq (config/problems (config/configure [{:window [7 :fortnights]}]))))
    (t/is (seq (config/problems (config/configure [{:retention -1}])))))

  (t/testing "anchors must be two well-formed pairs"
    (t/is (seq (config/problems (config/configure [{:anchors [1 2]}]))))
    (t/is (seq (config/problems (config/configure [{:anchors [[10 10]]}])))))

  (t/testing "anchors must ascend in score"
    (t/is (seq (config/problems (config/configure [{:anchors [[50 120] [10 10]]}])))))

  (t/testing "an anchor outside (0, max-friction) is refused"
    (doseq [anchors [[[10 0] [50 120]]     ; No wait at all
                     [[10 10] [50 1200]]   ; The ceiling itself
                     [[10 10] [50 1500]]]] ; Beyond it
      (t/is (seq (config/problems (config/configure [{:anchors anchors}]))))))

  (t/testing "a log path must be a path"
    (t/is (seq (config/problems (config/configure [{:log 42}])))))

  (t/testing "every fault is reported at once, not one per attempt"
    (t/is (< 1 (count (config/problems (config/configure [{:windwo 1 :retention -1}])))))))

(t/deftest failing-wide-open
  ; What the anchor guard is really for. Outside (0, max-friction) the
  ; logit is not finite, and the wait that falls out of it is nothing at
  ; all: Methadone quietly disabling itself, which is the one direction
  ; it must never fail in.
  (t/testing "an anchor beyond the ceiling would earn no wait whatsoever"
    (t/is (zero? (policy/friction (assoc config :anchors [[10 10] [50 1500]])
                                  {:count 500 :duration 0}))))

  (t/testing "an anchor of no wait at all would do the same"
    (t/is (zero? (policy/friction (assoc config :anchors [[10 0] [50 120]])
                                  {:count 500 :duration 0}))))

  (t/testing "and every one of them is refused before it can"
    (doseq [anchors [[[10 10] [50 1500]] [[10 0] [50 120]] [[10 10] [50 1200]]]]
      (t/is (seq (config/problems (assoc config :anchors anchors)))))))
