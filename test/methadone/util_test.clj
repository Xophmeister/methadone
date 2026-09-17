(ns methadone.util-test
  "Utility tests."
  (:require [clojure.test :as t]
            [methadone.fixtures :refer [hour minute]]
            [methadone.util :as util]))

(t/deftest dating-by-tz
  (let [london   (java.time.ZoneId/of "Europe/London")
        new-york (java.time.ZoneId/of "America/New_York")

        before-midnight (-> "2026-09-14T23:30:00Z" java.time.Instant/parse .toEpochMilli)
        after-midnight  (-> "2026-09-14T00:30:00Z" java.time.Instant/parse .toEpochMilli)]

    (t/testing "starting before midnight is dated the next day in London"
      (t/is (= "2026-09-15" (.toString (util/epoch->date before-midnight london)))))

    (t/testing "starting before midnight is dated the same day in New York"
      (t/is (= "2026-09-14" (.toString (util/epoch->date before-midnight new-york)))))

    (t/testing "starting after midnight is dated the same day in London"
      (t/is (= "2026-09-14" (.toString (util/epoch->date after-midnight london)))))

    (t/testing "starting after midnight is dated the previous day in New York"
      (t/is (= "2026-09-13" (.toString (util/epoch->date after-midnight new-york)))))))

(t/deftest spoken-spans
  (t/testing "seconds, while that is all there is"
    (t/is (= "45s" (util/spoken (* 45 1000)))))

  (t/testing "minutes, once there are any"
    (t/is (= "20m" (util/spoken (* 20 minute)))))

  (t/testing "hours and minutes together"
    (t/is (= "9h 11m" (util/spoken (+ (* 9 hour) (* 11 minute))))))

  (t/testing "nothing at all still reads as a span"
    (t/is (= "0s" (util/spoken 0)))))
