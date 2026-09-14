; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.policy
  "What can be gleaned from the log: the sessions that bear on a
  decision, what they add up to and the friction they incur.")

(defn sessions-for
  "Returns all sessions for the given binary."
  [log binary]

  (into [] (get log binary)))

(defn usage
  "Weighs a binary's sessions by age, so that recent use counts for more
  than old. Weight decays exponentially with a mean lifetime of one
  window -- a half-life of window * ln 2, or a little under five days
  at a week -- so nothing is excluded outright and there is no cliff to
  sit out.

  The two terms are weighed differently on purpose: a launch is an
  instant and takes the weight of its moment, whereas time spent is a
  span and is integrated across the session, discounting its older part
  against its newer. Both are therefore fractional and time spent
  remains in milliseconds. A session left running converges on one
  window's worth, which is what bounds the cost of never closing one."
  [sessions now window]

  (let [weight (fn [t] (Math/exp (/ (- (min t now) now) window)))]
    (reduce (fn [{:keys [count duration]} {:keys [start end]}]
              (let [w-start (weight start)
                    w-end   (weight (max start (or end now)))]

                {:count    (+ count w-start)
                 :duration (+ duration (* window (- w-end w-start)))}))

            {:count 0.0 :duration 0.0}
            sessions)))

(defn score
  "Usage as a single figure, trading time spent against launches: a
  session running for one session-equivalent counts for as much as
  starting another. Both terms arrive already decayed by age."
  [{:keys [session-equivalent]} {:keys [count duration]}]

  (+ count (/ duration session-equivalent)))

(defn curve
  "Steepness and midpoint, solved for from the anchors. Inverting the
  sigmoid gives ln(f / (max - f)) at each; its gradient in score is the
  steepness and the score at which it vanishes is the midpoint."
  [{:keys [max-friction anchors]}]

  (let [[[u1 f1] [u2 f2]] anchors
        logit             (fn [f] (Math/log (/ f (- max-friction f))))
        k                 (/ (- (logit f2) (logit f1)) (- u2 u1))]

    {:steepness k
     :midpoint  (- u1 (/ (logit f1) k))}))

(defn friction
  "The wait a given usage has earned, in seconds. A logistic in the
  score: mild while usage is ordinary, steep once it is not and
  levelling off at max-friction so that the tool stays worth obeying
  rather than worth deleting."
  [config usage]

  (let [{:keys [steepness midpoint]} (curve config)]
    ; Rounded rather than truncated: the anchors are transcendental
    ; round-trips that land a hair below their own target and taking the
    ; floor would miss every one of them by a second.
    (Math/round (/ (double (:max-friction config))
                   (+ 1 (Math/exp (- (* steepness (- (score config usage)
                                                     midpoint)))))))))
