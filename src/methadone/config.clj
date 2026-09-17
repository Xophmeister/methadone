; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.config
  "Every setting Methadone has and the reading and vetting of the
  methadone.edn that may turn them. Pure and impure are kept together
  here, because a setting and the checking of it belong side by side."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [methadone.util :refer [die]]))

(def ^:const defaults
  "Every setting Methadone has, at its factory value.

  Spans of history are milliseconds and a configuration may give them
  as either a bare number of those or an [n unit] pair. Waits are plain
  seconds throughout, being what one actually sits through."
  {:window             (* 7 24 60 60 1000)   ; The decay's mean lifetime
   :retention          (* 30 24 60 60 1000)  ; How long a session is kept
   :heartbeat          (* 60 1000)           ; Between proofs of life
   :session-equivalent (* 30 60 1000)        ; Runtime worth one launch
   :max-friction       1200                  ; The longest possible wait
   :anchors            [[10 10] [50 120]]    ; See below
   :log                nil})                 ; Defaults to the XDG path

; The friction curve is pinned by two opinions rather than by its own
; parameters: [score seconds] pairs saying what a light week and a heavy
; one ought to cost. Its steepness and midpoint fall out of them.

(def ^:const spans
  "The settings measured in milliseconds."
  [:window :retention :heartbeat :session-equivalent])

(def ^:const units
  "What a span may be written in, in milliseconds apiece."
  {:ms 1 :seconds 1000 :minutes 60000 :hours 3600000 :days 86400000})

(defn span
  "A span in milliseconds, from either a bare number of them or an
  [n unit] pair; nil from anything else, which the vetting reports."
  [value]

  (cond
    (number? value) value

    (and (vector? value)
         (= 2 (count value))
         (number? (first value))
         (contains? units (second value)))
    (* (first value) (units (second value)))))

(defn problems
  "Everything wrong with a configuration, as a list of complaints; empty
  means it is fit to use. Every fault is reported at once, so that
  correcting a file is not a guessing game one error at a time."
  [{:keys [max-friction anchors log] :as config}]

  (let [ceiling   (when (and (number? max-friction) (pos? max-friction))
                    max-friction)
        valid     #(let [ms (span (get config %))] (when (pos? (or ms 0)) ms))
        window    (valid :window)
        retention (valid :retention)]

    (concat
     (for [k (remove (set (keys defaults)) (keys config))]
       (str k " is not a setting Methadone has"))

     (for [k     spans
           :when (not (pos? (or (span (get config k)) 0)))]
       (str k " must be a positive span: milliseconds, or [n unit] with"
            " unit one of " (str/join ", " (sort (map name (keys units))))))

     (when (and window retention (< retention window))
       [":retention must be at least as long as :window"])

     (when-not ceiling
       [":max-friction must be a positive number of seconds"])

     (if-not (and (vector? anchors)
                  (= 2 (count anchors))
                  (every? #(and (vector? %) (= 2 (count %)) (every? number? %))
                          anchors))
       [":anchors must be two [score seconds] pairs"]

       (let [[[u1 f1] [u2 f2]] anchors]
         (concat
          (when-not (< u1 u2)
            [":anchors must be given in ascending order of score"])

          ; The logit is finite only strictly inside (0, max-friction).
          ; At the ceiling exactly it divides by zero and throws; at
          ; nothing, or beyond the ceiling, it gives NaN, which rounds
          ; to a wait of nothing at all. Failing wide open, in silence,
          ; is the one direction Methadone must not fail in.
          (when (and ceiling (not (< 0 f1 f2 ceiling)))
            [(str ":anchors must rise, cost more than nothing and stay"
                  " under :max-friction (" ceiling " s)")]))))

     (when-not (or (nil? log) (string? log))
       [":log must be a path, given as a string"]))))

(defn configure
  "Methadone's defaults, overlaid by the given configurations inside
  ascending order of precedence."
  [configs]

  (apply merge defaults (reverse configs)))

(defn resolved
  "A configuration with every span reduced to milliseconds, so that
  nothing downstream need care how it was written."
  [config]

  (reduce #(update %1 %2 span) config spans))

(defn config-files
  "Where a Methadone.edn may live, in descending order of precedence:
  the user's own config home first, then each entry of the given
  XDG_CONFIG_DIRS, so that a system-wide file may set a policy its users
  can still overrule."
  [config-home config-dirs]

  (cons (fs/path config-home "methadone.edn")
        (for [dir   (str/split (or config-dirs "/etc/xdg") #":")
              :when (seq dir)]
          (fs/path dir "methadone.edn"))))

(defn- log-file
  "Where the log lives: as configured, or else the XDG state path."
  [config]

  (str (or (:log config) (fs/path (fs/xdg-state-home "methadone") "log.edn"))))

(defn configure!
  "The effective configuration -- the defaults, overlaid by every
  methadone.edn on the search path, with its spans reduced and its log-file
  path settled -- or death listing everything wrong with it."
  []

  (let [read (fn [path]
               (try (edn/read-string (slurp (fs/file path)))
                    (catch Exception e
                      (die (str path " is not readable EDN: "
                                (ex-message e))))))

        config (configure (mapv read
                                (filter fs/exists?
                                        (config-files (fs/xdg-config-home)
                                                      (System/getenv "XDG_CONFIG_DIRS")))))]

    (when-let [faults (seq (problems config))]
      (apply die "Methadone cannot use its configuration:"
             (map #(str "  " %) faults)))

    (let [effective (resolved config)]
      (assoc effective :log (log-file effective)))))
