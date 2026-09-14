; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.fixtures
  "Values and helpers the test namespaces share."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [methadone.config :as config]
            [methadone.policy :as policy]))

(def minute (* 60 1000))
(def hour (* 60 minute))
(def day (* 24 hour))

; The pure layers take now as an argument, so any epoch will do.
(def now 1000000000000)
(defn ago [ms] (- now ms))

; Persistence is another matter: update-log! reaps and prunes on every
; transaction, so a log of 1970 timestamps deletes itself on first touch.
(defn recently [ms] (- (System/currentTimeMillis) ms))

(def config
  "Methadone's own defaults, which the policy tests are written against.
  They are already in milliseconds, so they need no resolving."
  config/defaults)

(def log
  {"claude"
   [{:id :brief :start (ago (* 50 minute)) :end (ago (* 40 minute))}  ; ran 10m, ended 40m ago
    {:id :lengthy :start (ago (* 90 minute)) :end (ago (* 30 minute))}  ; ran 60m, ended 30m ago
    {:id :running :start (ago (* 20 minute))}                           ; 20m so far, still going
    {:id :long-run :start (ago (* 3 hour))}                              ; 3h so far, still going
    {:id :ancient :start (ago (* 5 hour)) :end (ago (* 4 hour))}]    ; ran 60m, ended 4h ago

   "copilot"
   [{:id :other :start (ago (* 10 minute)) :end (ago (* 5 minute))}]})

(defn ids [] (set (map :id (policy/sessions-for log "claude"))))
(defn u [] (policy/usage (policy/sessions-for log "claude") now hour))

(defn close?
  "Whether two figures agree. The decayed terms are transcendental, so
  exact equality is not on offer; the tolerance is relative, save near
  zero where it falls back to absolute."
  ([expected actual] (close? expected actual 1e-9))
  ([expected actual tolerance]
   (<= (Math/abs (- (double expected) (double actual)))
       (* tolerance (Math/max 1.0 (Math/abs (double expected)))))))

(defn integrate
  "The decay weight integrated across a span by the midpoint rule: a
  check on usage's closed form that is independent of it, the two
  agreeing only if that form really is the integral it claims to be."
  [start end window steps]

  (let [h (/ (double (- end start)) steps)]
    (* h (reduce (fn [total i]
                   (+ total (Math/exp (/ (- (+ start (* h (+ i 0.5))) now)
                                         window))))
                 0.0
                 (range steps)))))

(defn session
  "The session with the given ID, wherever in the log it appears."
  [log id]

  (first (filter #(= id (:id %)) (mapcat val log))))

(defn own-pid [] (.pid (java.lang.ProcessHandle/current)))

(defn dead-pid
  "A PID that is no longer in use: run a trivial process to completion
  and take its PID. (Strictly this races with PID recycling, but not
  within the lifetime of a test run.)"
  []

  (let [proc (p/process ["true"])]
    @proc
    (.pid (:proc proc))))

(def bb
  "Babashka's own path, so that a subprocess can still be started under
  an environment that has no PATH to find it by."
  (str (fs/which "bb")))

(defn bb-eval
  "Evaluate the given code in a fresh babashka process, under exactly
  the given environment.

  Resolution reads PATH and METHADONE_BINARY from the environment, which
  cannot be changed in place, so these are the only tests that need a
  process of their own."
  [env code]

  (let [{:keys [exit out err]} @(p/process [bb "--classpath" (str (fs/absolutize "src")) "-e" code]
                                           {:env env :out :string :err :string})]
    {:exit exit :out out :err err}))

; Each test gets its own state directory, so that they cannot interfere
; through the filesystem and none of them touch the real log.
(def ^:dynamic *dir* nil)
(defn log-path [] (str (fs/path *dir* "log.edn")))

(defn store
  "A configuration pointing at this test's own log, so that the
  persistence layer has somewhere private to write."
  []

  (assoc config :log (log-path)))

(defn with-temp-dir [f]
  (let [dir (fs/create-temp-dir {:prefix "methadone-test"})]
    (try
      (binding [*dir* dir] (f))
      (finally (fs/delete-tree dir)))))
