; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.nag
  "What Methadone says while it makes the user wait, and the waiting
  itself."
  (:require [methadone.policy :as policy]
            [methadone.tty :as tty]))

(def ^:private consternations
  "The list of consternations given to the user during the nag countdown,
  in ascending order of severity, with their styles baked-in."

  ["A modest dose. Mind how you go."
   "Back for a top-up already, are we?"
   (str (tty/ansi :yellow) "This was meant to be the substitute, not the habit.")
   (str (tty/ansi :yellow) "You are building quite the tolerance.")
   (str (tty/ansi :red) "Do not overuse this! Use your brain, instead!")
   (str (tty/ansi :red) "Whose theory of this program is it now? Yours...or its?")
   (str (tty/ansi :bold) (tty/ansi :red) "You are becoming a stranger to your own work!")
   (str (tty/ansi :bold) (tty/ansi :red) "The means of production are slipping away, comrade!")])

(def commendations
  "The list of Ctrl+C commendations, should the user realise the error
  of their ways and abandon the launch."

  ["You are a credit to the species!"
   "Humanity applauds you!"
   "You're right: Drugs don't work."
   "Your grey cells will live to see another day!"
   "Welcome to the revolution, comrade!"])

(defn spoken
  "A span of milliseconds, in whichever units read most naturally."
  [ms]

  (let [seconds (long (/ ms 1000))
        hours   (quot seconds 3600)
        minutes (rem (quot seconds 60) 60)]

    (cond
      (pos? hours) (format "%dh %dm" hours minutes)
      (pos? minutes) (format "%dm" minutes)
      :else (format "%ds" (rem seconds 60)))))

(defn scorn
  "Scorn the user for (over)using the agent, with a consternation chosen
  based on the score from the list available."
  [config usage]

  (let [midpoint (:midpoint (policy/curve config))
        length   (count consternations)]

    (-> (policy/score config usage)
        (/ (* 2 midpoint))
        (* length)
        (min (dec length))
        int
        consternations)))

(defn summary
  "What the wait was earned with, in a line.

  The figures are decayed by age, so they are what Methadone is weighing
  rather than a raw tally: a fortnight-old session is in there, but
  barely. Worth saying at all because the cost of leaving a session open
  is charged the next time round and a penalty nobody can connect to
  what caused it teaches nothing."
  [config usage]

  (let [launches (Math/round (double (:count usage)))]
    (format "Lately: %d %s, %s running, for a score of %d."
            launches
            (if (= 1 launches) "launch" "launches")
            (spoken (:duration usage))
            (Math/round (double (policy/score config usage))))))

(defn- countdown
  "Count down the given number of seconds, rewriting a single line in
  place. Without a terminal the wait still happens, in silence: cursor
  control smeared through a redirected log helps nobody."
  ([seconds] (countdown seconds tty/terminal?))

  ([seconds draw?]
   (if-not draw?
     (Thread/sleep (* 1000 seconds))

     (do (doseq [remaining (range seconds 0 -1)]
           (print (format "\r%s%sPaused: %d s remaining...%s"
                          (tty/ansi :erase) (tty/ansi :dim) remaining (tty/ansi :reset)))
           (flush)
           (Thread/sleep 1000))

         (print (str "\r" (tty/ansi :erase)))
         (flush)))))

(defn nag
  "Scold the user, account for why, then pause for the given number of
  seconds."
  [config usage seconds]

  (println (str (scorn config usage) (tty/ansi :reset)))
  (println (str (tty/ansi :dim) (summary config usage) (tty/ansi :reset)))

  (countdown seconds))
