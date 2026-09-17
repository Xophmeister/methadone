; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.signals
  "Which signals Methadone answers, and how differently it answers them
  before the agent exists and after."
  (:require [methadone.nag :as nag]
            [methadone.tty :as tty]))

(def ^:private absorb
  "A handler that does nothing, so the signal is neither acted upon nor
  passed to the default disposition."
  (reify sun.misc.SignalHandler (handle [_ _] nil)))

(def ^:private applaud
  "A handler that applauds the user for abandoning the launch with a
  random commendation."
  (reify sun.misc.SignalHandler
    (handle [_ _]
      (when tty/terminal? (print (str "\r" (tty/ansi :erase))))
      (println (str (tty/ansi :green) (rand-nth nag/commendations) (tty/ansi :reset)))
      (flush)
      (System/exit 130))))

(defn signals!
  "Set the disposition of each named signal."
  [dispositions]

  (doseq [[signal handler] dispositions]
    (sun.misc.Signal/handle (sun.misc.Signal. signal) handler)))

; Absorbed rather than ignored: SIG_IGN survives exec, so an ignored
; signal would leave the agent itself unable to receive it and, under
; Babashka, SIGINT cannot be restored once ignored. A caught signal is
; reset to its default across exec, so the agent starts clean and no
; restoration step is needed.
; Applauded rather than absorbed: SIGINT is handled by abandoning the
; launch altogether, so the user is congratulated for doing so and
; returned to the terminal.
(def nag-signals
  {"INT"  applaud
   "QUIT" absorb
   "TSTP" absorb})

; Both dispositions that change do so here, in opposite directions.
; SIGINT must now be absorbed: it reaches the whole foreground process
; group, so the agent receives it and answers it itself, whereas
; Methadone dying on it would orphan the agent and return a prompt while
; it still held the terminal. SIGTSTP must now work, for the mirror image
; of that reason: were the agent to stop and Methadone not, the shell
; would still be waiting on Methadone and the terminal would sit with no
; prompt. SIGQUIT stays absorbed throughout, being the agent's to answer
; too.
(def supervise-signals
  {"INT"  absorb
   "TSTP" sun.misc.SignalHandler/SIG_DFL})
