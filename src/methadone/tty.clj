; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.tty
  "Whether there is a terminal to draw on, and the attributes with which
  to draw.")

(def terminal?
  "Whether Methadone has a terminal to draw on. Without one, colour and
  cursor control alike are just noise in somebody's log file."
  (some? (System/console)))

(def ^:private ansi-attributes
  "ANSI attributes, empty when there is no terminal so that redirected
  output stays clean."
  (when terminal?
    {:bold   "\033[1m"
     :dim    "\033[2m"
     :green  "\033[32m"
     :yellow "\033[33m"
     :red    "\033[31m"
     :reset  "\033[0m"
     :erase  "\033[K"}))

(defn ansi [attribute] (get ansi-attributes attribute ""))
