; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.util
  "Odds and ends with no better home.")

(defn die
  "Complain on stderr and give up."
  [& lines]

  (binding [*out* *err*] (run! println lines))
  (System/exit 1))
