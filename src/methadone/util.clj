; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.util
  "Odds and ends with no better home.")

(defn die
  "Complain on stderr and give up."
  [& lines]

  (binding [*out* *err*] (run! println lines))
  (System/exit 1))

(defn epoch->date
  "Return the local date of given Unix epoch (in milliseconds), relative
  to the given time zone.

  NOTE tz ought to be a java.time.ZoneId; this is the responsibility of
  the caller."
  [when tz]

  (-> when
      java.time.Instant/ofEpochMilli
      (.atZone tz)
      .toLocalDate))

(defn spoken
  "A span of milliseconds, in whichever units read most naturally."
  [ms]

  (let [seconds (long (/ ms 1000))
        hours   (quot seconds 3600)
        minutes (rem (quot seconds 60) 60)]

    (cond
      (pos? hours)   (format "%dh %dm" hours minutes)
      (pos? minutes) (format "%dm" minutes)
      :else          (format "%ds" (rem seconds 60)))))
