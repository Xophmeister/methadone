; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or (at
; your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.
(ns methadone.main
  "Wrap an agentic coding tool in a start-up delay that grows with how
  much it has lately been leant on, counting both how often it was
  launched and how long it was left running.

  Methadone supervises the tool rather than exec'ing it -- Babashka has
  no exec -- so it stays in the process tree for the whole session and
  can therefore record when that session began and ended."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [methadone.config :as config]
            [methadone.log :as log]
            [methadone.nag :as nag]
            [methadone.policy :as policy]
            [methadone.process :as process]
            [methadone.signals :as signals]
            [methadone.store :as store]))

(defn -main [& args]
  (let [config (config/configure!)
        binary (process/target (System/getProperty "babashka.file"))
        name   (fs/file-name binary)]

    (fs/create-dirs (fs/parent (:log config)))
    (signals/signals! signals/nag-signals)

    (let [now  (System/currentTimeMillis)
          used (-> (store/read-log (:log config))
                   (log/reap store/pid-alive?)
                   (policy/sessions-for name)
                   (policy/usage now (:window config)))]

      (nag/nag config used (policy/friction config used)))

    (signals/signals! signals/supervise-signals)

    (let [id   (store/open-session! config name (System/currentTimeMillis))
          proc (process/spawn binary args)]

      (store/start-heartbeat! config id)

      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. (fn []
                                   (p/destroy-tree proc)
                                   (store/close-session! config id (System/currentTimeMillis)))))

      (let [exit (:exit @proc)]
        (store/close-session! config id (System/currentTimeMillis))
        (System/exit exit)))))
