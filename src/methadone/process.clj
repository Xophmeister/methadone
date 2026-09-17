; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.process
  "Finding the agent and running it."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [methadone.util :refer [die]]))

(def ^:const methadone-bin "methadone")
(def ^:const wrapper-env "METHADONE_BINARY")

(defn spawn
  "Run the given binary with the given arguments, inheriting Methadone's
  streams so that the agent has the terminal.

  Where setpriv is available, the agent is given a parent-death signal,
  so that killing Methadone outright takes the agent with it rather than
  leaving it orphaned. SIGKILL cannot be caught, so this is the only way
  to cover that case; it is a Linux-specific facility, so elsewhere, the
  shutdown hook is the only safeguard.

  The environment is passed on intact but for the wrapper variable,
  which is Methadone's own affair and no business of what it supervises.
  Left in place it would be inherited by everything the agent runs, and
  Methadone invoked from in there would take itself for a wrapper with
  an agent to launch -- and launch a second one, instead of reporting."
  [binary args]

  (p/process (cond->> (cons binary args)
               (fs/which "setpriv") (concat ["setpriv" "--pdeathsig" "KILL" "--"]))

             {:inherit true
              :env     (dissoc (into {} (System/getenv)) wrapper-env)}))

(defn- discover
  "The first binary in the PATH under the name Methadone was invoked as,
  other than Methadone itself."
  [me]

  (let [self (fs/real-path me)]
    (->> (fs/which-all (fs/file-name me))
         (remove #(= self (fs/real-path %)))
         first)))

(defn target
  "The binary Methadone is supervising, as an absolute path."
  [me]

  (or (System/getenv wrapper-env)  ; Env var, mostly for Nix...
      (some-> (discover me) str)   ; ...otherwise, PATH discovery...

      ; ...or die horribly
      (die (str wrapper-env " not set, nor " (fs/file-name me)
                " found in PATH!"))))

(defn alone?
  "Under Nix, Methadone is always invoked under its own name, with the
  wrapper env var set. As such, to detect whether we're running alone,
  we check both the binary name and the nilness of the env var."
  [me]

  (and (some? me)
       (nil? (System/getenv wrapper-env))
       (= methadone-bin (fs/file-name me))))
