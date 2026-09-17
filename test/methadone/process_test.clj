; Copyright (C) 2026 Christopher Harrison
; SPDX-License-Identifier: GPL-3.0-or-later

(ns methadone.process-test
  "Finding the binary Methadone is meant to be standing in front of."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.test :as t]
            [methadone.fixtures :refer [*dir* bb-eval with-temp-dir]]))

(t/use-fixtures :each with-temp-dir)

(defn- executable
  "Create a trivial executable at the given path, standing in for the
  binary Methadone is meant to find."
  [path]

  (fs/create-dirs (fs/parent path))
  (spit (fs/file path) "#!/bin/sh\nexit 0\n")
  (fs/set-posix-file-permissions path "rwxr-xr-x")
  path)

(defn- shadowed
  "A PATH in which Methadone, symlinked as claude, shadows a real claude
  further along. Returns the directories in order, the symlink and the
  binary it ought to resolve to."
  []

  (let [methadone (str (executable (fs/path *dir* "methadone")))
        near      (fs/path *dir* "near")
        far       (fs/path *dir* "far")
        link      (str (fs/path near "claude"))
        real      (str (fs/path far "claude"))]

    (fs/create-dirs near)
    (executable real)
    (fs/create-sym-link link methadone)

    {:methadone methadone :near near :far far :link link :real real}))

(t/deftest discovery-by-name
  (let [{:keys [methadone near far link real]} (shadowed)
        discovers                              (fn [path]
                                                 (let [{:keys [exit out err]}
                                                       (bb-eval {"PATH" (str path)}
                                                                (str "(require '[methadone.process :as m])"
                                                                     "(prn (some-> (#'m/discover \"" link "\") str))"))]

                                                   (t/is (zero? exit) err)
                                                   (edn/read-string out)))]

    (t/testing "the binary that Methadone's own symlink shadows is found"
      (t/is (= real (discovers (str near ":" far)))))

    (t/testing "Methadone is skipped however often it appears"
      (let [also (fs/path *dir* "also")]
        (fs/create-dirs also)
        (fs/create-sym-link (fs/path also "claude") methadone)
        (t/is (= real (discovers (str near ":" also ":" far))))))

    (t/testing "nothing is found when Methadone is the only candidate"
      (t/is (nil? (discovers near))))))

(t/deftest target-resolution
  (let [{:keys [near far link real]} (shadowed)
        code                         (str "(require '[methadone.process :as m])"
                                          "(prn (#'m/target \"" link "\"))")]

    (t/testing "discovery supplies the binary when nothing else does"
      (t/is (= real (edn/read-string (:out (bb-eval {"PATH" (str near ":" far)} code))))))

    (t/testing "METHADONE_BINARY takes precedence over anything on PATH"
      (t/is (= "/elsewhere/claude"
               (edn/read-string (:out (bb-eval {"PATH"             (str near ":" far)
                                                "METHADONE_BINARY" "/elsewhere/claude"}
                                               code))))))

    (t/testing "failing to resolve exits non-zero rather than running nothing"
      ; The empty string is truthy in Clojure, so an empty result must
      ; not be allowed to pass for an answer.
      (let [{:keys [exit err]} (bb-eval {"PATH" (str near)} code)]
        (t/is (= 1 exit))
        (t/is (re-find #"claude" err))))))
