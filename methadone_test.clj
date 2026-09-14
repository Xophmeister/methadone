(ns methadone-test)

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.test :as t]
         '[methadone :as m])

;; Fixtures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  m/defaults)

(def log
  {"claude"
   [{:id :brief :start (ago (* 50 minute)) :end (ago (* 40 minute))}  ; ran 10m, ended 40m ago
    {:id :lengthy :start (ago (* 90 minute)) :end (ago (* 30 minute))}  ; ran 60m, ended 30m ago
    {:id :running :start (ago (* 20 minute))}                           ; 20m so far, still going
    {:id :long-run :start (ago (* 3 hour))}                              ; 3h so far, still going
    {:id :ancient :start (ago (* 5 hour)) :end (ago (* 4 hour))}]    ; ran 60m, ended 4h ago

   "copilot"
   [{:id :other :start (ago (* 10 minute)) :end (ago (* 5 minute))}]})

(defn ids [] (set (map :id (m/sessions-for log "claude"))))
(defn u [] (m/usage (m/sessions-for log "claude") now hour))

(defn- close?
  "Whether two figures agree. The decayed terms are transcendental, so
  exact equality is not on offer; the tolerance is relative, save near
  zero where it falls back to absolute."
  ([expected actual] (close? expected actual 1e-9))
  ([expected actual tolerance]
   (<= (Math/abs (- (double expected) (double actual)))
       (* tolerance (Math/max 1.0 (Math/abs (double expected)))))))

(defn- integrate
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

(defn- session
  "The session with the given ID, wherever in the log it appears."
  [log id]

  (first (filter #(= id (:id %)) (mapcat val log))))

(defn- own-pid [] (.pid (java.lang.ProcessHandle/current)))

(defn- dead-pid
  "A PID that is no longer in use: run a trivial process to completion
  and take its PID. (Strictly this races with PID recycling, but not
  within the lifetime of a test run.)"
  []

  (let [proc (p/process ["true"])]
    @proc
    (.pid (:proc proc))))

(def ^:private bb
  "Babashka's own path, so that a subprocess can still be started under
  an environment that has no PATH to find it by."
  (str (fs/which "bb")))

(defn- bb-eval
  "Evaluate the given code in a fresh babashka process, under exactly
  the given environment.

  Resolution reads PATH and METHADONE_BINARY from the environment, which
  cannot be changed in place, so these are the only tests that need a
  process of their own."
  [env code]

  (let [{:keys [exit out err]} @(p/process [bb "-e" code]
                                           {:env env :out :string :err :string})]
    {:exit exit :out out :err err}))

; Each test gets its own state directory, so that they cannot interfere
; through the filesystem and none of them touch the real log.
(def ^:dynamic *dir* nil)
(defn- log-path [] (str (fs/path *dir* "log.edn")))

(defn- store
  "A configuration pointing at this test's own log, so that the
  persistence layer has somewhere private to write."
  []

  (assoc config :log (log-path)))

(defn- with-temp-dir [f]
  (let [dir (fs/create-temp-dir {:prefix "methadone-test"})]
    (try
      (binding [*dir* dir] (f))
      (finally (fs/delete-tree dir)))))

(t/use-fixtures :each with-temp-dir)

;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest span-parsing
  (t/testing "a bare number is already milliseconds"
    (t/is (= 500 (m/span 500))))

  (t/testing "an [n unit] pair is converted"
    (t/is (= (* 7 day) (m/span [7 :days])))
    (t/is (= (* 30 minute) (m/span [30 :minutes])))
    (t/is (= 250 (m/span [250 :ms]))))

  (t/testing "anything else is nothing, for the vetting to complain about"
    (t/is (nil? (m/span [7 :fortnights])))
    (t/is (nil? (m/span "7 days")))
    (t/is (nil? (m/span [7])))
    (t/is (nil? (m/span nil)))))

(t/deftest configuration-precedence
  (t/testing "with nothing found, the defaults stand"
    (t/is (= m/defaults (m/configure []))))

  (t/testing "a partial file overrides only what it names"
    (let [merged (m/configure [{:window [1 :days]}])]
      (t/is (= [1 :days] (:window merged)))
      (t/is (= (:max-friction m/defaults) (:max-friction merged)))))

  (t/testing "the earlier file wins, being the more particular"
    ; config-files lists the user's own first, so a system-wide policy
    ; is something to be overruled rather than something that overrules.
    (t/is (= [1 :days] (:window (m/configure [{:window [1 :days]}
                                              {:window [2 :days]}])))))

  (t/testing "spans are reduced once the configuration has settled"
    (t/is (= (* 7 day)
             (:window (m/resolved (m/configure [{:window [7 :days]}])))))))

(t/deftest configuration-search-path
  (t/testing "the user's own configuration comes first"
    (t/is (= ["/home/me/.config/methadone.edn" "/etc/xdg/methadone.edn"]
             (mapv str (m/config-files "/home/me/.config" "/etc/xdg")))))

  (t/testing "every XDG_CONFIG_DIRS entry follows, in the order given"
    (t/is (= ["/home/me/.config/methadone.edn"
              "/etc/xdg/methadone.edn"
              "/nix/etc/xdg/methadone.edn"]
             (mapv str (m/config-files "/home/me/.config"
                                       "/etc/xdg:/nix/etc/xdg")))))

  (t/testing "an unset XDG_CONFIG_DIRS falls back to the specified default"
    (t/is (= ["/home/me/.config/methadone.edn" "/etc/xdg/methadone.edn"]
             (mapv str (m/config-files "/home/me/.config" nil))))))

(t/deftest configuration-guards
  (t/testing "the defaults are themselves acceptable"
    ; Not a tautology: the vetting is quite capable of disagreeing with
    ; the values shipped beside it.
    (t/is (empty? (m/problems m/defaults))))

  (t/testing "a misspelled setting is caught rather than quietly ignored"
    (t/is (seq (m/problems (m/configure [{:windwo (* 7 day)}])))))

  (t/testing "a span must parse, and must be positive"
    (t/is (seq (m/problems (m/configure [{:window [7 :fortnights]}]))))
    (t/is (seq (m/problems (m/configure [{:retention -1}])))))

  (t/testing "anchors must be two well-formed pairs"
    (t/is (seq (m/problems (m/configure [{:anchors [1 2]}]))))
    (t/is (seq (m/problems (m/configure [{:anchors [[10 10]]}])))))

  (t/testing "anchors must ascend in score"
    (t/is (seq (m/problems (m/configure [{:anchors [[50 120] [10 10]]}])))))

  (t/testing "an anchor outside (0, max-friction) is refused"
    (doseq [anchors [[[10 0] [50 120]]     ; No wait at all
                     [[10 10] [50 1200]]   ; The ceiling itself
                     [[10 10] [50 1500]]]] ; Beyond it
      (t/is (seq (m/problems (m/configure [{:anchors anchors}]))))))

  (t/testing "a log path must be a path"
    (t/is (seq (m/problems (m/configure [{:log 42}])))))

  (t/testing "every fault is reported at once, not one per attempt"
    (t/is (< 1 (count (m/problems (m/configure [{:windwo 1 :retention -1}])))))))

(t/deftest failing-wide-open
  ; What the anchor guard is really for. Outside (0, max-friction) the
  ; logit is not finite, and the wait that falls out of it is nothing at
  ; all: Methadone quietly disabling itself, which is the one direction
  ; it must never fail in.
  (t/testing "an anchor beyond the ceiling would earn no wait whatsoever"
    (t/is (zero? (m/friction (assoc config :anchors [[10 10] [50 1500]])
                             {:count 500 :duration 0}))))

  (t/testing "an anchor of no wait at all would do the same"
    (t/is (zero? (m/friction (assoc config :anchors [[10 0] [50 120]])
                             {:count 500 :duration 0}))))

  (t/testing "and every one of them is refused before it can"
    (doseq [anchors [[[10 10] [50 1500]] [[10 0] [50 120]] [[10 10] [50 1200]]]]
      (t/is (seq (m/problems (assoc config :anchors anchors)))))))

;; Policy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest binary-selection
  (t/testing "other binaries are not considered"
    (t/is (not (contains? (ids) :other))))

  (t/testing "a binary that has never run has no sessions"
    (t/is (= [] (m/sessions-for log "cursor"))))

  (t/testing "every session is offered, however old"
    ; Age is a weight now rather than a cutoff, so there is nothing left
    ; to filter on: even :ancient is handed over, to be discounted later.
    (t/is (= #{:brief :lengthy :running :long-run :ancient} (ids)))))

(t/deftest decay-weighting
  (t/testing "a launch this instant counts in full"
    (t/is (close? 1.0 (:count (m/usage [{:start now}] now hour)))))

  (t/testing "a launch one window ago counts 1/e"
    ; The window is the mean lifetime of the decay, not a cutoff.
    (t/is (close? (/ 1 Math/E) (:count (m/usage [{:start (ago hour)}] now hour)))))

  (t/testing "every further window divides the weight by e again"
    (t/is (close? (/ 1 (* Math/E Math/E))
                  (:count (m/usage [{:start (ago (* 2 hour))}] now hour)))))

  (t/testing "the half-life falls at window * ln 2, as documented"
    ; Loosely, because the half-life is not a whole number of
    ; milliseconds and truncating it moves the answer more than the code
    ; ever would.
    (t/is (close? 0.5 (:count (m/usage [{:start (ago (long (* hour (Math/log 2))))}]
                                       now hour))
                  1e-6))))

(t/deftest decayed-duration
  (t/testing "time spent is the weight integrated across the session"
    (let [start (ago (* 90 minute))
          end (ago (* 30 minute))]
      (t/is (close? (integrate start end hour 10000)
                    (:duration (m/usage [{:start start :end end}] now hour))
                    1e-6))))

  (t/testing "a session running up to now keeps 1 - 1/e of its last window"
    (t/is (close? (* hour (- 1 (/ 1 Math/E)))
                  (:duration (m/usage [{:start (ago hour)}] now hour)))))

  (t/testing "a session left running forever converges on one window's worth"
    ; Its older end decays exactly as fast as its newer end accrues,
    ; which is what bounds the cost of never closing one.
    (t/is (close? (double hour) (:duration (m/usage [{:start 0}] now hour)))))

  (t/testing "the same session is worth 1/e as much a window later"
    (let [session {:start (ago (* 90 minute)) :end (ago (* 30 minute))}
          spent (fn [t] (:duration (m/usage [session] t hour)))]

      (t/is (close? (/ (spent now) Math/E) (spent (+ now hour)))))))

(t/deftest accumulation
  (t/testing "both terms sum across every session for the binary"
    (let [apart (map #(m/usage [%] now hour) (m/sessions-for log "claude"))]
      (t/is (close? (reduce + (map :count apart)) (:count (u))))
      (t/is (close? (reduce + (map :duration apart)) (:duration (u))))))

  (t/testing "an ancient session still contributes, though barely"
    ; Five windows back is e^-5 of a launch: present, and negligible.
    (let [sessions (m/sessions-for log "claude")
          without (:count (m/usage (remove #(= :ancient (:id %)) sessions) now hour))]

      (t/is (< without (:count (u))))
      (t/is (< (- (:count (u)) without) 0.01)))))

(t/deftest orphan-accounting
  ; A session abandoned by SIGKILL keeps :end nil, so an unreaped log
  ; still reads it as running and charges it right up to now. Nothing
  ; else bounds the claim, which is why -main reaps before it measures.
  (let [spent (fn [log] (:duration (m/usage (m/sessions-for log "claude") now hour)))
        launch (fn [log] (:count (m/usage (m/sessions-for log "claude") now hour)))

        ; Orphaned: a dead PID, no :end, and a heartbeat two hours stale.
        orphan {"claude" [{:id :orphan :pid 2 :start (ago (* 3 hour))
                           :seen (ago (* 2 hour))}]}
        reaped (m/reap orphan #{})]

    (t/testing "an unreaped orphan is charged as though it were still running"
      (t/is (close? (:duration (m/usage [{:start (ago (* 3 hour))}] now hour))
                    (spent orphan))))

    (t/testing "reaping charges it only up to its last heartbeat"
      (t/is (close? (:duration (m/usage [{:start (ago (* 3 hour))
                                          :end (ago (* 2 hour))}] now hour))
                    (spent reaped))))

    (t/testing "which is far less, there being no window to fall out of"
      ; The decay discounts the dead hours rather than excluding them,
      ; so the saving is a ratio now rather than all or nothing.
      (t/is (< (* 5 (spent reaped)) (spent orphan))))

    (t/testing "the count is unmoved either way, depending only on :start"
      (t/is (close? (launch orphan) (launch reaped))))))

(t/deftest degenerate-cases
  (t/testing "empty log"
    (t/is (= {:count 0.0 :duration 0.0} (m/usage (m/sessions-for {} "claude") now hour))))

  (t/testing "a clock-skewed session starting in the future never goes negative"
    (t/is (= {:count 1.0 :duration 0.0} (m/usage [{:start (+ now (* 5 minute))}] now hour))))

  (t/testing "an end before its start never goes negative either"
    (t/is (= 0.0 (:duration (m/usage [{:start (ago minute) :end (ago (* 5 minute))}]
                                     now hour))))))

(t/deftest exchange-rate
  (t/testing "an unused window scores nothing"
    (t/is (close? 0.0 (m/score config {:count 0.0 :duration 0.0}))))

  (t/testing "a session-equivalent of runtime counts as another launch"
    (t/is (close? (m/score config {:count 2.0 :duration 0.0})
                  (m/score config {:count 0.0 :duration (* 2 (:session-equivalent config))}))))

  (t/testing "the two terms simply add"
    (t/is (close? 3.5 (m/score config {:count 3.0 :duration (/ (:session-equivalent config) 2)}))))

  (t/testing "leaving one session open outweighs launching another"
    ; The whole point of the change: an eight-hour session is sixteen
    ; session-equivalents, against the one it cost to start.
    (t/is (< (m/score config {:count 2.0 :duration 0.0})
             (m/score config {:count 1.0 :duration (* 8 hour)})))))

(t/deftest friction-curve
  (t/testing "the anchors are met exactly"
    ; They are the two opinions the curve is solved from -- what a light
    ; week and a heavy one ought to cost -- so they must come back whole.
    (doseq [[usage seconds] (:anchors config)]
      (t/is (= seconds (m/friction config {:count usage :duration 0})))))

  (t/testing "an unused window still costs a few seconds"
    (t/is (= 5 (m/friction config {:count 0 :duration 0}))))

  (t/testing "the wait never falls as usage rises"
    (let [waits (mapv #(m/friction config {:count % :duration 0}) (range 0 200 5))]
      (t/is (= waits (vec (sort waits))))
      (t/is (< (first waits) (last waits)))))

  (t/testing "the wait levels off at max-friction, however absurd the usage"
    ; Bounded on purpose: a wait long enough to be worth circumventing
    ; buys no deterrence at all, the bypass being a single rm.
    (t/is (= (:max-friction config) (m/friction config {:count 1000 :duration 0})))
    (t/is (= (:max-friction config) (m/friction config {:count 1e9 :duration 0}))))

  (t/testing "time spent tells, where once it was ignored"
    (t/is (< (m/friction config {:count 1 :duration 0})
             (m/friction config {:count 1 :duration (* 5 hour)}))))

  (t/testing "the leave-it-open habit is punished hardest"
    ; A week of each, at steady state: light use, many short sessions,
    ; and one long session a day. The last is what the duration term
    ; exists to catch, and it must come out worst.
    (let [light (m/friction config {:count 5 :duration (* 5 20 minute)})
          churner (m/friction config {:count 40 :duration (* 40 5 minute)})
          gamer (m/friction config {:count 7 :duration (* 7 8 hour)})]

      (t/is (< light churner gamer))))

  (t/testing "the wait is integral, being both formatted and slept on"
    ; (format "%2d" 5.0) throws
    (t/is (integer? (m/friction config {:count 3 :duration 0})))))

;; Nag UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest spoken-spans
  (t/testing "seconds, while that is all there is"
    (t/is (= "45s" (m/spoken (* 45 1000)))))

  (t/testing "minutes, once there are any"
    (t/is (= "20m" (m/spoken (* 20 minute)))))

  (t/testing "hours and minutes together"
    (t/is (= "9h 11m" (m/spoken (+ (* 9 hour) (* 11 minute))))))

  (t/testing "nothing at all still reads as a span"
    (t/is (= "0s" (m/spoken 0)))))

(t/deftest accounting-for-the-wait
  ; Time spent is charged the next time round, so a nag that does not
  ; say what earned it teaches nothing: the one thing the user needs to
  ; connect is the session they left open to the wait they are sitting
  ; through now.
  (let [line (m/summary config {:count 6.3 :duration (+ (* 9 hour) (* 11 minute))})]

    (t/testing "the launches are reported, rounded"
      (t/is (re-find #"6 launches" line)))

    (t/testing "and counted, a lone one not being 1 launches"
      (t/is (re-find #"1 launch," (m/summary config {:count 1.0 :duration 0}))))

    (t/testing "so is the time spent, which is the whole point"
      (t/is (re-find #"9h 11m running" line)))

    (t/testing "and the score, it being what the wait is drawn from"
      (t/is (re-find #"score of 25" line)))))

(t/deftest climbing-the-ladder
  ; The rung is arrived at by arithmetic on an unbounded score, which is
  ; a thing to keep pinned: a wrong index is silent where it isn't fatal,
  ; and the worst of it only shows at the extremes nobody reaches by
  ; hand. Sweeping the range is cheaper than reasoning about it.
  (let [ladder  @#'m/consternations
        rung    (zipmap ladder (range))
        chosen  #(m/scorn config {:count (double %) :duration 0})
        climbed (mapv (comp rung chosen) (range 0 400 0.5))]

    (t/testing "every score lands on the ladder, none of them beside it"
      (t/is (every? some? climbed)))

    (t/testing "which is climbed, and never descended"
      (t/is (= climbed (sort climbed))))

    (t/testing "every rung of it, or the harshest words are never said"
      (t/is (= (set (range (count ladder))) (set climbed))))

    (t/testing "a clean slate is met with the mildest"
      (t/is (= (first ladder) (chosen 0))))

    (t/testing "and an absurd score with the sternest, not an index off the end"
      (t/is (= (last ladder) (chosen 1e6))))

    ; The anchors exist to tell these two apart, so the words must too:
    ; scolding both alike is what keying the rung to the wait would have
    ; done, the logistic being nearly flat across everyday usage.
    (t/testing "a light week and a heavy one are not scolded alike"
      (t/is (not= (chosen 8) (chosen 47))))

    (t/testing "and what is chosen is what the nag says"
      (t/is (str/includes? (with-out-str (m/nag config {:count 8.0 :duration 0} 0))
                           (chosen 8))))))

(t/deftest commending-the-change-of-heart
  ; applaud draws one of these at the moment it fires and exits on the
  ; next line, so an empty list would throw inside a signal handler,
  ; during a wait, with nothing left to report it.
  (let [praise @#'m/commendations]
    (t/testing "there is something to say"
      (t/is (seq praise)))

    (t/testing "and all of it is worth saying"
      (t/is (every? #(and (string? %) (seq (str/trim %))) praise)))))

(t/deftest drawing-only-on-a-terminal
  ; The countdown rewrites one line with cursor control. Redirected,
  ; that lands as escape sequences in somebody's log file, so the wait
  ; is taken in silence instead.
  (t/testing "given a terminal, the line is cleared even at zero seconds"
    (t/is (seq (with-out-str (#'m/countdown 0 true)))))

  (t/testing "without one, nothing is written at all"
    (t/is (= "" (with-out-str (#'m/countdown 0 false))))))

;; Log ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest reaping
  ; alive? is injected precisely so that this needs no real processes: a
  ; set of live PIDs is a perfectly good liveness predicate.
  (let [before {"claude" [{:id :live :pid 1 :start (ago hour)}
                          {:id :gone :pid 2 :start (ago hour)}
                          {:id :beat :pid 3 :start (ago hour) :seen (ago (* 10 minute))}
                          {:id :ended :pid 4 :start (ago hour) :end (ago (* 30 minute))}]
                "copilot" [{:id :nopid :start (ago hour)}]}
        after (m/reap before #{1})]

    (t/testing "a session whose process is alive is left running"
      (t/is (nil? (:end (session after :live)))))

    (t/testing "a session whose process is gone is closed at its own start"
      (t/is (= (ago hour) (:end (session after :gone)))))

    (t/testing "a session with a heartbeat is charged up to its last beat"
      (t/is (= (ago (* 10 minute)) (:end (session after :beat)))))

    (t/testing "the heartbeat is dropped once an end is known, being redundant"
      (t/is (not (contains? (session after :beat) :seen))))

    (t/testing "a session that already ended is untouched"
      (t/is (= (ago (* 30 minute)) (:end (session after :ended)))))

    (t/testing "a session with no PID at all is treated as gone"
      (t/is (some? (:end (session after :nopid)))))

    (t/testing "reaping is idempotent"
      (t/is (= after (m/reap after #{1}))))))

(t/deftest pruning
  (let [before {"claude" [{:id :recent :start (ago (* 2 hour)) :end (ago (* 30 minute))}
                          {:id :expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}
                          {:id :old-run :start (ago (* 9 hour))}]
                "copilot" [{:id :also-expired :start (ago (* 5 hour)) :end (ago (* 4 hour))}]}
        after (m/prune before now hour)]

    (t/testing "a session that ended inside the retention period is kept"
      (t/is (some? (session after :recent))))

    (t/testing "a session that ended before it is dropped"
      (t/is (nil? (session after :expired))))

    (t/testing "a running session is kept however old, since it has yet to end"
      (t/is (some? (session after :old-run))))

    (t/testing "every binary is pruned, not just the first"
      (t/is (nil? (session after :also-expired))))))

(t/deftest opening
  (t/testing "the first session for a binary creates a vector, not a list"
    (t/is (vector? (get (m/open {} "claude" {:id :a}) "claude"))))

  (t/testing "further sessions are appended in order"
    (t/is (= [{:id :a} {:id :b}]
             (get (-> {} (m/open "claude" {:id :a}) (m/open "claude" {:id :b})) "claude"))))

  (t/testing "other binaries are left alone"
    (t/is (= [{:id :c}]
             (get (m/open {"copilot" [{:id :c}]} "claude" {:id :a}) "copilot")))))

(t/deftest touching
  (let [before {"claude" [{:id :a :start (ago hour)}
                          {:id :b :start (ago hour)}
                          {:id :done :start (ago hour) :end (ago (* 30 minute))}]
                "copilot" [{:id :c :start (ago hour)}]}]

    (t/testing "the matching session gains a heartbeat"
      (t/is (= now (:seen (session (m/touch before :a now) :a)))))

    (t/testing "no other session is touched"
      (t/is (nil? (:seen (session (m/touch before :a now) :b)))))

    (t/testing "sessions are found under any binary"
      (t/is (= now (:seen (session (m/touch before :c now) :c)))))

    (t/testing "a later heartbeat replaces an earlier one"
      (t/is (= now (:seen (session (-> before
                                       (m/touch :a (ago minute))
                                       (m/touch :a now))
                                   :a)))))

    (t/testing "a session that has already ended gains no heartbeat"
      (t/is (nil? (:seen (session (m/touch before :done now) :done)))))

    (t/testing "an unknown ID is ignored"
      (t/is (= before (m/touch before :nonesuch now))))))

(t/deftest closing
  (let [before {"claude" [{:id :a :start (ago hour) :seen (ago minute)}
                          {:id :b :start (ago hour)}]
                "copilot" [{:id :c :start (ago hour)}]}]

    (t/testing "the matching session gains an :end"
      (t/is (= now (:end (session (m/close before :a now) :a)))))

    (t/testing "no other session is touched"
      (t/is (nil? (:end (session (m/close before :a now) :b)))))

    (t/testing "sessions are found under any binary"
      (t/is (= now (:end (session (m/close before :c now) :c)))))

    (t/testing "the first close wins, so a second cannot inflate the duration"
      (t/is (= now (:end (session (-> before (m/close :a now) (m/close :a (+ now hour))) :a)))))

    (t/testing "closing drops the heartbeat, which the :end supersedes"
      (t/is (not (contains? (session (m/close before :a now) :a) :seen))))

    (t/testing "an unknown ID is ignored"
      (t/is (= before (m/close before :nonesuch now))))))

;; Persistence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest round-tripping
  (t/testing "a missing file reads as an empty log"
    (t/is (= {} (m/read-log (log-path)))))

  (t/testing "a log survives a write and read unchanged"
    (let [before {"claude" [{:id (random-uuid) :pid 1 :start (recently hour) :end (recently minute)}
                            {:id (random-uuid) :pid 2 :start (recently minute)}]}]
      (m/write-log! before (log-path))
      (t/is (= before (m/read-log (log-path))))))

  (t/testing "timestamps are stored as #inst, so the file stays legible"
    (m/write-log! {"claude" [{:id :a :pid 1 :start (recently hour)}]} (log-path))
    (t/is (re-find #"#inst" (slurp (log-path)))))

  (t/testing "a running session is not given an :end by the round trip"
    (m/write-log! {"claude" [{:id :a :pid 1 :start (recently hour)}]} (log-path))
    (t/is (not (contains? (session (m/read-log (log-path)) :a) :end))))

  (t/testing "a heartbeat is stored as an #inst too, not left as a raw count"
    (m/write-log! {"claude" [{:id :a :pid 1 :start (recently hour) :seen (recently minute)}]}
                  (log-path))
    (t/is (inst? (:seen (session (edn/read-string (slurp (log-path))) :a)))))

  (t/testing "writing leaves no temporary files behind"
    (m/write-log! {"claude" []} (log-path))
    (t/is (empty? (fs/glob *dir* "*.tmp")))))

(t/deftest transactions
  (t/testing "the function is applied, and the resulting log returned"
    (t/is (= {"claude" []} (m/update-log! (store) #(assoc % "claude" [])))))

  (t/testing "the result is what a subsequent read sees"
    (m/update-log! (store) #(assoc % "claude" []))
    (t/is (= {"claude" []} (m/read-log (log-path)))))

  (t/testing "the lock lives beside the log, not in it"
    (m/update-log! (store) identity)
    (t/is (fs/exists? (str (log-path) ".lock")))))

(t/deftest self-healing
  (t/testing "a session abandoned by a dead process is reaped in passing"
    (let [start (recently (* 10 minute))]
      (m/write-log! {"claude" [{:id :orphan :pid (dead-pid) :start start}]} (log-path))
      (t/is (= start (:end (session (m/update-log! (store) identity) :orphan))))))

  (t/testing "a session whose process still lives is left running"
    (m/write-log! {"claude" [{:id :live :pid (own-pid) :start (recently minute)}]} (log-path))
    (t/is (nil? (:end (session (m/update-log! (store) identity) :live)))))

  (t/testing "a session that ended beyond the retention period is pruned in passing"
    (m/write-log! {"claude" [{:id :ancient :pid 1
                              :start (recently (* 40 day)) :end (recently (* 31 day))}]}
                  (log-path))
    (t/is (nil? (session (m/update-log! (store) identity) :ancient)))))

(t/deftest session-lifecycle
  (let [started (recently (* 5 minute))
        id (m/open-session! (store) "claude" started)]

    (t/testing "opening returns an ID that identifies a session in the log"
      (t/is (= started (:start (session (m/read-log (log-path)) id)))))

    (t/testing "the new session is running"
      (t/is (nil? (:end (session (m/read-log (log-path)) id)))))

    (t/testing "the PID recorded is Methadone's own, so that we can be reaped"
      (t/is (= (own-pid) (:pid (session (m/read-log (log-path)) id)))))

    (t/testing "closing ends the session"
      (m/close-session! (store) id (recently minute))
      (t/is (some? (:end (session (m/read-log (log-path)) id)))))

    (t/testing "closing twice does not move the end, as the shutdown hook may repeat it"
      (let [ended (:end (session (m/read-log (log-path)) id))]
        (m/close-session! (store) id (System/currentTimeMillis))
        (t/is (= ended (:end (session (m/read-log (log-path)) id))))))))

(t/deftest heartbeating
  (let [id (m/open-session! (store) "claude" (recently (* 5 minute)))
        beat (recently (* 3 minute))]

    (t/testing "a heartbeat is recorded against the running session"
      (m/touch-session! (store) id beat)
      (t/is (= beat (:seen (session (m/read-log (log-path)) id)))))

    (t/testing "a heartbeat arriving after the close cannot reopen the session"
      (m/close-session! (store) id (recently minute))
      (let [closed (session (m/read-log (log-path)) id)]
        (m/touch-session! (store) id (System/currentTimeMillis))
        (t/is (= closed (session (m/read-log (log-path)) id)))))))

(t/deftest thread-safety
  (t/testing "simultaneous transactions in one process do not collide"
    ; The heartbeat runs on a thread of its own, so update-log! must
    ; tolerate two callers in the same process. A file lock belongs to
    ; the process rather than to the thread that took it, so without a
    ; monitor beneath it the second caller gets an
    ; OverlappingFileLockException rather than its turn.
    ; The configuration is built here rather than in the workers because
    ; binding is thread-local: a bare Thread sees *dir*'s root value,
    ; and fs/path quietly turns a nil parent into a relative path.
    (let [cfg (store)
          threads 4
          each 25
          failed (atom [])
          workers (doall (for [_ (range threads)]
                           (Thread.
                            (fn []
                              (dotimes [_ each]
                                (try (m/open-session! cfg "claude"
                                                      (System/currentTimeMillis))
                                     (catch Exception e (swap! failed conj (class e)))))))))]

      (run! #(.start %) workers)
      (run! #(.join %) workers)

      (t/is (empty? @failed) "every transaction succeeded")
      (t/is (= (* threads each) (count (get (m/read-log (log-path)) "claude")))))))

(t/deftest concurrency
  (t/testing "simultaneous transactions do not lose one another's updates"
    ; This is the only test that justifies the lock existing: the failure
    ; it guards against cannot be reached from a single process.
    (let [n 8
          code (str "(require '[methadone :as m]) "
                    "(m/open-session! (assoc m/defaults :log \"" (log-path) "\") "
                    "\"claude\" (System/currentTimeMillis))")
          procs (doall (repeatedly n #(p/process ["bb" "-e" code])))
          exits (mapv (comp :exit deref) procs)]

      (t/is (every? zero? exits) "every appending process exited cleanly")
      (t/is (= n (count (get (m/read-log (log-path)) "claude")))))))

;; Resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  (let [methadone (str (fs/real-path "methadone.clj"))
        near (fs/path *dir* "near")
        far (fs/path *dir* "far")
        link (str (fs/path near "claude"))
        real (str (fs/path far "claude"))]

    (fs/create-dirs near)
    (executable real)
    (fs/create-sym-link link methadone)

    {:methadone methadone :near near :far far :link link :real real}))

(t/deftest discovery-by-name
  (let [{:keys [methadone near far link real]} (shadowed)
        discovers (fn [path]
                    (let [{:keys [exit out err]}
                          (bb-eval {"PATH" (str path)}
                                   (str "(require '[methadone :as m])"
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
        code (str "(require '[methadone :as m])"
                  "(prn (#'m/target \"" link "\"))")]

    (t/testing "discovery supplies the binary when nothing else does"
      (t/is (= real (edn/read-string (:out (bb-eval {"PATH" (str near ":" far)} code))))))

    (t/testing "METHADONE_BINARY takes precedence over anything on PATH"
      (t/is (= "/elsewhere/claude"
               (edn/read-string (:out (bb-eval {"PATH" (str near ":" far)
                                                "METHADONE_BINARY" "/elsewhere/claude"}
                                               code))))))

    (t/testing "failing to resolve exits non-zero rather than running nothing"
      ; The empty string is truthy in Clojure, so an empty result must
      ; not be allowed to pass for an answer.
      (let [{:keys [exit err]} (bb-eval {"PATH" (str near)} code)]
        (t/is (= 1 exit))
        (t/is (re-find #"claude" err))))))
