(ns taoensso.tufte "TODO docstring"
  {:author "Peter Taoussanis (@ptaoussanis)"}
  #+clj  (:require [clojure.string  :as str]
                   [taoensso.encore :as enc :refer (qb have have?)])
  #+cljs (:require [clojure.string  :as str]
                   [taoensso.encore :as enc :refer-macros (have have?)])
  #+clj  (:import  [java.util LinkedList]))

;; TODO Move appropriate stuff to an impl.cljx ns?

;; (def ^:dynamic *foo1* {:a :A :b :B})
;; (def ^:dynamic *foo2* :A)
;; (def ^:dynamic *foo3* :B)

;; (qb 1e5
;;   (let [{:keys [a b]} *foo1*] [a b])
;;   [*foo2* *foo3*]
;;   (binding [*foo1* {:a :A :b :B}])
;;   (binding [*foo2* :A]
;;     (binding [*foo3* :B])
;;     )
;;   (binding [*foo2* :A *foo3* :B])
;;   )

;; [22.97 20.42 54.88 120.89 95.96]
;; [16.53 4.86]

;;;; Level filtering

(defn          valid-level? [x] (if (#{0 1 2 3 4 5} x) true false))
(defn ^:static valid-level
  "Returns argument if it's a valid profiling level, else throws."
  [x]
  (or
    (#{0 1 2 3 4 5} x)
    (throw
      (ex-info "Invalid profiling level: should be int e/o #{0 1 2 3 4 5}"
        {:given x :type (type x)}))))

(comment (qb 1e5 (valid-level 4)))

(def ^:dynamic *-min-level* "e/o #{0 1 2 3 4 5}" 2)
(defn        set-min-level!
  "Sets root binding of minimum profiling level, e/o #{0 1 2 3 4 5}."
  [level]
  (valid-level level)
  #+cljs (set!             *-min-level*        level)
  #+clj  (alter-var-root #'*-min-level* (fn [_] level)))

(comment (qb 1e5 *-min-level*))

(defmacro with-min-level
  "Executes body with dynamic minimum profiling level, e/o #{0 1 2 3 4 5}."
  [level & body]
  (when (integer? level) (valid-level level))
  `(binding [*-min-level* (valid-level ~level)]
     ~@body))

(defn #+clj  ^:static >=min-runtime-level?
      #+cljs ^boolean >=min-runtime-level?
  "Returns true iff given level >= current dynamic minimum profiling level."
  [level]
  (>= ^long (valid-level level)
      ^long *-min-level*))

(comment (qb 1e5 (>=min-runtime-level? 2)))

;;;; Namespace filtering (shares code with Timbre)

(def ^:private compile-ns-pattern
  "Returns (fn [?ns]) -> truthy"
  (let [compile1
        (fn [x]
          (cond
            (enc/re-pattern? x) (fn [ns-str] (re-find x ns-str))
            (string? x)
            (if (enc/str-contains? x "*")
              (let [re
                    (re-pattern
                      (-> (str "^" x "$")
                          (str/replace "." "\\.")
                          (str/replace "*" "(.*)")))]
                (fn [ns-str] (re-find re ns-str)))
              (fn [ns-str] (= ns-str x)))

            :else (throw (ex-info "Unexpected ns-pattern type"
                           {:given x :type (type x)}))))]

    (enc/memoize_
      (fn self
        ([ns-pattern] ; Useful for user-level matching
         (let [x ns-pattern]
           (cond
             (map? x) (self (:whitelist x) (:blacklist x))
             (or (vector? x) (set? x)) (self x nil)
             (= x "*") (fn [?ns] true)
             :else
             (let [match? (compile1 x)]
               (fn [?ns] (if (match? (str ?ns)) true))))))

        ([whitelist blacklist]
         (let [white
               (when (seq whitelist)
                 (let [match-fns (mapv compile1 whitelist)
                       [m1 & mn] match-fns]
                   (if mn
                     (fn [ns-str] (enc/rsome #(% ns-str) match-fns))
                     (fn [ns-str] (m1 ns-str)))))

               black
               (when (seq blacklist)
                 (let [match-fns (mapv compile1 blacklist)
                       [m1 & mn] match-fns]
                   (if mn
                     (fn [ns-str] (not (enc/rsome #(% ns-str) match-fns)))
                     (fn [ns-str] (not (m1 ns-str))))))]
           (cond
             (and white black)
             (fn [?ns]
               (let [ns-str (str ?ns)]
                 (if (white ns-str)
                   (if (black ns-str)
                     true))))

             white (fn [?ns] (if (white (str ?ns)) true))
             black (fn [?ns] (if (black (str ?ns)) true))
             :else (fn [?ns] true) ; Common case
             )))))))

(def ns-filter
  (fn [ns-pattern] ; (fn [?ns]) -> truthy
    (enc/memoize_ (compile-ns-pattern ns-pattern))))

(def ^:private ns-pass?
  "Returns true iff given ns passes white/black lists."
  (enc/memoize_
    (fn [whitelist blacklist ?ns]
      ((compile-ns-pattern whitelist blacklist) ?ns))))

(comment
  (qb 1e6 (ns-pass? ["foo.*"] ["foo.baz"] "foo.bar")) ; 217.4
  (ns-pass? nil nil "")
  (ns-pass? nil nil nil))

(def ^:dynamic *-ns-filter* (ns-filter "*"))
(defn set-ns-pattern! [ns-pattern]
  (let [f (ns-filter ns-pattern)]
    #+cljs (set!             *-ns-filter*        f)
    #+clj  (alter-var-root #'*-ns-filter* (fn [_] f))))

(defmacro with-ns-pattern [ns-pattern & body]
  (let [f (ns-filter ns-pattern)]
    `(binding [*-ns-filter* f]
       ~@body)))

;;;; Elision support

#+clj
(def ^:private compile-time-min-level ; Will stack with runtime checks
  (when-let [level (enc/read-sys-val "TUFTE_LEVEL")]
    (println (str "Compile-time (elision) Tufte level: " level))
    (valid-level level)))

#+clj
(def ^:private compile-time-ns-pass? ; Will stack with runtime checks
  (let [ns-pattern (enc/read-sys-val "TUFTE_NS_PATTERN")]
    (when ns-pattern (println (str "Compile-time (elision) Tufte ns-pattern: " ns-pattern)))
    (ns-filter (or ns-pattern "*"))))

#+clj ; Call only at compile-time
(defn -elide? [level-form ns-str-form]
  (not
    (and
      (or ; Level okay
        (nil? compile-time-min-level)
        (not (valid-level? level-form)) ; Not a compile-time level const
        (>= ^long level-form ^long compile-time-min-level))

      (or ; Namespace okay
        (not (string? ns-str-form)) ; Not a compile-time ns-str const
        (compile-time-ns-pass? ns-str-form)))))

;;;; Runtime filtering

(defn runtime-pass?
  ([level   ] (runtime-pass? level *ns*))
  ([level ns]
   (if (>= ^long (valid-level level)
           ^long *-min-level*)
     (if (*-ns-filter* ns) true))))

(comment (qb 1e5 (runtime-pass? 2)))

;;;; Handlers

;; TODO
;; `profile` -> put {:id _ :stats_ _ :line _ :ns-str _ :str_ (delay)}
;; note that stats can be forcible/delayed (esp. for merges)

(def ^:private handlers_ "{<sub-id> [<ch> <handler>]}" (atom nil))
(defn       set-handler!
  ([handler-id ?handler-fn] (set-handler! handler-id ?handler-fn "*"))
  ([handler-id ?handler-fn ns-pattern]
   ;; TODO pattern
   (if-let [f ?handler-fn]
     (swap! handlers_ assoc  handler-id f)
     (swap! handlers_ dissoc handler-id))
   nil))

(defn- handle! [val] (enc/run-kv! (fn [_ f] (f val)) @handlers_))

;;;; Time tracking

(defmacro nano-time [] `(enc/if-cljs (enc/nano-time) (System/nanoTime)))
(comment (macroexpand '(nano-time)))

(do ; Time accumulators (can be mutable since they'll be thread-local)
  (defn- add-time    [#+clj ^LinkedList x #+cljs ^ArrayList x t] (.add   x t))
  (defn- count-times [#+clj ^LinkedList x #+cljs ^ArrayList x]   (.size  x))
  (defn- clear-times [#+clj ^LinkedList x #+cljs ^ArrayList x]   (.clear x))
  (defn- new-times [] #+clj (LinkedList.) #+cljs (array-list)))

(def ^:static -pdata-proxy
  "{<id> <times> :__stats <m-stats> :__clock <m-clock>} iff profiling active
  on thread. This is substantially faster than a ^:dynamic atom. Would esp.
  benefit from ^:static support / direct linking / a Java class."
  #+clj
  (let [^ThreadLocal proxy (proxy [ThreadLocal] [])]
    (fn
      ([]        (.get proxy))
      ([new-val] (.set proxy new-val) new-val)))

  #+cljs
  (let [state_ (volatile! false)] ; Automatically thread-local in js
    (fn
      ([]                @state_)
      ([new-val] (vreset! state_ new-val)))))

(declare ^:private times->stats)

;;; Low-level primitives (undocumented), use with caution
#+clj  (defn          profiling? [] (if (-pdata-proxy) true false))
#+cljs (defn ^boolean profiling? [] (if (-pdata-proxy) true false))
(defn           start-profiling! [] (-pdata-proxy {:__t0 (nano-time)}) nil)
(defn            stop-profiling! [] ; Returns ?m-stats
  (when-let [pdata (-pdata-proxy)]
    (let [t1      (nano-time)
          t0      (get    pdata :__t0)
          m-stats (get    pdata :__stats) ; Interim stats
          m-times (dissoc pdata :__stats :__t0)
          m-stats ; {<id> <stats>}
          (reduce-kv
            (fn [m id times]
              (assoc m id (times->stats times (get m-stats id))))
            {:__clock {:t0 t0 :t1 t1 :total (- t1 ^long t0)}}
            m-times)]
      (-pdata-proxy nil)
      m-stats)))

(defn ^:static -capture-time! [pdata id t-elapsed]
  (if-let [times (get pdata id)]
    (if (>= (long (count-times times)) #_20 2000000) ; Rare in real-world use
      ;; Compact: merge interim stats to help prevent OOMs
      (let [m-stats (get pdata :__stats)
            m-stats (assoc m-stats id (times->stats times (get m-stats id)))]

        (clear-times times)
        (add-time    times t-elapsed) ; Nb: never leave our accumulator empty
        (-pdata-proxy (assoc pdata :__stats m-stats)))

      ;; Common case
      (add-time times t-elapsed))

    ;; Init case
    (let [times (new-times)]
      (add-time times t-elapsed)
      (-pdata-proxy (assoc pdata id times))))

  nil)

(def ^:private ^:const max-long #+clj Long/MAX_VALUE #+cljs 9007199254740991)

(defn- times->stats [times ?interim-stats]
  (let [ts-count   (long (count-times times))
        _          (assert (not (zero? ts-count)))
        times      (vec times) ; Faster to reduce
        ts-time    (reduce (fn [^long acc ^long in] (+ acc in)) 0 times)
        ts-mean    (/ (double ts-time) (double ts-count))
        ts-mad-sum (reduce (fn [^long acc ^long in] (+ acc (Math/abs (- in ts-mean)))) 0 times)
        ts-min     (reduce (fn [^long acc ^long in] (if (< in acc) in acc)) max-long     times)
        ts-max     (reduce (fn [^long acc ^long in] (if (> in acc) in acc)) 0            times)]

    (if-let [stats ?interim-stats] ; Merge over previous stats
      (let [s-count   (+ ^long (get stats :count) ts-count)
            s-time    (+ ^long (get stats :time) ^long ts-time)
            s-mean    (/ (double s-time) (double s-count))
            s-mad-sum (+ ^long (get stats :mad-sum) ^long ts-mad-sum)
            s-mad     (/ (double s-mad-sum) (double s-count))
            s0-min    (get stats :min)
            s0-max    (get stats :max)]

        ;; Batched "online" MAD calculation here is >= the standard
        ;; Knuth/Welford method, Ref. http://goo.gl/QLSfOc,
        ;;                            http://goo.gl/mx5eSK.

        {:count   s-count
         :time    s-time
         :mean    s-mean
         :mad-sum s-mad-sum
         :mad     s-mad
         :min     (if (< ^long s0-min ^long ts-min) s0-min ts-min)
         :max     (if (> ^long s0-max ^long ts-max) s0-max ts-max)})

      {:count   ts-count
       :time    ts-time
       :mean    ts-mean
       :mad-sum ts-mad-sum
       :mad     (/ (double ts-mad-sum) (double ts-count))
       :min     ts-min
       :max     ts-max})))

(comment (times->stats (new-times) nil))

;;;; Core macros

(def ^:dynamic *sacc* "Dynamic stats accumulator, experimental" nil)

(defmacro profiled
  "When logging is enabled, executes body with thread-local profiling.
  Always returns [<body-result> ?<stats>]."
  {:arglists '([level            & body]
               [level :when test & body])}
  [level & sigs]
  (let [[s1 s2 & sn] sigs]
    (if-not (= s1 :when)
      `(profiled ~level :when true ~@sigs)
      (let [test s2, body sn]
        (if (-elide? level (str *ns*))
          `[(do ~@body)]
          `(if (and (runtime-pass? ~level ~(str *ns*)) ~test)
             (try
               (start-profiling!)
               (let [result# (do ~@body)
                     stats#  (stop-profiling!)
                     stats#
                     (when stats#
                       (let [sacc# *sacc*] ; Dynamic capture
                         (if sacc#
                           (do (sacc# stats#) nil)
                           stats#)))]
                 [result# stats#])
               (finally (-pdata-proxy nil)))
             [(do ~@body)]))))))

(comment
  (qb 1e5 (profiled 2 (p :p1))) ; 176
  (profiled :info :when (chance 0.5) (p :p1) "foo"))

(defn- compile-time-pid [id]
  (if (enc/qualified-keyword? id)
    id
    (if (enc/ident? id)
      (keyword (str *ns*) (name id))
      (throw (ex-info "Unexpected `timbre/profiling` id type"
               {:id id :type (type id)})))))

(comment (compile-time-pid :foo))

(defmacro pspy
  "Profiling spy. When profiling, records execution time of named body.
  Always returns body's result."
  {:arglists '([id & body] [level id & body])}
  [& specs]
  (let [[s1 s2] specs
        [level id body]
        (if (and (valid-level? s1) (enc/ident? s2))
          [s1      s2 (nnext specs)]
          [:report s1  (next specs)])]

    (let [id (compile-time-pid id)]
      (if (-elide? level (str *ns*))
        `(do ~@body)
        `(let [pdata# (-pdata-proxy)]
           (if pdata#
             (let [t0#     (nano-time)
                   result# (do ~@body)
                   t1#     (nano-time)]
               (-capture-time! pdata# ~id (- t1# t0#))
               result#)
             (do ~@body)))))))

(comment (macroexpand '(pspy :info foo/id "foo")))


;;;; User-level utils







(defn chance [p] (< ^double (rand) (double p)))




;;;;



(comment ; TODO temp ideas

  ;; capture id'd times iff *level* okay, ns okay, tlp is on
  (p :level :id ...)

  ;; enable tlp iff *level* okay, ns okay
  (profiled :level ...)

  ;; enable dynamic capture iff *level* okay, ns okay
  ;; question is: should this override local levels? conds?
  (dynamic-profiled :level ...)

  ;; :trace :debug :info :report
  ;; or verbosity: just an arbitrary integer

  ;; *min-level* 4
  ;; (profiled 5)

  ;; maybe 0-5

  (profiled 0 ...) ; => never
  (profiled 5 ...) ; => always
  ;; 1, 2, 3, 4    ; depends

  (defn  profiling-thread? [] (if (-pdata-proxy) true false))
  (defn profiling-dynamic? [] (if *sacc*         true false))

  )




;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Helper macros, etc.



(defmacro p "`pspy` alias" [& specs] `(pspy ~@specs))
(comment (macroexpand '(p :foo (+ 4 2))))

(declare format-stats)
(defmacro  -log-stats [level id stats]
  `(handle! {:level ~level :id ~id :stats ~stats})
  #_`(timbre/log! ~level :p
     ["Profiling: " ~id "\n" (format-stats ~stats)]
     {:?base-data {:profile-stats ~stats}}))

(comment (profile 2 :my-id :when true "foo"))

(defmacro profile
  "When logging is enabled, executes named body with thread-local profiling
  and logs stats. Always returns body's result."
  {:arglists '([level id            & body]
               [level id :when test & body])}
  [level id & sigs]
  (let [id (compile-time-pid id)]
    `(let [[result# stats#] (profiled ~level ~@sigs)]
       (when stats# (-log-stats ~level ~id stats#))
       result#)))

;;;; Multi-threaded profiling ; Experimental

(defn merge-stats
  "Merges stats maps from multiple runs or threads.
  Automatically identifies and merges concurrent time windows."
  [s1 s2]
  (if s1
    (if s2
      (let [clock1 (get s1 :__clock)
            clock2 (get s2 :__clock)
            clock3
            (enc/if-lets
              [^long s1-t0 (get clock1 :t0)
               ^long s1-t1 (get clock1 :t1)
               ^long s2-t0 (get clock2 :t0)
               ^long s2-t1 (get clock2 :t1)
               any-clock-overlap?
               (or (and (<= s2-t0 s1-t1)
                        (>= s2-t1 s1-t0))
                   (and (<= s1-t0 s2-t1)
                        (>= s1-t1 s2-t0)))]

              (let [^long s3-t0 (if (< s1-t0 s2-t0) s1-t0 s2-t0)
                    ^long s3-t1 (if (< s1-t1 s2-t1) s1-t1 s2-t1)]

                {:t0 s3-t0 :t1 s3-t1 :total (- s3-t1 s3-t0)})

              {:total (+ ^long (get clock1 :total)
                         ^long (get clock2 :total))})

            s1      (dissoc s1 :__clock)
            s2      (dissoc s2 :__clock)
            all-ids (into (set (keys s1)) (keys s2))]

        (reduce
          (fn [m id]
            (let [sid1 (get s1 id)
                  sid2 (get s2 id)]
              (if sid1
                (if sid2
                  (let [^long s1-count   (get sid1 :count)
                        ^long s1-time    (get sid1 :time)
                        ^long s1-mad-sum (get sid1 :mad-sum)
                        ^long s1-min     (get sid1 :min)
                        ^long s1-max     (get sid1 :max)

                        ^long s2-count   (get sid2 :count)
                        ^long s2-time    (get sid2 :time)
                        ^long s2-mad-sum (get sid2 :mad-sum)
                        ^long s2-min     (get sid2 :min)
                        ^long s2-max     (get sid2 :max)

                        s3-count   (+ s1-count   s2-count)
                        s3-time    (+ s1-time    s2-time)
                        s3-mad-sum (+ s1-mad-sum s2-mad-sum)]

                    (assoc m id
                      {:count   s3-count
                       :time    s3-time
                       :mean    (/ (double s3-time) (double s3-count))
                       :mad-sum s3-mad-sum
                       :mad     (/ (double s3-mad-sum) (double s3-count))
                       :min     (if (< s1-min s2-min) s1-min s2-min)
                       :max     (if (> s1-max s2-max) s1-max s2-max)}))
                  m #_(assoc m id sid1))
                (assoc m id sid2))))
          (assoc s1 :__clock clock3)
          all-ids))
      s1)
    s2))

(defn stats-accumulator
  "Experimental, subject to change!
  Small util to help merge stats maps from multiple runs or threads.
  Returns a stateful fn with arities:
    ([stats-map]) ; Accumulates the given stats (you may call this from any thread)
    ([])          ; Deref: returns the merged value of all accumulated stats"
  []
  (let [acc_ (atom nil)
        reduce-stats_
        (delay
          (let [merge-stats (enc/memoize_ merge-stats)]
            (enc/memoize_ (fn [acc] (reduce merge-stats nil acc)))))]

    (fn stats-accumulator
      ([stats-map] (when stats-map (swap! acc_ conj stats-map)))
      ([] (when-let [acc @acc_] (@reduce-stats_ acc))))))

(comment (qb 1e5 (stats-accumulator)))

(defmacro dynamic-profiled
  "Experimental, subject to change!
  When logging is enabled, executes body with a dynamic binding that
  will capture and merge profiling stats from all (dynamic) threads.
  Always returns [<body-result> ?<merged-stats>]."
  [level & body]
  (if (-elide? level (str *ns*))
    `[(do ~@body)]
    `(if (runtime-pass? ~level ~(str *ns*))
       (let [sacc# (stats-accumulator)]
         (binding [*sacc* sacc#]
           [(do ~@body) (sacc#)]))
       [(do ~@body)])))

(defmacro dynamic-profile
  "Experimental, subject to change!
  Like `profile` but captures stats from all profiling calls in expr's
  dynamic scope."
  [level id & body]
  (if (-elide? level (str *ns*))
    `[(do ~@body)]
    `(if (runtime-pass? ~level ~(str *ns*))
       (let [sacc# (stats-accumulator)]
         (binding [*sacc* sacc#]
           (let [result# (do ~@body)
                 stats#  (sacc#)]
             (when stats# (-log-stats ~level ~id stats#))
             result#)))
       [(do ~@body)])))

(comment
  (dynamic-profile :info :id
    (future (profile :info :thread1                    (p :p1)))
    (future (profile :info :thread2 :when (chance 0.5) (p :p2)))
    (future (profile :info :thread3 :when (chance 0.5) (p :p3)))
    (Thread/sleep 20)
    "foo"))

;;;; Output formatting

(defn- perc [n d] (Math/round (/ (double n) (double d) 0.01)))
(comment (perc 14 24))

(defn- ft [nanosecs]
  (let [ns (long nanosecs)] ; Truncate any fractionals
    (cond
      (>= ns 1000000000) (str (enc/round2 (/ ns 1000000000))  "s") ; 1e9
      (>= ns    1000000) (str (enc/round2 (/ ns    1000000)) "ms") ; 1e6
      (>= ns       1000) (str (enc/round2 (/ ns       1000)) "μs") ; 1e3
      :else              (str                ns              "ns"))))

(defn format-stats
  ([stats           ] (format-stats stats :time))
  ([stats sort-field]
   (when stats
     (let [clock-time      (get-in stats [:__clock :total])
           stats           (dissoc stats  :__clock)
           ^long accounted (reduce-kv (fn [^long acc k v] (+ acc ^long (:time v))) 0 stats)

           sorted-stat-ids
           (sort-by
             (fn [id] (get-in stats [id sort-field]))
             enc/rcompare
             (keys stats))

           ^long max-id-width
           (reduce-kv
             (fn [^long acc k v]
               (let [c (count (str k))]
                 (if (> c acc) c acc)))
             #=(count "Accounted Time")
             stats)]

       #+cljs
       (let [sb
             (reduce
               (fn [acc id]
                 (let [{:keys [count min max mean mad time]} (get stats id)]
                   (enc/sb-append acc
                     (str
                       {:id      id
                        :n-calls count
                        :min     (ft min)
                        :max     (ft max)
                        :mad     (ft mad)
                        :mean    (ft mean)
                        :time%   (perc time clock-time)
                        :time    (ft time)}
                       "\n"))))
               (enc/str-builder)
               sorted-stat-ids)]

         (enc/sb-append sb "\n")
         (enc/sb-append sb (str "Clock Time: (100%) " (ft clock-time) "\n"))
         (enc/sb-append sb (str "Accounted Time: (" (perc accounted clock-time) "%) " (ft accounted) "\n"))
         (str           sb))

       #+clj
       (let [pattern   (str "%" max-id-width "s %,11d %9s %10s %9s %9s %7d %1s%n")
             s-pattern (str "%" max-id-width  "s %11s %9s %10s %9s %9s %7s %1s%n")
             sb
             (reduce
               (fn [acc id]
                 (let [{:keys [count min max mean mad time]} (get stats id)]
                   (enc/sb-append acc
                     (format pattern id count (ft min) (ft max) (ft mad)
                       (ft mean) (perc time clock-time) (ft time)))))

               (enc/str-builder (format s-pattern "Id" "nCalls" "Min" "Max" "MAD" "Mean" "Time%" "Time"))
               sorted-stat-ids)]

         (enc/sb-append sb (format s-pattern "Clock Time"     "" "" "" "" "" 100 (ft clock-time)))
         (enc/sb-append sb (format s-pattern "Accounted Time" "" "" "" "" "" (perc accounted clock-time) (ft accounted)))
         (str sb))))))

;;;; fnp stuff

(defn -fn-sigs [fn-name sigs]
  (let [single-arity? (vector? (first sigs))
        sigs    (if single-arity? (list sigs) sigs)
        get-id  (if single-arity?
                  (fn [fn-name _params] (keyword (str *ns*) (str "fn_" (name fn-name))))
                  (fn [fn-name  params] (keyword (str *ns*) (str "fn_" (name fn-name) \_ (count params)))))
        new-sigs
        (map
          (fn [[params & others]]
            (let [has-prepost-map?      (and (map? (first others)) (next others))
                  [?prepost-map & body] (if has-prepost-map? others (cons nil others))]
              (if ?prepost-map
                `(~params ~?prepost-map (pspy ~(get-id fn-name params) ~@body))
                `(~params               (pspy ~(get-id fn-name params) ~@body)))))
          sigs)]
    new-sigs))

(defmacro fnp "Like `fn` but wraps fn bodies with `p` macro."
  {:arglists '([name?  [params*] prepost-map? body]
               [name? ([params*] prepost-map? body)+])}
  [& sigs]
  (let [[?fn-name sigs] (if (symbol? (first sigs)) [(first sigs) (next sigs)] [nil sigs])
        new-sigs        (-fn-sigs (or ?fn-name (gensym "")) sigs)]
    (if ?fn-name
      `(fn ~?fn-name ~@new-sigs)
      `(fn           ~@new-sigs))))

(comment
  (-fn-sigs "foo"      '([x]            (* x x)))
  (macroexpand '(fnp     [x]            (* x x)))
  (macroexpand '(fn      [x]            (* x x)))
  (macroexpand '(fnp bob [x] {:pre [x]} (* x x)))
  (macroexpand '(fn      [x] {:pre [x]} (* x x))))

(defmacro defnp "Like `defn` but wraps fn bodies with `p` macro."
  {:arglists
   '([name doc-string? attr-map?  [params*] prepost-map? body]
     [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])}
  [& sigs]
  (let [[fn-name sigs] (enc/name-with-attrs (first sigs) (next sigs))
        new-sigs       (-fn-sigs fn-name sigs)]
    `(defn ~fn-name ~@new-sigs)))

(comment
  (defnp foo "Docstring"                [x]   (* x x))
  (macroexpand '(defnp foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defn  foo "Docstring"  [x]   (* x x)))
  (macroexpand '(defnp foo "Docstring" ([x]   (* x x))
                                       ([x y] (* x y))))
  (profile :info :defnp-test (foo 5)))

;;;; Deprecated

#+clj (defn pspy* "Deprecated" [_id f]   (pspy :pspy*/no-id (f)))
(defmacro p*      "Deprecated" [& body] `(pspy ~@body))

(comment (profile :info :pspy* (pspy* :foo (fn [] (Thread/sleep 100)))))

(defmacro sampling-profile "Deprecated" [level probability id & body]
  `(profile ~level ~id :when (< (rand) ~probability) ~@body))

;;;;

(comment
  (profile :info :sleepy-threads
    (dotimes [n 5]
      (Thread/sleep 100) ; Unaccounted
      (p :future/outer @(future (Thread/sleep 500)))
      @(future (p :future/inner (Thread/sleep 500)))
      (p :1ms    (Thread/sleep 1))
      (p :2s     (Thread/sleep 2000))
      (p :50ms   (Thread/sleep 50))
      (p :rand   (Thread/sleep (if (> 0.5 (rand)) 10 500)))
      (p :10ms   (Thread/sleep 10))
      "Result"))

  (p :hello "Hello, this is a result") ; Falls through (no thread context)

  (defnp my-fn
    []
    (let [nums (vec (range 1000))]
      (+ (p :fast-sleep (Thread/sleep 1) 10)
         (p :slow-sleep (Thread/sleep 2) 32)
         (p :add  (reduce + nums))
         (p :sub  (reduce - nums))
         (p :mult (reduce * nums))
         (p :div  (reduce / nums)))))

  (profile  :info :Arithmetic (dotimes [n 100] (my-fn)))
  (profile  :info :high-n     (dotimes [n 1e5] (p :p1 nil))) ; ~22ms
  (profile  :info :high-n     (dotimes [n 1e6] (p :p1 nil))) ; ~119ms
  (profiled :info (dotimes [n 1e6] (p :p1 nil)))
  (sampling-profile :info 0.5 :sampling-test (p :p1 "Hello!")))

(comment
  (def ^:dynamic *pdata_* "(atom {<id> <times>}), nnil iff profiling" nil)
  (defmacro -with-pdata_ [& body] `(binding [*pdata_* (atom {})] ~@body)) ; Dev

  (comment (qb 1e6 (if *pdata_* true false) (if false true false)))

  (declare ^:private times->stats)
  (defn -capture-time!
    ([       id t-elapsed] (-capture-time! *pdata_* id t-elapsed)) ; Dev
    ([pdata_ id t-elapsed] ; Common case
     (let [?pulled-times
           (loop []
             (let [pdata @pdata_]
               (let [times (get pdata id ())]
                 (if (>= (count times) 2000000) ; Rare in real-world use
                   (if (compare-and-set! pdata_ pdata ; Never leave empty times:
                         (assoc pdata id (conj () t-elapsed)))
                     times ; Pull accumulated times
                     (recur))

                   (if (compare-and-set! pdata_ pdata
                         (assoc pdata id (conj times t-elapsed)))
                     nil
                     (recur))))))]

       (when-let [times ?pulled-times]
         ;; Compact: merge interim stats to help prevent OOMs
         (let [base-stats (get-in @pdata_ [:__stats id])
               stats (times->stats times base-stats)]
           ;; Can safely assume that base-stats should be stable
           (swap! pdata_ assoc-in [:__stats id] stats)))

       nil))))

