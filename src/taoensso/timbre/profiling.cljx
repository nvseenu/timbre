(ns taoensso.timbre.profiling
  "Simple, fast, cross-platform logging profiler for Timbre."
  {:author "Peter Taoussanis (@ptaoussanis)"}
  (:require [taoensso.encore :as enc    :refer-macros ()]
            [taoensso.timbre :as timbre :refer-macros ()])
  #+clj (:import [java.util LinkedList]))

(comment (require '[taoensso.encore :as enc :refer (qb)]))

;;;; Platform stuff

#+clj
(def ^:private elide-profiling? "Completely elide all profiling?"
  (enc/read-sys-val "TIMBRE_ELIDE_PROFILING"))

(defmacro nano-time [] `(enc/if-cljs (System/nanoTime) (enc/nano-time)))
(comment (macroexpand '(nano-time)))

(do ; Time accumulators
  (defn- add-time    [#+clj ^LinkedList x #+cljs ^ArrayList x t] (.add   x t))
  (defn- count-times [#+clj ^LinkedList x #+cljs ^ArrayList x]   (.size  x))
  (defn- clear-times [#+clj ^LinkedList x #+cljs ^ArrayList x]   (.clear x))
  (defn- new-times [] #+clj (LinkedList.) #+cljs (array-list)))

(def -pdata-proxy
  "{<id> <times> :__stats <m-stats> :__clock <m-clock>} iff profiling active
  on thread. This is substantially faster than a ^:dynamic atom. Would
  further benefit from ^:static / direct linking / a Java class."
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

;;;; Core fns

(declare ^:private times->stats)

;;; Low-level utils (undocumented), use with caution
(defn       profiling? [] (if (-pdata-proxy) true false))
(defn start-profiling! [] (-pdata-proxy {:__t0 (nano-time)}) nil)
(defn  stop-profiling! [] ; Returns ?m-stats
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

(defn -capture-time!
  ([      id t-elapsed] (-capture-time! (-pdata-proxy) id t-elapsed)) ; Dev
  ([pdata id t-elapsed] ; Common case
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

   nil))

(def ^:private ^:const max-long #+clj Long/MAX_VALUE #+cljs 9223372036854775807)

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

(defmacro profiled
  "Executes expr with thread-local profiling enabled, then executes body
  with `[<stats> <expr-result>]` binding, e.g:
    (profiled \"foo\" [stats result] (do (println stats) result))"
  [expr-to-profile params & body]
  (assert (vector?    params))
  (assert (= 2 (count params)))
  (let [[stats result] params]
    `(try
       (start-profiling!)
       (let [~result ~expr-to-profile
             ~stats (stop-profiling!)]
         (do ~@body))
       (finally (-pdata-proxy nil)))))

(comment (qb 1e5 (profiled (p :p1) [stats result] [stats result]))) ; 127

(defn- compile-time-pid [id]
  (if (enc/qualified-keyword? id)
    id
    (keyword (str *ns*) (name id))))

(comment (compile-time-pid 'foo))

(defmacro pspy
  "Profiling spy. When thread-local profiling is enabled, records
  execution time of named body. Always returns the body's result."
  [id & body]
  (let [id (compile-time-pid id)]
    (if elide-profiling?
      `(do ~@body)
      `(let [pdata# (-pdata-proxy)]
         (if pdata#
           (let [t0#     (nano-time)
                 result# (do ~@body)
                 t1#     (nano-time)]
             (-capture-time! pdata# ~id (- t1# t0#))
             result#)
           (do ~@body))))))

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

(defn -format-stats
  ([stats           ] (-format-stats stats :time))
  ([stats sort-field]
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
       (str sb)))))

;;;; Helper macros

(defmacro p "Alias for `pspy`" [id & body] `(pspy ~id ~@body))
(comment (macroexpand '(p :foo (+ 4 2))))

(defmacro -runtime-log? [level] `(timbre/log? ~level ~(str *ns*)))
(comment (macroexpand '(-runtime-log? :info)))

(defn -elide? [level-form ns-str-form]
  (or elide-profiling? (timbre/-elide? level-form ns-str-form)))

(defmacro -if-profile
  ([level then else]
   (if (-elide? level (str *ns*))
     else
     (if (= level :report)
       then
       `(if (-runtime-log? ~level) ~then ~else))))

  ([level test then else]
   (if (-elide? level (str *ns*))
     else
     (if (= level :report)
       `(if                             ~test  ~then ~else)
       `(if (and (-runtime-log? ~level) ~test) ~then ~else)))))

(defmacro -if-let-profile [level bindings then else]
  (if (-elide? level (str *ns*))
    else
    (if (= level :report)
      `(if-let ~bindings ~then ~else)
      `(if-let [~(first bindings) (and (-runtime-log? ~level) ~(second bindings))]
         ~then
         ~else))))

(comment (macroexpand '(-if-let-profile :info [a "boo"] :then :else)))

(defmacro -log-stats [level id & body]
  `(profiled (do ~@body) [stats# result#]
     (timbre/log! ~level :p
       ["Profiling: " ~id "\n" (-format-stats stats#)]
       {:?base-data {:profile-stats stats#}})))

(defn- assert-probability [p] (assert (<= 0 p 1) "Probability: 0<=p<=1"))

(defmacro profile
  "When logging is enabled, executes named body with thread-local profiling
  enabled and logs profiling stats. Always returns body's result."
  [level id & body]
  (let [id (compile-time-pid id)]
    `(-if-profile ~level
       (-log-stats ~level ~id ~@body)
       (do ~@body))))

(defmacro cond-profile
  "Like `profile`, but only enables profiling when (pred) is truthy."
  [level id pred & body]
  (let [id (compile-time-pid id)]
    `(-if-profile ~level (~pred)
       (-log-stats ~level ~id ~@body)
       (do ~@body))))

(defmacro sampling-profile
  "Like `profile`, but only enables profiling with given probability."
  [level id probability & body]
  (assert-probability probability)
  (let [id (compile-time-pid id)]
    `(-if-profile ~level (< (rand) ~probability)
       (-log-stats ~level ~id ~@body)
       (do ~@body))))

;;;; Multi-threaded profiling

(defn merge-stats
  "Merges stats maps from multiple runs, threads, etc."
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
  Small util to help merge stats maps from multiple threads. Returns a
  stateful fn with arities:
    ([stats-map]) ; Accumulates the given stats (you may call this from any thread)
    ([])          ; Returns the merged value of all accumulated stats

  See also `profile-into`."
  []
  (let [acc_ (atom nil)]
    (fn stats-accumulator
      ([stats-map] (swap! acc_ conj stats-map))
      ([] (when-let [acc @acc_] (reduce merge-stats nil acc))))))

(defmacro profile-into
  "Experimental, subject to change!
  When logging is enabled, executes body with thread-local profiling
  enabled and merges stats into given stats accumulator. Always returns
  body's result.

  See also `stats-accumulator`."
  [?stats-accumulator level & body]
  `(-if-let-profile ~level [sacc# ~?stats-accumulator]
     (profiled (do ~@body) [stats# result#]
       (sacc# stats#) result#)
     (do ~@body)))

(defmacro cond-profile-into
  "Like `profile-into`, but only enables profiling when (pred) is truthy."
  [?stats-accumulator level pred & body]
  `(-if-let-profile ~level
     [sacc# (when-let [sacc# ~?stats-accumulator] (and (~pred) sacc#))]
     (profiled (do ~@body) [stats# result#]
       (sacc# stats#) result#)
     (do ~@body)))

(defmacro sampling-profile-into
  "Like `profile-into`, but only enables profiling with given probability."
  [?stats-accumulator level probability & body]
  (assert-probability probability)
  `(-if-let-profile ~level
     [sacc# (when-let [sacc# ~?stats-accumulator]
              (and (< (rand) ~probability) sacc#))]
     (profiled (do ~@body) [stats# result#]
       (sacc# stats#) result#)
     (do ~@body)))


(def ^:dynamic *dynamic-stats-accumulator* nil)
(defmacro  with-dynamic-stats "Experimental, subject to change!"
  [& body]
  `(binding [*dynamic-stats-accumulator* (stats-accumulator)]
     ~@body))

(defn get-dynamic-stats "Experimental, subject to change!"
  [] (when-let [sacc *dynamic-stats-accumulator*] (sacc)))

(defmacro dynamic-profile
  "Experimental, subject to change!
  Like `(profile-into *dynamic-stats-accumulator* ...)`.

  See also `with-dynamic-stats`, `get-dynamic-stats`."
  [level id & body]
  `(profile-into *dynamic-stats-accumulator* ~level ~id ~@body))

(comment
  (with-dynamic-stats
    (dynamic-profile :info :foo "boo")
    (get-dynamic-stats)))

(defmacro sampling-dynamic-profile "Experimental, subject to change!"
  [level probability id & body]
  (assert-probability probability)
  (if elide-profiling?
    `(do ~@body)
    `(if (< (rand) ~probability)
       (profile-into *dynamic-stats-accumulator* ~level ~id ~@body)
       (do                                                  ~@body))))

;;;; fnp stuff

(defn -fn-sigs [fn-name sigs]
  (let [single-arity? (vector? (first sigs))
        sigs    (if single-arity? (list sigs) sigs)
        get-id  (if single-arity?
                  (fn [fn-name _params]      (name fn-name))
                  (fn [fn-name  params] (str (name fn-name) \_ (count params))))
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
        new-sigs        (-fn-sigs (or ?fn-name 'anonymous-fn) sigs)]
    (if ?fn-name
      `(fn ~?fn-name ~@new-sigs)
      `(fn           ~@new-sigs))))

(comment
  (-fn-sigs "foo"      '([x]            (* x x)))
  (macroexpand '(fnp     [x]            (* x x)))
  (macroexpand '(fn       [x]            (* x x)))
  (macroexpand '(fnp bob [x] {:pre [x]} (* x x)))
  (macroexpand '(fn       [x] {:pre [x]} (* x x))))

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

  (profile :info :Arithmetic (dotimes [n 100] (my-fn)))
  (profile :info :high-n     (dotimes [n 1e5] (p :nil nil))) ; ~20ms
  (profile :info :high-n     (dotimes [n 1e6] (p :nil nil))) ; ~116ms
  (profiled (dotimes [n 1e6] (p :nil nil)) [stats result] [stats result])
  (sampling-profile :info 0.5 :sampling-test (p :string "Hello!")))
