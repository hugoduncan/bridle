(ns bridle.constraint
  (:require
   [clojure.core.match :as match]
   [clojure.set :as set])
  (:use
   [slingshot.slingshot :only [throw+]]))

(defn key-not-found? [x] (= x :clojure.core.match/not-found))
(defn key-found? [x] (not= x :clojure.core.match/not-found))

(defn- check-metadata
  "Extracts a map if passed as the first argument, and builds a map of the
remaining arguments."
  [args]
  (if (map? (first args))
    [(first args) (apply hash-map (rest args))]
    [nil (apply hash-map args)]))

(defmacro map-check
  "Builds a predicate to check the structure of a map. You may specify :has
and :allow keys.  any constraints on component types are specified in a map from
key to constraint, passed to :types.

If supplied, metadata can include :name and :doc."
  {:arglists '([& {:keys [has allow types]}]
                 [metadata & {:keys [has allow types]}])}
  [& opts]
  (let [[{:keys [name doc]} {:keys [has allow types]}] (check-metadata opts)
        allow-specified? allow ;; allow specification of empty sequence
        allow (set allow)
        inferred (keys types)
        has (set has)
        map-keys (fn []
                   (into {}
                         (map
                          (fn possibly-guarded [k]
                            (let [t (get types k)]
                              (vector
                               k
                               (list
                                '_
                                :when
                                (let [t (or t [])
                                      t (if (sequential? t) t [t])]
                                  (vec
                                   (concat
                                    (if (has k) [`key-found?])
                                    (map
                                     (fn [t1]
                                       (if (has k)
                                         t1
                                         `(fn [x#]
                                            (or (key-not-found? x#)
                                                (~t1 x#)))))
                                     t))))
                                t))))
                          (distinct (concat has allow (keys types))))))
        err-fn-sym (gensym)
        err-fn `(fn [~'s]
                  ~(when (seq has)
                     `(when-let [missing# (set/difference
                                           ~has (set (keys ~'s)))]
                        (throw+
                         (merge
                          (~'base-map :has)
                          {:missing-keys missing#})
                         (str ~(or name (list 'quote &form)) " failed"))))
                  ~(when (or (seq has) (seq allow) (seq inferred))
                     `(when-let [additional# (set/difference
                                              (set (keys ~'s))
                                              ~(set/union has allow inferred))]
                        (throw+
                         (merge
                          (~'base-map :allow)
                          {:additional-keys additional#})
                         (str ~(or name (list 'quote &form)) " failed"))))
                  ~(when (seq types)
                     `(let [types# ~types]
                        (when-let [failed# (filter
                                            (fn [[dk# dv#]]
                                              (when-let [f# (get types# dk#)]
                                                (not (f# dv#))))
                                            ~'s)]
                          (throw+
                           (merge
                            (~'base-map :types)
                            {:failed-types (into {} failed#)})
                           (str ~(or name (list 'quote &form)) " failed")))))
                  nil)]
    `(fn [data#]
       (letfn [(~'base-map [clause#] {:type :seq-check-failed
                                      :name ~name
                                      :form '~&form
                                      :failed-clause clause#
                                      :line ~(-> &form meta :line)
                                      :file ~*file*})]
         (and
          (or (instance? java.util.Map data#)
              (throw+ (merge
                       (~'base-map :type)
                       {:actual-type (type data#)})))
          (let [~err-fn-sym ~err-fn]
            (match/match
             [data#]
             [~(if allow-specified?
                 (list (map-keys) :only (vec (concat has allow)))
                 (map-keys))] true
             [~'s] (~err-fn-sym ~'s))))))))

(defmacro seq-check
  "Builds a predicate to check the structure of a seq. You may specify :count or
:min-count to check the count of the seq, and :every? and :some to pass a
predicate to check each element in the seq.

If supplied, metadata can include :name and :doc."
  {:arglists '([& {:keys [count min-count every? some]}]
                 [metadata & {:keys [count min-count every? some]}])}
  [& opts]
  (let [[{:keys [name doc]}
         {:keys [count min-count every? some]}] (check-metadata opts)
         _   (when (and count min-count)
               (throw (IllegalArgumentException.
                       (str "Can not specify count and min-count for "
                            (or name &form)))))
         [every?-fn some-fn count-fn min-count-fn] (repeatedly 4 gensym)
         fns {every?-fn (when every? `(fn [~'s] (every? ~every? ~'s)))
              some-fn (when some `(fn [~'s] (some ~some ~'s)))
              count-fn (when count `(fn [~'s] (= ~count (count ~'s))))
              min-count-fn (when min-count
                             `(fn [~'s] (>= (count ~'s) ~min-count)))}
         fns (into {} (filter val fns))
         err-fn-sym (gensym)
         err-fn `(fn [~'s]
                   ~(when every?
                      `(when-not (every? ~every? ~'s)
                         (throw+
                          (merge
                           (~'base-map :every?)
                           {:failed-values
                            (take 5 (remove ~every? ~'s))})
                          (str ~(or name (list 'quote &form)) " failed"))))
                   ~(when some
                      `(when-not (some ~some ~'s)
                         (throw+
                          (merge
                           (~'base-map :some)
                           {:failed-values (take 5 ~'s)})
                          (str ~(or name (list 'quote &form)) " failed"))))
                   ~(when count
                      `(when-not (= ~count (count ~'s))
                         (throw+
                          (merge
                           (~'base-map :count)
                           {:expected-count ~count
                            :actual-count (count ~'s)})
                          (str ~(or name (list 'quote &form)) " failed"))))
                   ~(when min-count
                      `(when (> ~min-count (count ~'s))
                         (throw+
                          (merge
                           (~'base-map :min-count)
                           {:min-count ~min-count
                            :actual-count (count ~'s)})
                          (str ~(or name (list 'quote &form)) " failed"))))
                   nil)]
    `(fn [data#]
       (letfn [(~'base-map [clause#] {:type :seq-check-failed
                                      :name ~name
                                      :form '~&form
                                      :failed-clause clause#
                                      :line ~(-> &form meta :line)
                                      :file ~*file*})]
         (and
          (and (or (seq? data#) (sequential? data#)
                   (throw+ (merge
                            (~'base-map :type)
                            {:actual-type (type data#)}))))
          (let [~@(mapcat identity fns)
                ~err-fn-sym ~err-fn]
            (match/match
             [data#]
             [~(if (or every? some count min-count)
                 `((:or ([] :seq) ([_# & __#] :seq))
                   :when [~@(keys fns)])
                 `(:or ([] :seq) ([_# & __#] :seq)))] true
             [~'s] (~err-fn-sym ~'s))))))))
