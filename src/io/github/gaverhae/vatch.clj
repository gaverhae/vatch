(ns io.github.gaverhae.vatch
  (:require [clojure.walk :refer [postwalk]]))

(defn fail
  [pat]
  (throw (ex-info "Unsupported pattern." {:pattern pat})))

(defn literal-atom?
  [p]
  (or (keyword? p)
      (number? p)
      (string? p)
      (boolean? p)
      (nil? p)))

(declare match-vector)

(defn match-vector-pat-elems
  [pats r]
  (->> pats
       (map-indexed vector)
       (mapcat (fn [[idx pat]]
                 (cond (literal-atom? pat) [`(= ~pat (if (indexed? ~r)
                                                       (get ~r ~idx)
                                                       (nth ~r ~idx)))]
                       (symbol? pat) []
                       (vector? pat) [`(let [~r (if (indexed? ~r)
                                                  (get ~r ~idx)
                                                  (nth ~r ~idx))]
                                         ~(match-vector pat r))]
                       :else (fail pat))))))

(defn let-vector-pat-elems
  [pats r]
  (->> pats
       (map-indexed vector)
       (mapcat (fn [[idx pat]]
                 (cond (literal-atom? pat) []
                       (symbol? pat) [pat `(if (indexed? ~r)
                                             (get ~r ~idx)
                                             (nth ~r ~idx))]
                       (vector? pat) (->> (let-vector-pat-elems pat r)
                                          (partition 2)
                                          (mapcat (fn [[sym expr]]
                                                    [sym `(let [~r (if (indexed? ~r)
                                                                     (get ~r ~idx)
                                                                     (nth ~r ~idx))]
                                                            ~expr)])))
                       :else (fail pat))))))

(defn match-vector
  [pat r]
  (if (contains? (set pat) '&)
    (let [pre-pat (vec (take-while (fn [p] (not= '& p)) pat))
          post-pat (last pat)]
      (assert (= 1 (->> pat (filter #(= '& %)) count))
              "If & appears in a pattern, it must appear exactly once.")
      (assert (= 2 (- (count pat) (count pre-pat)))
              "If & appears in a pattern, there can only be exactly one pattern after it.")
      `(and (sequential? ~r)
            (>= (count ~r) ~(count pre-pat))
            ;; TODO: check post-pat
            ~@(match-vector-pat-elems pre-pat r)))
    `(and (sequential? ~r)
          (= (count ~r) ~(count pat))
          ~@(match-vector-pat-elems pat r))))

(defn let-vector
  [pat r]
  (if (contains? (set pat) '&)
    (let [pre-pat (vec (take-while (fn [p] (not= '& p)) pat))
          post-pat (last pat)]
      (concat (let-vector-pat-elems pre-pat r)
              [post-pat `(drop ~(count pre-pat) ~r)]))
    (let-vector-pat-elems pat r)))

(defn match-pattern
  [pat r]
  (cond (symbol? pat) true
        (vector? pat) (match-vector pat r)
        :else (fail pat)))

(defn let-pattern
  [pat r body]
  (cond (symbol? pat) `(let [~pat ~r] ~body)
        (vector? pat) `(let [~@(let-vector pat r)]
                         ~body)))

(defmacro vatch
  "Pared-down version of core.match/match, specialized for variants. Assumes
   expr is a seq, with a fast path for vectors, and matches on length and all
   top-level keywords. Non-keyword elements of patterns are bound as per let.
   Symbols are NOT captured. Use _ as pattern for default case.

   See tests for examples."
  [expr & clauses]
  (let [r (gensym)]
    `(let [~r ~expr]
       (cond
         ~@(->> (partition 2 clauses)
                (mapcat (fn [[pat body]]
                          [(match-pattern pat r) (let-pattern pat r body)])))
         :else (throw (ex-info (str "Value did not vatch any clause: " (pr-str ~expr))
                               {:expr ~expr}))))))

(defmacro fatch
  "Convenience macro for cases where you need a function of a single argument,
   and the only thing you do with it is pass it as the first argument to
   `vatch`. Binds `!` as the fn name for recursion. I.e.

       (fatch [:kw a] (println a))

   is equivalent to:

       (fn ! [arg#] (vatch arg# [:kw a] (println a)))"
  [& forms]
  `(fn ~'! [arg#]
     (vatch arg# ~@forms)))

(defmacro postvalk
  [expr & clauses]
  (let [default (gensym)]
    `(postwalk (fatch ~@clauses ~default ~default)
               ~expr)))

(defmacro vatch->
  [sym expr & clauses]
  `(let [~sym ~expr]
     (vatch ~sym ~@clauses)))
