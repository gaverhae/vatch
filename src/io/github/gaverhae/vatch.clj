(ns io.github.gaverhae.vatch
  (:require [clojure.walk :refer [postwalk]]))

(defn literal-atom?
  [p]
  (or (keyword? p)
      (number? p)
      (string? p)
      (nil? p)))

(defn match-vector-pat-elems
  [pats r]
  (->> pats
       (map-indexed vector)
       (mapcat (fn [[idx pat]]
                 (when (literal-atom? pat)
                   [`(= ~pat (if (indexed? ~r)
                               (get ~r ~idx)
                               (nth ~r ~idx)))])))))

(defn let-vector-pat-elems
  [pats r]
  (->> (range (count pats))
       (mapcat (fn [idx]
                 (when (not (literal-atom? (get pats idx)))
                   [(get pats idx) `(if (indexed? ~r)
                                      (get ~r ~idx)
                                      (nth ~r ~idx))])))))

(defn match-vector
  [pat r]
  (if (contains? (set pat) '&)
    (let [pre-pat (vec (take-while (fn [p] (not= '& p)) pat))
          post-pat (last pat)]
      `(and (sequential? ~r)
            (>= (count ~r) ~(count pre-pat))
            ~@(match-vector-pat-elems pre-pat r)))
    `(and (sequential? ~r)
          (= (count ~r) ~(count pat))
          ~@(match-vector-pat-elems pat r))))

(defn let-vector
  [pat r body]
  (if (contains? (set pat) '&)
    (let [pre-pat (vec (take-while (fn [p] (not= '& p)) pat))
          post-pat (last pat)]
      `(let [~@(let-vector-pat-elems pre-pat r)
             ~post-pat (drop ~(count pre-pat) ~r)]
         ~body))
    `(let [~@(let-vector-pat-elems pat r)]
       ~body)))

(defn match-pattern
  [pat r]
  (cond (symbol? pat) true
        (vector? pat) (match-vector pat r)
        :else (throw (ex-info "Unsupported pattern." {:pattern pat}))))

(defn let-pattern
  [pat r body]
  (cond (symbol? pat) `(let [~pat ~r] ~body)
        (vector? pat) (let-vector pat r body)))

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
  [& forms]
  `(fn [arg#]
     (vatch arg# ~@forms)))

(defmacro postvalk
  [expr & clauses]
  (let [default (gensym)]
    `(postwalk (fatch ~@clauses ~default ~default)
               ~expr)))
