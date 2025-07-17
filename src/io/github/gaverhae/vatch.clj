(ns io.github.gaverhae.vatch)

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

(defmacro vatch
  "Pared-down version of core.match/match, specialized for variants. Assumes
   expr is a seq, with a fast path for vectors, and matches on length and all
   top-level keywrods. Non-keyword elements of patterns are bound as per let.
   Symbols are NOT captured. Use _ as pattern for default case.

   See tests for examples."
  [expr & clauses]
  (let [r (gensym)]
    `(let [~r ~expr]
       (cond
         ~@(->> (partition 2 clauses)
                (mapcat (fn [[pat body]]
                          (cond (symbol? pat) [true `(let [~pat ~r] ~body)]
                                (vector? pat) (if (contains? (set pat) '&)
                                                (let [pre-pat (vec (take-while (fn [p] (not= '& p)) pat))
                                                      post-pat (last pat)]
                                                  [`(and (sequential? ~r)
                                                         (>= (count ~r) ~(count pre-pat))
                                                         ~@(match-vector-pat-elems pre-pat r))
                                                   `(let [~@(let-vector-pat-elems pre-pat r)
                                                          ~post-pat (drop ~(count pre-pat) ~r)]
                                                      ~body)])
                                                [`(and (sequential? ~r)
                                                       (= (count ~r) ~(count pat))
                                                       ~@(match-vector-pat-elems pat r))
                                                 `(let [~@(let-vector-pat-elems pat r)]
                                                    ~body)])
                                :else (throw (ex-info "Unsupported pattern." {:pattern pat}))))))))))

(defmacro fatch
  [& forms]
  `(fn [arg#]
     (vatch arg# ~@forms)))

(defmacro postvalk
  [expr & clauses]
  (let [default (gensym)]
    `(postwalk (fatch ~@clauses ~default ~default)
               ~expr)))
