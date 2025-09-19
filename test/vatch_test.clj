(ns vatch-test
  (:require [clojure.test :refer [deftest are is testing]]
            [io.github.gaverhae.vatch :as v :refer [vatch fatch postvalk]]))

(deftest expansion
  (let [actual (macroexpand-1 `(vatch ~'v
                                 [:add ~'a ~'b] [:add2 (+ ~'a ~'b)]
                                 [:add ~'a ~'b ~'c] [:add3 (+ ~'a ~'b ~'c)]
                                 ~'_ :no-match))
        gs (-> actual second first)]
    (is (= `(let [~gs ~'v]
              (cond (and (sequential? ~gs)
                         (= (count ~gs) 3)
                         (= :add (if (indexed? ~gs)
                                   (get ~gs 0)
                                   (nth ~gs 0))))
                    (let [~'a (if (indexed? ~gs)
                                (get ~gs 1)
                                (nth ~gs 1))
                          ~'b (if (indexed? ~gs)
                                (get ~gs 2)
                                (nth ~gs 2))]
                      [:add2 (+ ~'a ~'b)])
                    (and (sequential? ~gs)
                         (= (count ~gs) 4)
                         (= :add (if (indexed? ~gs)
                                   (get ~gs 0)
                                   (nth ~gs 0))))
                    (let [~'a (if (indexed? ~gs)
                                (get ~gs 1)
                                (nth ~gs 1))
                          ~'b (if (indexed? ~gs)
                                (get ~gs 2)
                                (nth ~gs 2))
                          ~'c (if (indexed? ~gs)
                                (get ~gs 3)
                                (nth ~gs 3))]
                      [:add3 (+ ~'a ~'b ~'c)])
                    true (let [~'_ ~gs]
                           :no-match)
                    :else (throw (ex-info (str "Value did not vatch any clause: " (pr-str ~'v)) {:expr ~'v}))))
           actual))))

(deftest expansion-match
  (is (= `(and (sequential? ~'G__3022)
               (= (count ~'G__3022) 3)
               (= :add (if (indexed? ~'G__3022)
                         (get ~'G__3022 0)
                         (nth ~'G__3022 0))))
         (v/match-pattern '[:add a b] 'G__3022)))
  (is (= `(and (sequential? ~'G__1)
               (= (count ~'G__1) 2)
               (let [~'G__1 (if (indexed? ~'G__1)
                              (get ~'G__1 0)
                              (nth ~'G__1 0))]
                 (and (sequential? ~'G__1)
                      (= (count ~'G__1) 2)
                      (= :symbol (if (indexed? ~'G__1)
                                   (get ~'G__1 0)
                                   (nth ~'G__1 0)))
                      (= "x" (if (indexed? ~'G__1)
                               (get ~'G__1 1)
                               (nth ~'G__1 1))))))
         (v/match-pattern '[[:symbol "x"] expr] 'G__1))))

(deftest expansion-let
  (is (= `(let [~'a (if (indexed? ~'G__3021)
                      (get ~'G__3021 1)
                      (nth ~'G__3021 1))
                ~'b (if (indexed? ~'G__3021)
                      (get ~'G__3021 2)
                      (nth ~'G__3021 2))]
            :body)
         (v/let-pattern '[:add a b] 'G__3021 :body)))
  (is (= `(let [~'a (if (indexed? ~'G__3011)
                      (get ~'G__3011 0)
                      (nth ~'G__3011 0))
                ~'b (let [~'G__3011 (if (indexed? ~'G__3011)
                                      (get ~'G__3011 1)
                                      (nth ~'G__3011 1))]
                      (if (indexed? ~'G__3011)
                        (get ~'G__3011 1)
                        (nth ~'G__3011 1)))
                ~'c (if (indexed? ~'G__3011)
                      (get ~'G__3011 2)
                      (nth ~'G__3011 2))
                ~'d (let [~'G__3011 (if (indexed? ~'G__3011)
                                      (get ~'G__3011 4)
                                      (nth ~'G__3011 4))]
                      (let [~'G__3011 (if (indexed? ~'G__3011)
                                        (get ~'G__3011 1)
                                        (nth ~'G__3011 1))]
                        (if (indexed? ~'G__3011)
                          (get ~'G__3011 1)
                          (nth ~'G__3011 1))))
                ~'e (let [~'G__3011 (if (indexed? ~'G__3011)
                                      (get ~'G__3011 4)
                                      (nth ~'G__3011 4))]
                      (if (indexed? ~'G__3011)
                        (get ~'G__3011 2)
                        (nth ~'G__3011 2)))]
            :body)
         (v/let-pattern '[a [:x b] c ["skip"] [:y [:z d] e]] 'G__3011 :body))))

(deftest test-vatch
  (testing "basic examples"
    (let [f (fn [v]
              (vatch v
                [:add2 a b] (+ a b)
                [:add3 a b c] (+ a b c)
                [:add-all & xs] (reduce + 0 xs)
                [:mul2 a b] (* a b)
                [:div a b] (quot a b)
                _ :no-match))]
      (are [x y] (= x (f y))
        :no-match [:add3 1 2 3 4]
        6 [:add3 1 2 3]
        3 [:add2 1 2]
        :no-match [:mul2 3 3 12]
        9 [:mul2 3 3]
        4 [:add2 1 3]
        2 [:div 8 3]
        10 [:add-all 1 2 3 4])))
  (testing "order matters!"
    (is (= :no-match
           (vatch [:add 1 2]
             short-circuit :no-match
             [:add x y] (+ x y)))
        (= 10
           (vatch [:add 1 2 3 4]
             [:add & xs] (reduce + 0 xs)
             [:add x y z t] (* x y z t)))))
  (testing "symbols are not captured"
    (let [v [:add 1 4]
          x :should-be-ignored]
      (is (= 5
             (vatch v
               [:add x y] (+ x y)
               _ :no-match)))))
  (testing "top-level keywords must match"
    (let [f (fn [v]
              (vatch v
                [:a :b n] (+ n 3)
                [:a x y] (+ x y)))]
    (is (= 8 (f [:a :b 5])))
    (is (= 10 (f [:a 4 6])))))
  (testing "throws on unmatched expr"
    (let [f (fn [v]
              (vatch v
                [:a b] b))]
      (is (thrown? clojure.lang.ExceptionInfo (f [:does-not-match])))))
  (testing "nested values"
    (let [f (fn [v]
              (vatch v
                [[:symbol x] expr] [x expr]
                otherwise :did-not-match))]
      (is (= ["a" "expr"]
             (f [[:symbol "a"] "expr"])))
      (is (= :did-not-match
             (f :ploup))))))

(deftest test-fatch
  (testing "lambda match"
    (is (= 15
           (->> [[:one] [:two] [:val 3] [:mod 14 10] [:plus 2 3] [:ignored "hello"]]
                (map (fatch
                       [:one] 1
                       [:two] 2
                       [:val v] v
                       [:mod a b] (mod a b)
                       [:plus a b] (+ a b)
                       anything-else? 0))
                (reduce + 0))))))

(deftest test-postvalk
  (testing "example use"
    (is (= [:div ["here is"] ["a list"] [:ul.list-disc.ml-6 [:li "item 1"] [:li ["item" " 2"]]]]
           (postvalk [:div [:<> "here is"] [:<> "a list"] [:ul [:li "item 1"] [:li [:<> "item" " 2"]]]]
             [:<> & args] args
             [:ul & args] `[:ul.list-disc.ml-6 ~@args]
             _ _)))))
