(ns vatch-test
  (:require [clojure.test :refer [deftest are is testing]]
            [io.github.gaverhae.vatch :refer [vatch fatch postvalk]]))

;; Real test would need alpha-equivalence, which is a bit more work than I'm
;; willing to put in right now. So, instead, examples.

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
      (is (thrown? clojure.lang.ExceptionInfo (f [:does-not-match]))))))

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
