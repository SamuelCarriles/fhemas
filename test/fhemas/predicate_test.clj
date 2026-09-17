(ns fhemas.predicate-test
  (:require [clojure.test :refer [deftest testing is]]
            [fhemas.predicate :as pred]))

(deftest normalize-test
  (testing "wraps keyword paths into vectors"
    (is (= [{[:status] "active"}]
           (pred/normalize {:status "active"}))))

  (testing "single map clause becomes vector of one clause"
    (is (= 1 (count (pred/normalize {:a 1})))))

  (testing "vector of clauses stays as vector"
    (is (= 2 (count (pred/normalize [{:a 1} {:b 2}])))))

  (testing "keyword path becomes single-element vector"
    (let [normalized (pred/normalize {:status "active"})
          keys (keys (first normalized))]
      (is (= [[:status]] keys))))

  (testing "vector path stays as vector"
    (let [normalized (pred/normalize {[:meta :status] "active"})
          keys (keys (first normalized))]
      (is (= [[:meta :status]] keys)))))

(deftest ->and-test
  (testing "equality match"
    (let [checker (pred/->and {[:status] "active"})]
      (is (true? (checker {:status "active"})))))

  (testing "equality no match"
    (let [checker (pred/->and {[:status] "active"})]
      (is (false? (checker {:status "draft"})))))

  (testing "set uses contains?"
    (let [checker (pred/->and {[:type] #{"a" "b"}})]
      (is (true? (checker {:type "a"})))
      (is (false? (checker {:type "c"})))))

  (testing "multiple conditions are AND"
    (let [checker (pred/->and {[:a] 1 [:b] 2})]
      (is (true? (checker {:a 1 :b 2})))
      (is (false? (checker {:a 1 :b 3})))))

  (testing "empty clause matches everything"
    (let [checker (pred/->and {})]
      (is (true? (checker {:anything "here"}))))))

(deftest ->checker-test
  (testing "single clause match"
    (let [checker (pred/->checker (pred/normalize {:status "active"}))]
      (is (true? (checker {:status "active"})))))

  (testing "single clause no match"
    (let [checker (pred/->checker (pred/normalize {:status "active"}))]
      (is (false? (checker {:status "draft"})))))

  (testing "multiple clauses are OR"
    (let [checker (pred/->checker (pred/normalize [{:type "a"} {:type "b"}]))]
      (is (true? (checker {:type "a"})))
      (is (true? (checker {:type "b"})))
      (is (false? (checker {:type "c"})))))

  (testing "empty predicate matches everything"
    (let [checker (pred/->checker [])]
      (is (true? (checker {:anything "here"})))))

  (testing "nested paths resolve correctly"
    (let [checker (pred/->checker (pred/normalize {[:meta :status] "active"}))]
      (is (true? (checker {:meta {:status "active"}})))
      (is (false? (checker {:meta {:status "draft"}})))
      (is (false? (checker {})))))

  (testing "nil in set matches missing fields"
    (let [checker (pred/->checker (pred/normalize {[:snapshot] #{nil []}}))]
      (is (true? (checker {})))
      (is (true? (checker {:snapshot []})))
      (is (false? (checker {:snapshot [{:id 1}]})))))

  (testing "set values in checker"
    (let [checker (pred/->checker (pred/normalize {:type #{"cs" "vs"}}))]
      (is (true? (checker {:type "cs"})))
      (is (true? (checker {:type "vs"})))
      (is (false? (checker {:type "sd"})))))

  (testing "checker is reusable across multiple data"
    (let [checker (pred/->checker (pred/normalize {:status "active"}))]
      (is (true? (checker {:status "active"})))
      (is (false? (checker {:status "draft"})))
      (is (true? (checker {:status "active"}))))))
