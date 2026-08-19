(ns fhemas.match-test
  (:require [clojure.test :refer [deftest testing are is]]
            [fhemas.match :as match]))

;; ---------------------------------------------------------------------------
;; submap?
;; ---------------------------------------------------------------------------

(deftest submap?-test
  (testing "returns true when m1 is a submap of m2"
    (is (true? (match/submap? {:a 1} {:a 1 :b 2}))))
  (testing "returns true when m1 equals m2"
    (is (true? (match/submap? {:a 1} {:a 1}))))
  (testing "returns false when m1 has a key not in m2"
    (is (false? (match/submap? {:a 1 :c 3} {:a 1 :b 2}))))
  (testing "returns false when values don't match"
    (is (false? (match/submap? {:a 1} {:a 2}))))
  (testing "empty map is a submap of anything"
    (is (true? (match/submap? {} {:a 1}))))
  (testing "returns false when either m1 or m2 is nil"
    (are [m1 m2] (is (false? (match/submap? m1 m2)))
      nil {:a 2}
      {:a 2} nil
      nil nil)))

;; ---------------------------------------------------------------------------
;; valid?
;; ---------------------------------------------------------------------------

(deftest valid?-test
  (testing "returns true when conds is empty"
    (is (true? (match/satisfies? [] {:resource-type "StructureDefinition"}))))
  (testing "returns true when conds is nil"
    (is (true? (match/satisfies? nil {:resource-type "StructureDefinition"}))))
  (testing "returns true when one condition matches (OR logic)"
    (is (true? (match/satisfies? [{:resource-type "StructureDefinition"}
                                  {:resource-type "ValueSet"}]
                                 {:resource-type "StructureDefinition"}))))
  (testing "returns false when no condition matches"
    (is (false? (match/satisfies? [{:resource-type "StructureDefinition"}]
                                  {:resource-type "ValueSet"}))))
  (testing "multiple keys in one condition are AND logic"
    (is (true? (match/satisfies? [{:resource-type "StructureDefinition" :kind "resource"}]
                                 {:resource-type "StructureDefinition" :kind "resource" :name "Patient"})))
    (is (false? (match/satisfies? [{:resource-type "StructureDefinition" :kind "resource"}]
                                  {:resource-type "StructureDefinition" :kind "primitive-type"})))))


