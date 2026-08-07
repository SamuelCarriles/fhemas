(ns fhemas.validator-definition.field.validate-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fhemas.validator-definition.field.validate :as validate]))

(defn throws-error? [code f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= code (:code (ex-data e))))))

;; ---------------------------------------------------------------------------
;; cardinality
;; ---------------------------------------------------------------------------

(deftest cardinality-no-bounds-test
  (testing "returns field unchanged when no min or max"
    (is (= {:path [:x] :value "a"}
           (validate/cardinality {:path [:x] :value "a"})))))

(deftest cardinality-min-test
  (testing "passes when scalar value meets min (count = 1)"
    (is (= {:path [:x] :min 1 :value "a"}
           (validate/cardinality {:path [:x] :min 1 :value "a"}))))
  (testing "passes when vector value meets min"
    (is (= {:path [:x] :min 2 :value ["a" "b"]}
           (validate/cardinality {:path [:x] :min 2 :value ["a" "b"]}))))
  (testing "fails when scalar value is below min"
    (is (throws-error? :invalid/cardinality
                       #(validate/cardinality {:path [:x] :min 2 :value "a"}))))
  (testing "fails when value is nil and min > 0"
    (is (throws-error? :invalid/cardinality
                       #(validate/cardinality {:path [:x] :min 1 :value nil})))))

(deftest cardinality-max-test
  (testing "passes when scalar value meets max (count = 1)"
    (is (= {:path [:x] :max 1 :value "a"}
           (validate/cardinality {:path [:x] :max 1 :value "a"}))))
  (testing "passes when vector value meets max"
    (is (= {:path [:x] :max 2 :value ["a" "b"]}
           (validate/cardinality {:path [:x] :max 2 :value ["a" "b"]}))))
  (testing "fails when vector value exceeds max"
    (is (throws-error? :invalid/cardinality
                       #(validate/cardinality {:path [:x] :max 1 :value ["a" "b"]}))))
  (testing "passes when value is nil and max >= 0 (count = 0)"
    (is (= {:path [:x] :max 1 :value nil}
           (validate/cardinality {:path [:x] :max 1 :value nil})))))

(deftest cardinality-min-and-max-test
  (testing "passes when value is between min and max"
    (is (= {:path [:x] :min 1 :max 3 :value ["a" "b"]}
           (validate/cardinality {:path [:x] :min 1 :max 3 :value ["a" "b"]}))))
  (testing "fails when value is below min"
    (is (throws-error? :invalid/cardinality
                       #(validate/cardinality {:path [:x] :min 2 :max 3 :value ["a"]}))))
  (testing "fails when value is above max"
    (is (throws-error? :invalid/cardinality
                       #(validate/cardinality {:path [:x] :min 1 :max 2 :value ["a" "b" "c"]}))))

  (testing "error contains correct details for min failure"
    (try
      (validate/cardinality {:path [:x] :min 2 :value "a"})
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (is (= :invalid/cardinality (:code data)))
          (is (= :validate-cardinality (:operation data)))
          (is (= "a" (get-in data [:details :value])))
          (is (= 2 (get-in data [:details :cardinality :expected]))))))))

;; ---------------------------------------------------------------------------
;; type
;; ---------------------------------------------------------------------------

(deftest type-nil-test
  (testing "returns field unchanged when type is nil (used for polymorphic fields)"
    (is (= {:path [:x] :value "anything"}
           (validate/type {:path [:x] :value "anything"})))))

(deftest type-string-test
  (testing "passes for valid string"
    (is (= {:path [:x] :type :string :value "hello"}
           (validate/type {:path [:x] :type :string :value "hello"}))))
  (testing "fails for non-string"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :string :value 123})))))

(deftest type-integer-test
  (testing "passes for valid integer"
    (is (= {:path [:x] :type :integer :value 42}
           (validate/type {:path [:x] :type :integer :value 42}))))
  (testing "fails for non-integer"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :integer :value "42"})))))

(deftest type-vector-test
  (testing "passes for valid vector"
    (is (= {:path [:x] :type :vector :value [1 2 3]}
           (validate/type {:path [:x] :type :vector :value [1 2 3]}))))
  (testing "fails for list"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :vector :value '(1 2 3)}))))
  (testing "fails for map"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :vector :value {}})))))

(deftest type-boolean-test
  (testing "passes for true"
    (is (= {:path [:x] :type :boolean :value true}
           (validate/type {:path [:x] :type :boolean :value true}))))
  (testing "passes for false"
    (is (= {:path [:x] :type :boolean :value false}
           (validate/type {:path [:x] :type :boolean :value false}))))
  (testing "fails for non-boolean"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :boolean :value "true"})))))

(deftest type-map-test
  (testing "passes for valid map"
    (is (= {:path [:x] :type :map :value {:a 1}}
           (validate/type {:path [:x] :type :map :value {:a 1}}))))
  (testing "fails for non-map"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :map :value [1 2]})))))

(deftest type-uri-test
  (testing "passes for valid http URI"
    (is (= {:path [:x] :type :uri :value "http://example.org"}
           (validate/type {:path [:x] :type :uri :value "http://example.org"}))))
  (testing "passes for valid URN"
    (is (= {:path [:x] :type :uri :value "urn:oid:1.2.3.4"}
           (validate/type {:path [:x] :type :uri :value "urn:oid:1.2.3.4"}))))
  (testing "fails for invalid URI (e.g., contains spaces)"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :uri :value "not a uri"}))))
  (testing "fails for non-string"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :uri :value 123})))))

(deftest type-unsupported-test
  (testing "fails for unsupported type keyword"
    (is (throws-error? :invalid/type
                       #(validate/type {:path [:x] :type :bogus :value "x"})))))
