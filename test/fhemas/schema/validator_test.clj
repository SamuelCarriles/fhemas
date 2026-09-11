(ns fhemas.schema.validator-test
  (:require [clojure.test :refer [deftest is testing]]
            [fhemas.schema.validator :as validator]))

(def valid-validator
  {:resource-type "Validator"
   :id "validator-fhir-r4"
   :entry-dispatcher 'fhemas.r4/dispatch
   :meta {:version-id "1"
          :source "https://github.com/samuelcarriles/fhemas.artifacts"
          :profile "https://github.com/samuelcarriles/fhemas.artifacts/vd/r4"
          :tag [{:system "http://hl7.org/fhir/FHIR-version"
                 :code :r4}]}
   :registry {:order 'fhemas.r4.compile/order
              :elements {:sd-primary {"Patient" [{:path [:id] :type :string}]}}
              :queries {}}})

(deftest validate-valid-validator
  (testing "A valid Validator passes validation and returns the same map"
    (is (= valid-validator (validator/validate valid-validator)))))

(deftest validate-missing-required-fields
  (testing "Missing :entry-dispatcher should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (dissoc valid-validator :entry-dispatcher)))))

  (testing "Missing :meta should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (dissoc valid-validator :meta)))))

  (testing "Missing :registry should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (dissoc valid-validator :registry)))))

  (testing "Missing :resource-type should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (dissoc valid-validator :resource-type))))))

(deftest validate-wrong-resource-type
  (testing "Wrong :resource-type should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc valid-validator :resource-type "Wrong"))))))

(deftest validate-entry-dispatcher-errors
  (testing ":entry-dispatcher is not a qualified-symbol"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc valid-validator :entry-dispatcher "not-a-symbol"))))))

(deftest validate-meta-errors
  (testing "Meta without :version-id should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc-in valid-validator [:meta :version-id] "")))))

  (testing "Meta with non-url :source should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc-in valid-validator [:meta :source] "not-a-url")))))

  (testing "Tag with non-url :system should throw error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc-in valid-validator
                                               [:meta :tag 0 :system]
                                               "not-a-url"))))))

(deftest validate-registry-errors
  (testing ":order is not a qualified-symbol"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc-in valid-validator [:registry :order] "not-a-symbol")))))

  (testing ":elements is not a map"
    (is (thrown? clojure.lang.ExceptionInfo
                 (validator/validate (assoc-in valid-validator [:registry :elements] []))))))

(deftest validate-error-format
  (testing "The thrown error has the correct format"
    (try
      (validator/validate (dissoc valid-validator :meta))
      (is false "Should have thrown exception")
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (is (= :schema/invalid-entry (:code data)))
          (is (= 'fhemas.schema.core/validate (:location data)))
          (is (= :validate-schema (:operation data)))
          (is (contains? (:details data) :value))
          (is (contains? (:details data) :expected)))))))
