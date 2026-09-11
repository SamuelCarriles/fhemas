(ns fhemas.schema.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [fhemas.schema.core :as core]))

(deftest url?-test
  (testing "Valid URLs should return true"
    (is (true? (core/url? "http://hl7.org/fhir")))
    (is (true? (core/url? "https://example.com/path")))
    (is (true? (core/url? "https://example.com/path?query=value")))
    (is (true? (core/url? "file:///tmp/file.json"))))

  (testing "Invalid URLs should return false"
    (is (false? (core/url? "not a url")))
    (is (false? (core/url? "")))
    (is (false? (core/url? nil)))
    (is (false? (core/url? 123)))))

(deftest validate-function-test
  (let [simple-schema [:map [:name :string] [:age :int]]]

    (testing "Valid input returns the same value"
      (let [input {:name "test" :age 42}]
        (is (= input (core/validate simple-schema input "error message")))))

    (testing "Invalid input throws ExceptionInfo"
      (is (thrown? clojure.lang.ExceptionInfo
                   (core/validate simple-schema {:name "test" :age "not-int"} "error")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (core/validate simple-schema {:name "test"} "error")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (core/validate simple-schema nil "error"))))

    (testing "The exception has the correct format"
      (try
        (core/validate simple-schema {:name "test"} "my error message")
        (is false "should have thrown exception")
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            (testing "Exception message matches the provided error-msg"
              (is (= "my error message" (.getMessage e))))
            (testing "Exception data contains :schema/invalid-entry code"
              (is (= :schema/invalid-entry (:code data))))
            (testing "Exception data contains location"
              (is (= 'fhemas.schema.core/validate (:location data))))
            (testing "Exception data contains operation"
              (is (= :validate-schema (:operation data))))
            (testing "Exception details contain :value and :expected"
              (is (contains? (:details data) :value))
              (is (= {:name "test"} (get-in data [:details :value])))
              (is (contains? (:details data) :expected)))))))))
