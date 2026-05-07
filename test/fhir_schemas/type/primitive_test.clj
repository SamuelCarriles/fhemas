(ns fhir-schemas.type.primitive-test
  (:require [clojure.test :refer [is are deftest testing]]
            [fhir-schemas.type.primitive :as tp]))

(deftest not-blank-str?-test
  (testing "Returns true for non-blank strings"
    (are [v] (tp/not-blank-str? v)
      "Hello"
      "   a   "
      "123"))

  (testing "Returns false for blank strings"
    (are [v] (false? (tp/not-blank-str? v))
      ""
      "    "
      "\t"
      "\n"))

  (testing "Returns false for non-string values"
    (are [v] (false? (tp/not-blank-str? v))
      :keyword
      123
      {}
      []
      true
      nil)))

(deftest int32?-test
  (testing "Returns true for 32 bits integers"
    (are [v] (tp/int32? v)
      -1
      0
      1
      Integer/MIN_VALUE
      Integer/MAX_VALUE))

  (testing "Returns false for out-of-range integers"
    (are [v] (false? (tp/int32? v))
      (dec Integer/MIN_VALUE)
      (inc Integer/MAX_VALUE)))

  (testing "Returns false for non-integer values"
    (are [v] (false? (tp/int32? v))
      "string"
      :keyword
      {}
      []
      true
      nil)))

(deftest uint32?-test
  (testing "Returns true for 32 bits non-negative integers"
    (are [v] (tp/uint32? v)
      0
      1
      Integer/MAX_VALUE))

  (testing "Returns false for negative 32 bits integers"
    (are [v] (false? (tp/uint32? v))
      -1
      Integer/MIN_VALUE))

  (testing "Returns false for out-of-range values"
    (is (false? (tp/uint32? (inc Integer/MAX_VALUE))))) 

  (testing "Returns false for non-integer values"
    (are [v] (false? (tp/uint32? v))
      1.5
      "0"
      nil)))

(deftest pos-int32?-test
  (testing "Returns true for 32 bits positive integers"
    (are [v] (tp/pos-int32? v)
      1
      Integer/MAX_VALUE))

  (testing "Returns false for zero"
    (is (false? (tp/pos-int32? 0))))

  (testing "Returns false for negative 32 bits integers"
    (are [v] (false? (tp/pos-int32? v))
      -1
      Integer/MIN_VALUE))

  (testing "Returns false for out-of-range values"
    (is (false? (tp/pos-int32? (inc Integer/MAX_VALUE)))))

  (testing "Returns false for non-integer values"
    (are [v] (false? (tp/pos-int32? v))
      1.5
      "1"
      nil)))

(deftest fhir-sized-str-test
  (testing "Returns true for strings within the 1 MB limit"
    (are [v] (tp/fhir-sized-str? v)
      ""
      "hello"
      (apply str (repeat 1048576 "a"))))

  (testing "Returns false for strings exceeding the 1MB limit"
    (is (false? (tp/fhir-sized-str? (apply str (repeat 1048577 "a")))))))

(deftest fhir-uri?-test
  (testing "Returns true for valid URIs"
    (are [v] (tp/fhir-uri? v)
      "https://example.com"
      "http://hl7.org/fhir"
      "urn:uuid:53fefa32-fcbb-4ff8-8a92-55ee120877b7"
      "urn:oid:1.2.3.4.5"
      "ftp://files.example.com"))

  (testing "Returns false for invalid URIs"
    (are [v] (false? (tp/fhir-uri? v))
      "not a valid uri"
      "://missing-scheme"))

  (testing "Returns false for non-uri-string values"
    (are [v] (false? (tp/fhir-uri? v))
      123
      []
      {}
      :keyword
      nil)))

(deftest fhir-url?-test
  (testing "Returns true for valid URLs"
    (are [v] (tp/fhir-url? v)
      "https://example.com"
      "http://hl7.org/fhir/Patient/123"
      "ftp://files.example.com/data"))

  (testing "Returns false for URNs (not locatable)"
    (are [v] (false? (tp/fhir-url? v))
      "urn:uuid:53fefa32-fcbb-4ff8-8a92-55ee120877b7"
      "urn:oid:1.2.3.4"))

  (testing "Returns false for invalid URLs"
    (are [v] (false? (tp/fhir-url? v))
      "just-a-string"
      "not a url"))

  (testing "Returns false for non-url-string values"
    (are [v] (false? (tp/fhir-url? v))
      123
      {}
      []
      nil
      :keyword)))

(deftest fhir-canonical?-test
  (testing "Returns true for absolute URIs"
    (are [v] (tp/fhir-canonical? v)
      "https://example.com/ValueSet/123"
      "http://hl7.org/fhir/StructureDefinition/Patient"
      "urn:uuid:53fefa32-fcbb-4ff8-8a92-55ee120877b7"))

  (testing "Returns true for absolute URIs with version"
    (are [v] (tp/fhir-canonical? v)
      "https://example.com/ValueSet/123|1.0"
      "http://hl7.org/fhir/ValueSet/languages|4.0.1"))

  (testing "Returns true for fragment references"
    (are [v] (tp/fhir-canonical? v)
      "#local-reference"
      "#vs-1"))

  (testing "Returns false for relative URIs"
    (are [v] (false? (tp/fhir-canonical? v))
      "ValueSet/123"
      "../Patient/456"))
  
  (testing "Returns false for multiple pipes"
    (is (false? (tp/fhir-canonical? "http://example.com|1.0|2.0"))))
  
  (testing "Returns false for invalid values"
    (are [v] (false? (tp/fhir-canonical? v))
      "not a valid uri"
      "")))