(ns fhemas.json-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [fhemas.json :as json])
  (:import [java.io File]))

(def ^File age-sd-file
  (io/file "test/resources/fhir/age-structure-definition.json"))

(deftest json-key-path-encoder-test
  (testing "converts keywords to camelCase strings"
    (is (= "resourceType" (json/key-encoder :resource-type)))
    (is (= "lastUpdated" (json/key-encoder :last-updated)))
    (is (= "fhirVersion" (json/key-encoder :fhir-version)))
    (is (= "name" (json/key-encoder :name)))))

(deftest enrich-path-test
  (testing "enriches single keyword path with :$ marker"
    (let [result (json/enrich-path [:resource-type] json/key-encoder)]
      (is (= {"resourceType" :$} result))))

  (testing "enriches vector path with nested structure"
    (let [result (json/enrich-path [:meta :last-updated] json/key-encoder)]
      (is (= {"meta" {"lastUpdated" :$}} result))))

  (testing "enriches deeply nested path"
    (let [result (json/enrich-path [:snapshot :element :id] json/key-encoder)]
      (is (= {"snapshot" {"element" {"id" :$}}} result)))))

(deftest deep-merge-test
  (testing "merges flat maps"
    (let [m1 {"a" :$}
          m2 {"b" :$}
          result (json/deep-merge m1 m2)]
      (is (= {"a" :$ "b" :$} result))))

  (testing "merges nested maps"
    (let [m1 {"user" {"name" :$}}
          m2 {"user" {"email" :$}}
          result (json/deep-merge m1 m2)]
      (is (= {"user" {"name" :$ "email" :$}} result))))

  (testing "overwrites non-map values"
    (let [m1 {"a" :$}
          m2 {"a" :$ "b" :$}
          result (json/deep-merge m1 m2)]
      (is (= {"a" :$ "b" :$} result)))))

(deftest ->extraction-route-test
  (testing "builds extraction route from multiple paths"
    (let [paths [[:resource-type] [:url] [:meta :last-updated]]
          result (json/->extraction-route paths)]
      (is (= {"resourceType" :$
              "url" :$
              "meta" {"lastUpdated" :$}}
             result)))))

(deftest normalize-extraction-result-test
  (testing "converts string paths to keywords"
    (let [m {["resourceType"] "StructureDefinition"
             ["url"] "http://example.com"}
          result (json/normalize-extraction-result m)]
      (is (= {:resource-type "StructureDefinition"
              :url "http://example.com"}
             result))))

  (testing "converts nested string paths to keyword vectors"
    (let [m {["meta" "lastUpdated"] "2019-11-01"}
          result (json/normalize-extraction-result m)]
      (is (= {[:meta :last-updated] "2019-11-01"}
             result))))

  (testing "unwraps single-element paths"
    (let [m {["name"] "Age"}
          result (json/normalize-extraction-result m)]
      (is (= {:name "Age"} result)))))

(deftest extract-primitive-fields-test
  (testing "extract simple primitive fields from root level"
    (let [paths [[:resource-type] [:url] [:status] [:name] [:version]]
          result (json/extract age-sd-file paths)]
      (is (= {:resource-type "StructureDefinition"
              :url "http://hl7.org/fhir/StructureDefinition/Age"
              :status "draft"
              :name "Age"
              :version "4.0.1"}
             result)))))

(deftest extract-nested-object-test
  (testing "extract field from nested object"
    (let [paths [[:meta :last-updated]]
          result (json/extract age-sd-file paths)]
      (is (= {[:meta :last-updated] "2019-11-01T09:29:23.356+11:00"}
             result)))))

(deftest extract-complete-object-test
  (testing "extract complete object value"
    (let [paths [[:meta]]
          result (json/extract age-sd-file paths)]
      (is (= {:meta {:last-updated "2019-11-01T09:29:23.356+11:00"}}
             result)))))

(deftest extract-complete-array-test
  (testing "extract complete array value"
    (let [paths [[:mapping]]
          result (json/extract age-sd-file paths)]
      (is (= {:mapping [{:identity "rim" :uri "http://hl7.org/v3" :name "RIM Mapping"}
                        {:identity "v2" :uri "http://hl7.org/v2" :name "HL7 v2 Mapping"}]}
             result)))))

(deftest skip-unmatched-fields-test
  (testing "skip fields not present in paths"
    (let [paths [[:id] [:type]]
          result (json/extract age-sd-file paths)]
      (is (= {:id "Age" :type "Age"} result))
      (is (= 2 (count result)))
      (is (not (contains? result :url)))
      (is (not (contains? result :status))))))

(deftest extract-mixed-fields-test
  (testing "extract multiple fields at different nesting levels"
    (let [paths [[:resource-type]
                 [:url]
                 [:meta :last-updated]
                 [:kind]
                 [:abstract]]
          result (json/extract age-sd-file paths)]
      (is (= {:resource-type "StructureDefinition"
              :url "http://hl7.org/fhir/StructureDefinition/Age"
              [:meta :last-updated] "2019-11-01T09:29:23.356+11:00"
              :kind "complex-type"
              :abstract false}
             result)))))

(deftest non-existent-fields-test
  (testing "return empty map when requesting non-existent fields"
    (let [paths [[:non-existent-field] [:another-missing-field]]
          result (json/extract age-sd-file paths)]
      (is (= {} result)))))

(deftest descend-into-array-throws-error-test
  (testing "throws error when trying to descend into array"
    (let [paths [[:extension :url]]]
      (is (thrown? clojure.lang.ExceptionInfo (json/extract age-sd-file paths))))))

(deftest deeply-nested-field-test
  (testing "extract field from deeply nested structure"
    (let [paths [[:snapshot :element]]
          result (json/extract age-sd-file paths)]
      (is (contains? result [:snapshot :element]))
      (is (vector? (get result [:snapshot :element])))
      (is (map? (first (get result [:snapshot :element])))))))

(deftest boolean-and-numeric-types-test
  (testing "correctly handle boolean and numeric values"
    (let [paths [[:abstract] [:fhir-version]]
          result (json/extract age-sd-file paths)]
      (is (= {:abstract false
              :fhir-version "4.0.1"}
             result)))))
