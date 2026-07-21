(ns fhemas.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [fhemas.schema :as schema]))

(def valid-schema-profile
  {:resource-type "SchemaProfile"
   :id "fhir-r4-schema-profile"
   :url "https://example.com/fhemas/R4/schema-profile.edn"
   :version "1.0.0"
   :title "FHIR R4 Schema Profile"
   :status :active
   :description "Defines how to parse and compile FHIR R4 StructureDefinitions"
   :fhir-version "4.0.1"
   :source "https://hl7.org/fhir/R4/structuredefinition.html"
   :schema
   {:meta [{:path [:id] :type :string :max 1}]
    :definition [{:path [:kind] :type :string :min 1 :max 1}]
    :invariants [{:path [:context-invariant]
                  :type :vector
                  :compile/field :fhemas.compile.r4/fhirpath-constraint}]
    :elements
    {:snapshot {:path [:snapshot :element]}
     :fields
     [{:path [:path] :type :string :min 1 :max 1 :compile/field :fhemas.compile.r4/path}
      {:path [:id] :type :string :max 1 :compile/field :fhemas.compile.r4/id}
      {:path [:slice-name] :type :string :max 1} ;; Pass-through
      {:path [:min] :type :integer :max 1 :compile/field :fhemas.compile.r4/min-cardinality}
      {:path [:max] :type :string :max 1 :compile/field :fhemas.compile.r4/max-cardinality}
      {:path [:slicing]
       :type :map :max 1
       :compile/with-group :fhemas.compile.r4/slicing}
      {:path [:constraint]
       :type :vector
       :compile/field :fhemas.compile.r4/fhirpath-constraint}]}}})

(deftest validate-schema-profile-test
  (testing "Valid SchemaProfile with new compile keys"
    (is (= valid-schema-profile
           (schema/validate-schema-profile valid-schema-profile))
        "Should return the map intact if valid"))

  (testing "Elements with only differential (no snapshot)"
    (let [differential-only (assoc-in valid-schema-profile
                                      [:schema :elements]
                                      {:differential {:path [:differential :element]}
                                       :fields [{:path [:path] :type :string :min 1 :max 1}]})]
      (is (map? (schema/validate-schema-profile differential-only))
          "Should be valid if only differential is provided")))

  (testing "Elements missing both snapshot and differential"
    (let [invalid-data (assoc-in valid-schema-profile
                                 [:schema :elements]
                                 {:fields [{:path [:path] :type :string}]})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":compile/field is not a qualified keyword"
    (let [invalid-data (assoc-in valid-schema-profile
                                 [:schema :elements :fields 0 :compile/field]
                                 :unqualified-keyword)] ;; Falta el namespace
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":compile/with-group is a string instead of keyword"
    (let [invalid-data (assoc-in valid-schema-profile
                                 [:schema :elements :fields 5 :compile/with-group]
                                 "fhemas.compile.r4/slicing")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":min is 0 (violates {:min 1} rule per omission convention)"
    (let [invalid-data (assoc-in valid-schema-profile
                                 [:schema :elements :fields 3 :min]
                                 0)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data))
          ":min must be >= 1 if present")))

  (testing ":max is 0 (violates {:min 1} rule per omission convention)"
    (let [invalid-data (assoc-in valid-schema-profile
                                 [:schema :elements :fields 4 :max]
                                 0)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data))
          ":max must be >= 1 if present")))

  (testing "Missing required root field (:url)"
    (let [invalid-data (dissoc valid-schema-profile :url)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing "Exception contains correct metadata (code, scope, operation)"
    (let [invalid-data (assoc valid-schema-profile :status :invalid-status)
          exception (try
                      (schema/validate-schema-profile invalid-data)
                      (catch Exception e e))]
      (is (= :invalid/schema (:code (ex-data exception))))
      (is (= :fhemas.schema (:scope (ex-data exception))))
      (is (= :validate-schema (:operation (ex-data exception))))
      (is (contains? (ex-data exception) :details)))))
