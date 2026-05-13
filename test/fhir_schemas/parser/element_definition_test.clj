(ns fhir-schemas.parser.element-definition-test
  (:require [clojure.test :refer [deftest testing is are]]
            [fhir-schemas.parser.element-definition :as ed]))


(deftest parse-path-test
  (testing "Parses simple paths"
    (are [input expected] (= expected (ed/parse-path input))
      "Patient"              [:patient]
      "Patient.name"         [:patient :name]
      "Patient.name.family"  [:patient :name :family]))

  (testing "Converts camelCase to kebab-case"
    (are [input expected] (= expected (ed/parse-path input))
      "Patient.birthDate"           [:patient :birth-date]
      "Observation.valueQuantity"   [:observation :value-quantity]))

  (testing "Removes [x] from polymorphic fields"
    (are [input expected] (= expected (ed/parse-path input))
      "Observation.value[x]"      [:observation :value]
      "MedicationRequest.medication[x]" [:medication-request :medication]))

  (testing "Returns nil for non-string input"
    (are [input] (thrown? clojure.lang.ExceptionInfo (ed/parse-path input))
      nil
      123
      :keyword
      {})))

(deftest parse-max-test
  (testing "Parses numeric strings to integers"
    (are [input expected] (= expected (ed/parse-max input))
      "1" 1
      "5" 5
      "0" 0))

  (testing "Returns nil for unbounded"
    (is (nil? (ed/parse-max "*"))))

  (testing "Throws for non-string input"
    (are [input] (thrown? clojure.lang.ExceptionInfo (ed/parse-max input))
      nil
      123
      :keyword)))

;; fhirpath-system-type?

(deftest fhirpath-system-type?-test
  (testing "Returns true for FHIRPath System types"
    (are [input] (ed/fhirpath-type? input)
      "http://hl7.org/fhirpath/System.String"
      "http://hl7.org/fhirpath/System.Boolean"
      "http://hl7.org/fhirpath/System.Integer"
      "http://hl7.org/fhirpath/System.DateTime"))

  (testing "Returns false for normal FHIR types or malformed FHIRPath System types"
    (are [input] (false? (ed/fhirpath-type? input))
      "string"
      "boolean"
      "HumanName"
      "Reference"
      "http://hl7.org/fhirpath/System.Foo"))

  (testing "Returns false for non-string values"
    (are [input] (false? (ed/fhirpath-type? input))
      nil
      123
      :keyword)))

(deftest fhirpath-type->fhir-type-test
  (testing "Extracts FHIR type from valid extension"
    (is (= "string"
           (ed/fhirpath-type->fhir-type
            [{:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
              :value-url "string"}]))))

  (testing "Extracts different FHIR types"
    (are [expected ext-value]
         (= expected
            (ed/fhirpath-type->fhir-type
             [{:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
               :value-url ext-value}]))
      "id"      "id"
      "boolean" "boolean"
      "code"    "code"))

  (testing "Throws when no matching extension found"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ed/fhirpath-type->fhir-type []))))

  (testing "Throws when no extensions at all"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ed/fhirpath-type->fhir-type nil))))

  (testing "Throws when duplicate extensions found"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ed/fhirpath-type->fhir-type
                  [{:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                    :value-url "string"}
                   {:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                    :value-url "id"}])))))

(deftest normalize-type-test
  (testing "Normalizes simple type codes"
    (are [input expected] (= expected (:code (ed/normalize-type input)))
      {:code "string"}     :fhir.type/string
      {:code "boolean"}    :fhir.type/boolean
      {:code "HumanName"}  :fhir.type/HumanName
      {:code "Reference"}  :fhir.type/Reference))

  (testing "Resolves FHIRPath System types via extension"
    (is (= :fhir.type/string
           (:code (ed/normalize-type
                   {:code "http://hl7.org/fhirpath/System.String"
                    :extension [{:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                                 :value-url "string"}]})))))

  (testing "Preserves other fields"
    (let [result (ed/normalize-type {:code "Reference"
                                     :target-profile ["http://hl7.org/fhir/StructureDefinition/Patient"]})]
      (is (= :fhir.type/Reference (:code result)))
      (is (= ["http://hl7.org/fhir/StructureDefinition/Patient"] (:target-profile result)))))

  (testing "Throws for non-map input"
    (are [input] (thrown? clojure.lang.ExceptionInfo (ed/normalize-type input))
      "string"
      123
      nil
      []))

  (testing "Throws for map without :code"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ed/normalize-type {:profile ["http://example.com"]})))))

(deftest parse-type-test
  (testing "Parses vector of type objects"
    (let [result (ed/parse-type [{:code "string"}])]
      (is (= 1 (count result)))
      (is (= :fhir.type/string (:code (first result))))))

  (testing "Parses polymorphic types"
    (let [result (ed/parse-type [{:code "string"}
                                 {:code "integer"}
                                 {:code "boolean"}])]
      (is (= 3 (count result)))
      (is (= [:fhir.type/string :fhir.type/integer :fhir.type/boolean]
             (mapv :code result)))))

  (testing "Throws for non-vector input"
    (are [input] (thrown? clojure.lang.ExceptionInfo (ed/parse-type input))
      "string"
      {:code "string"}
      nil
      '({:code "string"}))))