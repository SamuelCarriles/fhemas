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
      {:code "string"}     :fhir-schemas.type.primitive/string
      {:code "boolean"}    :fhir-schemas.type.primitive/boolean
      {:code "HumanName"}  :fhir-schemas.type.complex/human-name
      {:code "Reference"}  :fhir-schemas.type.complex/reference))

  (testing "Resolves FHIRPath System types via extension"
    (is (= :fhir-schemas.type.primitive/string
           (:code (ed/normalize-type
                   {:code "http://hl7.org/fhirpath/System.String"
                    :extension [{:url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type"
                                 :value-url "string"}]})))))

  (testing "Preserves other fields"
    (let [result (ed/normalize-type {:code "Reference"
                                     :target-profile ["http://hl7.org/fhir/StructureDefinition/Patient"]})]
      (is (= :fhir-schemas.type.complex/reference (:code result)))
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
      (is (= :fhir-schemas.type.primitive/string (:code (first result))))))

  (testing "Parses polymorphic types"
    (let [result (ed/parse-type [{:code "string"}
                                 {:code "integer"}
                                 {:code "boolean"}])]
      (is (= 3 (count result)))
      (is (= [:fhir-schemas.type.primitive/string :fhir-schemas.type.primitive/integer :fhir-schemas.type.primitive/boolean]
             (mapv :code result)))))

  (testing "Throws for non-vector input"
    (are [input] (thrown? clojure.lang.ExceptionInfo (ed/parse-type input))
      "string"
      {:code "string"}
      nil
      '({:code "string"}))))

(deftest parse-binding-test
  (testing "Converts strength to keyword"
    (is (= :required
           (:strength (ed/parse-binding {:strength "required"})))))

  (testing "Preserves other fields"
    (let [result (ed/parse-binding {:strength "extensible"
                                    :value-set "http://hl7.org/fhir/ValueSet/languages"
                                    :description "Language codes"})]
      (is (= :extensible (:strength result)))
      (is (= "http://hl7.org/fhir/ValueSet/languages" (:value-set result)))
      (is (= "Language codes" (:description result)))))

  (testing "Returns binding without strength unchanged"
    (let [input {:value-set "http://example.com"}]
      (is (= input (ed/parse-binding input)))))

  (testing "Throws for non-map input"
    (are [v] (thrown? clojure.lang.ExceptionInfo (ed/parse-binding v))
      "string"
      123
      nil
      [])))

(deftest normalize-constraint-test
  (testing "Converts severity to keyword"
    (is (= :error
           (:severity (ed/normalize-constraint {:severity "error"})))))

  (testing "Preserves all fields"
    (let [result (ed/normalize-constraint
                  {:key "ele-1"
                   :severity "error"
                   :human "All elements must have value"
                   :expression "hasValue() or (children().count() > id.count())"
                   :xpath "@value|f:*|h:div"
                   :source "http://hl7.org/fhir/StructureDefinition/Element"})]
      (is (= :error (:severity result)))
      (is (= "ele-1" (:key result)))
      (is (= "hasValue() or (children().count() > id.count())" (:expression result)))
      (is (= "@value|f:*|h:div" (:xpath result)))))

  (testing "Returns constraint without severity unchanged"
    (let [input {:key "test-1" :human "Test constraint"}]
      (is (= input (ed/normalize-constraint input)))))

  (testing "Throws for non-map input"
    (are [v] (thrown? clojure.lang.ExceptionInfo (ed/normalize-constraint v))
      "string"
      123
      nil
      [])))

(deftest parse-constraint-test
  (testing "Parses array of constraints"
    (let [result (ed/parse-constraint [{:severity "error" :key "ele-1"}
                                       {:severity "warning" :key "ele-2"}])]
      (is (= 2 (count result)))
      (is (= :error (:severity (first result))))
      (is (= :warning (:severity (second result))))))

  (testing "Throws for non-vector input"
    (are [v] (thrown? clojure.lang.ExceptionInfo (ed/parse-constraint v))
      "string"
      {:severity "error"}
      nil)))

(deftest parse-content-reference-test
  (testing "Parses content reference to path vector"
    (are [input expected] (= expected (ed/parse-content-reference input))
      "#Bundle.entry.request"  [:bundle :entry :request]
      "#Element.extension"     [:element :extension]
      "#Patient.name"          [:patient :name]))

  (testing "Throws for non-string input"
    (are [v] (thrown? clojure.lang.ExceptionInfo (ed/parse-content-reference v))
      123
      nil
      :keyword
      {}))

  (testing "Throws for string not starting with #"
    (are [v] (thrown? clojure.lang.ExceptionInfo (ed/parse-content-reference v))
      "Bundle.entry.request"
      "Patient.name")))

(deftest parse-field-test
  (testing "Applies parse-prop to each field"
    (let [result (ed/parse-field {:path "Patient.name"
                                  :max "1"
                                  :min 0
                                  :short "A name"})]
      (is (= [:patient :name] (:path result)))
      (is (= 1 (:max result)))
      (is (= 0 (:min result)))
      (is (= "A name" (:short result)))))

  (testing "Handles unbounded max"
    (let [result (ed/parse-field {:path "Patient.name" :max "*"})]
      (is (nil? (:max result)))))

  (testing "Parses type within field"
    (let [result (ed/parse-field {:path "Patient.active"
                                  :type [{:code "boolean"}]})]
      (is (= :fhir-schemas.type.primitive/boolean
             (get-in result [:type 0 :code])))))

  (testing "Parses binding within field"
    (let [result (ed/parse-field {:path "Patient.gender"
                                  :binding {:strength "required"
                                            :value-set "http://hl7.org/fhir/ValueSet/gender"}})]
      (is (= :required (get-in result [:binding :strength]))))))

(deftest elements->map-test
  (testing "Creates map indexed by parsed path"
    (let [result (ed/elements->map [{:path "Patient" :min 0 :max "*"}
                                    {:path "Patient.id" :min 0 :max "1"}
                                    {:path "Patient.name" :min 0 :max "*"}])]
      (is (= 3 (count result)))
      (is (contains? result [:patient]))
      (is (contains? result [:patient :id]))
      (is (contains? result [:patient :name]))))

  (testing "Path is not included in the element data"
    (let [result (ed/elements->map [{:path "Patient" :min 0}])]
      (is (nil? (:path (get result [:patient]))))))

  (testing "Applies parse-prop to element fields"
    (let [result (ed/elements->map [{:path "Patient.name"
                                     :min 0
                                     :max "*"
                                     :type [{:code "HumanName"}]}])
          element (get result [:patient :name])]
      (is (= :fhir-schemas.type.complex/human-name (get-in element [:type 0 :code])))))

  (testing "Handles empty elements"
    (is (= {} (ed/elements->map [])))))
