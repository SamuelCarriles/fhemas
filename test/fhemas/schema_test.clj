(ns fhemas.schema-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fhemas.schema :as schema]))

;; ---------------------------------------------------------------------------
;; not-blank-str?
;; ---------------------------------------------------------------------------

(deftest not-blank-str?-test
  (testing "accepts non-blank strings"
    (is (true? (schema/not-blank-str? "hello"))))
  (testing "rejects blank string"
    (is (false? (schema/not-blank-str? ""))))
  (testing "rejects whitespace-only string"
    (is (false? (schema/not-blank-str? "   "))))
  (testing "rejects nil"
    (is (false? (schema/not-blank-str? nil))))
  (testing "rejects non-string values"
    (is (false? (schema/not-blank-str? 42)))
    (is (false? (schema/not-blank-str? :keyword)))))

;; ---------------------------------------------------------------------------
;; url?
;; ---------------------------------------------------------------------------

(deftest url?-test
  (testing "accepts a valid http url"
    (is (true? (schema/url? "http://hl7.org/fhir/StructureDefinition/StructureDefinition"))))
  (testing "accepts a valid https url"
    (is (true? (schema/url? "https://raw.githubusercontent.com/SamuelCarriles/artifacts/main/fhemas/ValidatorDefinitions/R4/validator-definition.edn"))))
  (testing "rejects a malformed url (no protocol)"
    (is (false? (schema/url? "not-a-url"))))
  (testing "rejects an empty string"
    (is (false? (schema/url? ""))))
  (testing "rejects nil (as-url throws on nil, should be caught)"
    (is (false? (schema/url? nil)))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn passes? [schema x]
  (try
    (schema/validate-schema schema x "test failure")
    true
    (catch Exception _ false)))

(defn fails? [schema x]
  (not (passes? schema x)))

(defn vd-passes? [m]
  (try
    (schema/validate-validator-definition m)
    true
    (catch Exception _ false)))

(defn vd-fails? [m]
  (not (vd-passes? m)))

(defn coerces-to [schema input expected]
  (= expected (schema/coerce schema input)))

;; ---------------------------------------------------------------------------
;; Field schema
;; ---------------------------------------------------------------------------

(deftest field-min-max-test
  (testing "valid when min <= max"
    (is (passes? schema/Field
                 {:path [:min]
                  :min 1
                  :max 2
                  :compiler 'fhemas.compile.r4/min-cardinality})))
  (testing "valid when min == max"
    (is (passes? schema/Field
                 {:path [:x]
                  :min 1
                  :max 1
                  :compiler 'fhemas.compile.r4/path})))
  (testing "invalid when min > max"
    (is (fails? schema/Field
                {:path [:x]
                 :min 5
                 :max 1
                 :compiler 'fhemas.compile.r4/path})))
  (testing "valid when only min is present"
    (is (passes? schema/Field
                 {:path [:x]
                  :min 1
                  :compiler 'fhemas.compile.r4/path})))
  (testing "valid when only max is present"
    (is (passes? schema/Field
                 {:path [:x]
                  :max 1
                  :compiler 'fhemas.compile.r4/path})))
  (testing "valid when neither min nor max is present"
    (is (passes? schema/Field
                 {:path [:x]
                  :compiler 'fhemas.compile.r4/path}))))

(deftest field-compiler-parser-exclusivity-test
  (testing "valid with neither compiler nor parser (purely declarative field)"
    (is (passes? schema/Field {:path [:base-definition]})))
  (testing "valid with only compiler"
    (is (passes? schema/Field
                 {:path [:path]
                  :compiler 'fhemas.compile.r4/path})))
  (testing "valid with only parser"
    (is (passes? schema/Field
                 {:path [:url]
                  :parser 'fhemas.parse.r4/normalize-url})))
  (testing "invalid with both compiler and parser"
    (is (fails? schema/Field
                {:path [:x]
                 :compiler 'fhemas.compile.r4/path
                 :parser 'fhemas.parse.r4/normalize-url}))))

(deftest field-path-test
  (testing "valid with vector-of-keyword path"
    (is (passes? schema/Field {:path [:snapshot :element]})))
  (testing "valid with regex-style path (re-str map)"
    (is (passes? schema/Field {:path {:re-str "^fixed-.*$"}})))
  (testing "invalid with path as a plain string"
    (is (fails? schema/Field {:path "not-a-valid-path-shape"}))))

(deftest field-without-path-test
  (testing "valid with compiler but no path (entire resource)"
    (is (passes? schema/Field
                 {:compiler 'fhemas.compile.r4/process-structure-definition})))
  (testing "valid with parser but no path"
    (is (passes? schema/Field
                 {:parser 'fhemas.parse.r4/some-parser})))
  (testing "valid with type but no path"
    (is (passes? schema/Field
                 {:type :map})))
  (testing "valid empty field (though unusual)"
    (is (passes? schema/Field {}))))

;; ---------------------------------------------------------------------------
;; Elements schema
;; ---------------------------------------------------------------------------

(deftest elements-snapshot-or-differential-test
  (testing "valid with only snapshot"
    (is (passes? schema/Elements
                 {:base-definition {:path [:base-definition]}
                  :snapshot {:path [:snapshot :element]
                             :compiler 'fhemas.compile.r4/snapshot}
                  :compile-order 'fhemas.r4.compile.element/compile-order
                  :fields []})))
  (testing "valid with only differential"
    (is (passes? schema/Elements
                 {:base-definition {:path [:base-definition]}
                  :differential {:path [:differential :element]}
                  :compile-order 'fhemas.r4.compile.element/compile-order
                  :fields []})))
  (testing "valid with both snapshot and differential"
    (is (passes? schema/Elements
                 {:base-definition {:path [:base-definition]}
                  :snapshot {:path [:snapshot :element]
                             :compiler 'fhemas.compile.r4/snapshot}
                  :differential {:path [:differential :element]}
                  :compile-order 'fhemas.r4.compile.element/compile-order
                  :fields []})))
  (testing "invalid with neither snapshot nor differential"
    (is (fails? schema/Elements
                {:base-definition {:path [:base-definition]}
                 :compile-order 'fhemas.r4.compile.element/compile-order
                 :fields []}))))

(deftest elements-base-definition-required-test
  (testing "invalid without base-definition"
    (is (fails? schema/Elements
                {:snapshot {:path [:snapshot :element]
                            :compiler 'fhemas.compile.r4/snapshot}
                 :compile-order 'fhemas.r4.compile.element/compile-order
                 :fields []})))
  (testing "valid with base-definition present"
    (is (passes? schema/Elements
                 {:base-definition {:path [:base-definition]}
                  :snapshot {:path [:snapshot :element]
                             :compiler 'fhemas.compile.r4/snapshot}
                  :compile-order 'fhemas.r4.compile.element/compile-order
                  :fields []}))))

(deftest elements-compile-order-required-test
  (testing "invalid without compile-order"
    (is (fails? schema/Elements
                {:base-definition {:path [:base-definition]}
                 :snapshot {:path [:snapshot :element]
                            :compiler 'fhemas.compile.r4/snapshot}
                 :fields []})))
  (testing "valid with compile-order present"
    (is (passes? schema/Elements
                 {:base-definition {:path [:base-definition]}
                  :snapshot {:path [:snapshot :element]
                             :compiler 'fhemas.compile.r4/snapshot}
                  :compile-order 'fhemas.r4.compile.element/compile-order
                  :fields []})))
  (testing "compile-order must be a qualified symbol"
    (is (fails? schema/Elements
                {:base-definition {:path [:base-definition]}
                 :snapshot {:path [:snapshot :element]}
                 :compile-order "not-a-symbol"
                 :fields []}))))

;; ---------------------------------------------------------------------------
;; Lookup schema
;; ---------------------------------------------------------------------------

(deftest lookup-required-fields-test
  (testing "valid minimal lookup (with required :relation)"
    (is (passes? schema/Lookup
                 {:name :match/url->resource
                  :key {:path [:url]}
                  :relation :1->1})))
  (testing "missing :name is invalid"
    (is (fails? schema/Lookup
                {:key {:path [:url]}
                 :relation :1->1})))
  (testing "missing :key is invalid"
    (is (fails? schema/Lookup
                {:name :match/test
                 :relation :1->1})))
  (testing "missing :relation is invalid (validation without coercion)"
    (is (fails? schema/Lookup
                {:name :match/test
                 :key {:path [:url]}}))))

(deftest lookup-optional-fields-test
  (testing "valid with :value present"
    (is (passes? schema/Lookup
                 {:name :match/name->url
                  :key {:path [:name]}
                  :value {:path [:url]}
                  :relation :1->1})))
  (testing "valid with :when present"
    (is (passes? schema/Lookup
                 {:name :match/sd-name->url
                  :when [{:resource-type "StructureDefinition"}]
                  :key {:path [:name]}
                  :value {:path [:url]}
                  :relation :1->1})))
  (testing "valid with all optional fields"
    (is (passes? schema/Lookup
                 {:name :match/sd-kind->urls
                  :when [{:resource-type "StructureDefinition"}]
                  :key {:path [:kind]}
                  :value {:path [:url]}
                  :relation :1->*}))))

(deftest lookup-key-and-value-are-fields-test
  (testing ":key accepts regex path (inherits Field schema)"
    (is (passes? schema/Lookup
                 {:name :match/test
                  :key {:path {:re-str "^fixed-.*$"}}
                  :relation :1->1})))
  (testing ":value accepts regex path (inherits Field schema)"
    (is (passes? schema/Lookup
                 {:name :match/test
                  :key {:path [:url]}
                  :value {:path {:re-str "^some-.*$"}}
                  :relation :1->1})))
  (testing ":key with invalid path shape is invalid"
    (is (fails? schema/Lookup
                {:name :match/test
                 :key {:path "not-valid"}
                 :relation :1->1}))))

(deftest lookup-coercion-test
  (testing "missing :relation defaults to :1->1 during coercion"
    (is (coerces-to schema/Lookup
                    {:name :test
                     :key {:path [:url]}}
                    {:name :test
                     :key {:path [:url]}
                     :relation :1->1})))
  (testing "explicit :relation is preserved during coercion"
    (is (coerces-to schema/Lookup
                    {:name :test
                     :key {:path [:url]}
                     :relation :1->*}
                    {:name :test
                     :key {:path [:url]}
                     :relation :1->*}))))

;; ---------------------------------------------------------------------------
;; Full ValidatorDefinition
;; ---------------------------------------------------------------------------

(def sample-vd
  {:resource-type "ValidatorDefinition"
   :id "fhir-4.0.1-validator-definition"
   :url "https://raw.githubusercontent.com/SamuelCarriles/artifacts/main/fhemas/ValidatorDefinitions/R4/validator-definition.edn"
   :version "1.0.0"
   :title "FHIR R4 Validator Definition"
   :status :active
   :description "A validator definition that specifies how to parse, validate, and process FHIR R4 resources according to the official HL7 FHIR R4 specification. It defines the structure, constraints, and compilation rules needed to build validators for FHIR resources."
   :fhir-version "4.0.1"
   :dispatch-by {:path [:resource-type]
                 :type :string
                 :min 1
                 :max 1}
   :registry [{:name :elements
               :key {:path [:resource-type]
                     :type :string
                     :min 1
                     :max 1}
               :value {:compiler 'fhemas.compile.r4/process-structure-definition}
               :when [{:resource-type "StructureDefinition"}]
               :relation :1->1}]
   :terminology [{:name :value-sets
                  :key {:path [:url]
                        :type :string
                        :min 1
                        :max 1}
                  :value {:compiler 'fhemas.r4.compile.terminology/value-set}
                  :when [{:resource-type "ValueSet"}]
                  :relation :1->1}
                 {:name :code-systems
                  :key {:path [:url]
                        :type :string
                        :min 1
                        :max 1}
                  :value {:compiler 'fhemas.r4.compile.terminology/code-system}
                  :when [{:resource-type "CodeSystem"}]
                  :relation :1->1}]
   :lookups [{:name :match/url->resource
              :key {:path [:url]}
              :value {:path [:resource-type]}
              :relation :1->1}
             {:name :match/structure-definition.name->url
              :when [{:resource-type "StructureDefinition"}]
              :key {:path [:name]}
              :value {:path [:url]}
              :relation :1->1}
             {:name :match/structure-definition.kind->urls
              :when [{:resource-type "StructureDefinition"}]
              :key {:path [:kind]}
              :value {:path [:url]}
              :relation :1->*}
             {:name :match/resource-type->urls
              :key {:path [:resource-type]}
              :value {:path [:url]}
              :relation :1->*}]
   :schema
   {:source "https://hl7.org/fhir/R4/structuredefinition.html"
    :base "http://hl7.org/fhir/StructureDefinition/StructureDefinition"
    :invariants [{:path [:context-invariant]
                  :type :vector
                  :compiler 'fhemas.compile.r4/fhirpath-constraints}]
    :elements
    {:base-definition {:path [:base-definition]}
     :snapshot {:path [:snapshot :element]
                :compiler 'fhemas.compile.r4/snapshot}
     :differential {:path [:differential :element]}
     :compile-order 'fhemas.r4.compile.element/compile-order
     :fields
     [{:path [:path] :type :string :min 1 :max 1
       :compiler 'fhemas.compile.r4/path}
      {:path [:id] :type :string :max 1
       :compiler 'fhemas.compile.r4/id}
      {:path [:slice-name] :type :string :max 1}
      {:path [:slice-is-constraining] :type :boolean :max 1}
      {:path [:min] :type :integer :max 1
       :compiler 'fhemas.compile.r4/min-cardinality}
      {:path [:max] :type :string :max 1
       :compiler 'fhemas.compile.r4/max-cardinality}
      {:path [:slicing] :type :map :max 1
       :compiler 'fhemas.compile.r4/slicing}
      {:path [:type] :type :vector
       :compiler 'fhemas.compile.r4/type}
      {:path [:content-reference] :type :uri :max 1
       :compiler 'fhemas.compile.r4/content-reference}
      {:path {:re-str "^fixed-.*$"}
       :max 1 :compiler 'fhemas.compile.r4/fixed-value}
      {:path {:re-str "^pattern-.*$"}
       :max 1 :compiler 'fhemas.compile.r4/pattern-value}
      {:path {:re-str "^min-value-.*$"}
       :max 1 :compiler 'fhemas.compile.r4/min-value}
      {:path {:re-str "^max-value-.*$"}
       :max 1 :compiler 'fhemas.compile.r4/max-value}
      {:path [:max-length] :type :integer :max 1
       :compiler 'fhemas.compile.r4/max-length}
      {:path [:constraint] :type :vector
       :compiler 'fhemas.compile.r4/fhirpath-constraints}
      {:path [:condition] :type :vector
       :compiler 'fhemas.compile.r4/condition}
      {:path [:binding] :type :map :max 1
       :compiler 'fhemas.compile.r4/binding}]}}})

(deftest validator-definition-happy-path-test
  (testing "the real R4 ValidatorDefinition document validates cleanly"
    (is (vd-passes? sample-vd))))

(deftest validate-validator-definition-happy-path-test
  (testing "returns the input unchanged when valid"
    (is (= sample-vd (schema/validate-validator-definition sample-vd)))))

;; ---------------------------------------------------------------------------
;; Full ValidatorDefinition — required top-level fields
;; ---------------------------------------------------------------------------

(deftest validator-definition-required-fields-test
  (testing "missing :resource-type is invalid"
    (is (vd-fails? (dissoc sample-vd :resource-type))))
  (testing "wrong :resource-type value is invalid"
    (is (vd-fails? (assoc sample-vd :resource-type "StructureDefinition"))))
  (testing "missing :url is invalid"
    (is (vd-fails? (dissoc sample-vd :url))))
  (testing "invalid :url value is invalid"
    (is (vd-fails? (assoc sample-vd :url "not-a-url"))))
  (testing "missing :version is invalid"
    (is (vd-fails? (dissoc sample-vd :version))))
  (testing "missing :status is invalid"
    (is (vd-fails? (dissoc sample-vd :status))))
  (testing "invalid :status enum value is invalid"
    (is (vd-fails? (assoc sample-vd :status :bogus))))
  (testing "each valid :status enum value passes"
    (doseq [status [:active :draft :unknown :retired]]
      (is (vd-passes? (assoc sample-vd :status status)))))
  (testing "missing :fhir-version is invalid"
    (is (vd-fails? (dissoc sample-vd :fhir-version))))
  (testing "missing :dispatch-by is invalid"
    (is (vd-fails? (dissoc sample-vd :dispatch-by))))
  (testing "invalid :dispatch-by (not a Field) is invalid"
    (is (vd-fails? (assoc sample-vd :dispatch-by "not-a-field"))))
  (testing "valid :dispatch-by as Field passes"
    (is (vd-passes? (assoc sample-vd :dispatch-by {:path [:resource-type]
                                                   :type :string
                                                   :min 1
                                                   :max 1}))))
  (testing ":id is optional"
    (is (vd-passes? (dissoc sample-vd :id))))
  (testing ":title is optional"
    (is (vd-passes? (dissoc sample-vd :title))))
  (testing ":description is optional"
    (is (vd-passes? (dissoc sample-vd :description)))))

;; ---------------------------------------------------------------------------
;; ValidatorDefinition — registry
;; ---------------------------------------------------------------------------

(deftest validator-definition-registry-test
  (testing "missing :registry is invalid"
    (is (vd-fails? (dissoc sample-vd :registry))))
  (testing "empty :registry vector is invalid"
    (is (vd-fails? (assoc sample-vd :registry []))))
  (testing "single valid registry entry passes"
    (is (vd-passes? (assoc sample-vd :registry
                           [{:name :elements
                             :key {:path [:resource-type]}
                             :relation :1->1}]))))
  (testing "registry entry with invalid :key fails"
    (is (vd-fails? (assoc sample-vd :registry
                          [{:name :test
                            :key {:path "bad"}
                            :relation :1->1}])))))

;; ---------------------------------------------------------------------------
;; ValidatorDefinition — terminology
;; ---------------------------------------------------------------------------

(deftest validator-definition-terminology-test
  (testing "missing :terminology is invalid"
    (is (vd-fails? (dissoc sample-vd :terminology))))
  (testing "empty :terminology vector is invalid"
    (is (vd-fails? (assoc sample-vd :terminology []))))
  (testing "single valid terminology entry passes"
    (is (vd-passes? (assoc sample-vd :terminology
                           [{:name :valuesets
                             :key {:path [:url]}
                             :relation :1->1}]))))
  (testing "multiple valid terminology entries pass"
    (is (vd-passes? (assoc sample-vd :terminology
                           [{:name :valuesets
                             :key {:path [:url]}
                             :relation :1->1}
                            {:name :codesystems
                             :key {:path [:url]}
                             :relation :1->1}]))))
  (testing "terminology entry with invalid :key fails"
    (is (vd-fails? (assoc sample-vd :terminology
                          [{:name :test
                            :key {:path "bad"}
                            :relation :1->1}])))))

;; ---------------------------------------------------------------------------
;; ValidatorDefinition — lookups
;; ---------------------------------------------------------------------------

(deftest validator-definition-lookups-test
  (testing "missing :lookups is invalid"
    (is (vd-fails? (dissoc sample-vd :lookups))))
  (testing "empty :lookups vector is invalid"
    (is (vd-fails? (assoc sample-vd :lookups []))))
  (testing "single valid lookup passes"
    (is (vd-passes? (assoc sample-vd :lookups
                           [{:name :match/url->resource
                             :key {:path [:url]}
                             :relation :1->1}]))))
  (testing "lookup with invalid :key fails"
    (is (vd-fails? (assoc sample-vd :lookups
                          [{:name :match/test
                            :key {:path "bad"}
                            :relation :1->1}])))))

;; ---------------------------------------------------------------------------
;; ValidatorDefinition — schema required fields
;; ---------------------------------------------------------------------------

(deftest validator-definition-schema-required-fields-test
  (testing "missing :schema :invariants is invalid"
    (is (vd-fails? (update sample-vd :schema dissoc :invariants))))
  (testing "missing :schema :elements is invalid"
    (is (vd-fails? (update sample-vd :schema dissoc :elements))))
  (testing ":schema :source is optional"
    (is (vd-passes? (update sample-vd :schema dissoc :source))))
  (testing "invalid :schema :source url is invalid"
    (is (vd-fails? (assoc-in sample-vd [:schema :source] "not-a-url"))))
  (testing "missing :schema :elements :compile-order is invalid"
    (is (vd-fails? (update-in sample-vd [:schema :elements] dissoc :compile-order)))))

;; ---------------------------------------------------------------------------
;; validate-schema / validate-validator-definition error behavior
;; ---------------------------------------------------------------------------

(deftest validate-schema-throws-on-invalid-test
  (testing "throws when the document is invalid"
    (is (thrown? Exception
                 (schema/validate-validator-definition
                  (dissoc sample-vd :resource-type)))))
  (testing "the thrown error carries the given message, code, scope, operation and humanized details"
    (try
      (schema/validate-validator-definition (dissoc sample-vd :url))
      (is false "expected an exception to be thrown")
      (catch Exception e
        (is (= "Invalid ValidatorDefinition" (ex-message e)))
        (let [data (ex-data e)]
          (is (= :invalid/schema (:code data)))
          (is (= 'fhemas.schema/validate-schema (:location data)))
          (is (= :validate-schema (:operation data)))
          (is (some? (:details data))))))))

(deftest validate-schema-returns-input-on-valid-test
  (testing "returns x unchanged, not the explain result"
    (is (= sample-vd (schema/validate-schema schema/ValidatorDefinition sample-vd "should not throw")))))

;; ---------------------------------------------------------------------------
;; CompileOrderResult schema
;; ---------------------------------------------------------------------------

(deftest compile-order-result-test
  (testing "valid compile order result"
    (is (passes? schema/CompileOrderResult [0 1 2 3 4])))
  (testing "valid compile order with different order"
    (is (passes? schema/CompileOrderResult [0 2 3 1])))
  (testing "invalid: empty vector"
    (is (fails? schema/CompileOrderResult [])))
  (testing "invalid: non-integer element"
    (is (fails? schema/CompileOrderResult [0 1 "two"])))
  (testing "invalid: negative integer"
    (is (fails? schema/CompileOrderResult [0 -1 2]))))
