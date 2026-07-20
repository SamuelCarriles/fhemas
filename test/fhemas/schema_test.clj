(ns fhemas.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [fhemas.schema :as schema]))

(def valid-schema-profile
  {:resource-type "SchemaProfile"
   :id "test-profile"
   :url "https://example.com/fhemas/R4/schema-profile.edn"
   :version "1.0.0"
   :title "Test Schema Profile"
   :status :active
   :description "A valid test profile"
   :fhir-version "4.0.1"
   :source "https://hl7.org/fhir/R4/structuredefinition.html"
   :schema
   {:meta [{:path [:id] :type :string :max 1}]
    :definition [{:path [:kind] :type :string :min 1 :max 1}]
    :invariants [{:path [:context-invariant] :type :vector}]
    :elements
    {:snapshot {:path [:snapshot :element]}
     :differential {:path [:differential :element]}
     :fields
     [{:path [:path] :type :string :min 1 :max 1}
      {:path {:re-str "^fixed-.*$"}
       :type [:string :integer :boolean]
       :compile-as :fhemas.compile.r4/fixed-value}
      {:path [:constraint]
       :type :vector
       :constraint {:fhirpath {:id :key :severity :severity :description :human :expression :expression}}}]}}})

;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------

(deftest validate-schema-profile-test
  (testing "Valid SchemaProfile"
    (is (= valid-schema-profile
           (schema/validate-schema-profile valid-schema-profile))
        "Should return the map intact if valid"))

  (testing "Optional fields omitted (min/max)"
    (let [minimal-field {:path [:test] :type :string}]
      (is (map? (schema/validate-schema-profile
                 (assoc-in valid-schema-profile [:schema :meta] [minimal-field]))))
      "A field without :min or :max should be valid"))

  (testing "Missing required field (:url)"
    (let [invalid-data (dissoc valid-schema-profile :url)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing "Incorrect :resource-type"
    (let [invalid-data (assoc valid-schema-profile :resource-type "InvalidType")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":status not in allowed enum"
    (let [invalid-data (assoc valid-schema-profile :status :deprecated)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":min is 0 (must be >= 1 if present, per convention)"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :meta 0 :min] 0)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":max is 0 (must be >= 1 if present, per convention)"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :meta 0 :max] 0)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":type is not a keyword or vector of keywords"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :meta 0 :type] "string")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing ":compile-as is not a qualified keyword (missing namespace)"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :elements :fields 1 :compile-as] :fixed-value)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data))
          ":compile-as must be :namespace/name, not just :name")))

  (testing ":path with regex has blank value"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :elements :fields 1 :path :re-str] "   ")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing "Invalid :elements structure (missing :fields)"
    (let [invalid-data (assoc-in valid-schema-profile [:schema :elements] {:snapshot {:path [:snap]}})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid SchemaProfile"
           (schema/validate-schema-profile invalid-data)))))

  (testing "Exception contains correct details (code, scope, operation)"
    (let [invalid-data (assoc valid-schema-profile :status :invalid)
          exception (try
                      (schema/validate-schema-profile invalid-data)
                      (catch Exception e e))]
      (is (= :invalid/schema (:code (ex-data exception))))
      (is (= :fhemas.schema (:scope (ex-data exception))))
      (is (= :validate-schema (:operation (ex-data exception))))
      (is (contains? (ex-data exception) :details)))))
