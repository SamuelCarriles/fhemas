(ns fhemas.schema.validator-definition-test
  (:require [clojure.test :refer [deftest is testing]]
            [fhemas.schema.validator-definition :as vd]))

(def valid-field
  {:path [:name]
   :type :string
   :min 1
   :max 1})

(def valid-primary-index
  {:name "sd-primary"
   :type :primary
   :key valid-field
   :value {:path [:element]
           :parser 'fhemas.parse/elements}
   :relation :1->1})

(def valid-query-index
  {:name "sd-by-url"
   :type :query
   :key {:path [:url] :type :string}
   :value {:path [:name] :type :string}
   :relation :1->1})

(def valid-vd
  {:resource-type "ValidatorDefinition"
   :url "https://github.com/samuelcarriles/fhemas.artifacts/vd/r4"
   :version "1.0.0"
   :status :stable
   :fhir-version "4.0.1"
   :processor {:dispatch-resource-by {:path [:resourceType] :type :string}
               :order {:elements 'fhemas.parse.r4/elements-order
                       :compile 'fhemas.compile.r4/compile-order}
               :indexes [valid-primary-index valid-query-index]
               :context-invariants {:path [:contextInvariant]}
               :elements {:locations {:base-definition {:path [:baseDefinition]}
                                      :snapshot {:path [:snapshot :element]}
                                      :differential {:path [:differential :element]}}
                          :fields [valid-field]}}})

(deftest validate-valid-vd
  (testing "A valid ValidatorDefinition passes validation"
    (is (= valid-vd (vd/validate valid-vd)))))

(deftest validate-primary-index-rules
  (testing "No primary index should fail"
    (let [invalid (update-in valid-vd
                             [:processor :indexes]
                             (fn [idxs] (mapv #(assoc % :type :query) idxs)))]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid)))))

  (testing "Two primary indexes should fail"
    (let [second-primary (assoc valid-primary-index :name "sd-primary-2")
          invalid (update-in valid-vd [:processor :indexes] conj second-primary)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid)))))

  (testing "Primary index without fhemas.parse/elements parser should fail"
    (let [invalid (assoc-in valid-vd
                            [:processor :indexes 0 :value :parser]
                            'some.other/parser)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid)))))

  (testing "Primary index without parser in :value should fail"
    (let [invalid (update-in valid-vd
                             [:processor :indexes 0 :value]
                             dissoc :parser)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid)))))

  (testing "Primary index with :1->* relation should fail"
    (let [invalid (assoc-in valid-vd
                            [:processor :indexes 0 :relation]
                            :1->*)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid))))))

(deftest validate-unique-index-names
  (testing "Indexes with duplicate names should fail"
    (let [duplicate (assoc valid-query-index :name "sd-primary")
          invalid (assoc-in valid-vd [:processor :indexes 1] duplicate)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid))))))

(deftest validate-field-rules
  (testing "Field with min > max should fail"
    (let [invalid-field {:path [:x] :min 5 :max 2}
          invalid (assoc-in valid-vd
                            [:processor :elements :fields 0]
                            invalid-field)]
      (is (thrown? clojure.lang.ExceptionInfo (vd/validate invalid))))))

(deftest validate-url-fields
  (testing "Invalid :url should fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (vd/validate (assoc valid-vd :url "not-a-url")))))

  (testing "Invalid :design-source should fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (vd/validate (assoc-in valid-vd
                                        [:processor :design-source]
                                        "not-a-url"))))))

(deftest validate-status-enum
  (testing ":status out of enum should fail"
    (is (thrown? clojure.lang.ExceptionInfo
                 (vd/validate (assoc valid-vd :status :unknown))))))
