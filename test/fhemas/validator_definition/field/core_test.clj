(ns fhemas.validator-definition.field.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fhemas.validator-definition.field.core :as field]))

;; ---------------------------------------------------------------------------
;; get-value
;; ---------------------------------------------------------------------------

(deftest get-value-normal-path-test
  (testing "returns field with :value when path exists"
    (is (= {:path [:status] :value "active"}
           (field/get-value {:path [:status]} {:status "active"}))))
  (testing "returns field unchanged when path does not exist"
    (is (= {:path [:status]}
           (field/get-value {:path [:status]} {:name "test"}))))
  (testing "returns field with :value false (if-some handles false correctly)"
    (is (= {:path [:experimental] :value false}
           (field/get-value {:path [:experimental]} {:experimental false}))))
  (testing "returns field unchanged when value is nil"
    (is (= {:path [:status]}
           (field/get-value {:path [:status]} {:status nil}))))
  (testing "preserves other field keys"
    (is (= {:path [:status] :type :string :max 1 :value "active"}
           (field/get-value {:path [:status] :type :string :max 1}
                            {:status "active"}))))
  (testing "handles nested paths"
    (is (= {:path [:snapshot :element] :value [{:id "1"} {:id "2"}]}
           (field/get-value {:path [:snapshot :element]}
                            {:snapshot {:element [{:id "1"} {:id "2"}]}})))))

(deftest get-value-regex-path-test
  (testing "returns field with matched key in :path and value in :value"
    (is (= {:path [:fixed-string] :value "abc"}
           (field/get-value {:path {:re-str "^fixed-.*$"}}
                            {:fixed-string "abc" :name "test"}))))
  (testing "returns field unchanged when no key matches regex"
    (is (= {:path {:re-str "^fixed-.*$"}}
           (field/get-value {:path {:re-str "^fixed-.*$"}}
                            {:name "test" :status "active"}))))
  (testing "preserves other field keys"
    (is (= {:path [:fixed-string] :max 1 :value "abc"}
           (field/get-value {:path {:re-str "^fixed-.*$"} :max 1}
                            {:fixed-string "abc"})))))

;; ---------------------------------------------------------------------------
;; compile
;; ---------------------------------------------------------------------------

(defn- test-compiler [ctx field]
  (fn [resource] {:ctx ctx :field field :resource resource}))

(deftest compile-test
  (testing "returns field unchanged when no compiler"
    (is (= {:path [:status]}
           (field/compile {:path [:status]} {}))))
  (testing "adds :validator with compiler result"
    (let [field {:path [:status] :compile/field test-compiler}
          result (field/compile field {:some :context})]
      (is (contains? result :validator))
      (is (fn? (:validator result)))))
  (testing "compiler receives context and field"
    (let [field {:path [:status] :compile/field test-compiler}
          ctx {:some :context}
          result (field/compile field ctx)
          validator (:validator result)]
      (is (= {:ctx ctx :field field :resource {:data 1}}
             (validator {:data 1}))))))

;; ---------------------------------------------------------------------------
;; require-value
;; ---------------------------------------------------------------------------

(deftest require-value-test
  (testing "returns field when :value is present"
    (is (= {:path [:status] :value "active"}
           (field/require-value {:path [:status] :value "active"}))))
  (testing "returns nil when :value is absent"
    (is (nil? (field/require-value {:path [:status]}))))
  (testing "returns field when :value is false"
    (is (= {:path [:experimental] :value false}
           (field/require-value {:path [:experimental] :value false}))))
  (testing "returns nil when :value is nil"
    (is (nil? (field/require-value {:path [:status] :value nil})))))

;; ---------------------------------------------------------------------------
;; process
;; ---------------------------------------------------------------------------

(defn- string-compiler [_ctx _field]
  (fn [resource] (str "validated-" (:status resource))))

(deftest process-successful-pipeline-test
  (testing "full pipeline succeeds and returns field with :validator"
    (let [field {:path [:status] :type :string :compile/field string-compiler}
          result (field/process field {} {:status "active"})]
      (is (contains? result :validator))
      (is (fn? (:validator result)))
      (is (= "validated-active" ((:validator result) {:status "active"}))))))

(deftest process-optional-field-absent-test
  (testing "returns nil when optional field is absent"
    (is (nil? (field/process {:path [:status] :type :string}
                             {}
                             {:name "test"})))))

(deftest process-required-field-absent-test
  (testing "throws cardinality error when required field is absent"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :string :min 1}
                                {}
                                {:name "test"})))))

(deftest process-invalid-type-test
  (testing "throws type error when value has wrong type"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :integer}
                                {}
                                {:status "active"})))))

(deftest process-invalid-cardinality-test
  (testing "throws cardinality error when max is exceeded"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :vector :max 1}
                                {}
                                {:status ["a" "b"]})))))

(deftest process-no-compiler-test
  (testing "returns field without :validator when no compiler"
    (let [result (field/process {:path [:status] :type :string}
                                {}
                                {:status "active"})]
      (is (= {:path [:status] :type :string :value "active"}
             result)))))
