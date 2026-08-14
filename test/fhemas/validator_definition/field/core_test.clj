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
           (field/get-value {:path [:status]} {:name "a"}))))
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
                            {:fixed-string "abc"}))))
  (testing "throws :invalid/path when multiple keys match"
    (let [e (try
              (field/get-value {:path {:re-str "^fixed-.*$"}}
                               {:fixed-string "abc" :fixed-integer 42})
              (catch Exception e e))]
      (is (= :invalid/path (-> e ex-data :code)))
      (is (= [:fixed-string :fixed-integer]
             (-> e ex-data :details :matches))))))

;; ---------------------------------------------------------------------------
;; value?
;; ---------------------------------------------------------------------------

(deftest value?-test
  (testing "returns field when :value is present"
    (is (= {:path [:status] :value "active"}
           (field/value? {:path [:status] :value "active"}))))
  (testing "returns nil when :value is absent"
    (is (nil? (field/value? {:path [:status]}))))
  (testing "returns field when :value is false"
    (is (= {:path [:experimental] :value false}
           (field/value? {:path [:experimental] :value false})))))

;; ---------------------------------------------------------------------------
;; process
;; ---------------------------------------------------------------------------

(deftest process-successful-pipeline-test
  (testing "full pipeline succeeds and returns field with :value"
    (let [result (field/process {:path [:status] :type :string}
                                {:status "active"})]
      (is (= {:path [:status] :type :string :value "active"}
             result)))))

(deftest process-optional-field-absent-test
  (testing "returns nil when optional field is absent"
    (is (nil? (field/process {:path [:status] :type :string}
                             {:name "test"})))))

(deftest process-required-field-absent-test
  (testing "throws cardinality error when required field is absent"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :string :min 1}
                                {:name "test"})))))

(deftest process-invalid-type-test
  (testing "throws type error when value has wrong type"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :integer}
                                {:status "active"})))))

(deftest process-invalid-cardinality-test
  (testing "throws cardinality error when max is exceeded"
    (is (thrown? Exception
                 (field/process {:path [:status] :type :vector :max 1}
                                {:status ["a" "b"]})))))
