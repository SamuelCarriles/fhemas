 (ns fhemas.index-test
   (:require
    [clojure.test :refer [deftest testing is are]]
    [fhemas.index :as idx]))

(defn throws-code? [code f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= code (:code (ex-data e))))))

;; ---------------------------------------------------------------------------
;; submap?
;; ---------------------------------------------------------------------------

(deftest submap?-test
  (testing "returns true when m1 is a submap of m2"
    (is (true? (idx/submap? {:a 1} {:a 1 :b 2}))))
  (testing "returns true when m1 equals m2"
    (is (true? (idx/submap? {:a 1} {:a 1}))))
  (testing "returns false when m1 has a key not in m2"
    (is (false? (idx/submap? {:a 1 :c 3} {:a 1 :b 2}))))
  (testing "returns false when values don't match"
    (is (false? (idx/submap? {:a 1} {:a 2}))))
  (testing "empty map is a submap of anything"
    (is (true? (idx/submap? {} {:a 1}))))
  (testing "returns false when either m1 or m2 is nil"
    (are [m1 m2] (is (false? (idx/submap? m1 m2)))
      nil {:a 2}
      {:a 2} nil
      nil nil)))

;; ---------------------------------------------------------------------------
;; valid?
;; ---------------------------------------------------------------------------

(deftest valid?-test
  (testing "returns true when conds is empty"
    (is (true? (idx/valid? [] {:resource-type "StructureDefinition"}))))
  (testing "returns true when conds is nil"
    (is (true? (idx/valid? nil {:resource-type "StructureDefinition"}))))
  (testing "returns true when one condition matches (OR logic)"
    (is (true? (idx/valid? [{:resource-type "StructureDefinition"}
                            {:resource-type "ValueSet"}]
                           {:resource-type "StructureDefinition"}))))
  (testing "returns false when no condition matches"
    (is (false? (idx/valid? [{:resource-type "StructureDefinition"}]
                            {:resource-type "ValueSet"}))))
  (testing "multiple keys in one condition are AND logic"
    (is (true? (idx/valid? [{:resource-type "StructureDefinition" :kind "resource"}]
                           {:resource-type "StructureDefinition" :kind "resource" :name "Patient"})))
    (is (false? (idx/valid? [{:resource-type "StructureDefinition" :kind "resource"}]
                            {:resource-type "StructureDefinition" :kind "primitive-type"})))))

;; ---------------------------------------------------------------------------
;; create
;; ---------------------------------------------------------------------------

(def sample-indexes
  [{:name :idx/url->resource
    :key {:path [:url]}
    :relation :1->1}
   {:name :idx/name->url
    :when [{:resource-type "StructureDefinition"}]
    :key {:path [:name]}
    :value {:path [:url]}
    :relation :1->1}
   {:name :idx/kind->urls
    :when [{:resource-type "StructureDefinition"}]
    :key {:path [:kind]}
    :value {:path [:url]}
    :relation :1->*}])

(deftest create-basic-test
  (testing "creates index entry with full resource when no :value"
    (let [idx-def {:name :idx/url->resource
                   :key {:path [:url]}
                   :relation :1->1}
          resource {:url "http://example.com" :name "test"}
          result (idx/create idx-def resource)]
      (is (= {:idx/url->resource {"http://example.com" resource}} result))))

  (testing "creates index entry with extracted value when :value is present"
    (let [idx-def {:name :idx/name->url
                   :key {:path [:name]}
                   :value {:path [:url]}
                   :relation :1->1}
          resource {:url "http://example.com" :name "test"}
          result (idx/create idx-def resource)]
      (is (= {:idx/name->url {"test" "http://example.com"}} result))))

  (testing "returns nil when resource lacks the key field"
    (let [idx-def {:name :idx/name->url
                   :key {:path [:name]}
                   :value {:path [:url]}
                   :relation :1->1}
          resource {:url "http://example.com"}]
      (is (nil? (idx/create idx-def resource)))))

  (testing "returns nil when resource lacks the value field"
    (let [idx-def {:name :idx/name->url
                   :key {:path [:name]}
                   :value {:path [:url]}
                   :relation :1->1}
          resource {:name "test"}]
      (is (nil? (idx/create idx-def resource))))))

(deftest create-with-when-test
  (testing "creates index when :when condition matches"
    (let [idx-def {:name :idx/name->url
                   :when [{:resource-type "StructureDefinition"}]
                   :key {:path [:name]}
                   :value {:path [:url]}
                   :relation :1->1}
          resource {:resource-type "StructureDefinition" :name "test" :url "http://example.com"}
          result (idx/create idx-def resource)]
      (is (= {:idx/name->url {"test" "http://example.com"}} result))))

  (testing "returns nil when :when condition doesn't match"
    (let [idx-def {:name :idx/name->url
                   :when [{:resource-type "StructureDefinition"}]
                   :key {:path [:name]}
                   :value {:path [:url]}
                   :relation :1->1}
          resource {:resource-type "ValueSet" :name "test" :url "http://example.com"}]
      (is (nil? (idx/create idx-def resource)))))

  (testing "creates index when :when is absent"
    (let [idx-def {:name :idx/url->resource
                   :key {:path [:url]}
                   :relation :1->1}
          resource {:url "http://example.com" :name "test"}
          result (idx/create idx-def resource)]
      (is (= {:idx/url->resource {"http://example.com" resource}} result)))))

;; ---------------------------------------------------------------------------
;; insert (multimethod)
;; ---------------------------------------------------------------------------

(deftest insert-1->1-test
  (testing "inserts into empty page"
    (let [idx {:idx/url->resource {"http://example.com" {:url "http://example.com"}}}
          page {}
          result (idx/insert idx page :1->1)]
      (is (= idx result))))

  (testing "inserts into page with different index"
    (let [idx {:idx/name->url {"test" "http://example.com"}}
          page {:idx/url->resource {"http://other.com" {:url "http://other.com"}}}
          result (idx/insert idx page :1->1)]
      (is (= {:idx/url->resource {"http://other.com" {:url "http://other.com"}}
              :idx/name->url {"test" "http://example.com"}}
             result))))

  (testing "inserts into same index with different key"
    (let [idx {:idx/url->resource {"http://example2.com" {:url "http://example2.com"}}}
          page {:idx/url->resource {"http://example.com" {:url "http://example.com"}}}
          result (idx/insert idx page :1->1)]
      (is (= {:idx/url->resource {"http://example.com" {:url "http://example.com"}
                                  "http://example2.com" {:url "http://example2.com"}}}
             result))))

  (testing "throws :invalid/index on duplicate key"
    (let [idx {:idx/url->resource {"http://example.com" {:url "http://example.com"}}}
          page {:idx/url->resource {"http://example.com" {:url "http://example.com"}}}]
      (is (throws-code? :invalid/index
                        #(idx/insert idx page :1->1))))))

(deftest insert-1->*-test
  (testing "inserts into empty page creating vector"
    (let [idx {:idx/kind->urls {"primitive-type" "http://example.com/string"}}
          page {}
          result (idx/insert idx page :1->*)]
      (is (= {:idx/kind->urls {"primitive-type" #{"http://example.com/string"}}} result))))

  (testing "appends to existing vector"
    (let [idx {:idx/kind->urls {"primitive-type" "http://example.com/integer"}}
          page {:idx/kind->urls {"primitive-type" #{"http://example.com/string"}}}
          result (idx/insert idx page :1->*)]
      (is (= {:idx/kind->urls {"primitive-type" #{"http://example.com/string"
                                                 "http://example.com/integer"}}}
             result))))

  (testing "inserts into different key within same index"
    (let [idx {:idx/kind->urls {"complex-type" "http://example.com/HumanName"}}
          page {:idx/kind->urls {"primitive-type" #{"http://example.com/string"}}}
          result (idx/insert idx page :1->*)]
      (is (= {:idx/kind->urls {"primitive-type" #{"http://example.com/string"}
                               "complex-type" #{"http://example.com/HumanName"}}}
             result)))))

(deftest insert-default-test
  (testing "throws :invalid/index for unsupported relation"
    (let [idx {:idx/test {"key" "value"}}
          page {}]
      (is (throws-code? :invalid/index
                        #(idx/insert idx page :bogus))))))

;; ---------------------------------------------------------------------------
;; apply-indexes
;; ---------------------------------------------------------------------------

(deftest apply-indexes-test
  (testing "applies all indexes to a single resource"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}
                   {:name :idx/name->url
                    :key {:path [:name]}
                    :value {:path [:url]}
                    :relation :1->1}]
          resource {:url "http://example.com" :name "test"}
          result (idx/apply-indexes indexes {} resource)]
      (is (= {:idx/url->resource {"http://example.com" resource}
              :idx/name->url {"test" "http://example.com"}}
             result))))

  (testing "skips indexes when :when doesn't match"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}
                   {:name :idx/name->url
                    :when [{:resource-type "StructureDefinition"}]
                    :key {:path [:name]}
                    :value {:path [:url]}
                    :relation :1->1}]
          resource {:resource-type "ValueSet" :url "http://example.com" :name "test"}
          result (idx/apply-indexes indexes {} resource)]
      (is (= {:idx/url->resource {"http://example.com" resource}}
             result))))

  (testing "merges with existing page"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}]
          existing-page {:idx/name->url {"old" "http://old.com"}}
          resource {:url "http://new.com" :name "new"}
          result (idx/apply-indexes indexes existing-page resource)]
      (is (= {:idx/name->url {"old" "http://old.com"}
              :idx/url->resource {"http://new.com" resource}}
             result)))))

;; ---------------------------------------------------------------------------
;; build
;; ---------------------------------------------------------------------------

(deftest build-test
  (testing "builds complete indexes map from multiple resources"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}
                   {:name :idx/kind->urls
                    :key {:path [:kind]}
                    :value {:path [:url]}
                    :relation :1->*}]
          resources [{:url "http://example.com/string" :kind "primitive-type" :name "string"}
                     {:url "http://example.com/integer" :kind "primitive-type" :name "integer"}
                     {:url "http://example.com/Patient" :kind "resource" :name "Patient"}]
          result (idx/build indexes resources)]
      (is (= {:idx/url->resource {"http://example.com/string" (first resources)
                                  "http://example.com/integer" (second resources)
                                  "http://example.com/Patient" (nth resources 2)}
              :idx/kind->urls {"primitive-type" #{"http://example.com/string"
                                                 "http://example.com/integer"}
                               "resource" #{"http://example.com/Patient"}}}
             result))))

  (testing "handles resources that don't match some indexes"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}
                   {:name :idx/name->url
                    :when [{:resource-type "StructureDefinition"}]
                    :key {:path [:name]}
                    :value {:path [:url]}
                    :relation :1->1}]
          resources [{:resource-type "StructureDefinition" :url "http://sd1.com" :name "sd1"}
                     {:resource-type "ValueSet" :url "http://vs1.com" :name "vs1"}]
          result (idx/build indexes resources)]
      (is (= {:idx/url->resource {"http://sd1.com" (first resources)
                                  "http://vs1.com" (second resources)}
              :idx/name->url {"sd1" "http://sd1.com"}}
             result))))

  (testing "throws on duplicate key in 1->1 index"
    (let [indexes [{:name :idx/url->resource
                    :key {:path [:url]}
                    :relation :1->1}]
          resources [{:url "http://example.com" :name "first"}
                     {:url "http://example.com" :name "duplicate"}]]
      (is (throws-code? :invalid/index
                        #(idx/build indexes resources))))))

