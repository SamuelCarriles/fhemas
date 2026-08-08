(ns fhemas.validator-definition.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.string]
   [fhemas.validator-definition.core :as validator-def]))

;; ---------------------------------------------------------------------------
;; resolve-compiler
;; ---------------------------------------------------------------------------

(deftest resolve-compiler-happy-path-test
  (testing "resolves an existing stdlib function"
    (is (= #'clojure.core/identity
           (validator-def/resolve-compiler 'clojure.core/identity))))
  (testing "resolves a function from a non-core namespace"
    (is (= #'clojure.string/upper-case
           (validator-def/resolve-compiler 'clojure.string/upper-case)))))

(deftest resolve-compiler-nonexistent-test
  (testing "throws when the namespace does not exist"
    (try
      (validator-def/resolve-compiler 'fhemas.nonexistent/foo)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e)))))))
  (testing "throws when the function does not exist in a valid namespace"
    (try
      (validator-def/resolve-compiler 'clojure.core/nonexistent-fn-xyz)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; coerce-compilers
;; ---------------------------------------------------------------------------

(deftest coerce-compilers-happy-path-test
  (testing "replaces a :compile/field symbol with its resolved var"
    (let [input  {:compile/field 'clojure.core/identity}
          result (validator-def/coerce-compilers input)]
      (is (= #'clojure.core/identity (:compile/field result)))))

  (testing "replaces a :compile/group symbol with its resolved var"
    (let [input  {:compile/group 'clojure.core/identity}
          result (validator-def/coerce-compilers input)]
      (is (= #'clojure.core/identity (:compile/group result)))))

  (testing "does not modify a map without :compile/* keys"
    (let [input  {:path [:some-path] :type :string}
          result (validator-def/coerce-compilers input)]
      (is (= input result))))

  (testing "does not modify keys with a different namespace"
    (let [input  {:other/field 'clojure.core/identity}
          result (validator-def/coerce-compilers input)]
      (is (= input result))))

  (testing "handles nested maps with :compile/* keys at multiple levels"
    (let [input  {:schema {:meta     [{:compile/field 'clojure.core/identity}]
                           :elements {:fields [{:compile/group 'clojure.core/inc}]}}}
          result (validator-def/coerce-compilers input)]
      (is (= #'clojure.core/identity
             (get-in result [:schema :meta 0 :compile/field])))
      (is (= #'clojure.core/inc
             (get-in result [:schema :elements :fields 0 :compile/group]))))))

(deftest coerce-compilers-nonexistent-test
  (testing "throws when a :compile/field symbol cannot be resolved"
    (try
      (validator-def/coerce-compilers {:compile/field 'fhemas.nonexistent/foo})
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e))))))))

(deftest coerce-compilers-immutability-test
  (testing "does not modify the original map"
    (let [input {:compile/field 'clojure.core/identity}]
      (validator-def/coerce-compilers input)
      (is (= 'clojure.core/identity (:compile/field input))))))

;; ---------------------------------------------------------------------------
;; process
;; ---------------------------------------------------------------------------

(def test-vd
  {:resource-type "ValidatorDefinition"
   :url "https://example.com/test-vd"
   :version "1.0.0"
   :status :active
   :fhir-version "4.0.1"
   :indexes [{:name :idx/example
              :key {:path [:url]}
              :relation :1->1}]
   :schema
   {:base "http://example.base.com"
    :meta [{:path [:status]
            :type :string
            :min 1
            :max 1
            :compile/field 'clojure.core/identity}]
    :invariants [{:path [:constraint]
                  :type :vector
                  :compile/field 'clojure.core/identity}]
    :elements
    {:base-definition {:path [:base-definition]}
     :snapshot        {:path [:snapshot :element]
                       :compile/field 'clojure.core/identity}
     :fields          [{:path [:path]
                        :type :string
                        :min 1
                        :max 1
                        :compile/field 'clojure.core/identity}]}}})

(deftest process-happy-path-test
  (testing "validates and coerces a valid VD with stdlib compilers"
    (let [result (validator-def/process test-vd)]
      (is (= #'clojure.core/identity
             (get-in result [:schema :meta 0 :compile/field])))
      (is (= #'clojure.core/identity
             (get-in result [:schema :elements :snapshot :compile/field]))))))

(deftest process-invalid-schema-test
  (testing "throws when the VD is structurally invalid"
    (try
      (validator-def/process (dissoc test-vd :resource-type))
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/schema (:code (ex-data e))))))))

(deftest process-nonexistent-compiler-test
  (testing "throws when a compiler symbol cannot be resolved"
    (let [bad-vd (assoc-in test-vd [:schema :meta 0 :compile/field]
                           'fhemas.nonexistent/foo)]
      (try
        (validator-def/process bad-vd)
        (is false "expected an exception")
        (catch Exception e
          (is (= :invalid/compiler (:code (ex-data e)))))))))
