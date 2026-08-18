(ns fhemas.validator-definition.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.string]
   [fhemas.validator-definition.core :as validator-def]))

;; ---------------------------------------------------------------------------
;; ->compiler
;; ---------------------------------------------------------------------------

(deftest ->compiler-happy-path-test
  (testing "resolves an existing stdlib function"
    (is (= #'clojure.core/identity
           (validator-def/->compiler 'clojure.core/identity))))
  (testing "resolves a function from a non-core namespace"
    (is (= #'clojure.string/upper-case
           (validator-def/->compiler 'clojure.string/upper-case)))))

(deftest ->compiler-nonexistent-test
  (testing "throws when the namespace does not exist"
    (try
      (validator-def/->compiler 'fhemas.nonexistent/foo)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e)))))))
  (testing "throws when the function does not exist in a valid namespace"
    (try
      (validator-def/->compiler 'clojure.core/nonexistent-fn-xyz)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; ->parser
;; ---------------------------------------------------------------------------

(deftest ->parser-happy-path-test
  (testing "resolves an existing stdlib function"
    (is (= #'clojure.core/identity
           (validator-def/->parser 'clojure.core/identity))))
  (testing "resolves a function from a non-core namespace"
    (is (= #'clojure.string/upper-case
           (validator-def/->parser 'clojure.string/upper-case)))))

(deftest ->parser-nonexistent-test
  (testing "throws when the namespace does not exist"
    (try
      (validator-def/->parser 'fhemas.nonexistent/foo)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/parser (:code (ex-data e)))))))
  (testing "throws when the function does not exist in a valid namespace"
    (try
      (validator-def/->parser 'clojure.core/nonexistent-fn-xyz)
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/parser (:code (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; coerce-compilers
;; ---------------------------------------------------------------------------

(deftest coerce-compilers-happy-path-test
  (testing "replaces a :compiler symbol with its resolved var"
    (let [input  {:compiler 'clojure.core/identity}
          result (validator-def/coerce-compilers input)]
      (is (= #'clojure.core/identity (:compiler result)))))

  (testing "does not modify a map without :compiler keys"
    (let [input  {:path [:some-path] :type :string}
          result (validator-def/coerce-compilers input)]
      (is (= input result))))

  (testing "does not modify keys with a different namespace"
    (let [input  {:other/compiler 'clojure.core/identity}
          result (validator-def/coerce-compilers input)]
      (is (= input result))))

  (testing "handles nested maps with :compiler keys at multiple levels"
    (let [input  {:schema {:invariants [{:compiler 'clojure.core/identity}]
                           :elements   {:fields [{:compiler 'clojure.core/inc}]}}}
          result (validator-def/coerce-compilers input)]
      (is (= #'clojure.core/identity
             (get-in result [:schema :invariants 0 :compiler])))
      (is (= #'clojure.core/inc
             (get-in result [:schema :elements :fields 0 :compiler]))))))

(deftest coerce-compilers-nonexistent-test
  (testing "throws when a :compiler symbol cannot be resolved"
    (try
      (validator-def/coerce-compilers {:compiler 'fhemas.nonexistent/foo})
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/compiler (:code (ex-data e))))))))

(deftest coerce-compilers-immutability-test
  (testing "does not modify the original map"
    (let [input {:compiler 'clojure.core/identity}]
      (validator-def/coerce-compilers input)
      (is (= 'clojure.core/identity (:compiler input))))))

;; ---------------------------------------------------------------------------
;; coerce-parsers
;; ---------------------------------------------------------------------------

(deftest coerce-parsers-happy-path-test
  (testing "replaces a :parser symbol with its resolved var"
    (let [input  {:parser 'clojure.core/identity}
          result (validator-def/coerce-parsers input)]
      (is (= #'clojure.core/identity (:parser result)))))

  (testing "does not modify a map without :parser keys"
    (let [input  {:path [:some-path] :type :string}
          result (validator-def/coerce-parsers input)]
      (is (= input result))))

  (testing "does not modify keys with a different namespace"
    (let [input  {:other/parser 'clojure.core/identity}
          result (validator-def/coerce-parsers input)]
      (is (= input result))))

  (testing "handles nested maps with :parser keys at multiple levels"
    (let [input  {:schema {:invariants [{:parser 'clojure.core/identity}]
                           :elements   {:fields [{:parser 'clojure.core/inc}]}}}
          result (validator-def/coerce-parsers input)]
      (is (= #'clojure.core/identity
             (get-in result [:schema :invariants 0 :parser])))
      (is (= #'clojure.core/inc
             (get-in result [:schema :elements :fields 0 :parser]))))))

(deftest coerce-parsers-nonexistent-test
  (testing "throws when a :parser symbol cannot be resolved"
    (try
      (validator-def/coerce-parsers {:parser 'fhemas.nonexistent/foo})
      (is false "expected an exception")
      (catch Exception e
        (is (= :invalid/parser (:code (ex-data e))))))))

(deftest coerce-parsers-immutability-test
  (testing "does not modify the original map"
    (let [input {:parser 'clojure.core/identity}]
      (validator-def/coerce-parsers input)
      (is (= 'clojure.core/identity (:parser input))))))
