(ns fhemas.parse
  (:require [camel-snake-kebab.core :as csk]
            [jsonista.core :as json]))

(def ^com.fasterxml.jackson.databind.ObjectMapper
  fhir-mapper
  "A JSON ObjectMapper configured to parse FHIR JSON into idiomatic Clojure maps"
  (json/object-mapper
   {:decode-key-fn csk/->kebab-case-keyword
    :bigdecimals true}))

(defn ->map
  [^String json-str]
  (json/read-value json-str fhir-mapper))

(def meta-fields
  #{:id :url :type :name :version :fhir-version :status :experimental  :context})

(def definition-fields
  #{:kind :abstract :base-definition :derivation})

(defn ->meta [m]
  (-> (select-keys m meta-fields)
      (update :status keyword)))

(defn ->definition [m]
  (select-keys m definition-fields))

(defn ->invariants [m]
  (:context-invariant m))

(comment

  ;; AST Structure 
  ;;{:meta {...}
  ;; :definition {...}
  ;; :elements [{...} ...]
  ;; :invariants ["..."]
  ;; }
  ;;
  ;; De los ElementDefinition sol sacaremos 
  ;;
  #{:path :id :slice-name :slice-is-constraining
    :min :max :base :slicing :short :type :content-reference
    "fixed[x]" "pattern[x]" "minValue[x]" "maxValue[x]" :max-length
    :constraint :condition}

  (defn ->elements [m])

  (defn ->ast [m])
  :.)
