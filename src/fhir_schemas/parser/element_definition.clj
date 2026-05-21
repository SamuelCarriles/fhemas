(ns fhir-schemas.parser.element-definition
  (:require [camel-snake-kebab.core :as csk]
            [jsonista.core :as j]
            [clojure.string :as str]
            [fhir-schemas.parser.error :as err]
            [fhir-schemas.type.primitive :as tp]))

(def ^:private fhir-type-ext-url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")

(def ^:private fhirpath-system-types
  #{"http://hl7.org/fhirpath/System.String"
    "http://hl7.org/fhirpath/System.Boolean"
    "http://hl7.org/fhirpath/System.Integer"
    "http://hl7.org/fhirpath/System.Decimal"
    "http://hl7.org/fhirpath/System.Date"
    "http://hl7.org/fhirpath/System.DateTime"
    "http://hl7.org/fhirpath/System.Time"
    "http://hl7.org/fhirpath/System.Quantity"})

(def ^com.fasterxml.jackson.databind.ObjectMapper
  fhir-mapper
  "A JSON ObjectMapper configured to parse FHIR JSON into idiomatic Clojure maps"
  (j/object-mapper
   {:decode-key-fn csk/->kebab-case-keyword
    :bigdecimals true}))

;; NOTE: Parser functions are separeted of their methods to make easy
;; the tests and refactors

(defn parse-path
  "Parses an ElementDefinition path string into a vector of kebab-case keywords.
   e.g. \"Patient.birthDate\" => [:patient :birth-date]"
  [^String s]
  (if (string? s)
    (->> (str/split s #"\.")
         (map #(str/replace % "[x]" ""))
         (mapv csk/->kebab-case-keyword))

    (throw (err/info :invalid/type
                     {:message "The 'path' value must be a string"
                      :scope #'parse-path
                      :value s
                      :expected [java.lang.String]}))))

(defn parse-max
  "Parses an ElementDefinition max cardinality. Returns nil for unbounded (*), 
   otherwise returns the integer value."
  [^String s]
  (if (string? s)
    (when (not= "*" s)
      (parse-long s))

    (throw (err/info :invalid/type
                     {:message "The 'max' value must be a string"
                      :scope #'parse-max
                      :value s
                      :expected [java.lang.String]}))))

(defn fhirpath-type?
  "Returns true if `s` is a FHIRPath System type"
  [^String s]
  (and (string? s)
       (contains? fhirpath-system-types s)))

(defn fhirpath-type->fhir-type
  "Resolves a FHIRPath System type to its FHIR type by extracting the value 
   from the 'structuredefinition-fhir-type' extension. Expects exactly one 
   matching extension; throws :invalid/cardinality otherwise"
  [extensions]

  (when (nil? extensions)
    (throw (err/info :missing/field
                     {:message "Extensions field is required to resolve FHIRPath System type"
                      :scope #'fhirpath-type->fhir-type
                      :field :extension})))
  (when-not (vector? extensions)
    (throw (err/info :invalid/type
                     {:message "The 'extension' field must be an array of extension objects"
                      :scope   #'fhirpath-type->fhir-type
                      :value   extensions
                      :expected [clojure.lang.IPersistentVector]})))

  (let [matches (filter #(= fhir-type-ext-url (:url %)) extensions)
        c-matches (count matches)]
    (case c-matches

      1 (:value-url (first matches))

      (throw (err/info :invalid/cardinality
                       {:message (format "%s '%s' extensions found to resolve the FHIRPath Type"
                                         (if (zero? c-matches) "Zero" "Too many")
                                         fhir-type-ext-url)
                        :scope #'fhirpath-type->fhir-type
                        :field :extension
                        :cardinality c-matches
                        :expected [1]})))))

(defn- type-namespace
  [^String s]
  (if (and (string? s)
           (tp/primitive? s))
    "fhir-schemas.type.primitive"
    "fhir-schemas.type.complex"))

(defn normalize-type
  "Normalizes a single type definition object"
  [m]

  (when-not (map? m)
    (throw (err/info :invalid/type
                     {:message "Each type definition must be a map"
                      :scope #'normalize-type
                      :value m
                      :expected [clojure.lang.PersistentArrayMap]})))

  (when-not (:code m)
    (throw (err/info :missing/field
                     {:message "Each type definition must have a 'code' field"
                      :scope #'normalize-type
                      :field :code})))

  (cond-> m
    (fhirpath-type? (:code m))
    (assoc :code (fhirpath-type->fhir-type (:extension m)))

    (:code m)
    (update :code #(let [kebab-type (csk/->kebab-case %)]
                     (keyword (type-namespace kebab-type) kebab-type)))))

(defn parse-type
  "Parses an array of ElementDefinition.type objects into normalized type definitions.
   Expects a vector of maps. Each map must have a :code field"
  [types]
  (when-not (vector? types)
    (throw (err/info :invalid/type
                     {:message "The 'type' value must be an array of type-objects"
                      :scope #'parse-type
                      :value types
                      :expected [clojure.lang.IPersistentVector]})))

  (mapv normalize-type types))

(defn parse-binding
  "Normalizes the binding property. Converts strength to keyword when present."
  [m]
  (when-not (map? m)
    (throw (err/info :invalid/type
                     {:message "The 'binding' value must be an object (clojure map)"
                      :scope #'parse-binding
                      :value m
                      :expected [clojure.lang.IPersistentMap]})))

  (cond-> m
    (:strength m)
    (update :strength keyword)))

(defn normalize-constraint
  "Normalizes a single constraint object. Converts severity to keyword when present."
  [m]
  (when-not (map? m)
    (throw (err/info :invalid/type
                     {:message "Each constraint definition must be an object (clojure map)"
                      :scope   #'normalize-constraint
                      :value   m
                      :expected [clojure.lang.IPersistentMap]})))
  (cond-> m
    (:severity m) (update :severity keyword)))

(defn parse-constraint
  "Parses an array of constraint objects."
  [constraints]
  (when-not (vector? constraints)
    (throw (err/info :invalid/type
                     {:message "The 'constraint' value must be an array of constraint objects"
                      :scope   #'parse-constraint
                      :value   constraints
                      :expected [clojure.lang.IPersistentVector]})))

  (mapv normalize-constraint constraints))

(defn parse-content-reference
  "Parses a contentReference string to a vector of keywords
  e.g. \"Bundle.entry.request\" => [:bundle :entry :request]"
  [^String s]
  (when-not (string? s)
    (throw (err/info :invalid/type
                     {:message "The 'contentReference' value must be a string"
                      :scope #'parse-content-reference
                      :value s
                      :expected [java.lang.String]})))

  (when-not (str/starts-with? s "#")
    (throw (err/info :invalid/format
                     {:message "The 'contentReference' value must start with '#'"
                      :scope #'parse-content-reference
                      :value s
                      :expected [#"^#.*$"]})))

  (parse-path (subs s 1)))

;; NOTE: Parse methods

(defmulti parse-prop
  "Parses and normalizes a specific property of an ElementDefinition"
  (fn [field _] field))

(defmethod parse-prop :default [_ x] x)

(defmethod parse-prop :path [_ x]
  (parse-path x))

(defmethod parse-prop :max [_ x]
  (parse-max x))

(defmethod parse-prop :type [_ x]
  (parse-type x))

(defmethod parse-prop :binding [_ x]
  (parse-binding x))

(defmethod parse-prop :constraint [_ x]
  (parse-constraint x))

(defmethod parse-prop  :content-reference
  [_ x]
  (parse-content-reference x))

(defn parse-field
  [m]
  (reduce-kv
   (fn [acc k v]
     (if-let [new-val (parse-prop k v)]
       (assoc acc k new-val)
       acc))
   {} m))

(defn elements->map
  [elements]
  (reduce (fn [acc curr]
            (let [field (parse-field curr)
                  path (:path field)
                  data (dissoc field :path)]
              (assoc acc path data)))
          {} elements))
