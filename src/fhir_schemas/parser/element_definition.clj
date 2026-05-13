(ns fhir-schemas.parser.element-definition
  (:require [camel-snake-kebab.core :as csk]
            [jsonista.core :as j]
            [clojure.string :as str]
            [fhir-schemas.parser.error :as err]))

(def ^:private fhir-type-ext-url "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")

(def ^:private fhirpath-system-prefix "http://hl7.org/fhirpath/System.")

(def ^com.fasterxml.jackson.databind.ObjectMapper
  fhir-mapper
  "A JSON ObjectMapper configured to parse FHIR JSON into idiomatic Clojure maps"
  (j/object-mapper
   {:decode-key-fn csk/->kebab-case-keyword
    :bigdecimals true}))

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
         (str/starts-with? s fhirpath-system-prefix)))

  (defn fhirpath-type->fhir-type
    "Resolves a FHIRPath System type to its FHIR type by extracting the value 
   from the 'structuredefinition-fhir-type' extension. Expects exactly one 
   matching extension; throws :invalid/cardinality otherwise"
    [extensions]
    (let [matches (filter #(= fhir-type-ext-url (:url %)) extensions)
          c-matches (count matches)]
      (case c-matches

        1 (:value-url (first matches))

        (throw (err/info :invalid/cardinality {:message (format "%s '%s' extensions found to resolve the FHIRPath Type"
                                                                (if (zero? c-matches) "Zero" "Too many")
                                                                fhir-type-ext-url)
                                               :scope #'fhirpath-type->fhir-type
                                               :field :extension
                                               :cardinality c-matches
                                               :expected [1]})))))

  
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
      (update :code #(keyword "fhir.type" %))))

  

  (defn parse-type
    "Parses an array of ElementDefinition.type objects into normalized type definitions.
   Expects a vector of maps. Each map must have a :code field"
    [types]
    (if (vector? types)
      (mapv normalize-type types)

      (throw (err/info :invalid/type
                       {:message "The 'type' value must be an array of type-objects"
                        :scope #'parse-type
                        :value types
                        :expected [clojure.lang.PersistentVector]}))))

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

(defn elements->map
  [elements]
  (reduce (fn [acc curr]
            (let [path (:path curr)
                  data (dissoc curr :path)]
              (assoc acc path data)))
          {} elements))





(comment
  ;; Si max es * significa que no hay límite de cantidad, por lo tanto que exista max en ese caso es redundante, mejor se disocia del mapa
  (let [elementdef (-> (clojure.java.io/resource "structure_definitions/example.json")
                       slurp
                       (j/read-value fhir-mapper))
        elements (get-in elementdef [:snapshot :element])]
    (keys (first elements)))

  (defn parse-properties [m]
    (reduce-kv (fn [m k v]
                 (assoc m k (parse-prop k v)))
               {} m))

  (defn cardinality [m]
    (let [{:keys [min max]} m
          limits (into #{} (remove nil? [min max]))]
      (-> (dissoc m :min :max)
          (assoc :fhir/cardinality limits))))

  (try (->> [{:path "Patient.name" :type [{:code "string"}] :min 1 :max "1" :constraint {}}]
       (map parse-properties)
       (mapv cardinality)
       (elements->map))
       (catch Exception e (ex-data e)))



  :.)