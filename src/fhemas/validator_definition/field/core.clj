(ns fhemas.validator-definition.field.core
  (:refer-clojure :exclude [compile])
  (:require
   [fhemas.validator-definition.field.validate :as validate]))

(defn get-value
  "Extracts a value from a resource based on the field's path.
   For regex paths, updates :path with the matched key.
   Returns the field with :value if found, or the field unchanged if not."
  [{:keys [path] :as field} resource]
  (if (map? path)
    (let [regex (-> path :re-str re-pattern)
          [p v] (some #(when (->> % key name (re-matches regex)) %) resource)]
      (if p
        (assoc field :path [p] :value v)
        field))
    ;;
    (if-some [value (get-in resource path)]
      (assoc field :value value)
      field)))

(defn compile
  "Applies the field's compiler function to create a validator closure.
   Returns the field with :validator if a compiler exists, or unchanged if not."
  [field context]
  (let [compiler (:compile/field field)]
    (if-not compiler
      field
      (assoc field :validator (compiler context field)))))

(defn require-value
  "Returns the field if it has a :value, nil otherwise."
  [field]
  (when (some? (:value field)) field))

(defn process
  "Orchestrates the complete field processing pipeline:
   extract value, validate cardinality, require value presence,
   validate type, and compile validator."
  [field context resource]
  (some-> field
          (get-value resource)
          validate/cardinality
          require-value
          validate/type
          (compile context)))

