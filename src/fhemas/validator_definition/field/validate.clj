(ns fhemas.validator-definition.field.validate
  (:refer-clojure :exclude [type])
  (:require
   [fhemas.error :as error])
  (:import [java.net URI]))

(defn- wrap-val [value]
  (cond
    (vector? value) value
    (nil? value) []
    :else [value]))

(defn- min-cardinality
  [{:keys [min value] :as field}]
  (let [wrapped-val (wrap-val value)]
    (if (>= (count wrapped-val) min)
      field
      (throw (error/info :invalid/cardinality
                         {:message (format "Expected at least %d element(s), but found %d" min (count wrapped-val))
                          :location 'fhemas.validator-definition.field.validate/min-cardinality
                          :operation :validate-cardinality
                          :value value
                          :req-cardinality min})))))

(defn- max-cardinality
  [{:keys [max value] :as field}]
  (let [wrapped-val (wrap-val value)]
    (if (<= (count wrapped-val) max)
      field
      (throw (error/info :invalid/cardinality
                         {:message (format "Expected at most %d element(s), but found %d" max (count wrapped-val))
                          :location 'fhemas.validator-definition.field.validate/max-cardinality
                          :operation :validate-max-cardinality
                          :value value
                          :req-cardinality max})))))

(defn cardinality
  "Validates that the field's value count is within min/max bounds.
   Throws :invalid/cardinality error if validation fails."
  [{:keys [min max] :as field}]
  (cond->> field
    min min-cardinality
    max max-cardinality))

(defn- throw-type-error
  ([v expec-type] (throw-type-error v expec-type (format "The value must be %s" expec-type)))
  ([v expec-type msg]
   (throw (error/info :invalid/type
                      {:message msg
                       :location 'fhemas.validator-definition.field.validate/type
                       :operation :validate-type
                       :value v
                       :expected expec-type}))))

(defmulti type
  "Validates that the field's value matches the expected type.
   Dispatches on :type keyword. Throws :invalid/type error if validation fails."
  (fn [field] (:type field)))

(defmethod type :default
  [{:keys [value]}]
  (throw-type-error value
                    (keys (methods type))
                    (format "Unsupported type. Supported types are: %s" (keys (methods type)))))

(defmethod type nil
  [field]
  field)

(defmethod type :string
  [{:keys [value] :as field}]
  (if (string? value)
    field
    (throw-type-error value "a string")))

(defmethod type :integer
  [{:keys [value] :as field}]
  (if (integer? value)
    field
    (throw-type-error value "an integer")))

(defmethod type :vector
  [{:keys [value] :as field}]
  (if (vector? value)
    field
    (throw-type-error value "a vector")))

(defmethod type :boolean
  [{:keys [value] :as field}]
  (if (boolean? value)
    field
    (throw-type-error value "a boolean")))

(defmethod type :map
  [{:keys [value] :as field}]
  (if (map? value)
    field
    (throw-type-error value "a map")))

(defmethod type :uri
  [{:keys [value] :as field}]
  (let [valid? (and (string? value)
                    (try
                      (URI. value)
                      (catch Exception _ false)))]
    (if valid?
      field
      (throw-type-error value "a URI"))))


