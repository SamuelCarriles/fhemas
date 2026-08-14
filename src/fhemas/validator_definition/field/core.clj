(ns fhemas.validator-definition.field.core
  (:require
   [fhemas.validator-definition.field.validate :as validate]
   [fhemas.error :as error]))

(defn- regex-path->value
  [path resource]
  (let [regex (-> path :re-str re-pattern)
        matches (filter #(->> % key name (re-matches regex)) resource)]
    (cond
      (empty? matches) nil

      (second matches)
      (throw (error/info :invalid/path
                         {:message "Regex path matched multiple keys. Only one match is allowed."
                          :location 'fhemas.validator-definition.field.core/regex-path->value
                          :operation :resolve-value-using-regex-path
                          :details {:regex-str (:re-str path)
                                    :matches (mapv key matches)}}))

      :else
      (let [[p v] (first matches)]
        {:path [p] :value v}))))

(defn get-value
  "Extracts a value from a resource based on the field's path.
   For regex paths, updates :path with the matched key.
   Returns the field with :value if found, or the field unchanged if not."
  [{:keys [path] :as field} resource]
  (if (map? path)
    (merge field (regex-path->value path resource))
    ;;
    (if-some [value (get-in resource path)]
      (assoc field :value value)
      field)))

(defn value?
  "Returns the field if it has a :value, nil otherwise."
  [field]
  (when (some? (:value field)) field))

(defn process
  "Orchestrates the complete field processing pipeline:
   extract value, validate cardinality, require value presence,
   validate type, and compile validator."
  [field resource]
  (some-> field
          (get-value resource)
          validate/cardinality
          value?
          validate/type))

