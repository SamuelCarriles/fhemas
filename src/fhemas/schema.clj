(ns fhemas.schema
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [malli.core :as m]
   [malli.registry :as mr]
   [malli.error :as me]
   [fhemas.error :as error]
   [fhemas.validator-definition.field.validate :as validate]
   [fhemas.index :as idx]))

(defn not-blank-str? [s]
  (and (string? s)
       (not (str/blank? s))))

(defn url? [s]
  (try
    (boolean (jio/as-url s))
    (catch Exception _ false)))

(def registry
  (mr/composite-registry
   (m/default-schemas)
   {::not-blank-str [:fn {:error/message "must be a non blank string"} not-blank-str?]
    ::url [:fn {:error/message "must be a valid URL"} url?]}))

(def field-supported-types
  (-> validate/type
      methods
      (dissoc :default nil)
      keys
      vec))

(def Field
  [:and
   [:fn {:error/message "min must be less than or equal to max"}
    (fn [{:keys [min max]}]
      (if (and (some? min)
               (some? max))
        (>= max min)
        true))]
   [:fn {:error/message "cannot provide both compile/field and compile/group"}
    (fn [{:compile/keys [field group]}]
      (not (and field group)))]

   [:map
    [:path [:or
            [:vector :keyword]
            [:map
             [:re-str ::not-blank-str]]]]
    [:type {:optional true} (into [:enum] field-supported-types)]
    [:min {:optional true} [:int {:min 1}]]
    [:max {:optional true} [:int {:min 1}]]
    [:compile/field {:optional true} :qualified-symbol]
    [:compile/group {:optional true} :qualified-symbol]]])

(def Elements
  [:and
   [:fn {:error/message "must provide snapshot, differential, or both"}
    (fn [{:keys [snapshot differential]}]
      (or snapshot differential))]

   [:map
    [:base-definition Field]
    [:snapshot {:optional true} Field]
    [:differential {:optional true} Field]
    [:fields [:vector Field]]]])

(def Schema
  [:map
   [:source {:optional true} ::url]
   [:base ::url]
   [:meta [:vector Field]]
   [:invariants [:vector Field]]
   [:elements Elements]])

(def supported-idx-relations
  (-> idx/insert
      methods
      keys
      vec))

(def Index
  [:map
   [:name :keyword]
   [:when {:optional true} [:vector {:min 1} [:map-of :keyword :any]]]
   [:key Field]
   [:value {:optional true} Field]
   [:relation (into [:enum] supported-idx-relations)]])

(def ValidatorDefinition
  [:map
   [:resource-type [:= "ValidatorDefinition"]]
   [:id {:optional true} ::not-blank-str]
   [:url ::url]
   [:version ::not-blank-str]
   [:title {:optional true} ::not-blank-str]
   [:status [:enum :active :draft :unknown :retired]]
   [:description {:optional true} ::not-blank-str]
   [:fhir-version ::not-blank-str]
   [:indexes [:vector {:min 1} Index]]
   [:schema Schema]])

(defn validate-schema [schema x error-msg]
  (if-let [explain (m/explain schema x {:registry registry})]
    (throw (error/info :invalid/schema
                       {:message error-msg
                        :location 'fhemas.schema/validate-schema
                        :operation :validate-schema
                        :details (me/humanize explain)}))
    x))

(defn validate-validator-definition [m]
  (validate-schema ValidatorDefinition m "Invalid ValidatorDefinition"))

