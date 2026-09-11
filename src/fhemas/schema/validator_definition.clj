(ns fhemas.schema.validator-definition
  (:require
   [malli.core :as m]
   [fhemas.schema.core :as core]
   [fhemas.validator-definition.field.validate :as validate]))

(defn valid-primary-idx?
  [{:keys [indexes]}]
  (= 1 (count (filter #(and (= :primary (:type %))
                            (= :1->1 (:relation %))
                            (= 'fhemas.parse/elements (get-in % [:value :parser])))
                      indexes))))

(defn unique-idx-names?
  [{:keys [indexes]}]
  (let [names (map :name indexes)]
    (= (count names) (count (distinct names)))))

(def field-supported-types
  (-> validate/type
      methods
      (dissoc :default nil)
      keys
      vec))

(def FieldTypes
  (into [:enum] field-supported-types))

(def PathSegment
  [:or
   :keyword
   [:map
    [:re ::core/non-blank-str]]])

(def Field
  [:and
   [:fn {:error/message "min must be less than or equal to max"}
    (fn [{:keys [min max]}]
      (if (and (some? min)
               (some? max))
        (>= max min)
        true))]
   [:map
    [:path {:optional true}
     [:vector PathSegment]]
    [:type {:optional true} FieldTypes]
    [:min {:optional true} pos-int?]
    [:max {:optional true} pos-int?]
    [:parser {:optional true} :qualified-symbol]
    [:compiler {:optional true} :qualified-symbol]]])

(def Index
  [:map
   [:name :keyword]
   [:type {:default :query} [:enum :primary :query]]
   [:key Field]
   [:value Field]
   [:relation [:enum :1->1 :1->*]]
   [:when {:optional true}
    [:vector [:map-of :keyword :any]]]])

(def ElementsLocations
  [:map
   [:base-definition Field]
   [:snapshot {:optional true}  Field]
   [:differential {:optional true} Field]])

(def Processor
  [:and
   [:fn {:error/message "The primary index must be unique, have :1->1 relation, and have fhemas.parse/elements symbol in [:value :parser]"}
    valid-primary-idx?]
   [:fn {:error/message "All index names must be unique"} unique-idx-names?]
   [:map
    [:design-source {:optional true} ::core/url]
    [:entry-dispatcher :qualified-symbol]
    [:order
     [:map
      [:elements :qualified-symbol]
      [:compile :qualified-symbol]]]

    [:indexes
     [:vector Index]]
    [:context-invariants Field]
    [:elements
     [:map
      [:locations ElementsLocations]
      [:fields
       [:vector Field]]]]]])

(def Schema
  (m/schema
   [:map
    [:resource-type [:= "ValidatorDefinition"]]
    [:id {:optional true} ::core/non-blank-str]
    [:url ::core/url]
    [:version ::core/non-blank-str]
    [:status [:enum :stable :unstable :deprecated]]
    [:fhir-version ::core/non-blank-str]
    [:title {:optional true} ::core/non-blank-str]
    [:description {:optional true} ::core/non-blank-str]
    [:processor Processor]]
   {:registry core/registry}))

(defn validate
  "Validates a ValidatorDefinition resource against the ValidatorDefinition schema.
   Returns the resource if valid, throws ExceptionInfo otherwise."
  [m]
  (core/validate Schema m "Invalid ValidatorDefinition resource"))
