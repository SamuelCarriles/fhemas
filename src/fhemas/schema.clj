(ns fhemas.schema
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [malli.core :as m]
   [malli.registry :as mr]
   [malli.error :as me]
   [fhemas.error :as error]))

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

(def Field
  [:and
   [:fn {:error/message "min most be lower or equal than max"}
    (fn [{:keys [min max]}]
      (if (and (some? min)
               (some? max))
        (>= max min)
        true))]
   [:fn {:error/message "you most provide either compile/field or compile/with-group"}
    (fn [{:compile/keys [field with-group]}]
      (not (and field with-group)))]

   [:map
    [:path [:or
            [:vector :keyword]
            [:map
             [:re-str ::not-blank-str]]]]
    [:type {:optional true} [:or :keyword [:vector :keyword]]]
    [:min {:optional true} [:int {:min 1}]]
    [:max {:optional true} [:int {:min 1}]]
    [:compile/field {:optional true} :qualified-keyword]
    [:compile/with-group {:optional true} :qualified-keyword]]])

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
   [:schema
    [:map
     [:source {:optional true} ::url]
     [:identifier Field]

     [:meta [:vector Field]]
     [:invariants [:vector Field]]
     [:elements Elements]]]])

(defn validate-schema [schema x error-msg]
  (if-let [explain (m/explain schema x {:registry registry})]
    (throw (error/info :invalid/schema
                       {:message error-msg
                        :scope :fhemas.schema
                        :operation :validate-schema
                        :details (me/humanize explain)}))
    x))

(defn validate-validator-definition [m]
  (validate-schema ValidatorDefinition m "Invalid ValidatorDefinition"))

