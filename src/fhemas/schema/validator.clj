(ns fhemas.schema.validator
  (:require
   [malli.core :as m]
   [fhemas.schema.core :as core]))

(def Meta
  [:map
   [:version-id ::core/non-blank-str]
   [:source ::core/url]
   [:profile ::core/url]
   [:tag
    [:vector
     [:map
      [:system ::core/url]
      [:code :keyword]]]]])

(def ResolvedIndexes
  [:map-of :keyword :map])

(def Registry
  [:map
   [:order :qualified-symbol]
   [:elements ResolvedIndexes]
   [:queries {:optional true} ResolvedIndexes]])

(def Schema
  (m/schema
   [:map
    [:resource-type [:= "Validator"]]
    [:id {:optional true} ::core/non-blank-str]
    [:meta Meta]
    [:registry Registry]]
   {:registry core/registry}))

(defn validate
  "Validates a Validator resource against the Validator schema.
   Returns the resource if valid, throws ExceptionInfo otherwise."
  [m]
  (core/validate Schema m "Invalid Validator resource"))
