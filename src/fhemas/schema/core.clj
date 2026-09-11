(ns fhemas.schema.core
  (:require
   [malli.core :as m]
   [malli.registry :as mr]
   [malli.error :as me]
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [fhemas.error :as error]))

(defn url? [s]
  (try
    (boolean (jio/as-url s))
    (catch Exception _ false)))

(def registry
  (mr/composite-registry
   m/default-registry
   {::non-blank-str [:fn {:error/message "Must be a not blank string"} (complement str/blank?)]
    ::url [:fn {:error/message "Must be a valid URL"} url?]}))

(defn validate
  "Validates `entry` against `schema`. Returns the entry if valid.
   Throws ExceptionInfo with :schema/invalid-entry code and humanized
   error details when validation fails."
  [schema entry error-msg]
  (if-let [explain (m/explain schema entry)]
    (throw (error/info :schema/invalid-entry
                       {:message error-msg
                        :location 'fhemas.schema.core/validate
                        :operation :validate-schema
                        :value entry
                        :expected (me/humanize explain)}))
    entry))
