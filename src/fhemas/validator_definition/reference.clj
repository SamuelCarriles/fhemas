(ns fhemas.validator-definition.reference
  (:require [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))

(defn reference?
  [k]
  (and (qualified-keyword? k)
       (= "ref" (namespace k))))

(defn ->referent-path
  [k]
  (when (reference? k)
    (let [str-path (name k)]
      (->> (str/split str-path #"\.")
           (mapv keyword)))))


