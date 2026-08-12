(ns fhemas.core
  (:require [fhemas.schema :as schema]
            [fhemas.validator-definition.core :refer [process]]))

(comment
  (defn ->validator
    [validator-definition base-resources]
    (let [ready-validator-df (-> validator-definition
                                 schema/validate-validator-definition
                                 process)]
      ;;
      ))
  (defn validate
    [validator resource])

  :.)
