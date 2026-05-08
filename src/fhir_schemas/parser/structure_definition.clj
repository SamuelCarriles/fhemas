(ns fhir-schemas.parser.structure-definition
  (:require [camel-snake-kebab.core :as csk]
            [jsonista.core :as j]))

(def ^com.fasterxml.jackson.databind.ObjectMapper 
  fhir-mapper
  "A JSON ObjectMapper configured to parse FHIR JSON into idiomatic Clojure maps"
  (j/object-mapper
   {:decode-key-fn csk/->kebab-case-keyword
    :bigdecimals true}))

