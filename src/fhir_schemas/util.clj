(ns fhir-schemas.util)

(defn error-data
  [^String severity ^String code ^String message]
  {:severity severity
   :code code
   :diagnostics message})