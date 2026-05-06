(ns fhir-schemas.util)

(defn error-data 
  "Returns a FHIR-compliant OperationOutcome issue map with the given severity, code, and diagnostics message"
  [^String severity ^String code ^String message]
  {:severity severity
   :code code
   :diagnostics message})