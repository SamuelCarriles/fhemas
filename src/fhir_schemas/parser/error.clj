(ns fhir-schemas.parser.error)

(defmulti info (fn [code _data] code))

(defmethod info :default
  [code {:keys [scope message value expected]}]
  (ex-info message
           {:scope scope
            :operation :parse
            :code code
            :details {:value value
                      :expected expected}}))

(defmethod info :invalid/type
  [code {:keys [scope message value expected]}]
  (ex-info message 
           {:scope scope
            :operation :parse
            :code code
            :details {:value value
                      :type (type value)
                      :expected expected}}))

(defmethod info :invalid/cardinality
  [code {:keys [scope message field cardinality expected]}]
  (ex-info message
           {:scope scope
            :operation :parse
            :code code
            :details {:field field
                      :cardinality cardinality
                      :expected expected}}))

(defmethod info :missing/field
  [code {:keys [scope message field]}]
  (ex-info message
           {:scope scope
            :operation :parse
            :code code
            :details {:field field}}))
