(ns fhir-schemas.parser.error)

(defmulti info (fn [code _data] code))

(defmethod info :default
  [code {:keys [scope message value expected]}]
  (ex-info message
           {:scope scope
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