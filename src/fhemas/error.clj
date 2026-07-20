(ns fhemas.error)

(defmulti info (fn [code _data] code))

(defmethod info :invalid/schema
  [code {:keys [message scope operation details]}]
  (ex-info message {:code code
                    :scope scope
                    :operation operation
                    :details details}))
