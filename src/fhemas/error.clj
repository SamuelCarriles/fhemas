(ns fhemas.error)

(defmulti info (fn [code _data] code))

(defmethod info :invalid/schema
  [code {:keys [message location operation details]}]
  (ex-info message {:code code
                    :location location
                    :operation operation
                    :details details}))

(defmethod info :invalid/compiler
  [code {:keys [message location operation value]}]
  (ex-info message {:code code
                    :location location
                    :operation operation
                    :details {:compiler value
                              :expected "A valid compiler-fn symbol"}}))
