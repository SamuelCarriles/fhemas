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

(defmethod info :invalid/parser
  [code {:keys [message location operation value]}]
  (ex-info message {:code code
                    :location location
                    :operation operation
                    :details {:parser value
                              :expected "A valid parser-fn symbol"}}))

(defmethod info :invalid/cardinality
  [code {:keys [message location operation req-cardinality value]}]
  (ex-info message {:code code
                    :location location
                    :operation operation
                    :details {:value value
                              :cardinality {:current (count value)
                                            :expected req-cardinality}}}))

(defmethod info :invalid/type
  [code {:keys [message location operation value expected]}]
  (ex-info message {:code code
                    :location location
                    :operation operation
                    :details {:value value
                              :type {:current (type value)
                                     :expected expected}}}))

(defmethod info :default
  [code data]
  (ex-info (:message data)
           (-> data
               (dissoc :message)
               (assoc :code code))))



