(ns fhemas.validator-definition.core
  (:require [clojure.walk :refer [postwalk]]
            [fhemas.error :as error]))

(defn ->fn [sym {:keys [code message]}]
  (if-some [function (try
                       (requiring-resolve sym)
                       (catch Exception _ nil))]
    function
    (throw (error/info code
                       {:message message
                        :location 'fhemas.validator-definition.core/->fn
                        :operation :resolve-symbol
                        :value sym}))))

(defn ->compiler
  [sym]
  (->fn sym {:code :invalid/compiler :message (format "The compiler function %s can not be resolved" sym)}))

(defn ->parser [sym]
  (->fn sym {:code :invalid/parser :message (format "The parser function %s can not be resolved" sym)}))

(defn coerce-symb
  [m sym-key resolver]
  (postwalk
   (fn [x]
     (if (and (map-entry? x)
              (= sym-key (key x)))
       [(key x) (resolver (val x))]
       x))
   m))

(defn coerce-compilers
  "Walks a map replacing :compiler symbols with their resolved functions.
   Returns a new map."

  [m]
  (coerce-symb m :compiler ->compiler))

(defn coerce-parsers
  "Walks a map replacing :parser symbols with their resolved functions.
   Returns a new map."
  [m]
  (coerce-symb m :parser ->parser))

