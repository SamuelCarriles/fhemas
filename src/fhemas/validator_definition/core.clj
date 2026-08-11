(ns fhemas.validator-definition.core
  (:require [clojure.walk :refer [postwalk]]
            [fhemas.error :as error]))

(defn resolve-compiler
  "Resolves a qualified symbol to its compiler function.
   Throws if the symbol cannot be resolved."
  [sym]
  (if-some [compiler (try
                       (requiring-resolve sym)
                       (catch Exception _ nil))]
    compiler
    (throw (error/info :invalid/compiler
                       {:message (format "The compiler function %s can not be resolved" sym)
                        :location 'fhemas.validator-definition/resolve-compiler
                        :operation :resolve-symbol
                        :value sym}))))

(defn coerce-compilers
  "Walks a map replacing :compile/* symbols with their resolved functions.
   Returns a new map."
  [m]
  (postwalk
   (fn [x]
     (if (and (map-entry? x)
              (qualified-keyword? (key x))
              (= "compile" (namespace (key x))))
       [(key x) (resolve-compiler (val x))]
       x))
   m))

(defn process
  "Recieve a ValidatorDefinition schema and resolves
   all compiler symbols to their functions."
  [validator-def-map]
  (update validator-def-map :schema coerce-compilers))


