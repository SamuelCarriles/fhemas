(ns fhemas.compile
  (:refer-clojure :exclude [compile])
  (:require [fhemas.schema :refer [validate-compile-order]]
            [fhemas.validator-definition.field.core :as field]))

(defn process-element
  [fields element]
  (keep #(field/process % element) fields))

;;revisar pro qué no funciona
(defn element [fields compiled-elements indexes]
  (reduce (fn [result {:keys [path value compile]}]
            (if compile
              (update-in result :validator #(comp (compile indexes {:value value :elements compiled-elements}) %))
              (assoc-in result path value)))
          {}
          fields))
;;está mal pensada
#_(defn elements
    [{:keys [_base-definition snapshot _differential fields]} indexes resource]
    (let [snapshot (field/process snapshot resource)]
    ;; como el schema ya va a validar en core que el VD sea correto,
    ;; solo tenemos dos casos posibles, que haya snapshot o differential
    ;; porque con el schema exigiremos eso.

      (if snapshot
        (let [raw-elements (:value snapshot)
              processed-elements (mapv (partial process-element fields) raw-elements)]
          (reduce
           (fn [ready-elements processed-element]
             (conj ready-elements (element processed-element ready-elements indexes)))
           []
           processed-elements))

        (throw (ex-info "Unsupported case. Comming soon!" {})))))

(defn validator
  [elements-definition indexes resource]
  (let [order (validate-compile-order (:compile-order elements-definition))]))

(comment
  ;; todos los fields se le aplican a cada element de snapshot,
  ;; luego un element se convierte en un vector de fields, y el conjunto
  ;; de elements es un vector que contiene los vectores de fields, es 
  ;; decir, algo así [[{...} {...}] [{...} {...}]]
  (try
    (element [{:path [:id] :value "a" :type :string :max 1 :compile (fn [& o] o)}]
             [] nil)
    (catch Exception e (.getMessage e)))
  :.)
