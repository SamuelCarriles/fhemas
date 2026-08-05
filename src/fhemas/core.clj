(ns fhemas.core)

(comment 
  ;; Lo pirmero que hay que hacer es validar el ValidatorDefinition
  ;; tanto estructural como semáticamente. Esto quiere decir que hay que hacer
  ;; una función que bsuque cada campo :compile/field o :compile/with-group y 
  ;; valide si la función a la que está haciendo referencia se puede resolver,
  ;; y si se puede resolver que sustituya el valor de esa referencia por la función
  
  )



(defn process-field [resource field]
  (let [{:keys [path type min max]} field
        ]
    (->> (get-in resource path)
         (validate-type type)
         (validate-cardinality min max)
;; acá debe ejecutar la función que compila
         )
    )
  )

(->> get-in resource (:path field))
