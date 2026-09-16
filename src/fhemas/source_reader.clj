(ns fhemas.source-reader)

(comment

  ;; Versión con operadores keyword y vectores
  {:scanner
   {:from :dir
    :path "resources/fhir-pkg"
    :select [:resource-type :url]
    :when [{:resource-type #{"StructureDefinition" "CodeSystem" "ValueSet"}}
           {:kind "logical"}]}}

  :.)
