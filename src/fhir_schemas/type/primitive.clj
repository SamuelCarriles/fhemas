(ns fhir-schemas.type.primitive)

(def registry
  {;; string family
   :fhir/string {:kind ::base
                 :validation [{:type :pattern :value #"[\s\S]+"}
                              {:type :fn :value #(<= (count %) 1048576)}]
                 :description "A sequence of Unicode characters"}

   :fhir/code {:kind ::derived
               :based-on :fhir/string
               :validation [{:type :pattern :value #"[^\s]+( [^\s]+)*"}]
               :description "A string taken from a set of controlled strings defined elsewhere"}

   :fhir/markdown {:kind ::derived
                   :based-on :fhir/string
                   :validation [{:type :pattern :value #"[\s\S]+"}]
                   :description "A FHIR string that may contain markdown syntax in GFM extension of CommonMark format"}

   :fhir/id {:kind ::derived
             :based-on :fhir/string
             :validation [{:type :pattern :value #"[A-Za-z0-9\-\.]{1,64}"}]
             :description "Any combination of upper- or lower-case ASCII letters ('A'..'Z', and 'a'..'z', numerals ('0'..'9'), '-' and '.', with a length limit of 64 characters."}

   ;; integer family
   :fhir/integer {:kind ::base
                  :validation [{:type :fn :value #(<= -2147483648 % 2147483647)}]
                  :description "A signed integer in the range −2,147,483,648..2,147,483,647"}

   :fhir/unsigned-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn :value #(<= 0 % 2147483647)}]
                       :description "Any non-negative integer in the range 0..2,147,483,647"}
   
   :fhir/positive-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn :value #(<= 1 % 2147483647)}]
                       :description "Any positive integer in the range 1..2,147,483,647"}})

(comment
  
  (defn primitive?
  [^clojure.lang.Keyword type]
  ())

registry
  
  :.)

