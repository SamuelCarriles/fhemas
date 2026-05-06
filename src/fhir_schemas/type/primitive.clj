(ns fhir-schemas.type.primitive
  (:require [clojure.string :as str]
            [fhir-schemas.util :refer [error-data]])
  (:import [java.net URI]))


(defn valid-uri?
  "Returns true if uri is a valid RFC 3986 URI, else false"
  [^String uri]
  (try
    (URI. uri)
    true
    (catch Exception _
      false)))

(def registry
  "Primitive FHIR Types map"
  {;; string family
   :fhir/string {:kind ::base
                 :validation [{:type :fn :value string? :issue (error-data "error" "structure" "The value must be a string")}
                              {:type :fn :value (complement str/blank?) :issue (error-data "error" "value" "The string is blank")}
                              {:type :fn :value #(<= (count %) 1048576) :issue (error-data "error" "too-long" "The value length is over than 1048576 characters")}]
                 :description "A sequence of Unicode characters"}

   :fhir/code {:kind ::derived
               :based-on :fhir/string
               :validation [{:type :pattern :value #"[^\s]+( [^\s]+)*" :issue (error-data "error" "invalid" "The code format is invalid (no leading/trailing or double spaces allowed)")}]
               :description "A string taken from a set of controlled strings defined elsewhere"}

   :fhir/markdown {:kind ::derived
                   :based-on :fhir/string
                   :description "A FHIR string that may contain markdown syntax in GFM extension of CommonMark format"}

   :fhir/id {:kind ::derived
             :based-on :fhir/string
             :validation [{:type :pattern :value #"[A-Za-z0-9\-\.]{1,64}" :issue (error-data "error" "invalid" "The id does not match the required format (letters, numbers, dots, dashes, max 64 chars)")}]
             :description "Any combination of upper- or lower-case ASCII letters ('A'..'Z', and 'a'..'z', numerals ('0'..'9'), '-' and '.', with a length limit of 64 characters."}

   ;; integer family
   :fhir/integer {:kind ::base
                  :validation [{:type :fn :value integer? :issue (error-data "error" "structure" "The value must be an integer")}
                               {:type :fn :value #(<= -2147483648 % 2147483647) :issue (error-data "error" "value" "The value is outside the 32-bit signed integer range")}]
                  :description "A signed integer in the range −2,147,483,648..2,147,483,647"}

   :fhir/unsigned-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn :value #(<= 0 %) :issue (error-data "error" "value" "The integer must be greater or equal to zero")}]
                       :description "Any non-negative integer in the range 0..2,147,483,647"}

   :fhir/positive-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn :value pos? :issue (error-data "error" "value" "The integer must be a positive value (greater than zero)")}]
                       :description "Any positive integer in the range 1..2,147,483,647"}

   ;; 
   :fhir/uri {:kind ::base
              :validation [{:type :fn :value string? :issue (error-data "error" "structure" "The URI must be a string")}
                           {:type :fn :value (complement str/blank?) :issue (error-data "error" "value" "The URI cannot be empty or consist only of whitespace")}
                           {:type :fn :value valid-uri? :issue (error-data "error" "invalid" "The value is not a valid RFC 3986 URI")}]
              :description "A Uniform Resource Identifier Reference"}})

