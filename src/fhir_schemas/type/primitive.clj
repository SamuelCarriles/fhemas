(ns fhir-schemas.type.primitive
  (:require [clojure.string :as str]
            [fhir-schemas.util :refer [error-data]])
  (:import [java.net URI]
           [java.util Base64]))

(defn not-blank-str?
  "Returns true if `s` is a non-blank string, else false"
  [^String s]
  (and (string? s)
       (not (str/blank? s))))

(defn int32?
  "Returns true if `n` is a 32 bits integer"
  [^Integer n]
  (and (integer? n)
       (<= Integer/MIN_VALUE n Integer/MAX_VALUE)))

(defn int64?
  "Returns true if `n` is a 64 bits integer"
  [^Long n]
  (and (integer? n)
       (<= Long/MIN_VALUE n Long/MAX_VALUE)))

(defn uint32?
  "Returns true if `n` is a unsigned 32 bits integer"
  [^Integer n]
  (and (int32? n)
       (nat-int? n)))

(defn pos-int32?
  "Returns true if `n` is a positive 32 bits integer"
  [^Integer n]
  (and (int32? n)
       (pos-int? n)))

(defn fhir-sized-str?
  "Returns true if `s` is under FHIR string limit, else false"
  [^String s]
  (and (string? s)
       (<= (count s) 1048576)))

(defn fhir-uri?
  "Returns true if uri is a valid RFC 3986 URI, else false"
  [^String uri]
  (try
    (URI. uri)
    true
    (catch Exception _
      false)))

(defn fhir-url?
  "Returns true if url is a valid RFC 1738 URL, else false"
  [^String url]
  (try
    (.toURL (URI. url))
    true
    (catch Exception _
      false)))

(defn fhir-canonical?
  "Returns true if `canonical` is a valid FHIR Canonical URL, else false"
  [^String canonical]
  (let [parts (str/split canonical #"\|")]
    (if (< 2 (count parts))
      false
      (let [base-uri (first parts)]
        (if (str/starts-with? base-uri "#")
          true
          (try (.isAbsolute (URI. base-uri))
               (catch Exception _ false)))))))

(defn fhir-base64?
  "Returns true if `s` is a base64 encoded string"
  [^String s]
  (try
    (.decode (Base64/getMimeDecoder) s)
    true
    (catch Exception _
      false)))

(def registry
  "A map containing the definitions for FHIR primitive types. 
    Each entry specifies the kind of type (base or derived), its parent, 
    and a sequence of validation steps.
    Example:
   
      {:fhir/url 
       {:kind ::derived
        :based-on :fhir/uri
        :validation [{:type :fn 
                      :value fhir-url? 
                      :issue (error-data \"error\" \"invalid\" \"...\")}]
        :description \"A Uniform Resource Locator\"}}"
  {;; String family
   :fhir/string {:kind ::base
                 :validation [{:type :fn
                               :value string?
                               :issue (error-data "error" "structure" "The value must be a string")}

                              {:type :fn
                               :value not-blank-str?
                               :issue (error-data "error" "value" "The string is blank")}

                              {:type :fn
                               :value fhir-sized-str?
                               :issue (error-data "error" "too-long" "The value length is over than 1048576 characters")}]
                 :description "A sequence of Unicode characters"}

   :fhir/code {:kind ::derived
               :based-on :fhir/string
               :validation [{:type :pattern
                             :value #"[^\s]+( [^\s]+)*"
                             :issue (error-data "error" "invalid" "The code format is invalid (no leading/trailing or double spaces allowed)")}]
               :description "A string taken from a set of controlled strings defined elsewhere"}

   :fhir/markdown {:kind ::derived
                   :based-on :fhir/string
                   :description "A FHIR string that may contain markdown syntax in GFM extension of CommonMark format"}

   :fhir/id {:kind ::derived
             :based-on :fhir/string
             :validation [{:type :pattern
                           :value #"[A-Za-z0-9\-\.]{1,64}"
                           :issue (error-data "error" "invalid" "The id does not match the required format (letters, numbers, dots, dashes, max 64 chars)")}]
             :description "Any combination of upper- or lower-case ASCII letters ('A'..'Z', and 'a'..'z', numerals ('0'..'9'), '-' and '.', with a length limit of 64 characters."}

   ;; Integer family
   :fhir/integer {:kind ::base
                  :validation [{:type :fn
                                :value integer?
                                :issue (error-data "error" "structure" "The value must be a 32 bits integer")}

                               {:type :fn
                                :value int32?
                                :issue (error-data "error" "value" "The value is outside the 32-bit signed integer range")}]
                  :description "A signed integer in the range −2,147,483,648..2,147,483,647"}

   :fhir/unsigned-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn
                                     :value uint32?
                                     :issue (error-data "error" "value" "The integer must be greater or equal to zero")}]
                       :description "Any non-negative integer in the range 0..2,147,483,647"}

   :fhir/positive-int {:kind ::derived
                       :based-on :fhir/integer
                       :validation [{:type :fn
                                     :value pos-int32?
                                     :issue (error-data "error" "value" "The integer must be a positive value (greater than zero)")}]
                       :description "Any positive integer in the range 1..2,147,483,647"}

   :fhir/integer64 {:kind ::base
                    :validation [{:type :fn 
                                  :value integer?
                                  :issue (error-data "error" "structure" "The value must be a 64 bits integer")}
                                 {:type :fn
                                  :value int64?
                                  :issue (error-data "error" "value" "The value is outside the 64-bit signed integer range")}]
                    :description "A signed integer in the range -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807 (64-bit)"}

   ;; URI family
   :fhir/uri {:kind ::base
              :validation [{:type :fn
                            :value string?
                            :issue (error-data "error" "structure" "The URI must be a string")}

                           {:type :fn
                            :value not-blank-str?
                            :issue (error-data "error" "value" "The URI cannot be empty or consist only of whitespace")}

                           {:type :fn
                            :value fhir-uri?
                            :issue (error-data "error" "invalid" "The value is not a valid RFC 3986 URI")}]
              :description "A Uniform Resource Identifier Reference"}

   :fhir/url {:kind ::derived
              :based-on :fhir/uri
              :validation [{:type :fn
                            :value fhir-url?
                            :issue (error-data "error" "invalid" "The value is not a valid RFC 1738 URL")}]
              :description "A Uniform Resource Locator"}

   :fhir/canonical {:kind ::derived
                    :based-on :fhir/uri
                    :exclude [fhir-uri?]
                    :validation [{:type :fn
                                  :value fhir-canonical?
                                  :issue (error-data "error" "invalid" "The value is not a valid canonical URI (must be absolute or a fragment, allowing a single '|' for versioning)")}]
                    :description "A URI that refers to a resource by its canonical URL"}

   :fhir/uuid {:kind ::derived
               :based-on :fhir/uri
               :validation [{:type :pattern
                             :value #"urn:uuid:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                             :issue (error-data "error" "invalid" "The value is not a valid UUID URN (must have the prefix 'urn:uuid:' followed by lowercase hexadecimal characters)")}]
               :description "A UUID represented as a URI; e.g., urn:uuid:c757873d-ec9a-4326-a141-556f43239520"}

   :fhir/oid {:kind ::derived
              :based-on :fhir/uri
              :validation [{:type :pattern
                            :value #"urn:oid:[0-2](\.(0|[1-9][0-9]*))+"
                            :issue (error-data "error" "invalid" "The value is not a valid OID URN (must have the prefix 'urn:oid:' followed by dot-separated numbers)")}]}

   ;;
   :fhir/boolean {:kind ::base
                  :validation [{:type :fn
                                :value boolean?
                                :issue (error-data "error" "structure" "The value must be a boolean")}]
                  :description "'true' or 'false'"}
   
  ;; base64Binary 
  :fhir/base64 {:kind ::base
                 :validation [{:type :fn
                               :value string?
                               :issue (error-data "error" "structure" "The value must be a string")}
                              
                              {:type :fn
                               :value not-blank-str?
                               :issue (error-data "error" "value" "The base64 content cannot be empty")}

                              {:type :fn
                               :value fhir-base64?
                               :issue (error-data "error" "invalid" "The value is not valid Base64 encoded data (RFC 4648)")}]
                 :description "A stream of bytes, base64 encoded (RFC 4648)"}

   ;; TODO: add :fhir/date-time
   ;; TODO: add :fhir/date
   ;; TODO: add :fhir/time
   ;; TODO: add :fhir/instant
   ;; TODO: add :fhir/decimal
   ;; TODO: add :fhir/integer64 
   })

(comment
  ;; For cases like 'canonical', where a child type violates a parent constraint,
  ;; we can bypass that constraint by adding it to the :exclude vector.
  ;; Supports either named functions or regular expressions. 
  :.)