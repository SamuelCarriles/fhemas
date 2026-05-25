(ns fhir-schemas.type.primitive
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :as csk])
  (:import [java.net URI]
           [java.util Base64]
           [java.time Year YearMonth LocalDate OffsetDateTime]))

(defn error-data
  "Returns a FHIR-compliant OperationOutcome issue map with the given severity, code, and diagnostics message"
  [^String severity ^String code ^String message]
  {:severity severity
   :code code
   :diagnostics message})

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

(defn fhir-date?
  "Returns true if `s` is a real/existing FHIR date"
  [^String s]
  (and (string? s)
       (try
         (case (count s)
           4 (do (Year/parse s) true)
           7 (do (YearMonth/parse s) true)
           10 (do (LocalDate/parse s) true)
           false)
         (catch Exception _
           false))))

(defn fhir-instant?
  "Returns true if `s` is a valid FHIR instant (full date + time + timezone)"
  [^String s]
  (and (string? s)
       (try
         (OffsetDateTime/parse s)
         true
         (catch Exception _
           false))))

(defn fhir-date-time?
  "Returns true if `s` is a real/existing FHIR dateTime"
  [^String s]
  (and (string? s)
       (let [[date time] (str/split s #"T" 2)]
         (if time
           (fhir-instant? s)
           (fhir-date? date)))))

(def fhir-regex
  "A map of official FHIR regular expressions for primitive type validation.
  Keys represent the type or format, and values are the compiled regex patterns"
  {:code  #"[^\s]+( [^\s]+)*"

   :id #"[A-Za-z0-9\-\.]{1,64}"

   :uuid #"urn:uuid:[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"

   :oid #"urn:oid:[0-2](\.(0|[1-9][0-9]*))+"

   :decimal #"-?(0|[1-9][0-9]{0,17})(\.[0-9]{1,17})?([eE][+-]?[0-9]{1,9})?"

   :base-64-binary #"(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?"

   :date-time #"([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]{1,9})?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)?)?)?)?)?"

   :date #"([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1]))?)?"

   :time #"([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]{1,9})?"

   :instant #"([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]{1,9})?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))"})

(def type->definition
  "A map containing the definitions for FHIR primitive types. 
    Each entry specifies the kind of type (base or derived), its parent, 
    and a sequence of validation steps.
    Example:
   
      {:fhir-schemas.type.primitive/url
       {:kind ::derived
        :based-on :fhir-schemas.type.primitive/uri
        :validation [{:type :fn 
                      :value fhir-url? 
                      :issue (error-data \"error\" \"invalid\" \"...\")}]
        :description \"A Uniform Resource Locator\"}}"
  {;; String family
   ::string {:kind ::base
             :validation [{:type :fn
                           :value string?
                           :issue (error-data "error" "structure" "The value must be a string")}

                          {:type :fn
                           :value not-blank-str?
                           :issue (error-data "error" "value" "The string is blank")}

                          {:type :fn
                           :value fhir-sized-str?
                           :issue (error-data "error" "too-long" "The value exceeds the maximum length of 1048576 characters")}]
             :description "A sequence of Unicode characters"}

   ::code {:kind ::derived
           :based-on ::string
           :validation [{:type :pattern
                         :value (:code fhir-regex)
                         :issue (error-data "error" "invalid" "The code format is invalid (no leading/trailing or double spaces allowed)")}]
           :description "A string taken from a set of controlled strings defined elsewhere"}

   ::markdown {:kind ::derived
               :based-on ::string
               :description "A FHIR string that may contain markdown syntax in GFM extension of CommonMark format"}

   ::id {:kind ::derived
         :based-on ::string
         :validation [{:type :pattern
                       :value (:id fhir-regex)
                       :issue (error-data "error" "invalid" "The id does not match the required format (letters, numbers, dots, dashes, max 64 chars)")}]
         :description "Any combination of upper- or lower-case ASCII letters ('A'..'Z', and 'a'..'z', numerals ('0'..'9'), '-' and '.', with a length limit of 64 characters."}

   ;; Integer family
   ::integer {:kind ::base
              :validation [{:type :fn
                            :value integer?
                            :issue (error-data "error" "structure" "The value must be a 32 bits integer")}

                           {:type :fn
                            :value int32?
                            :issue (error-data "error" "value" "The value is outside the 32-bit signed integer range")}]
              :description "A signed integer in the range −2,147,483,648..2,147,483,647"}

   ::unsigned-int {:kind ::derived
                   :based-on ::integer
                   :validation [{:type :fn
                                 :value uint32?
                                 :issue (error-data "error" "value" "The integer must be greater or equal to zero")}]
                   :description "Any non-negative integer in the range 0..2,147,483,647"}

   ::positive-int {:kind ::derived
                   :based-on ::integer
                   :validation [{:type :fn
                                 :value pos-int32?
                                 :issue (error-data "error" "value" "The integer must be a positive value (greater than zero)")}]
                   :description "Any positive integer in the range 1..2,147,483,647"}

   ::integer-64 {:kind ::base
                 :validation [{:type :fn
                               :value integer?
                               :issue (error-data "error" "structure" "The value must be a 64 bits integer")}
                              {:type :fn
                               :value int64?
                               :issue (error-data "error" "value" "The value is outside the 64-bit signed integer range")}]
                 :description "A signed integer in the range -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807 (64-bit)"}

   ;; URI family
   ::uri {:kind ::base
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

   ::url {:kind ::derived
          :based-on ::uri
          :validation [{:type :fn
                        :value fhir-url?
                        :issue (error-data "error" "invalid" "The value is not a valid RFC 1738 URL")}]
          :description "A Uniform Resource Locator"}

   ::canonical {:kind ::derived
                :based-on ::uri
                :exclude [fhir-uri?]
                :validation [{:type :fn
                              :value fhir-canonical?
                              :issue (error-data "error" "invalid" "The value is not a valid canonical URI (must be absolute or a fragment, allowing a single '|' for versioning)")}]
                :description "A URI that refers to a resource by its canonical URL"}

   ::uuid {:kind ::derived
           :based-on ::uri
           :validation [{:type :pattern
                         :value (:uuid fhir-regex)
                         :issue (error-data "error" "invalid" "The value is not a valid UUID URN (must have the prefix 'urn:uuid:' followed by lowercase hexadecimal characters)")}]
           :description "A UUID represented as a URI; e.g., urn:uuid:c757873d-ec9a-4326-a141-556f43239520"}

   ::oid {:kind ::derived
          :based-on ::uri
          :validation [{:type :pattern
                        :value (:oid fhir-regex)
                        :issue (error-data "error" "invalid" "The value is not a valid OID URN (must have the prefix 'urn:oid:' followed by dot-separated numbers)")}]
          :description "An OID represented as a URI; e.g., urn:oid:1.2.3.4.5"}

   ;;others
   ::boolean {:kind ::base
              :validation [{:type :fn
                            :value boolean?
                            :issue (error-data "error" "structure" "The value must be a boolean")}]
              :description "'true' or 'false'"}

   ::base-64-binary {:kind ::base
                     :validation [{:type :fn
                                   :value string?
                                   :issue (error-data "error" "structure" "The value must be a string")}

                                  {:type :fn
                                   :value not-blank-str?
                                   :issue (error-data "error" "value" "The base64 content cannot be empty")}

                                  {:type :pattern
                                   :value (:base-64-binary fhir-regex)
                                   :issue (error-data "error" "invalid" "The value contains invalid Base64 characters")}

                                  {:type :fn
                                   :value fhir-base64?
                                   :issue (error-data "error" "value" "The value is not valid Base64 encoded data (RFC 4648)")}]
                     :description "A stream of bytes, base64 encoded (RFC 4648)"}

   ::decimal {:kind ::base
              :validation [{:type :fn
                            :value number?
                            :issue (error-data "error" "structure" "The value must be a number")}

                           {:type :pattern
                            :value (:decimal fhir-regex)
                            :issue (error-data "error" "value" "The decimal does not meet FHIR format (max 18 digits)")}]
              :description "Rational numbers that have a decimal representation, max 18 digits"}

   ;; Date and Time
   ::date-time {:kind ::base
                :validation [{:type :fn
                              :value string?
                              :issue (error-data "error" "structure" "The value must be a date-time-string")}

                             {:type :pattern
                              :value (:date-time fhir-regex)
                              :issue (error-data "error" "invalid" "The value does not match the FHIR dateTime format")}

                             {:type :fn
                              :value fhir-date-time?
                              :issue (error-data "error" "value" "The value is not a valid date")}]
                :description "A date, date-time or partial date as used in human communication"}

   ::date {:kind ::base
           :validation [{:type :fn
                         :value string?
                         :issue (error-data "error" "structure" "The value must be a date-string")}

                        {:type :pattern
                         :value (:date fhir-regex)
                         :issue (error-data "error" "invalid" "The value does not match the FHIR date format")}

                        {:type :fn
                         :value fhir-date?
                         :issue (error-data "error" "value" "The value is not a valid date")}]
           :description "A date or partial date (year, year-month, or full date)"}

   ::time {:kind ::base
           :validation [{:type :fn
                         :value string?
                         :issue (error-data "error" "structure" "The value must be a time-string")}

                        {:type :pattern
                         :value (:time fhir-regex)
                         :issue (error-data "error" "invalid" "The value does not match the FHIR time format (hh:mm:ss)")}]
           :description "A time during the day, with no date specified (hh:mm:ss)"}

   ::instant {:kind ::base
              :validation [{:type :fn
                            :value string?
                            :issue (error-data "error" "structure" "The value must be a string")}

                           {:type :pattern
                            :value (:instant fhir-regex)
                            :issue (error-data "error" "invalid" "The value does not match the FHIR instant format")}

                           {:type :fn
                            :value fhir-instant?
                            :issue (error-data "error" "value" "The value is not a valid instant")}]
              :description "An instant in time, with mandatory full date, time and timezone offset"}})

(defn primitive?
  "Returns true if s is a FHIR primitive type name. Accepts string or keyword."
  [x]
  (when-let [name-str (cond
                        (keyword? x) (name x)
                        (string? x) x
                        :else nil)]
    (->> name-str
         csk/->kebab-case
         (keyword "fhir-schemas.type.primitive")
         (contains? type->definition))))

(comment
  ;; For cases like 'canonical', where a child type violates a parent constraint,
  ;; we can bypass that constraint by adding it to the :exclude vector.
  ;; Supports either named functions or regular expressions.  
  :.)
