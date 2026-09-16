(ns fhemas.json
  "High-performance, streaming JSON extraction utilities.
   
   This namespace provides tools to extract specific fields from large JSON files
   without loading the entire document into memory. It uses Jackson's streaming 
   API (`JsonParser`) to skip irrelevant data, making it highly memory-efficient 
   and fast for large FHIR resources."
  (:require
   [camel-snake-kebab.core :as csk]
   [jsonista.core :as jsonista]
   [fhemas.error :as error])
  (:import
   [com.fasterxml.jackson.core JsonFactory JsonParser JsonToken]
   [com.fasterxml.jackson.databind ObjectMapper]
   [java.io File]))

(defn enrich-path
  "Transforms a sequence of path keys into a nested map structure representing 
   an extraction route.
   
   Takes a `path` (either a single keyword or a vector of keywords) and an 
   `encode-key-fn` (e.g., to convert keywords to camelCase strings). It reverses 
   the path and reduces it into a nested map, marking the leaf node with the 
   keyword `:$` to indicate a capture point.
   
   Example:
   (enrich-path [:meta :last-updated] key-encoder)
   ;; => {\"meta\" {\"lastUpdated\" :$}}"
  [path encode-key-fn]
  (let [path (if (vector? path)
               path
               (vector path))]

    (->> path
         reverse
         (reduce
          (fn [acc k] {(encode-key-fn k) acc})
          :$))))

(defn deep-merge
  "Recursively merges multiple maps."
  [& maps]
  (apply merge-with
         (fn [a b]
           (if (and (map? a) (map? b))
             (deep-merge a b)
             b))
         maps))

(defn key-encoder
  "Encodes a Clojure keyword into a camelCase string."
  [k]
  (-> k name csk/->camelCase))

(defn ->extraction-route
  "Constructs a single extraction route from a collection of paths.
   
   Takes a collection of `paths` (e.g., `[:resource-type [:meta :last-updated]]`), 
   encodes each key using `key-encoder`, enriches them with the `:$` marker, and 
   deep-merges them into a single nested map. This route is consumed by `extract` 
   to make O(1) decisions on whether to skip, capture, or descend into a JSON field."
  [paths]
  (apply deep-merge (map #(enrich-path % key-encoder) paths)))

(defn normalize-extraction-result
  "Post-processes the raw extraction result map into an idiomatic Clojure format."
  [m]
  (reduce-kv
   (fn [acc k v]
     (let [path (mapv csk/->kebab-case-keyword k)]
       (assoc acc
              (if (= 1 (count path))
                (first path)
                path)
              v)))
   {}
   m))

;;

(defonce ^ObjectMapper
  mapper
  (jsonista/object-mapper
   {:decode-key-fn csk/->kebab-case-keyword
    :encode-key-fn (comp csk/->camelCase name)
    :bigdecimals true}))

(defonce ^:private ^JsonFactory factory
  (JsonFactory.))

(defn ->parser
  "Creates and configures a streaming `JsonParser` for the given file.
   Attaches the shared `mapper` as the codec. 
   **Note:** The returned parser must be used within a `with-open` block to 
   ensure the underlying file stream is properly closed."
  ^JsonParser
  [^File file]
  (doto (.createParser factory file)
    (.setCodec mapper)))

(defn next-token
  "Advances the parser to the next token and returns the `JsonToken` enum value."
  [^JsonParser parser]
  (.nextToken parser))

(defn skip-children
  "Skips the contents of the current token. 
   
   If the parser is on `START_OBJECT` or `START_ARRAY`, it fast-forwards to the 
   matching end token. For primitive values, this is a no-op.
   
   **Note:** This only mutates the parser's internal state. It does *not* return 
   or advance to the next `JsonToken`. A subsequent `next-token` call is strictly 
   required to proceed to the token following the skipped structure."
  [^JsonParser parser]
  (.skipChildren parser))

(defn field-name
  "Returns the name of the current field (key) as a string."
  [^JsonParser parser]
  (.getCurrentName parser))

(defn read-value
  "Reads the current token and all its descendants as a complete Clojure data structure.
   Uses the configured `mapper` to deserialize the value. This is used to capture 
   entire objects or arrays in a single operation once the parser is positioned 
   on their starting token."
  [^JsonParser parser]
  (.readValueAs parser Object))

(defn extract
  "Extracts specific fields from a JSON file using a high-performance streaming approach.
   Takes a `file` and a collection of `paths` to extract. Instead of loading the 
   entire JSON into memory, it streams the file token by token. 
   
   Behavior based on the extraction route:
   - If a field is not in the route, it is efficiently skipped (`skip-children`).
   - If a field is marked for capture (`:$`), its entire value (object, array, or primitive) 
     is read and added to the result.
   - If a field has nested paths in the route, the parser descends into it.
   
   **Important Caveat:** Attempting to define a path that descends *into* an array 
   (e.g., `[:items :name]` where `:items` is an array) will throw an 
   `:path/invalid-array-access` error. To capture an entire array, the path must 
   point to the array itself (e.g., `[:items]`).
   
   Returns a map with kebab-case keywords, normalized by `normalize-extraction-result`."

  [^File file paths]
  (let [route (->extraction-route paths)]
    (with-open [parser (->parser file)]
      (letfn [(advance [] (next-token parser))
              (skip []
                (advance)
                (skip-children parser)
                (advance))
              (->value [] (read-value parser))]
        (loop [curr-path [] curr-token (advance) result {}]
          (condp  = curr-token
            nil (normalize-extraction-result result)
            JsonToken/START_OBJECT (recur curr-path (advance) result)
            JsonToken/END_OBJECT (recur (if (seq curr-path) (pop curr-path) curr-path) (advance) result)
            JsonToken/START_ARRAY (throw (error/info :path/invalid-array-access
                                                     {:message "Cannot descend into array: arrays are not supported in paths yet"
                                                      :location 'fhemas.json/extract
                                                      :operation :extract-field-from-json-string
                                                      :path curr-path}))

            JsonToken/FIELD_NAME (let [field (field-name parser)
                                       new-path (conj curr-path field)
                                       data (get-in route new-path)]

                                   (cond
                                     (nil? data) (recur curr-path (skip) result)
                                     (= :$ data) (do
                                                   (advance)
                                                   (let [new-result (assoc result new-path (->value))]
                                                     (recur curr-path (advance) new-result)))
                                     (map? data) (recur new-path (advance) result)))))))))
