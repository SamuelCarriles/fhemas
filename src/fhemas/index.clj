(ns fhemas.index
  (:refer-clojure :exclude [resolve])
  (:require
   [fhemas.validator-definition.field.core :refer [get-value]]
   [fhemas.match :as match]
   [fhemas.error :as error]))

(defn create
  "Creates an index entry from a resource. Returns nil when the resource doesn't
  match the :when conditions or when the key/value extraction fails."
  [idx resource]
  (let [{k-path :key v-path :value n :name w :when} idx
        ->value #(-> % (get-value resource) :value)
        k (->value k-path)
        v (if v-path
            (->value v-path)
            resource)]
    (cond
      (or (nil? k) (nil? v)) nil
      (match/satisfies? w resource) {n {k v}}
      :else nil)))

(defmulti insert
  "Inserts an index entry into a page according to its relation."
  (fn [_idx _page relation] relation))

(defmethod insert :default
  [_idx _page relation]
  (throw
   (error/info :invalid/index
               {:message (format "Unsupported index relation %s" (name relation))
                :location 'fhemas.index/insert
                :operation :insert-index
                :details {:relation relation
                          :expected (-> (methods insert) keys vec)}})))

(defmethod insert :1->1
  [idx page _]
  (let [[n kv] (first idx)
        [k v] (first kv)]
    (if-not (get-in page [n k])
      (assoc-in page [n k] v)
      (throw (error/info :invalid/index
                         {:message (format "Duplicate key %s in 1->1 index %s" k n)
                          :location 'fhemas.index/insert
                          :operation :insert-index
                          :details {:index-name n
                                    :key k
                                    :value v}})))))

(defmethod insert :1->*
  [idx page _]
  (let [[n kv] (first idx)
        [k v] (first kv)]
    (update-in page [n k] (fnil conj #{}) v)))

(defn apply-indexes
  "Applies all indexes to a single resource, merging the results into init-page."
  [indexes init-page resource]
  (reduce
   (fn [page idx-data]
     (if-let [index (create idx-data resource)]
       (insert index page (:relation idx-data))
       page))
   init-page
   indexes))

(defn build
  "Builds the complete index page from all resources."
  [indexes resources]
  (reduce #(apply-indexes indexes %1 %2) {} resources))

(defn resolve
  "Resolves a key in the given index."
  [indexes idx-name k]
  (cond
    (not (contains? indexes idx-name))
    (throw (error/info :invalid/index
                       {:message (format "The required index '%s' doesn't exist" idx-name)
                        :location 'fhemas.r4.index/resolve
                        :operation :resolve-index
                        :details {:index idx-name
                                  :expected (keys indexes)}}))

    (not (contains? (get indexes idx-name) k))
    (throw (error/info :invalid/index
                       {:message (format "Unavailable key '%s' in index %s" k idx-name)
                        :location 'fhemas.r4.index/resolve
                        :operation :resolve-index-key
                        :details {:index-key k
                                  :expected (keys (get indexes idx-name))}}))

    :else
    (get-in indexes [idx-name k])))
