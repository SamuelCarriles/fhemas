(ns fhemas.index
  (:require
   [fhemas.validator-definition.field.core :refer [get-value]]
   [fhemas.error :as error]))

(defn submap?
  "Returns true when m1 is a submap of m2. Returns false when either m1 or m2 is not a map."
  [m1 m2]
  (and (map? m1)
       (map? m2)
       (= m1 (select-keys m2 (keys m1)))))

(defn valid?
  "Returns true when the resource matches any of the conditions (OR logic).
  Returns true when conds is empty or nil."
  [conds resource]
  (if (seq conds)
    (boolean (some #(submap? % resource) conds))
    true))

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
      (valid? w resource) {n {k v}}
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
    (update-in page [n k] (fnil conj []) v)))

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



