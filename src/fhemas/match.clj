(ns fhemas.match
  (:refer-clojure :exclude [satisfies?]))

(defn submap?
  "Returns true when m1 is a submap of m2. Returns false when either m1 or m2 is not a map."
  [m1 m2]
  (and (map? m1)
       (map? m2)
       (= m1 (select-keys m2 (keys m1)))))

(defn satisfies?
  "Returns true when the resource matches any of the conditions (OR logic).
  Returns true when conds is empty or nil."
  [conds resource]
  (if (seq conds)
    (boolean (some #(submap? % resource) conds))
    true))
