(ns fhemas.predicate
  "Predicate evaluation engine.
   
   Compiles declarative predicate structures into efficient checker functions.
   A predicate is a collection of clauses (OR), where each clause is a map of 
   path→value conditions (AND). Values that are sets use `contains?` semantics; 
   all other values use equality.")

(defn normalize
  "Normalizes a predicate into a vector of clauses with vector paths.

   Wraps a single clause map into a vector. Converts keyword paths into 
   single-element vectors so all paths can be resolved uniformly with `get-in`."
  [pred]
  (reduce
   (fn [acc curr]
     (conj acc (update-keys curr #(if (vector? %) % [%]))))
   []
   (if (map? pred)
     [pred]
     pred)))

(defn ->and
  "Compiles a single clause into a predicate function.
   
   Each path→value entry becomes a check: `contains?` if the value is a set, 
   `=` otherwise. All checks are combined with `every-pred` (AND logic)."
  [clausule]
  (if-not (seq clausule)
    (constantly true)

    (->> clausule
         (reduce-kv
          (fn [acc k v]
            (let [op (if (set? v) contains? =)]
              (conj acc #(op v (get-in % k)))))
          [])
         (apply every-pred))))

(defn ->checker
  "Compiles a normalized predicate into a reusable checker function.
   
   Clauses are compiled once via `->and` and combined with OR logic.
   Returns a closure predicate. An empty predicate matches everything."
  [pred]
  (if-not (seq pred)
    (constantly true)

    (let [clausules (map ->and pred)]
      (fn [data]
        (boolean (some #(true? (% data)) clausules))))))


