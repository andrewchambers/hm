# Helper functions for dealing with types. 

# wrapped in @{} to make them comparable by identity.
(def builtins {
  "void" @{:name "void"}
  "char" @{:name "char"}
  "int" @{:name "int"}
  "bool" "char"
})

(var- sort-visit nil)

(defn- sort-walk
  [type-tab visited order t]
  (cond
    (string? t)
    (sort-visit type-tab visited order t)
    (tuple? t)
    (each subt t
      (sort-walk type-tab visited order subt))
    (struct? t)
    (do
      (when-let [fields (get t :fields)]
        (each [_ subt] fields
          (sort-walk type-tab visited order subt))) 
      (each k [:sub-type :return-type] 
        (when-let [subt (get t k)]
          (sort-walk type-tab visited order subt))))))

(varfn sort-visit
  [type-tab visited order tname]
  (unless (in visited tname)
    (put visited tname true)
    (def t (get type-tab tname))
    (sort-walk type-tab visited order t)
    (array/push order tname)))

(defn sort
  [type-tab]
  (def visited @{})
  (def order @[])
  (eachk tname type-tab
    (sort-visit type-tab visited order tname))
  order)
