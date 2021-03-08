# A type inference engine that adds types to ast
# for a simple C like language.
#
# Original reference:
# https://eli.thegreenplace.net/2018/type-inference/

(import ./unify)

(defn type-equations
  [node &opt eqns]
  (tracev node)
  (tracev eqns)
  (default eqns @[])
  (cond
    (= (node :kind) :block)
    (each stmt (node :stmts)
      (type-equations stmt eqns))
    (= (node :kind) :address-of)
    (do
      (array/push eqns [{:kind :ptr :sub-type (get-in node [:expr :type])}  (node :type)])
      (type-equations (node :expr) eqns))
    (= (node :kind) :deref)
    (do
      (array/push eqns [{:kind :ptr :sub-type (get-in node [:expr :type])}  {:kind :ptr :sub-type (node :type)}])
      (type-equations (node :expr) eqns))
    (index-of (node :kind) [:let :tassert :->])
    (do
      (array/push eqns [(get-in node [:expr :type]) (node :type)])
      (type-equations (node :expr) eqns)))
  eqns)

(defn solve-type-equations
  [eqns]
  (var subst {})
  (each [l r] eqns
    (tracev [l r subst])
    (set subst (unify/unify l r subst))
    (unless subst
      (errorf "type inference failed")))
  (def solutions @{})
  
  # Due to a quirk in our unification code, we must
  # unwrap our solutions from their tuple containers.
  (eachk k subst
    (put solutions k (get-in subst [k 0])))
  
  # substitute any solution chains with the final result.
  (each k (keys solutions)
    (var x (get solutions k))
    (while true
      (def y (get solutions x x))
      (when (= x y)
        (break))
      (set x y))
    (put solutions k x))

  (table/to-struct solutions))

(defn concrete-type?
  [t]
  (cond
    (symbol? t) false
    (keyword? t) true
    :default
    (if-let [sub-ty (get t :sub-type)]
      (concrete-type? sub-ty)
      (error "unhandled type case"))))

(defn numeric-type?
  [t]
  (truthy? (index-of t [:int])))

(defn apply-types
  [solved-types node]
  (tracev node)
  (when-let [t (node :type)]
    (def new-t (prewalk |(get solved-types $ $) t))
    (tracev [t new-t])
    (put node :type new-t))
  (when-let [sub-expr (node :expr)]
    (apply-types solved-types sub-expr))
  (when-let [statements (node :stmts)]
    (each s statements
      (apply-types solved-types s))))

(defn validate-types
  [node]
  (tracev node)
  
  (when-let [t (node :type)]
    (unless (concrete-type? t)
      (errorf "type %p not constrained" t))

    (when (= (node :kind) :number)
      (unless (numeric-type? t)
        (errorf "number resolved to non numeric type - %p" t))))
  
  (when-let [sub-expr (node :expr)]
    (validate-types sub-expr))

  (when-let [statements (node :stmts)]
    (each s statements
      (validate-types s))))

(defn type-check
  [node]
  (def eqns (tracev (type-equations node)))
  (def solved-types (tracev (solve-type-equations eqns)))
  (apply-types solved-types node)
  (validate-types node)
  node)

(defn type-check-top-levels
  [top-levels]
  (each tl top-levels
    (case (tl :kind)
      :typedef
        nil
      :fn
        (when (tl :expr)
          (type-check (tl :expr)))
      (error "unhandled top level"))))

(comment
  (def ast1
    @{:kind :block
      :stmts
      [@{:kind :let
         :ident "i"
         :type '?t0
         :expr @{:kind :tassert
                :expr @{:kind :number
                        :value "0"
                        :type '?t1}
                :type :int}}
        
        @{:kind :deref
          :type '?t3
          :expr
          @{:kind :address-of
            :type '?t2
            :expr @{:kind :ident
                    :type '?t0
                    :ident "i"}}}]}))

# (printf "%.20M" (type-check-ast ast1))
