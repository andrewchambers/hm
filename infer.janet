# A type inference engine that adds types to ast
# for a simple C like language.
#
# Original reference:
# https://eli.thegreenplace.net/2018/type-inference/

(import ./unify)

(defn type-equations
  [node &opt eqns]
  (default eqns @[])
  (cond
    (= (node :kind) :let)
    (when-let [expr (node :expr)]
      (array/push eqns [(node :var-type) (expr :type)])
      (type-equations expr eqns))
    (= (node :kind) :while)
    (do
      (array/push eqns [(get-in node [:expr :type]) "bool"])
      (type-equations (node :body) eqns))
    (= (node :kind) :if)
    (let [if-true (node :if-true)]
      (array/push eqns [(get-in node [:expr :type]) "bool"])
      (type-equations if-true eqns)
      (if-let [if-false (node :if-false)]
        (do
          (array/push eqns [(node :type) (if-true :type)])
          (array/push eqns [(node :type) (if-false :type)])
          (type-equations if-false eqns))
        (array/push eqns [(node :type) "void"])))
    (= (node :kind) :block)
    (each stmt (node :exprs)
      (type-equations stmt eqns))
    (= (node :kind) :->)
    (do
      (array/push eqns [(get-in node [:expr :type]) (node :return-type)])
      (type-equations (node :expr) eqns))
    (= (node :kind) :address-of)
    (do
      (array/push eqns [{:kind :ptr :sub-type (get-in node [:expr :type])}  (node :type)])
      (type-equations (node :expr) eqns))
    (= (node :kind) :deref)
    (do
      (array/push eqns [{:kind :ptr :sub-type (get-in node [:expr :type])}  {:kind :ptr :sub-type (node :type)}])
      (type-equations (node :expr) eqns))
    (= (node :kind) :tassert)
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
    (string? t) true
    :default
    (if-let [sub-ty (get t :sub-type)]
      (concrete-type? sub-ty)
      (error "unhandled type case"))))

(defn numeric-type?
  [t]
  (case t
    "char" true
    "int" true
    false))

(defn apply-types
  [solved-types node]
  (tracev node)
  (each k [:type :return-type :var-type]
    (when-let [t (node k)]
      (def new-t (prewalk |(get solved-types $ $) t))
      (put node k new-t)))
  (when-let [sub-expr (node :expr)]
    (apply-types solved-types sub-expr))
  (when-let [body (node :body)]
    (apply-types solved-types body))
  (when-let [statements (node :exprs)]
    (each s statements
      (apply-types solved-types s))))

(defn validate-expr-types
  [node]
  (tracev node)

  (each k [:var-type :type]
    (when-let [t (node k)]
      (unless (concrete-type? t)
        (errorf "type %p not constrained" t))))

  (when (= (node :kind) :number)
    (unless (numeric-type? (node :type))
      (errorf "number resolved to non numeric type - %p" (node :type))))
  
  (when-let [sub-expr (node :expr)]
    (validate-expr-types sub-expr))

  (when-let [body-expr (node :body)]
    (validate-expr-types body-expr))

  (when-let [exprs (node :exprs)]
    (each e exprs
      (validate-expr-types e))))

(defn type-check-expr
  [node]
  (def eqns (tracev (type-equations node)))
  (def solved-types (tracev (solve-type-equations eqns)))
  (apply-types solved-types node)
  (validate-expr-types node)
  node)

(defn type-check-top-levels
  [top-levels]
  (each tl top-levels
    (case (tl :kind)
      :typedef
        nil
      :fn
        (when-let [expr (tl :expr)]
          (type-check-expr expr))
      (errorf "unhandled top level %p" tl))))
