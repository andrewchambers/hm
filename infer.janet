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
      (type-equations (node :expr) eqns)
      (type-equations (node :body) eqns))
    (= (node :kind) :if)
    (let [if-true (node :if-true)]
      (type-equations (node :expr) eqns)
      (type-equations if-true eqns)
      (array/push eqns [(get-in node [:expr :type]) "bool"])
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
    (= (node :kind) :&)
    (do
      (array/push eqns [{:kind :ptr :sub-type (get-in node [:expr :type])}  (node :type)])
      (type-equations (node :expr) eqns))
    (= (node :kind) :*)
    (do
      (array/push eqns [(get-in node [:expr :type])  {:kind :ptr :sub-type (node :type)}])
      (type-equations (node :expr) eqns))
    (= (node :kind) :type-assert)
    (do
      (array/push eqns [(get-in node [:expr :type]) (node :type)])
      (type-equations (node :expr) eqns))
    (= (node :kind) :call)
    (do
      (array/push eqns [{:kind :fn
                         :return-type (node :type)
                         :param-types (tuple ;(map |(get $ :type) (node :params)))}
                        (get-in node [:expr :type])])
      (type-equations (node :expr) eqns)
      (each p (node :params)
        (type-equations p eqns)))
    (= (node :kind) :binop)
    (do
      (array/push eqns [(get-in node [:left-expr :type]) (node :type)])
      (array/push eqns [(get-in node [:right-expr :type]) (node :type)])
      (type-equations (node :left-expr) eqns)
      (type-equations (node :right-expr) eqns)))
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
  (tracev t)
  (cond
    (symbol? t) false
    (string? t) true
    :default
    (case (get t :kind)
      :ptr
      (concrete-type? (t :sub-type))
      :fn
      (and (concrete-type? (t :return-type))
           (all concrete-type? (t :param-types)))
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

  (each k [:expr :body :if-true :if-false :left-expr :right-expr]
    (when-let [sub-expr (node k)]
      (apply-types solved-types sub-expr)))

  (each k [:exprs :params]
    (when-let [exprs (node k)]
      (each e exprs
        (apply-types solved-types e)))))

(defn validate-expr-types
  [node]
  (unless (get node :no-return)
    (each k [:var-type :type]
      (when-let [t (node k)]
        (unless (concrete-type? t)
          (errorf "type %p not constrained" t)))))

  (when (= (node :kind) :number)
    (unless (numeric-type? (node :type))
      (errorf "number resolved to non numeric type - %p" (node :type))))
  
  (each k [:expr :body :if-true :if-false :left-expr :right-expr]
    (when-let [sub-expr (node k)]
      (validate-expr-types sub-expr)))

  (each k [:exprs :params]
    (when-let [exprs (node k)]
      (each e exprs
        (validate-expr-types e)))))

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
