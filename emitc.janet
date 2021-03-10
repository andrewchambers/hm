(import ./types)

(defn emit-type
  [t &opt name accum]
  (default accum @[])

  (defn format-accumulated
    [name accum]
    (var s name)
    (while (not (empty? accum))
      (match (array/pop accum)
        :ptr
        (set s (string "(*" s ")"))
        [:array size]
        (set s (string "(" s "[" size "])"))))
    s)

  (cond
    (string? t)
    (do 
      (prin t " ")
      (prin (format-accumulated name accum)))
    (struct? t)
    (case (t :kind)
      :ptr
      (do
        (array/push accum :ptr)
        (emit-type (t :sub-type) name accum))
      :array
      (do
        (array/push accum [:array (t :size)])
        (emit-type (t :sub-type) name accum))
      :struct
      (do
        (prin "struct {")
        (each [field-name field-type] (t :fields)
          (emit-type field-type field-name)
          (prin "; "))
        (prin "} ")
        (prin (format-accumulated name accum))))))

(defn emit-types
  [type-tab]
  (each type-name (types/sort type-tab)
    (def t (type-tab type-name))
    (if (table? t) # Don't emit primitive types.
      (unless (= (t :c-repr) type-name)
        (printf "typedef %s %s;" (t :c-repr) type-name))
      (do
        (prin "typedef ")
        (emit-type t type-name)
        (prin ";\n")))))

(defn emit-decl
  [decl]
  (def decl-name (decl :name))
  (def decl-type (decl :type))
  (case (decl-type :kind)
    :fn
    (do
      (tracev decl-name)
      (emit-type (decl-type :return-type) decl-name)
      (prin "(")
      (if (empty? (decl-type :param-types))
        (prin "void")
        (eachk i (decl-type :param-types)
          (unless (zero? i)
            (prin ", "))
          (emit-type (in (decl-type :param-types) i))))
      (print ");"))))

(defn emit-decls
  [global-scope]
  (each name (sort (keys global-scope))
    (emit-decl (global-scope name))))

(defn emit-expr
  [e]
  (case (e :kind)
    :type-assert
    (emit-expr (e :expr))
    :type-cast
    (do
      (prin "((")
      (emit-type (e :type))
      (prin ")(")
      (emit-expr (e :expr))
      (prin "))"))
    :let
    (do
      (emit-type (e :var-type) (e :name))
      (prin " ")
      (when-let [init-expr (e :expr)]
        (prin "=")
        (emit-expr init-expr)))
    :block
    (do
      (prin "({")
      (each e (e :exprs)
        (emit-expr e)
        (prin ";"))
      (prin "})"))
    :ident
    (prin (e :name))
    :&
    (do
      (prin "&")
      (emit-expr (e :expr)))
    :*
    (do
      (prin "*")
      (emit-expr (e :expr)))
    :index
    (do
      (emit-expr (e :expr))
      (prin "[")
      (emit-expr (e :index-expr))
      (prin "]"))
    :number
    (do
      (prin "(")
      (prin "(")
      (emit-type (e :type))
      (prin ")")
      (prin (e :value))
      (prin ")"))
    :string-literal
    (do
      (prin "(")
      (prin "(")
      (emit-type (e :type))
      (prin ")")
      (prin (e :value))
      (prin ")"))
    :bool
    (do
      (prin "((bool)") 
      (if (e :value)
        (prin "1")
        (prin "0"))
      (prin ")"))
    :->
    (do
      (prin "({return ")
      (emit-expr (e :expr))
      (prin ";})"))
    :if
    (do
      (prin "(")
      (emit-expr (e :expr))
      (prin "?")
      (emit-expr (e :if-true))
      (prin ":")
      (emit-expr (e :if-false))
      (prin ")"))
    :while
    (do
      (prin "({while(")
      (emit-expr (e :expr))
      (prin ")")
      (emit-expr (e :body))
      (prin ";})"))
    :call
    (do
      (emit-expr (e :expr))
      (prin "(")
      (eachk i (e :params)
        (unless (zero? i)
          (prin ", "))
        (emit-expr ((e :params) i)))
      (prin ")"))
    :binop
    (do
      (prin "(")
      (emit-expr (e :left-expr))
      (prin (string (e :op)))
      (emit-expr (e :right-expr))
      (prin ")"))
    :=
    (do
      (prin "(")
      (emit-expr (e :left-expr))
      (prin "=")
      (emit-expr (e :right-expr))
      (prin ")"))))

(defn emit-fn
  [f]
  (def fn-type (f :type))
  (emit-type (fn-type :return-type) (f :name))
  (prin "(")
  (if (empty? (f :param-names))
    (prin "void")
    (eachk i (f :param-names)
      (unless (zero? i)
        (prin ", "))
      (emit-type (in (fn-type :param-types) i))
      (prinf " %s" (get-in f [:param-names i]))))
  (print ") {")
  (emit-expr (f :expr))
  (print ";\n}"))

(defn emit-fns
  [fns]
  (each f fns
    (emit-fn f)))
