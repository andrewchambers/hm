(import ./types)

(defn emit-type
  [t]
  (cond
    (string? t)
    (prinf "%s" t)
    (struct? t)
    (case (t :kind)
      :ptr
      (do
        (emit-type (t :sub-type))
        (prin "*"))
      :struct
      (do
        (prin "struct {")
        (each [name field-type] (t :fields)
          (prinf " %s " name)
          (emit-type field-type)
          (prin "; "))
        (prin "}")))))

(defn emit-types
  [type-tab]
  (each type-name (types/sort type-tab)
    (def t (type-tab type-name))
    (if (table? t) # Don't emit primitive types.
      (unless (= (t :crepr) type-name)
        (printf "typedef %s %s;" (t :crepr) type-name))
      (do
        (prin "typedef ")
        (emit-type t)
        (printf " %s;" type-name)))))

(defn emit-decl
  [decl]
  (def decl-name (decl :name))
  (def decl-type (decl :type))
  (case (decl-type :kind)
    :fn
    (do
      (emit-type (decl-type :return-type))
      (prin " ")
      (prin decl-name)
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
      (emit-type (e :var-type))
      (prin " ")
      (prin (e :name))
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
  (emit-type (fn-type :return-type))
  (prin " ")
  (prin (f :name))
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
