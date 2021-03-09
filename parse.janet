(import ./types)

(var *global-scope* @{})
(var *type-tab* @{})
(var *tokens* @[])
(var *scopes* @[])
(var *cur-fn* nil)
(var *eof-error* nil)
(var *type-var-counter* 0)

(defn eof?
  []
  (empty? *tokens*))

(defn expect
  [what]
  (when (eof?)
    (errorf "expected %p got EOF" what))
  (def tok (array/pop *tokens*))
  (unless (= (get tok :kind) what)
    (errorf "expected %p got %p" what tok))
  tok)

(defn next
  []
  (array/pop *tokens*))

(defn peek
  []
  (when (eof?)
    (errorf "unexpected %s" *eof-error*))
  (last *tokens*))

(defn fresh-type-var
  []
  (symbol "?t" (++ *type-var-counter*)))

(defn push-scope
  []
  (def s @{})
  (table/setproto s (last *scopes*))
  (array/push *scopes* s))

(defn pop-scope
  []
  (array/pop *scopes*))

(defn declare
  [name t]
  (def s (last *scopes*))
  (when (get s name)
    (errorf "redefinition of symbol '%s'" name))
  (put s name @{
    :name name
    :type t
  }))

(defn lookup
  [name]
  (def v (get (last *scopes*) name))
  (unless v
    (errorf "undefined variable '%s'" name))
  v)

(var parse-type nil)

(defn parse-struct-type
  []
  (def fields @[])
  (expect :struct)
  (expect :lbrace)
  (while (not= ((peek) :kind) :rbrace)
    (def name ((expect :ident) :text))
    (expect :colon)
    (def subt (parse-type))
    (expect :semicolon)
    (array/push fields [name subt]))
  (expect :rbrace)
  {:kind :struct :fields (tuple ;fields)})

(defn parse-fn-type
  []
  (expect :fn)
  (def param-types @[])
  (expect :lparen)
  (while (not= ((peek) :kind) :rparen)
    (def param-name ((expect :ident) :text))
    (expect :colon)
    (def param-type (parse-type))
    (array/push param-types param-type)
    (case ((peek) :kind)
      :comma
      (do (next))
      (break)))
  (expect :rparen)
  (expect :->)
  (def return-type (parse-type))
  {:kind :fn
   :param-types (tuple ;param-types)
   :return-type return-type})

(varfn parse-type
  []
  (def tok (peek))
  (cond
    (= (tok :kind) :fn)
    (parse-fn-type)
    (= (tok :kind) :struct)
    (parse-struct-type)
    (= (tok :kind) :*)
    (do
      (next)
      {:kind :ptr :sub-type (parse-type)})
    (= (tok :kind) :ident)
    (do
      (next)
      (tok :text))
    (errorf "expected type, got %p" tok)))

(defn parse-typedef
  []
  (expect :type)
  (def name ((expect :ident) :text))
  (expect :=)
  (def t (parse-type))
  (when (get *type-tab* name)
    (errorf "redefinition of type %s" name))
  (put *type-tab* name t)
  (expect :semicolon)
  @{:kind :typedef :name name :type t})

(defn parse-fn
  []
  (def param-names @[])
  (def param-types @[])
  (expect :fn)
  (def name-tok (expect :ident))
  (expect :lparen)
  (while (not= ((peek) :kind) :rparen)
    (def param-name ((expect :ident) :text))
    (expect :colon)
    (def param-type (parse-type))
    (array/push param-names param-name)
    (array/push param-types param-type)
    (case ((peek) :kind)
      :comma
      (do (next))
      (break)))
  (expect :rparen)
  (expect :->)
  (def return-type (parse-type))

  (def f 
    @{:name (name-tok :text)
      :kind :fn
      :type {:kind :fn
             :param-types (tuple ;param-types)
             :return-type return-type}
      :param-names (tuple ;param-names)})

  (declare (f :name) (f :type))

  (case ((peek) :kind)
    :semicolon
      (do (next) f)
    :lbrace
    (do
      (def body-tokens @[(expect :lbrace)])
      (var depth 1)
      (while (< 0 depth)
        (when (eof?)
          (error "function body without terminating '}'"))
        (def tok (next))
        (array/push body-tokens tok)
        (case (tok :kind)
          :lbrace
          (++ depth)
          :rbrace
          (-- depth)))
      (put f :body-tokens body-tokens)
      f)
    (errorf "expected ; or { but got %p" (peek))))


(defn validate-type
  [type-tab seen t]
  # TODO check for infinite size types.
  (unless (seen t)
    (match t
      (t (string? t))
      (if-let [lookup (type-tab t)]
        (do
          (put seen t true) 
          (validate-type type-tab seen lookup))
        (errorf "undefined type reference: %s" t))
      {:kind :ptr :sub-type sub-type}
      (validate-type type-tab seen sub-type)
      {:kind :struct :fields fields}
      (each f fields
        (validate-type type-tab seen (get f 1)))
      {:kind :fn :return-type return-type :param-types param-types}
      (do 
        (validate-type type-tab seen return-type)
        (each t param-types
          (validate-type type-tab seen t)))
      _
      nil)))

(defn validate-types
  [type-tab]
  (def seen @{})
  (eachk k type-tab
    (validate-type type-tab seen k)))

(defn parse-top-levels
  []
  (def top-levels @[])
  (while (not (eof?))
    (def tok (peek))
    (case (tok :kind)
      :type
      (array/push top-levels (parse-typedef))
      :fn
      (array/push top-levels (parse-fn))
      (errorf "expected type definition or function, got %p" tok)))

  (validate-types *type-tab*)

  top-levels)

(var parse-block nil)
(var parse-expr nil)

(defn parse-let
  []
  (expect :let)
  (def name ((expect :ident) :text))
  (def t
    (if (= ((peek) :kind) :colon)
      (do
        (next)
        (parse-type))
      (fresh-type-var)))
  (def expr 
    (when (= ((peek) :kind) :=)
      (next)
      (parse-expr)))
  (declare name t)
  @{:kind :let
    :name name
    :expr expr
    :var-type t
    :type "void"
    :no-return (get expr :no-return false)})

(defn parse-while
  []
  (expect :while)
  (def expr (parse-expr))
  (def body (parse-block))
  @{:kind :while
    :expr expr
    :body body
    :type "void"
    :no-return false})

(defn parse-if
  []
  (expect :if)
  (def expr (parse-expr))
  (def if-true (parse-block))
  (def if-false
    (when (= ((peek) :kind) :else)
      (next)
      (parse-block)))
  @{:kind :if
    :expr expr
    :if-true if-true
    :if-false if-false
    :type (fresh-type-var)
    :no-return (and (get if-true :no-return)
                    (get if-false :no-return))})

(defn parse-true-false
  []
  @{:kind :bool
    :type "bool"
    :value (= :true ((next) :kind))
    :no-return false})

(defn parse-expr-base
  []
  (def tok (peek))
  (var expr 
    (case (tok :kind) 
      :if
      (parse-if)
      :while
      (parse-while)
      :lbrace
      (parse-block)
      :lparen
      (do
        (next)
        (parse-expr)
        (expect :rparen))
      :*
      (do 
        (next)
        (def expr (parse-expr-base))
        @{:kind :*
          :expr expr
          :type (fresh-type-var)
          :no-return (expr :no-return)})  
      :&
      (do 
        (next)
        (def expr (parse-expr-base))
        @{:kind :&
          :expr expr
          :type (fresh-type-var)
          :no-return (expr :no-return)})  
      :string-literal
      (do 
        (next)
        @{:kind :string-literal
          :value (tok :text)
          :type (fresh-type-var)
          :no-return false})
      :number
      (do 
        (next)
        @{:kind :number
          :value (tok :text)
          :type (fresh-type-var)
          :no-return false})
      :ident
      (let [v (lookup ((next) :text))] 
        @{:kind :ident
          :name (v :name)
          :type (v :type)
          :no-return false})
      :true
      (parse-true-false)
      :false
      (parse-true-false)
      (errorf "expected expression, got %p" tok)))
  (while true
    (def next-kind ((peek) :kind))
    (cond
      (= next-kind :lparen)
      (do
        (next)
        (def params @[])
        (while (not= ((peek) :kind) :rparen)
          (when (not (empty? params))
            (expect :comma))
          (array/push params (parse-expr)))
        (expect :rparen)
        (set expr
          @{:kind :call
            :expr expr
            :type (fresh-type-var)
            :params params
            :no-return false}))
      (= next-kind :colon)
      (do
        (next) 
        (set expr
          @{:kind :type-assert
            :type (parse-type) 
            :expr expr
            :no-return (expr :no-return)}))
      (break)))
  expr)

(varfn parse-expr
  []
  (var l (parse-expr-base))
  (while true
    (def next-kind ((peek) :kind))
    (cond
      (index-of next-kind [:+ :- :* :/])
      (do
        (next)
        (def r (parse-expr-base))
        (set l
          @{:kind :binop
            :op next-kind
            :type (fresh-type-var)
            :left-expr l
            :right-expr r
            :no-return (or (l :no-return) (r :no-return))}))
      (break)))
  l)

(varfn parse-block
  []
  (push-scope)
  (expect :lbrace)
  (def exprs @[])
  (var prev-had-semi false)
  (while (not= ((peek) :kind) :rbrace)
    (set prev-had-semi false)
    
    (def expr
      (case ((peek) :kind)
        :->
        (do 
          (next)
          @{:kind :->
            :type (fresh-type-var)
            :return-type (get-in *cur-fn* [:type :return-type])
            :no-return true
            :expr (parse-expr)})
        :let
        (parse-let)
        (parse-expr)))
    
    (array/push exprs expr)
    
    (when (= (expr :kind) :->)
      (break))

    (if (not (index-of (expr :kind) [:block :if :while]))
      (case ((peek) :kind)
        :semicolon
        (do
          (next)
          (set prev-had-semi true))
        :rbrace
        nil
        (errorf "expected ';' or '}' after block expression, got %p" (peek))) 
      (when (= ((peek) :kind) :semicolon)
         (next)
         (set prev-had-semi true))))
  (expect :rbrace)
  (pop-scope)
  (def use-last (and (not prev-had-semi)
                     (not (empty? exprs))))
  (def block-type (if use-last ((last exprs) :type) "void"))
  (def no-return (any? (map |(get $ :no-return) exprs)))
  @{:kind :block
    :exprs exprs
    :use-last use-last
    :type block-type
    :no-return no-return})

(defn parse-fn-body
  []
  (push-scope)
  (eachk idx (*cur-fn* :param-names)
    (def param-name (get-in *cur-fn* [:param-names idx]))
    (def param-type (get-in *cur-fn* [:type :param-types idx]))
    (declare param-name param-type))
  (def body (parse-block))
  (pop-scope)
  (if (body :no-return)
      body
    @{:kind :->
      :type (fresh-type-var)
      :no-return true
      :return-type (get-in *cur-fn* [:type :return-type])
      :expr body}))

(defn parse
  [tokens]
  (set *type-var-counter* 0)
  (set *global-scope* @{})
  (set *scopes* @[*global-scope*])
  (set *type-tab* (merge @{} types/builtins))
  (set *tokens* (reverse tokens))
  (set *eof-error* "end of file")
  (def top-levels (parse-top-levels))
  (each f (filter |(and (= ($ :kind) :fn)
                        ($ :body-tokens))
                  top-levels)
    (set *cur-fn* f)
    (set *tokens* (reverse (f :body-tokens)))
    (put f :body-tokens nil)
    (set *eof-error* "end of function")
    (put f :expr (parse-fn-body)))

  {:top-levels top-levels
   :type-tab *type-tab*
   :global-scope *global-scope*})
