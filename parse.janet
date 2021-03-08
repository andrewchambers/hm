(import ./types)

(var *global-scope* @{})
(var *type-tab* @{})
(var *tokens* @[])
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

(var parse-type nil)

(defn parse-struct-type
  []
  (def fields @[])
  (expect :struct)
  (expect :lbrace)
  (while (not= ((peek) :kind) :rbrace)
    (def name (expect :ident))
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
    (array/push param-types param-types)
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
  (def nametok (expect :ident))
  (expect :lparen)
  (while (not= ((peek) :kind) :rparen)
    (def param-name ((expect :ident) :text))
    (expect :colon)
    (def param-type (parse-type))
    (array/push param-names param-name)
    (array/push param-types param-types)
    (case ((peek) :kind)
      :comma
      (do (next))
      (break)))
  (expect :rparen)
  (expect :->)
  (def return-type (parse-type))

  (def f 
    @{:name (nametok :text)
      :kind :fn
      :type {:kind :fn
             :param-types (tuple ;param-types)
             :return-type return-type}
      :param-names (tuple ;param-names)})

  (when (in *global-scope* (f :name))
    (errorf "redefining global variable '%s'" (f :name)))
  
  (put *global-scope* (f :name) @{
    :name (f :name)
    :type (f :type)
  })

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

  (types/validate *type-tab*)

  top-levels)

(defn parse
  [tokens]
  (set *type-var-counter* 0)
  (set *global-scope* @{})
  (set *type-tab* (merge @{} types/primitives))
  (set *tokens* (reverse tokens))
  (set *eof-error* "end of file")
  (def top-levels (parse-top-levels))
  (tracev *global-scope*)
  (tracev *type-tab*)
  top-levels)
