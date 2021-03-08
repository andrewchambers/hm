
(var *type-tab* @{})
(var *tokens* @[])

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
    (errorf "unexpected  EOF"))
  (last *tokens*))

(var type-var-counter 0)
(defn fresh-type-var
  []
  (symbol "?t" (++ type-var-counter)))

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

(varfn parse-type
  []
  (def tok (peek))
  (cond
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
  (def params @[])
  (expect :fn)
  (def nametok (expect :ident))
  (expect :lparen)
  (while (not= ((peek) :kind) :rparen)
    (def param-name ((expect :ident) :text))
    (expect :colon)
    (def param-type (parse-type))
    (array/push params [param-name param-type])
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
      :return-type return-type
      :params (tuple ;params)})

  (case ((peek) :kind)
    :semicolon
      (do (next) f)
    :lbrace
    (do
      (expect :lbrace)
      (expect :number)
      (expect :rbrace)
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
  top-levels)

# wrapped in @{} to make them comparable by identity.
(def primitive-types {
  "void" @{:name "void"}
  "char" @{:name "char"}
  "int" @{:name "int"}
})

(defn parse
  [tokens]
  (set *type-tab* (merge @{} primitive-types))
  (set *tokens* (reverse tokens))
  (parse-top-levels))
