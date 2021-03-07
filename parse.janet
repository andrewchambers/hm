
(var *tokens* nil)

(var type-var-counter 0)
(defn fresh-type-var
  []
  (symbol "?t" (++ type-var-counter)))

(defn expect
  [what]
  (def tok (array/pop *tokens*))
  (unless (= (get tok :kind) what)
    (errorf "expected %p got %p" what tok))
  tok)

(defn parse-fn
  []
  (expect :fn)
  (def nametok (expect :ident))
  (expect :lparen)
  (expect :rparen)
  (expect :->)
  (expect :int)
  (expect :lbrace)
  (def ret (expect :number))
  (expect :rbrace)
  @{:kind :fn
    :return-type :int
    :name (nametok :text)
    :expr @{:kind :->
            :type :int
            :expr @{:kind :number
                    :value (ret :text)
                    :type (fresh-type-var)}}})

(defn parse
  [tokens]
  (set *tokens* (reverse tokens))
  @[(parse-fn)])
