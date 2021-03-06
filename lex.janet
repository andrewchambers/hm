# A simple lexer that uses janet peg.
#
# > (lex "1 + 1 let hello fn")
# [{:text "1" :kind :number :span (0 1)} ...]

(def- ops ["(" ")" "[" "]"
           "+" "-" "*" "/"])

(def- keywords ["fn" "let"])

(def- keyword-tab
  (let [t @{}]
    (each kw keywords
      (put t kw (keyword kw)))
    t))

(defn- make-ident-or-kw
  [start text end]
  {:span [start end]
   :text text
   :kind (get keyword-tab text :ident)})

(defn- make-number
  [start text end]
  {:span [start end]
   :text text
   :kind :number})

(defn- make-op
  [start text end]
  {:span [start end]
   :text text
   :kind (keyword text)})

(def grammar
  ~@{:ws (any (set " \t\n"))
     :ident-or-kw-start (choice (range "az") (range "AZ") "_")
     :ident-or-kw-body (any (choice :ident-or-kw-start (range "09")))
     :ident-or-kw-text (sequence :ident-or-kw-start (any :ident-or-kw-body))
     :ident-or-kw (sequence :ws (cmt (sequence (position) (capture :ident-or-kw-text) (position)) ,make-ident-or-kw))
     :number (sequence :ws (cmt (sequence (position) (capture (some (range "09"))) (position)) ,make-number))
     :token (sequence :ws (choice
                            :lbrack
                            :rbrack
                            :lparen
                            :rparen
                            :+
                            :-
                            :*
                            :/
                            :ident-or-kw
                            :number))
     :main (any :token)})

# Add human readable aliases
(put grammar :lbrack (keyword "["))
(put grammar :rbrack (keyword "]"))
(put grammar :lparen (keyword "("))
(put grammar :rparen (keyword ")"))

# Fill in named rules for operators.
(each op ops
  (put grammar (keyword op) ~(cmt (sequence :ws (position) ,op (position)) ,(fn make-op [start end] {:span [start end] :text op :kind (keyword op)}))))

(def grammar (freeze grammar))

(def- lexer (peg/compile grammar))

(defn lex [source]
  (peg/match lexer source))

# (printf "%.20m" grammar)
(printf "%j" (lex "1 + 1 let hello fn"))
