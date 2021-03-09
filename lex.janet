# A simple lexer that uses janet peg.
#
# > (lex "1 + 1 let hello fn")
# [{:text "1" :kind :number :span (0 1)} ...]

(def- ops 
  ["(" ")" "[" "]" "{" "}"
   "->"
   "="
   "+" "-" "*" "/"
   ":" ";" ","
   "&"])

(def- keywords ["fn" "let" "type" "struct" "if" "else" "while" "true" "false"])

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

(defn- make-string-literal
  [start text end]
  {:span [start end]
   :text text
   :kind :string-literal})

(defn- make-op
  [start text end]
  {:span [start end]
   :text text
   :kind (keyword text)})

(def grammar
  ~@{:comment (sequence "//" (to "\n"))
     :ws (any (choice :comment (set " \t\n")))
     :ident-or-kw-start (choice (range "az") (range "AZ") "_")
     :ident-or-kw-body (any (choice :ident-or-kw-start (range "09")))
     :ident-or-kw-text (sequence :ident-or-kw-start (any :ident-or-kw-body))
     :ident-or-kw (sequence :ws (cmt (sequence (position) (capture :ident-or-kw-text) (position)) ,make-ident-or-kw))
     :string-escape (sequence "\\" 1)
     :string-part (choice :string-escape (sequence (not `"`) 1))
     :string-literal (cmt (sequence (position) (capture (sequence `"` (any :string-part) `"`)) (position)) ,make-string-literal)
     :number (cmt (sequence (position) (capture (some (range "09"))) (position)) ,make-number)
     :token (sequence :ws (choice
                            :lbrack :rbrack
                            :lparen :rparen
                            :lbrace :rbrace
                            :->
                            :+ :- :* :/ :=
                            :&
                            :colon :semicolon :comma
                            :ident-or-kw
                            :number
                            :string-literal))
     :main (any (sequence :ws 
                  (choice 
                    :token
                    (not 1)
                    (error))))})

# Add human readable aliases
(def op-remaps {
  "[" :lbrack
  "]" :rbrack
  "{" :lbrace
  "}" :rbrace
  "(" :lparen
  ")" :rparen
  ":" :colon
  ";" :semicolon
  "," :comma
})

(each [o r] (pairs op-remaps)
  (put grammar r (keyword o)))

# Fill in named rules for operators.
(each op ops
  (def kind (get op-remaps op (keyword op)))
  (put grammar (keyword op) ~(cmt (sequence :ws (position) ,op (position)) ,(fn make-op [start end] {:span [start end] :text op :kind kind}))))

(def grammar (freeze grammar))

(def- lexer (peg/compile grammar))

(defn lex [source]
  (peg/match lexer source))

# (printf "%.20m" grammar)
# (printf "%j" (lex "1 + 1 let hello fn // comment \n lol"))
