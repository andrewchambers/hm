(import ./lex)
(import ./parse)
(import ./infer)

(defn main
  [&]
  (def src (:read stdin :all))
  (tracev src)
  (def tokens (lex/lex src))
  (tracev tokens)
  (def top-levels (parse/parse tokens))
  (tracev top-levels)
  (infer/type-check-top-levels top-levels)
  (tracev top-levels)
  nil)