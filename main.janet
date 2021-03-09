(import ./lex)
(import ./parse)
(import ./infer)
(import ./emitc)

(defn main
  [&]
  (def src (:read stdin :all))
  (tracev src)
  (def tokens (lex/lex src))
  (tracev tokens)
  (def parsed (parse/parse tokens))
  (infer/type-check-top-levels (parsed :top-levels))
  (emitc/emit-types (parsed :type-tab))
  (emitc/emit-decls (parsed :global-scope))
  (emitc/emit-fns (filter |(and (= ($ :kind) :fn) ($ :expr) )
                          (parsed :top-levels)))
  nil)