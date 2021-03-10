(import sh)

(defn run-test
  [t]
  (def cfile (string t ".c"))
  (def exe (string t ".exe"))
  (def expected-stdout-path (string t ".expected-stdout"))
  (def stdout-path (string t ".actual-stdout"))
  (def csrc (sh/$< janet ./main.janet < (slurp t) > [stderr :null]))
  (spit cfile csrc)
  (sh/$ cc ,cfile -o ,exe)
  (spit stdout-path (sh/$< ,exe))
  (if (os/stat expected-stdout-path)
    (sh/$ diff -u ,stdout-path ,expected-stdout-path)
    (unless (empty? (slurp stdout-path))
      (error "unexpected test output"))))

(def tests
  (->> (os/dir "test")
       (sort)
       (filter |(string/has-suffix? ".hm" $))
       (map |(string "test/" $))))

(each t tests
  (prin t "...")
  (file/flush stdout)
  (run-test t)
  (print " ok"))
