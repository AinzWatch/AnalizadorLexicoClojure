(defn parse-expression [expression]
  (let [operators {"+" + "-" - "*" * "/" /}
        tokens (re-seq #"\d+|\+|\-|\*|\/" expression)]

    (reduce (fn [acc token]
              (if-let [op (get operators token)]
                (conj acc op)
                (conj acc (if (re-matches #"\d+" token)
                            (read-string token)
                            token))))
            '()
            tokens)))

(defn format-expression [expression]
  (let [[op arg1 arg2] expression]
    (str "(" (str arg1) op  arg2 ")")
  ))


(defn eval-expression [expression]
  (let [parsed (parse-expression expression)
        formatted (format-expression parsed)]
    (print parsed)
    ;(eval (read-string formatted))
    ))

(println (eval-expression "1+2"))
