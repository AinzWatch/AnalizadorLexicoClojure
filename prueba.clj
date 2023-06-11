(defn verificar-parentesis [expresion]
  (let [abiertos (count (re-seq #"\(" expresion))
        cerrados (count (re-seq #"\)" expresion))]
    (= abiertos cerrados)))

(def expresion "(1+1()")
(if (verificar-parentesis expresion)
  (println "Los paréntesis están balanceados.")
  (println "Los paréntesis no están balanceados."))
