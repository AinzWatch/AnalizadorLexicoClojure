(require '[clojure.java.io :as io])

(defn read-txt-files [directory]
  (let [files (file-seq (io/file directory))
        txt-files (filter #(= (.getName %) ".txt") files)]
    (map #(.getName %) txt-files)))

(def txt-file-names (read-txt-files "C:/Users/eliez/OneDrive/Desktop/Clases/Clojure/archivos"))

;; Imprime la lista de nombres de los archivos .txt
(println txt-file-names)