(require '[clojure.java.io :as io])
(declare is_OPERATION)
(def reservedWords ["let" "const" "function" "for" "while" "return" "print"])

(defn tokenize_complete [line]
  (cond
    (re-matches #"^(\s*|[{]\s*)//.*\s*$" line) [:COMMENT line]
    (re-matches #"^(\s*|[{]\s*)((let|const)?)\s+([a-zA-Z]+\w*)(\s*[=])(.*)\s*$" line) [:VARIABLE line]
    (re-matches #"^\s*(function)\s+([a-zA-Z]+\w*)\s*(\()((?:\s*[a-zA-Z]+\w*(?:\s*\,\s*[a-zA-Z]+\w*)*)\s*?)*\s*(\))\s*(\{|\s*)$" line) [:FUNCTION line]
    (re-matches #"^(\s*|[{]\s*)(while)\s*(([(])(.)+[ ]*([)]))[ ]*([{])?\s*$" line) [:WHILE line]
    (re-matches #"^(\s*|[{]\s*)(return)[ ]*(.)*\s*$" line) [:RETURN line]
    (re-matches #"^\s*(for)\s*(\()(\s*(let\s+)?([a-zA-Z]+\w*)(\s*\=)(.+));(([a-zA-Z0-9<>=!+\-\*\/() ]+));\s*([a-zA-Z]+\w*)(\s*\=)(.+)(\))\s*(\{|\s*)\s*$" line) [:FOR line]
    (re-matches #"^\s*$" line) [:NEWLINE line]
    (re-matches #"^\s*[({\[]+\s*$" line) [:OPENER line]
    (re-matches #"^\s*[)}\]]+\s*$" line) [:CLOSER line]
    (re-matches #"^(\s*|[{]\s*)(print)\s+((.+$)|([\"]([a-zA-Z0-9'?¡¿!#$%&\/()\\=*¨´{}\[\]|°,.:;_\-ñ ]*)[\"])|[a-zA-Z]+\w*)\s*$" line) [:PRINT line]
    :else [:ERROR line]
  )
)

(defn is_PARAMETER [string_list NewList]
  (if (nil? string_list)
    [:SPACE ""]
    (if (string? string_list)
        (is_PARAMETER (re-seq #"[a-zA-Z]+\w*|\,|\s*" string_list) [])
      (if (empty? string_list)
          NewList    
        (let
            [
              string (str (first string_list))
              operationTokenizer(cond
                                  (and (re-matches #"\s*\w+\s*$" string) (not (contains? (set reservedWords) string))) 
                                    (conj NewList [:IDENTIFIER string])                              
                                  (re-matches #"^\s*$" string) (conj NewList [:SPACE " "])
                                  (re-matches #"^\,$" string) (conj NewList [:SEPARATOR string])

                                  :else (conj NewList [:ERROR string])       
                                )
            ]          
            (is_PARAMETER (rest string_list) operationTokenizer)
        )
      )
    )
  )
)

(defn is_LOGIC [string_list NewList]
  (if (string? string_list)
    (if (re-matches #".*[+\-*\/\>\<\=]\s*$" string_list)
      [:ERROR string_list]
      (is_LOGIC (re-seq #"[a-zA-Z]+\w*|\d+|\<|\>|\<\=|\>\=|!=|\+|\-|\*|\/|\=\=|\{|\s*" string_list) [])
    )
    (if (empty? string_list)
        NewList    
      (let
          [
            string (str (first string_list))
            operationTokenizer(cond
                                (re-matches #"^\s*[0-9]+\s*$" string) (conj NewList [:NUMBER string])
                                (re-matches #"\s*^[+\-*\/<>=!]\s*$" string) (conj NewList [:OPERATOR string])    
                                (and (re-matches #"\s*\w+\s*$" string) (not (contains? (set reservedWords) string))) 
                                  (conj NewList [:IDENTIFIER string])                              
                                (re-matches #"^\s*$" string) (conj NewList [:SPACE " "])
                                :else (conj NewList [:ERROR string])       
                              )
          ]          
          (is_LOGIC (rest string_list) operationTokenizer)
      )
    )
  )
)

(defn is_OPERATION [string_list NewList complete_string]

  (if (string? string_list)
    (if (or (re-matches #".*[+\-*\/]\s*$" string_list) (re-matches #"\W+" string_list))
      [":ERROR" complete_string]
      (is_OPERATION (re-seq #"//.*|\W+|\w+|\d+|\+|\-|\*|\/|\(|\)|\s*" string_list) [] complete_string)
    )
    (if (empty? string_list)
        NewList    
        (if (= (count (re-seq #"\(" complete_string)) (count (re-seq #"\)" complete_string)))    
          (let
            [
              string (str (first string_list))
              operationTokenizer(cond
                                  (re-matches #"^\s*[0-9]+\s*$" string) (conj NewList [:NUMBER string])
                                  (re-matches #"\s*^[+\-*\/]\s*$" string) (conj NewList [:OPERATOR string])    
                                  (and (re-matches #"\s*([a-zA-Z]+\w*)\s*$" string) (not (contains? (set reservedWords) string))) 
                                    (conj NewList [:IDENTIFIER string])
                                  (re-matches #"\s*[(]\s*$" string)   (conj NewList [:PARENTHESIS string]) 
                                  (re-matches #"\s*[)]\s*$" string) (conj NewList [:PARENTHESIS string])
                                  (re-matches #"^\s*$" string) (conj NewList [:SPACE " "])
                                  (re-matches #"^//.*" string) (conj NewList [:COMMENT string])
                                  :else  [":ERROR" complete_string]      
                                )
            ]          
            (is_OPERATION (rest string_list) operationTokenizer complete_string)              
          )
        [":ERROR" complete_string]
      )
    )
  )
)

;-------------------------------  Definicion de Comentarios ------------------------------------

(defn is_COMMENT [token]
  (if (re-matches #"^\s*//.*\s*$" (str (first (rest token))))
    token
    [:ERROR  (rest token)]
  )
)

;-------------------------------  Definicion de Variables ------------------------------------
(defn is_VARIABLE_TOKENIZATION [string_list NewList complete_string]
  (if (empty? string_list)
    NewList       
    (let 
        [
          lineTokenizer (cond
                            (or (= (first string_list) "let") (= (first string_list) "const")) (conj NewList [:RESERVADO (first string_list)])
                            (re-matches #"[a-zA-Z]+\w*" (first string_list)) (conj NewList [:IDENTIFIER (first string_list)])
                            (re-matches #"^\s*[=]\s*$" (first string_list)) (conj NewList [:OPERATOR (first string_list)])
                            (re-matches #"^\s*$" (first string_list)) (conj NewList [:SPACE (first string_list)])

                            :else (conj NewList (is_OPERATION  (first string_list) [] complete_string))
                          )
        ]
      

        (if (some #(= ":ERROR" %) (flatten lineTokenizer))
            [:ERROR complete_string]
            (is_VARIABLE_TOKENIZATION (rest string_list) lineTokenizer complete_string)

        )
    )
  )
)

(defn is_VARIABLE_DEFINITION [type variable operation string_list complete_string]
  (cond
    (= type :DECLARATION) (is_VARIABLE_TOKENIZATION string_list [] complete_string)
    (= type :RESTORATION) (is_VARIABLE_TOKENIZATION string_list [] complete_string)
  )
)

(defn is_VARIABLE [token]
  (let 
      [
        variable (first (rest (first(re-seq #"(\w*)\s*=\s*(.*)" (str (first (rest token))) ))))
        operation  (first (rest (rest (first(re-seq #"([a-zA-Z]+\w*)\s*=\s*(.*)" (str (first (rest token))))))))
        string_list (rest (first(re-seq #"^\s*(let|const|\s*)\s*(\w*)(\s*[=])(.*)\s*$" (str (first (rest token))) )))
      ]
    (cond 
      (re-matches #"^\s*(let|const).*" (str (first (rest token)))) (is_VARIABLE_DEFINITION :DECLARATION variable operation string_list (str (first (rest token))))
      (re-matches #"^\s*.*" (str (first (rest token)))) (is_VARIABLE_DEFINITION :RESTORATION variable operation string_list (str (first (rest token))))    
    )
  )
)

;-------------------------------  Definicion de Print ------------------------------------
(defn is_PRINT_TOKENIZATION [string_list NewList complete_string]
 (if (empty? string_list)
    NewList
    (let 
        [
          lineTokenizer (cond
                            (= (first string_list) "print")  (conj NewList [:RESERVADO (first string_list)])
                            (re-matches #"^\s*\".*\"\s*$"  (first string_list)) 
                              (conj NewList [:STRING (first string_list)])
                            :else (conj NewList (is_OPERATION  (first string_list) [] complete_string))
                          )
        ]
        (if (some #(= ":ERROR" %) (flatten lineTokenizer))
            [:ERROR complete_string]
            (is_PRINT_TOKENIZATION (rest string_list) lineTokenizer complete_string)
        )        
    )
  )
)

(defn is_PRINT [token]
  (let 
      [
        string_list (rest (first(re-seq #"^\s*(print)\s*(.*)\s*$" (str (first (rest token))) )))
      ]
    (is_PRINT_TOKENIZATION string_list []  (str (first (rest token))) )
  )
)

;-------------------------------  Definicion de While ------------------------------------

(defn is_While_TOKENIZATION [string_list NewList error_found]
  (if (empty? string_list)
    NewList
    (let 
        [
          lineTokenizer (cond
                            (= (first string_list) "while") (conj NewList [:RESERVADO (first string_list)])
                            (re-matches #"^[()]$" (first string_list)) (conj NewList [:PARENTHESIS (first string_list)])
                            (re-matches #"^[{]$" (first string_list)) (conj NewList [:OPENER (first string_list)])

                            :else (conj NewList (is_LOGIC  (first string_list) [] ))
                          )
        ]
        (is_While_TOKENIZATION (rest string_list) lineTokenizer error_found)
    )
  )
)

(defn is_While [token]
  (let 
      [
        string_list  (rest (first(re-seq #"^\s*(while)\s*([(])(.+\s*)([)])\s*([{]|\s*)\s*$" (str (first (rest token))) )))
      ]
    (is_While_TOKENIZATION string_list [] 0)    
  )
)

;-------------------------------  Definicion de Function ------------------------------------

(defn is_FUNCTION_TOKENIZATION [string_list NewList error_found]
    (if (empty? string_list)
        NewList
        (if (nil? string_list)
          (is_FUNCTION_TOKENIZATION (rest string_list) (conj NewList [:SPACE (first string_list)]) error_found)
          (let 
              [
                lineTokenizer (cond
                                  (= (first string_list) "function") (conj NewList [:RESERVADO (first string_list)])
                                  (re-matches #"^[()]$" (first string_list)) (conj NewList [:PARENTHESIS (first string_list)])
                                  (re-matches #"^[{]$" (first string_list)) (conj NewList [:OPENER (first string_list)])
                                  :else (conj NewList (is_PARAMETER  (first string_list) [] ))
                                )
              ]
              (is_FUNCTION_TOKENIZATION (rest string_list) lineTokenizer error_found)
          )
      )
    )
  )

(defn is_FUNCTION [token]
  (let 
      [
        string_list  (rest (first(re-seq #"^\s*(function)\s+([a-zA-Z]+\w*)\s*(\()((?:\s*[a-zA-Z]+\w*(?:\s*\,\s*[a-zA-Z]+\w*)*)\s*?)*\s*(\))\s*(\{|\s*)$" (str (first (rest token))) )))
      ]
    (print string_list)
    (is_FUNCTION_TOKENIZATION string_list [] 0)    
  )
)

;-------------------------------  Definicion de For ------------------------------------

(defn is_FOR_TOKENIZATION [string_list NewList error_found variable_definition logic_definition increaser_definition]

  (if (empty? string_list)
    NewList
    (let 
        [
          lineTokenizer (cond
                            (= (first string_list) "for") (conj NewList [:RESERVADO (first string_list)])
                            (re-matches #"^[()]$" (first string_list)) (conj NewList [:PARENTHESIS (first string_list)])
                            (re-matches #"^[{]$" (first string_list)) (conj NewList [:OPENER (first string_list)])
                            (= (first string_list) "let") (conj NewList (is_VARIABLE_TOKENIZATION variable_definition [] (str variable_definition) ) )
                            (re-matches #"^[;]$" (first string_list)) (conj NewList [:SEPARATOR (first string_list)])
                            (re-matches #"^\w+$" (first string_list)) (conj NewList (is_VARIABLE_TOKENIZATION increaser_definition [] (str increaser_definition) ) )
                            (re-matches #"\s*$" (first string_list))  (conj NewList [:SPACE (first string_list)])
                              ;(conj NewList [:SEPARATOR (first string_list)])
                            :else  (conj NewList (is_LOGIC (str logic_definition) []  ) )
                          )
        ]
        (is_FOR_TOKENIZATION (rest string_list) lineTokenizer error_found variable_definition logic_definition increaser_definition)
    )
  )
)

(defn is_FOR [token]
  (let 
      [
        string_list  (rest (first(re-seq #"^\s*(for)\s*(\()(let|\s*)\s*.*(\;)(.+)(\;)\s*(\w*)\s*[=].*(\))(\s*)(\{|\s*)\s*$" (str (first (rest token))) )))
        variable_definition  (rest (first(re-seq #"^\s*for\s*\((let|\s*)\s*(\w*)(\s*[=])(.*)\s*\;.+\;.+\)(\s*)\{?\s*$" (str (first (rest token))) )))
        logic_definition  (rest (first(re-seq #"^\s*for\s*\(.+\;(.+)\;.+\)(\s*)\{?\s*$" (str (first (rest token))) )))
        increaser_definition  (rest (first(re-seq #"^\s*for\s*\(.+\;.+\;(\s*)(\w*)(\s*[=])(.*)\)(\s*)\{?\s*$" (str (first (rest token))) )))
      ]

    (is_FOR_TOKENIZATION string_list [] 0 variable_definition logic_definition increaser_definition)    
  )
)

;-------------------------------  Lexic Analizer  ------------------------------------
(defn GetToken [list]
  (if (empty? list)
    :NONE
    (if (keyword? (first list))
        (first list)
        (GetToken (first list))
    )
  )
)


(defn LexicAnalizer [tokensBase NewTokenizer index length LastTokenBase LastToken NumBrackets]
  ;(print NumBrackets)
  (if (>= index length)
        NewTokenizer

    (let 
      [
        token (nth tokensBase index)
        updatedTokenizer (if (or (= LastTokenBase :FOR) (= LastTokenBase :WHILE) (= LastTokenBase :FUNCTION))
                              (cond
                                (and (= (first token) :OPENER) (not= LastToken :ERROR)) (conj NewTokenizer token)
                                :else (conj NewTokenizer [:ERROR  (first (rest token))])
                              )
                            (cond
                                (= (first token) :COMMENT) (conj NewTokenizer (is_COMMENT token))
                                (= (first token) :VARIABLE) (conj NewTokenizer (is_VARIABLE token ))
                                (= (first token) :PRINT) (conj NewTokenizer (is_PRINT token ))
                                (= (first token) :WHILE) (conj NewTokenizer (is_While token ))
                                (= (first token) :FUNCTION) (conj NewTokenizer (is_FUNCTION token ))
                                (= (first token) :FOR) (conj NewTokenizer (is_FOR token ))
                                (= (first token) :NEWLINE) (conj NewTokenizer  token )
                                (and (= (first token) :CLOSER) (> NumBrackets 0) (>= (- NumBrackets 1) 0)) (conj NewTokenizer token)
                                :else (conj NewTokenizer [:ERROR  (first (rest token))])
                              )
                         )
        BracketsCounter (if (and (= (first token) :OPENER) (or (= LastTokenBase :FOR) (= LastTokenBase :WHILE) (= LastTokenBase :FUNCTION)) (not= LastToken :ERROR))
                            (+ NumBrackets 1)
                            (if (and (= (first token) :CLOSER) (>= (- NumBrackets 1) 0))
                              (- NumBrackets 1)
                              NumBrackets

                            )
                        )                              
      ]
      (LexicAnalizer tokensBase updatedTokenizer (inc index) length  (first token) (GetToken (last updatedTokenizer)) BracketsCounter)
    )
  )
)

(defn TokensToHTML [tokenlist NewString]
   (if (empty? tokenlist)
      NewString                    
      (let 
         [
         concatString (if (sequential?  (first tokenlist))
                        (TokensToHTML (first tokenlist) NewString)    
                        (if (keyword? (first tokenlist))
                           (if (= (first tokenlist) :NEWLINE)
                              (str NewString " " "<br>")
                              (if (= (first tokenlist) :SPACE)
                                 (str NewString "&nbsp;")
                              
                                (str NewString " " (str "<text class=\"" (subs (str  (first tokenlist)) 1) "\">" (str (first (rest  tokenlist))) "</text>"))                           
                              )
                           )
                           NewString
                        )                       
                     )
         ]
         ;(print concatString)
         (TokensToHTML (rest tokenlist) concatString)
      )
   )
)

                
(defn generate-html [tokens]
  (str "<!DOCTYPE html>\n<html>\n<head>\n<link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC\" crossorigin=\"anonymous\">\n<link rel=\"stylesheet\" href=\"style.css\">\n</head>\n<body>"
       (apply str (map (fn [token]            
                           (str (TokensToHTML token "") "<br>" "\n")                                                                                   
                        ) tokens))
       "</body>\n</html>")
)


(defn process-line [line filename]
  (let [tokens (reduce (fn [acc line] (conj acc (tokenize_complete line))) [] (line-seq line))
        lexic-tokens (LexicAnalizer tokens [] 0 (count tokens))
        output (generate-html lexic-tokens)]
    (spit (str filename ".html") output)
    )
)

(defn process-file [file-path]
   (println file-path)
   (with-open [reader (io/reader file-path)]
      (let [tokens (reduce (fn [acc line] (conj acc (tokenize_complete line))) [] (line-seq reader))
            LexicTokens (LexicAnalizer tokens [] 0 (count tokens) nil nil 0)
            output (generate-html LexicTokens)  
            ]
            (spit (str file-path ".html") output)
      )
   )
)

(defn process-files [file-paths]
  (doall (pmap process-file file-paths)))

;; Ejemplo de uso:
(def file-paths ["archivos/archivo1.txt" "archivos/archivo2.txt" "archivos/archivo3.txt" "archivos/archivo4.txt" "archivos/archivo5.txt" "archivos/archivo6.txt" "archivos/archivo7.txt" "archivos/archivo8.txt" "archivos/archivo9.txt" "archivos/archivo10.txt" "archivos/archivo11.txt" "archivos/archivo12.txt" "archivos/archivo13.txt" "archivos/archivo14.txt" "archivos/archivo15.txt" "archivos/archivo16.txt" "archivos/archivo17.txt" "archivos/archivo18.txt" "archivos/archivo19.txt" "archivos/archivo20.txt" "archivos/archivo21.txt" "archivos/archivo22.txt" "archivos/archivo23.txt" "archivos/archivo24.txt" "archivos/archivo25.txt" "archivos/archivo26.txt" "archivos/archivo27.txt" "archivos/archivo28.txt" "archivos/archivo29.txt" "archivos/archivo30.txt" "archivos/archivo31.txt" "archivos/archivo32.txt" "archivos/archivo33.txt" "archivos/archivo34.txt" "archivos/archivo35.txt" "archivos/archivo36.txt" "archivos/archivo37.txt" "archivos/archivo38.txt" "archivos/archivo39.txt" "archivos/archivo40.txt" "archivos/archivo41.txt" "archivos/archivo42.txt" "archivos/archivo43.txt" "archivos/archivo44.txt" "archivos/archivo45.txt" "archivos/archivo46.txt" "archivos/archivo47.txt" "archivos/archivo48.txt" "archivos/archivo49.txt" "archivos/archivo50.txt" "archivos/archivo51.txt" "archivos/archivo52.txt" "archivos/archivo53.txt" "archivos/archivo54.txt" "archivos/archivo55.txt" "archivos/archivo56.txt" "archivos/archivo57.txt" "archivos/archivo58.txt" "archivos/archivo59.txt" "archivos/archivo60.txt" "archivos/archivo61.txt" "archivos/archivo62.txt" "archivos/archivo63.txt" "archivos/archivo64.txt" "archivos/archivo65.txt" "archivos/archivo66.txt" "archivos/archivo67.txt" "archivos/archivo68.txt" "archivos/archivo69.txt" "archivos/archivo70.txt" "archivos/archivo71.txt" "archivos/archivo72.txt" "archivos/archivo73.txt" "archivos/archivo74.txt" "archivos/archivo75.txt" "archivos/archivo76.txt" "archivos/archivo77.txt" "archivos/archivo78.txt" "archivos/archivo79.txt" "archivos/archivo80.txt" "archivos/archivo81.txt" "archivos/archivo82.txt" "archivos/archivo83.txt" "archivos/archivo84.txt" "archivos/archivo85.txt" "archivos/archivo86.txt" "archivos/archivo87.txt" "archivos/archivo88.txt" "archivos/archivo89.txt" "archivos/archivo90.txt" "archivos/archivo91.txt" "archivos/archivo92.txt" "archivos/archivo93.txt" "archivos/archivo94.txt" "archivos/archivo95.txt" "archivos/archivo96.txt" "archivos/archivo97.txt" "archivos/archivo98.txt" "archivos/archivo99.txt" "archivos/archivo100.txt"])

(time (process-files file-paths))