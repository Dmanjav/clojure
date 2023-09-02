(ns analisis_lexico)
;(slurp "entrada.txt")

(def er #"(?xi)
     (-? \d+ \. \d* (?: e -? \d+)? )    # Grupo 1: Flotante
   | ([a-z] \w* )                       # Grupo 2: Variables
   | ( // .*)                           # Grupo 3: Comentarios
   | ( \d+)                             # Grupo 4: Enteros
   | ( [=] )                            # Grupo 5: Asignación
   | ( [+] )                            # Grupo 6: Suma
   | ( [-] )                            # Grupo 7: Resta
   | ( [*] )                            # Grupo 8: Multiplicación
   | ( [/] )                            # Grupo 9: División
   | ( \^ )                             # Grupo 10: Potencia
   | ( [(] )                            # Grupo 11: Paréntesis que abre
   | ( [)] )                            # Grupo 12: Paréntesis que cierra
   | ( [ \s ] )                         # Grupo 13: Espacios
   | ( . )                              # Grupo 14: Caracter inválido
")

;(remove (fn [v] (v 13) ) (re-seq er "x = 3.1416"))

(defn tokenize
  [input]
  (map (fn [token]
         (cond
           (token 1) [:flotante (token 0)]
           (token 2) [:variable (token 0)]
           (token 3) [:comentarios (token 0)]
           (token 4) [:enteros (token 0)]
           (token 5) [:asignacion (token 0)]
           (token 6) [:suma (token 0)]
           (token 7) [:resta (token 0)]
           (token 8) [:multiplicacion (token 0)]
           (token 9) [:division (token 0)]
           (token 10) [:potencia (token 0)]
           (token 11) [:paren-abre (token 0)]
           (token 12) [:pare-cierra (token 0)]
           (token 14) [:error (token 0)]))
       (remove (fn [v] (v 13)) (re-seq er input))))

(defn tokenize-file
  [file-name]
  (tokenize (slurp file-name)))n

(tokenize-file "entrada.txt")