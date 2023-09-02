(ns resaltador)

(json->html
  [file-name]
  (tokenize (slurp file-name)))

(def er #"(?xi)
    ( \d )                           # Grupo 1: Números
  | ( [a-z])                         # Grupo 2: Cadenas de caracteres
  | ( null | true | false)           # Grupo 3: Booleanos o null
  | ( )                              # Grupo 4: Llave que abre
  | ( )                              # Grupo 5: Llave que cierra
  | ( [(] )                          # Grupo 6: Paréntesis que abre
  | ( [)] )                          # Grupo 7: Paréntesis que cierra
  | ( \s )                           # Grupo 8: Espacios
  | ( . )                            # Grupo 9: Carácter inválido
")

(defn tokenize
  [input]
  (map (fn [token]
         (cond
           (token 1) [:numero (token 0)]
           (token 2) [:cadena (token 0)]
           (token 3) [:booleano (token 0)]
           (token 4) [:llave-izq (token 0)]
           (token 5) [:llave-der (token 0)]
           (token 6) [:par-der (token 0)]
           (token 7) [:par-izq (token 0)]
           (token 8) [:espacio (token 0)]
           (token 9) [:invalido (token 0)]))

       (remove (fn [v] (v 13)) (re-seq er input))))