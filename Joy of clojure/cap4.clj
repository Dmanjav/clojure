(ns cap4)

(float 0.0000000000000000000000000000000000000000000001)
;Imprime 0 debido a su tamaño tan pequeño
;Esto se llama Underflow

(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)

(+ (+ a b) c)
(+ a (+ b c))
;Los resultados anteriores deberían de ser 17 pero,
;uno es 17 y el segundo es 0

;;Para evitar lo anterior se deben usar números racionales,
;usando rationalize
(def a (rationalize 1.0e50))
(def b (rationalize -1.0e50))
(def c (rationalize 17.0e00))
;;Resultado: 17N, ES CORRECTO!!!
(+ (+ a b) c)
(+ a (+ b c))

(def population {:zombies 2700, :humans 9})
(println (/ (get population :zombies)
            (get population :humans))
         "zombies per capita")

;;Símbolos
;un símbolo con el mismo nombre no es igual al otro,
;no es el mismo objeto y tiene otra info asociada
;las Keywords siempre son el mismo objeto

;;Las kw y los simbs no pertenecen específicamente a un ns

(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y)
   (identical? x y)
   (meta x)
   (meta y)])

