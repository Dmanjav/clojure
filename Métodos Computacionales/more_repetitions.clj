;----------------------------------------------------------
; Problem Set #3: More Repetitions
; Date: March 24, 2023.
; Authors:
;          A01754917 Eduardo Alfredo Ramirez Muñoz
;          A01753486 Diego Manjarrez Viveros
;----------------------------------------------------------
(ns more-repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))

;; Problem 1
(defn expand
  ;; Esta funcion expande la secuencia recibida, repitiendo
  ;; el numero de veces en el indice que el elemento se encuentre.
  [s]
  (mapcat #(repeat %2 %1)
          s
          (range 1 (inc (count s)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))


;; Problem 2
(defn insert
  ;; Esta funcion inserta un elemento en una lista de manera que
  ;; quede ordenada dicha lista.
  [n s]
  (let [[menores mayores] (split-with #(< % n) s)]
    (concat menores (list n) mayores)))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))


;; Problem 3
(defn insertion-sort
  ;; Esta funcion regresa una secuencia ordenada.
  [s]
  (reduce #(insert %2 %1)
          ()
          s))

(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))


;; Problem 4
(defn rotate-left [n s]
  ;; Esta funcion regresa una secuncia con la rotacion de sus elementos
  ;; de acuerdo al nuumero que se le indique
  (if (= 0 (count s))
    s
    (let [contador (mod n (count s))]
      (concat (drop contador s) (take contador s)))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

;; Problem 5
(defn binary
  ;; Esta funcion regresa el numero pero en binario.
  [n]
  (loop [n n
         r ()]
    (if (zero? n)
      r
      (recur (quot n 2)
             (cons (rem n 2) r)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))


;; Problem 6
(defn prime-factors
  ;; Esta funcion regresa una lista con los numeros que dividen al
  ;; numero recibido de manera exacta.
  [n]
  (if (= n 1)
    ()
    (loop [n n
           r ()
           i 2]
      (cond
        (= n i) (reverse (cons i r))
        (zero? (rem n i)) (recur (quot n i)
                                 (cons i r)
                                 i)
        :else (recur n
                     r
                     (inc i))))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))


;; Problem 7
(defn gcd
  ;; Esta funcion regresa el maximo comun divisor de los numeros recibidos
  [a b]
  (first (drop-while (fn [x] (not (and (zero? (rem a x))
                                       (zero? (rem b x)))))
                     (if (< a b)
                       (range a 0 -1)
                       (range b 0 -1)))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))


;; Problem 8
(defn insert-everywhere
  ;; Esta funcion te regresa varias listas con
  ;; todas las posibles formas de insertar la x
  [x s]
  (map (fn [i]
         (let [[a b] (split-at i s)]
           (concat a (list x) b)))
       (range (inc (count s)))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))


;; Problem 9
(defn contains-all-digits?
  ;; Esta funcion regresa un booleano dependiendo
  ;; si el numero ingresado tiene todos los numeros (0-9)
  [s]
  (= 10 (count (distinct (seq (str (abs s)))))))

(deftest test-contains-all-digits?
  (is (contains-all-digits? 1023456789))
  (is (contains-all-digits? 5897230146))
  (is (contains-all-digits? 10123485679))
  (is (contains-all-digits?
        1223334444555566666677777778888888889999999990))
  (is (not (contains-all-digits? 1236)))
  (is (not (contains-all-digits? 1112223334455)))
  (is (not (contains-all-digits? -587230462413578)))
  (is (not (contains-all-digits?
             -122333444455556666667777777888888888999999999))))


;; Problem 10
(defn pack
  ;; Esta funcion regresa una lista de listas, cada lista
  ;; cuenta con su caracter propio, siempre y cuando
  ;; esten "juntos" dentro de la secuencia
  [s]
  (partition-by identity s))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))


;; Problem 11
(defn compress
  ;; Esta funcion solamente agarra el primer elemento de cada caracter
  ;; repetido
  [s]
  (map first (pack s))
  )

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))


;; Problem 12
(defn encode
  ;; Esta funcion te regresa un vector con el numeor de veces que
  ;; se repite el caracter en la secuencia, siempre y cuando
  ;; estos caracteres esten juntos dentro de la secuencia
  [s]
  (map (fn [x]
         [(count x) (first x)])
       (pack s)))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))


;; Problem 13
(defn encode-modified
  ;; Esta funcion hace lo mismo que la anterior pero
  ;; si se encuentra con un caracter sin repetirse, lo ignora y lo agrega
  ;; sin su numero correspondiente.
  [s]
  (map (fn [x]
         (if (> (count x) 1)
           [(count x) (first x)]
           (first x)))
       (pack s)))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))


;; Problem 14
(defn decode
  ;; Esta función hace lo contrario que la anterior.
  [x]
  (mapcat (fn [s]
               (if (vector? s)
                 (repeat (first s) (second s))
                 (list s)))
          x))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))


(run-tests)