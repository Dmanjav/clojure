(ns repetitions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]])
  (:require [clojure.algo.generic.math-functions
             :refer [sqr approx=]]))

;; Recursive version
;(defn enlist
;  [x]
;  (if (empty? x)
;    x
;    (cons (list (first x))
;          (enlist (rest x)))))

;; Loop/recur version
;(defn enlist
;  [x]
;  (loop [y x
;         r ()]
;    (if (empty? y)
;      (reverse r)
;      (recur (rest y)
;             (cons (list (first y))
;                   r)))))

;; Problem 1
;; Sequence API version
(defn enlist
  [x]
  (map list x))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

;; Problem 2
(defn positives
  [s]
  (filter pos? s))

(deftest test-positives
  (is (= () (positives ())))
  (is (= () (positives [-4 -1 -10 -13 -5])))
  (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
  (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))

;; Problem 3
(defn add-squares
  [s]
  (reduce #(+ %1 (sqr %2)) 0 s))

(deftest test-add-squares
  (is (= 0 (add-squares [])))
  (is (= 25 (add-squares [5])))
  (is (= 30 (add-squares [2 4 1 3])))
  (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))

;; Problem 4
(defn duplicate
  [s]
  (mapcat #(list % %) s))

(deftest test-duplicate
  (is (= [1 1 2 2 3 3 4 4 5 5]
         (duplicate [1 2 3 4 5])))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

;; Problem 5
;(defn fib
;  [n]
;  (loop [a 0
;         b 1
;         i 0]
;    (if (= i n)
;      a
;      (recur b (+' a b) (inc i)))))

;(defn fib
;  [n]
;  (first (first (drop n
;                      (iterate (fn [[a b]]
;                                 [b (+' a b)])
;                               [0 1])))))

(defn fib
  [n]
  (as-> [0 1] result
        (iterate (fn [[a b]] [b (+ a b)]) result)
        (drop n result)
        (first result)
        (first result)))


(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
          987 1597 2584 4181 6765]
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

;; Problem 6
(defn pow
  [a b]
  (reduce *' (repeat b a)))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22)))
  (is (= 1267650600228229401496703205376N (pow 2 100))))

;; Problem 7
(defn only-symbols?
  [s]
  (every? symbol? s))

(deftest test-only-symbols?
  (is (= true (only-symbols? [])))
  (is (= true (only-symbols? '(a))))
  (is (= true (only-symbols? '(a b c d e))))
  (is (= false (only-symbols? '(a b c d 42 e))))
  (is (= false (only-symbols? '(42 a b c))))
  (is (= false (only-symbols? [4 8 15 16 23 42]))))

;; Problem 8
(defn invert-pairs
  [s]
  (map (fn [[a b]] [b a]) s))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([y x]) (invert-pairs '([x y]))))
  (is (= '([1 a][2 a][1 b][2 b])
         (invert-pairs '([a 1][a 2][b 1][b 2]))))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))
;; Problem 9

(defn replic
  [n s]
  (flatten (map #(repeat n %)s))
  )

(replic 5 [1 2 3])

(deftest test-replic
  (is (= () (replic 7 [])))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]
         (replic 4 [1 2 3 4]))))





;; Problem 10

(defn dot-product
  [a b]
  (reduce + (map * a b)))

(deftest test-dot-product
  (is (= 0 (dot-product [] [])))
  (is (= 42 (dot-product [6] [7])))
  (is (= 32 (dot-product [1 2 3] [4 5 6])))
  (is (= 21.45 (dot-product [1.3 3.4 5.7 9.5 10.4]
                            [-4.5 3.0 1.5 0.9 0.0]))))
;; Problem 11
(defn average
  [s]
  (if (empty? s)
    nil
    (/ (reduce + s)
             (count s))))

(deftest test-average
  (is (nil? (average [])))
  (is (= 4
         (average [4])))
  (is (= 3
         (average [5 6 1 6 0 1 2])))
  (is (= 2.5
         (average [1.7 4.5 0.0 2.0 3.4 5.0 2.5 2.2 1.2]))))

;; Problem 12
(defn standard-deviation
  [s]
  (if (empty? s)
    nil
    (let [avg (average s)]
      (sqrt (/ (reduce + (map #(sqr (- % avg)) s))
               (count s))))))

(deftest test-standard-deviation
  (is (nil? (standard-deviation [])))
  (is (approx= 1.87
               (standard-deviation [6 2 3 1])
               0.01))
  (is (approx= 12.3153
               (standard-deviation [4 8 15 16 23 42])
               0.0001))
  (is (approx= 7.07106
               (standard-deviation [110 105 90 100 95])
               0.00001))
  (is (approx= 2.983
               (standard-deviation [9 2 5 4 12 7 8 11
                                    9 3 7 4 12 5 4 10
                                    9 6 9 4])
               0.001)))

(run-tests)

