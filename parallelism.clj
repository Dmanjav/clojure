(ns parallelism)

; Problem 1
;
; SPEED UP CALCULATION
;
; n = 100,000
; p = 8
;
; Run #1   T1 = 2458.9417    Tp = 232.4873
; Run #2   T1 = 2428.2642    Tp = 229.2616
; Run #3   T1 = 2438.3945    Tp = 229.1198
; Run #4   T1 = 2467.8919    Tp = 228.0468
; Run #5   T1 = 2439.5845    Tp = 225.4587
; Average  T1 = 2446.21536   Tp = 228.07484
;
; Sp = T1/Tp = 17.91375695

(defn bits
  [x]
  (.bitCount (biginteger x)))

(defn fact-seq
  [n]
  (bits (loop [i 2
               r 1]
          (if (> i n)
            r
            (recur (inc i) (*' r i))))))

(defn fact-ranges
  [n p]
  (partition 2 1 (concat (range 1 n (quot n p))
                         [(inc n)])))

(defn fact-partial
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i) (*' r i)))))

(defn fact-par
  [n]
  (as-> (.availableProcessors (Runtime/getRuntime)) RESULT
        (fact-ranges n RESULT)
        (pmap fact-partial RESULT)
        (reduce *' RESULT)
        (bits RESULT)))

(time (fact-seq 100000))

(time (fact-par 100000))

;--------------------------------------------------------------------
; Problem 4
;
; SPEED UP CALCULATION
;
; n = 200,000
; p = 8
;
; Run #1   T1 = 2628.4255    Tp = 649.2311
; Run #2   T1 = 2747.8792    Tp = 597.3755
; Run #3   T1 = 2655.4959    Tp = 609.9067
; Run #4   T1 = 2678.2785    Tp = 586.7254
; Run #5   T1 = 2684.9212    Tp = 555.2386
; Average  T1 = 2678.98006   Tp = 599.75546
;
; Sp = T1/Tp = 4.466787280269195

(defn create-random-data
  [n]
  (repeatedly n #(rand-int 1000)))

; (apply <= (sort (create-random-data 10000)))

(defn insertion-sort
  [s]
  (loop [s s
         r ()]
    (if (empty? s)
      r
      (let [x (first s)
            [before after] (split-with #(< % x) r)]
        (recur (rest s)
               (concat before [x] after))))))

; (apply <= (insertion-sort (create-random-data 100)))

(defn merge-algorithm
  [a b]
  (loop [a a
         b b
         r ()]
    (cond
      (empty? a)
      (concat (reverse r) b)

      (empty? b)
      (concat (reverse r) a)

      (< (first a) (first b))
      (recur (rest a)
             b
             (cons (first a) r))

      :else
      (recur a
             (rest b)
             (cons (first b) r)))))

; (merge-algorithm [1 2 5 6 8 9 10] [1 2 3 4 4 4 5 7 8])

(defn hybrid-sort-seq
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-algorithm (hybrid-sort-seq a)
                       (hybrid-sort-seq b)))))

(defn hybrid-sort-par
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm
             (pmap hybrid-sort-par splitted)))))


(def n 200000)
(def random-data (create-random-data n))
(apply <= random-data)
(apply <= (time (hybrid-sort-seq random-data)))
(apply <= (time (hybrid-sort-par random-data)))
(apply <= (time (sort random-data)))


(defn compute-pi [n]
  (let [width (/ 1.0 n)]
    (loop [i 0
           sum 0.0]
      (if (< i n)
        (let [mid (* (+ i 0.5) width)
              height (/ 4.0 (+ 1.0 (* mid mid)))]
          (recur (inc i) (+ sum height)))
        (* width sum)))))

(compute-pi 1000000)
