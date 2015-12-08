(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp current]
                 (if (zero? exp)
                   current
                   (recur base (dec exp) (* base current))
                  ))]
    (helper base exp 1)))

(defn last-element [a-seq]
    (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                   (recur (rest a-seq))
                  ))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq status]
    (cond
     (and (empty? a-seq) (empty? b-seq)) status
     (or (empty? a-seq) (empty? b-seq)) false
     :else (recur (rest a-seq) (rest b-seq) (and status (= (first a-seq) (first b-seq))))
     )
  )]
  (helper seq1 seq2 true)))

(defn find-first-index [pred a-seq]
  (loop [p pred
         lseq a-seq
         i 0]
    (cond
       (empty? lseq) nil
       (p (first lseq)) i
       :else (recur p (rest lseq) (inc i)))))

(defn avg [a-seq]
  (loop [sum 0
         div 0
         lseq a-seq]
    (cond
      (empty? lseq) (/ sum div)
      :else (recur (+ sum (first lseq)) (inc div) (rest lseq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)
  )
)
(defn parity [a-seq]
  (loop [a #{}
         lseq a-seq]
    (if (empty? lseq) a
        (recur (toggle a (first lseq)) (rest lseq)))))

(defn fast-fibo [n]
  (loop [fn1 0
         fn2 1
         x 2]
    (cond
     (= n 0) fn1
     (= n 1) fn2
     (= x n) (+ fn1 fn2)
     :else (recur fn2 (+ fn1 fn2) (inc x)))))

(defn cut-at-repetition [a-seq]
  (loop [a #{}
         acc []
         lseq a-seq]
    (cond
     (or (empty? lseq) (a (first lseq)))  acc
      :else (recur (conj a (first lseq)) (conj acc (first lseq)) (rest lseq))
     )))

