(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (cond
                  (zero? k) acc
                  :else (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc coll]
        (if (empty? coll)
          acc
          (recur (first coll) (rest coll))))]
    (helper (first a-seq) a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc a b]
                 (cond
                   (not (= (first a) (first b))) acc
                   (and (empty? a) (empty? b)) true
                   (or (empty? a) (empty? b)) acc
                   :else (recur acc (rest a) (rest b))
                  )
                 )]
    (helper false seq1 seq2)))

;(defn find-first-index [pred a-seq]
;  (let [helper (fn [acc pred aseq]
;                 (cond
;                   (empty? aseq) nil
;                   (pred (first aseq)) acc
;                   :else (recur (inc acc) pred (rest aseq))
;                 ))]
;    (helper 0 pred a-seq)))

(defn find-first-index [pred a-seq]
  (loop [acc  0
         aseq a-seq]
    (cond
     (empty? aseq) nil
     (pred (first aseq)) acc
     :else (recur (inc acc) (rest aseq)))))

(defn avg [a-seq]
  (loop [n    0
         sum  0
         aseq a-seq]
    (if (empty? aseq)
      (/ sum n)
      (recur (inc n)
             (+ sum (first aseq))
             (rest aseq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [n          0
         aseq       a-seq
         return     #{}]
    (cond
     (empty? aseq) return
     :else (recur (inc n)
                  (rest aseq)
                  (toggle return (first aseq))))))

(defn fast-fibo [n]
  (loop [Fn   n
         Fn-1 1
         Fn-2 0]
    (cond
     (= Fn 0) Fn-2
     (< Fn 2) Fn-1
     :else (recur (dec Fn) (+ Fn-2 Fn-1) Fn-1))))


(defn cut-at-repetition [a-seq]
  (loop [aseq  a-seq
         found []]
    (cond
     (empty? aseq) a-seq
     (contains? (set found) (first aseq)) found
     :else (recur (rest aseq) (conj found (first aseq))))))

