(ns looping-is-recursion)

(defn power [base exp]
    (let [helper (fn [acc base exp]
                     (if (= exp 1) 
                        acc
                        (recur (* acc base) base (dec exp))
                     )
                 )
         ]
      (if (= exp 0)
           1
          (helper base base exp))
    )
)

(defn last-element [a-seq]
    (let [helper (fn [a-seq]
                   (if (= (count a-seq) 1)
                     (first a-seq)
                     (recur (rest a-seq))
                   )
         )]
      (if (= (count a-seq) 0)
         nil
         (helper a-seq)
      )
    )
)

(defn seq= [seq1 seq2]
   (loop [a-seq seq1
          b-seq seq2]
      (cond 
         (and (empty? a-seq) (empty? b-seq)) true
         (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
         :else false
      )
   )
)

(defn find-first-index [pred a-seq]
   (loop [index 0
          seq1 a-seq]
      (cond 
        (= (count seq1) 0) nil
        (pred (first seq1)) index
        :else (recur (inc index) (rest seq1))
      )
   )
)

(defn avg [a-seq]
   (loop [sum 0
          seq1 a-seq
          n (count a-seq)]
      (if (= (count seq1) 0)
         (/ sum n)
         (recur (+ sum (first seq1)) (rest seq1) n)
      )
   )
)

(defn parity [a-seq]
   (loop [seq1 a-seq
          set1 (set [])]
     (if (empty? seq1)
         set1
         (recur (rest seq1) 
                (if (contains? set1 (first seq1))
                   (disj set1 (first seq1))
                   (conj set1 (first seq1))
                )
         )
     )
   )
)

(defn fast-fibo [n]
   (loop [n-2 0
          n-1 1
          i n]
      (cond
        (= i 0) n-2
        (= i 1) n-1
        :else (recur n-1 (+ n-1 n-2) (dec i))
      )
   )
)

(defn cut-at-repetition [a-seq]
   (loop [seq1 []
          seq2 a-seq]
     (if (or (empty? seq2) (contains? (set seq1) (first seq2))) 
         seq1
         (recur
            (conj seq1 (first seq2))
            (rest seq2)
         )
     )
   )
)