;; 习题*1.9*

;; 第一种为递归，第二种为迭代

;; 习题*1.10*

;; (A 1 10) 1024
;; (A 2 4) 65536
;; (A 3 3) 65536

;; (defn f [n] (A 0 n))代表2n
;; (defn g [n] (A 1 n))代表2^n
;; (defn h [n] (A 2 n))代表2^2^2^...（n次）
