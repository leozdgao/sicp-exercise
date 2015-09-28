;; 习题*1.29*

;; (integral cube 0 1.0 100.0) 得 0.25333333333333324
;; (integral cube 0 1.0 1000.0) 得 0.2503333333333336

(defn integral [f a b n]
  (def h (/ (- b a) n))
  (defn factor [k]
    (cond (= k 0) 1
          (= k n) 1
          (even? k) 2
          (odd? k) 4))
  (defn y [k]
    (* (factor k)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum y 0 inc n)))

;; 习题*1.30*

;; 线性迭代版本的sum

(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; 习题*1.34*

;; 会报错 【java.lang.Long cannot be cast to clojure.lang.IFn】
;; 第一次调用(f f)，在过程体内再次调用(f 2)，导致2无法被当做一个过程调用，于是报错
