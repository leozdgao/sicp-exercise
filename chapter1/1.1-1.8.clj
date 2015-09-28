;; *习题1.1*

;; 省略，直接通过REPL观察结果即可

;; *习题1.2*

(defn ex-1.2 []
  (/ (+ 5
        4
        (- 2 (- 3 (+ 6 ( / 4 5)))))
     (* 3
        (- 6 2)
        (- 2 7))))

;; *习题1.3*

;; 听说原版应该是平方和，这里就不改了

(defn big2sumof3 [a b c]
  (cond
    (and (<= a b) (<= a c)) (+ b c)
    (and (<= b a) (<= b c)) (+ a c)
    (and (<= c a) (<= c b)) (+ a b)))

;; *习题1.4*

;; 该过程的行为：返回 a + |b|

;; *习题1.5*

;; 由于 (define (p) (p)) 的关系，对其求值将进入无限循环。对于应用序而言，需要对每个运算对
;; 象(operand)求值，故在执行 (test 0 (p)) 时对(p)求值，从而进入无限循环，最终导致堆栈溢
;; 出。对于正则序而言则是『展开后规约』，故展开后 (= 0 0) 判断为真，并不会求值 (p) ，所以
;; 可以得到结果0

;; *习题1.6*

;; if 是一种特殊形式，通过谓词的判断，仅一个运算对象会被求值，然而 new-if 是一个常规过程，对
;; 于使用应用序的Lisp而言，会先对每个运算对象求值。

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) ;; 应用序的Lisp始终会对它求值，导致无限递归，最
                     x)))              ;; 终堆栈溢出

;; *习题1.7* *习题1.8*

;; 首先good-enough? 中的比较参考值为0.0001，对于较大浮点数的比较，可能永远都大于0.0001，对
;; 于较小浮点数的比较，可能直接就小于0.0001，导致返回错误的结果（推荐参考下关于计算机如何表示
;; 浮点数的部分）

;; 使用这个参考值不太好，而后面提出的使用比率，那么比率是多少才算近似值？也是需要一个参考值的，
;; 上面的大数和小数的问题依然存在

;; 给出一个更通用的解法

(defn good-enough? [guess oldguess x]
  (< (abs (- oldguess guess))
     (* guess 0.0001)))

(defn sqrt-improve [guess x]
  (/ (+ guess (/ x guess)) 2)

(defn cbrt-improve [guess x]
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(defn root-iter [guess x improve]
  (let [better (improve guess x)]
    (if (good-enough? better guess x)
      better
      (root-iter better x improve))))

;; 求平方根
(defn sqtr [n]
  (root-iter 1.0 n sqrt-improve))

;; 求立方根
(defn cbrt [n]
  (root-iter 1.0 n cbrt-improve))
