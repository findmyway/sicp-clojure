(ns sicp-clojure.ch1
  (:require [clojure.math.numeric-tower :as math]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            ))


;;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
; -37/150


;;; 1.3
(defn sum-square-larger2
  [a b c]
  (let [square (fn [x] (* x x))]
    (- (reduce + (map square [a b c]))
       (square (min a b c)))))


;;; 1.4
(defn a-plus-abs-b
  "a + b 的绝对值" 
  [a b]
  (if (> b 0)
    (+ a b)
    (- a b)))


;;; 1.5
(defn p []
  (p))

(defn judge-interpreter [x y]
  (if (= x 0)
    0
    y))
; 如果是applicative-order, 也就是“由内向外”的解释器，
; 会先执行p表达式，递归调用自己最后stackoverflow
; 如果是normal-order， 也就是逐步展开求值，
; 那么会先进行if判断，返回0


;;; 1.6
; 由于clojure中是先展开，后求解，所以new-if会导致
; 死循环，一直在展开sqrt-iter
; 本质区别在于，if是“短路”的，而cond不是


;;; 1.7
; ref: http://www.billthelizard.com/2009/10/sicp-exercises-16-18.html
; 对于较小的数，计算平方根的时候虽然平方后的结果达到了精确度
; 但是，平方根与准确值的差会远高于容忍值。
; 对于较大的数，可能永远达不到要求的精度，陷入无限循环
; 采用比例的方法对很大或者很小的数应该都是work的


;;; 1.8
(defn cube-root
  "用1.7提到的终止条件"
  ([guess x]
   (let [next-guess 
         (double (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
         good-enough? 
         (fn [guess next-guess]
           (< (/ (math/abs (- guess next-guess)) guess)
              0.001))]
     (if (good-enough? guess next-guess)
       next-guess
       (cube-root next-guess x))))
  ([x]
   (cube-root 1.0 x)))


;;; 1.9
(defn plus-1
  "递归"
  [a b]
  (if (= a 0)
    b
    (inc (plus-1 (dec a) b))))

(defn plus-2
  "迭代"
  [a b]
  (if (= a 0)
    b
    (plus-2 (dec a) (inc b))))


;;; 1.10
(defn A
  [x y]
  (cond 
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1)
             (A x (- y 1)))))

; (f n) -> $2*n$
; (g n) -> $2^n$
; (h n) -> $2^{2^n}$


;;; 1.11
(defn f-rec
  "递归版本"
  [n]
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(defn f-iter
  "迭代版本"
  [n]
  (if (< n 3)
    n
    (loop [a 0 b 1 c 2 counts 2]
      (if (= counts n)
        c
        (recur b 
               c 
               (+ c (* 2 b) (* 3 a))
               (inc counts))))))


;;; 1.12
; ref: https://github.com/zzp-me/_posts/blob/master/2011-09-22-pascal-triangle.md
(defn pascal []
  (iterate #(map + `(0 ~@%) `(~@% 0)) [1]))

(take 5 (pascal)) ; 取前面5行
; ->((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))


;;; 1.13
; 需要注意的是，其中$\gamma=\(1-\sqrt{5}\)/2$
; 归纳法即可得出fib的表达式，
; 再证， $\gamma^{n}/\sqrt{5}$与1/2的关系


;;; 1.16

(defn square [x]
  (* x x))

(defn fast-expt-iter
  [b n]
  (letfn [(help-f
          [a b n]
          (cond (= n 0) a
                (even? n) (help-f a (square b) (/ n 2))
                :else (help-f (* a b) b  (dec n))))]
    (help-f 1 b n)))


;;; 1.17

(defn halve-f [x] (/ x 2))

(defn double-f [x] (* x 2))

(defn fast-mul-rec
  [a b]
  (cond (= 1 a) b
        (= 1 b) a
        (even? b) (double-f (fast-mul-rec a (halve-f b)))
        :else (+ a (fast-mul-rec a (dec b)))))


;;; 1.18

(defn fast-mul-iter
  [a b]
  (letfn [(help-f
            [a b r]
            (cond (= 0 b) r
                  (even? b) (help-f (double-f a) (halve-f b) r)
                  :else (help-f a (dec b) (+ a r))))]
    (help-f a b 0)))


;;; 1.19

; $p'=(p^2+q^2)$
; $q'=(2pq+q^2)$ 


(defn fib-iter
  [a b p q counts]
  (cond (= 0 counts) b
        (even? counts) (fib-iter 
                         a
                         b 
                         (+ (square p)
                            (square q))
                         (+ (* 2 p q)
                            (square q))
                         (/ counts 2))
        :else (fib-iter
                (+ (* b q)
                   (* a q)
                   (* a p))
                (+ (* b p)
                   (* a q))
                p 
                q
                (- counts 1))
        ))


;;; 1.20
; normal order 要先展开后eval， 7*2 + 4
; applicative-order 边展开边eval， 4次即可
;(gcd 206 40) 
 
;(gcd 40 (remainder 206 40)) 
 
;(gcd 40 6) 
 
;(gcd 6 (remainder 40 6)) 
 
;(gcd 6 4) 
 
;(gcd 4 (remainder 6 4)) 
 
;(gcd 4 2) 
 
;(gcd 2 (remainder 4 2)) 
 
;(gcd 2 0) 
 
;2 


;;; 1.21
(defn smallest-divisor 
  [n]
  (letfn [(divides? 
            [a b]
            (= (rem a b) 0))
          (find-divisor 
            [n test-divisor]
            (cond (> (square test-divisor) n) n
                  (divides? n test-divisor) test-divisor
                  :else (find-divisor n (inc test-divisor))))]
    (find-divisor n 2)))

(smallest-divisor 199)  ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999)  ; 7

;;; 1.22
; 需要注意的是，smallest-divisor的方法在
; 数很大的时候会出现StackOverflow的错误

(defn prime? 
  [n]
  (= (smallest-divisor n) n))))

(defn start-prime-test 
  [n start-time]
  (if (prime? n)
    (do 
      (time (prime? n))
      (prn " *** ")
      (prn n)
      n)
    ))

(defn search-for-primes 
  [a b]
  (let [a (if (even? a) (inc a) a)
        b (if (even? b) (dec b) b)
        search-iter 
        (fn search-iter [cur lst]
          (if (< cur lst)
            (do
              (start-prime-test cur (c/to-long (t/now)))
              (search-iter (+ 2 cur) lst))))]
    (search-iter a b)))


;;; 1.23
(defn next-n
  [test-divisor]
  (if (= test-divisor 2)
    3
    (+ 2 test-divisor)))


;;; 1.24
; 显然，2倍的时间


;;; 1.25
; 注释46有解释，这样可以避免对一个很大的数求余


;;; 1.26
; 这会导致树形展开，计算量cheng呈指数增长


;;; 1.27

(defn expmod
  [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (rem (square (expmod base (/ exp 2) m))
             m)
        :else 
        (rem (* base (expmod base (dec exp) m))
             m)))

(defn fermat-check?
  [x n]
  (= (expmod x n n) x))

(defn cheat-fermat-test?
  [n]
  (= 
    ; 测试某个数符合费马小定理
    (true? 
       (every? true? (map fermat-check? (range 1 n) (repeat n))))
    ; 同时这个数并不是素数
     (false?
       (prime? n))) )

; (map cheat-fermat-test? [561 1105 1729 2465 2821 6601])


;;; 1.28

; 这个有点复杂，没理解怎么不会受骗的。。。
; 具体代码看这里吧 
; https://github.com/gregsexton/SICP-Clojure/blob/master/src/sicp/ch1.clj#L176-L201


