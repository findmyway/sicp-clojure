(ns sicp-clojure.ch2
  (:require [sicp-clojure.ch1 :refer [gcd]]))


;;; 2.1
(defn make-rat
  [n d]
  (let [g (Math/abs (gcd n d))
        gg (if (neg? d)
             (* -1 g)
             g)]
    [(/ n gg) (/ d gg)]))


;;; 2.2
(defn make-point
  [x y]
  [x y])

(defn x-point
  [p]
  (first p))

(defn y-point
  [p]
  (second p))

(defn make-segment
  [p1 p2]
  [p1 p2])

(defn start-segment
  [s]
  (first s))

(defn end-segment
  [s]
  (second s))

(defn mid-point-segment
  [s]
  (let [start-s (start-segment s)
        end-s (end-segment s)
        x1 (x-point start-s)
        x2 (x-point end-s)
        y1 (y-point start-s)
        y2 (y-point end-s)]
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))


;;; 2.3
; 题目的意思应该是
; 1. 用4个线段表示一个矩形
; 2. 用2个对角点表示一个矩形


