(ns closhal.core
(:require [clojure.pprint :as pp]
          [clojure.test :as t]
          [clojure.spec.alpha :as s])
(:use     [clojure.java.io :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn i [a f & s]
  (apply f a s))

(defn key->str
  [k]
  (-> k
      str
      (subs 1)))

(defn wr
  ([w x]
   (.write w x))
  ([w x & xs]
   (wr w x)
   (apply wr w xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPES

(s/def ::poses #{:f :m :e :s})
(s/def ::char char?)

(defrecord SChar [char pos])
(defn schar [c p]
  {:pre [(s/valid? ::char c)
         (s/valid? ::poses p)]}
  (->SChar c p))
(s/def ::schar #(instance? SChar %))

(defn seq->sstr
  ([x]
   {:pre [(s/valid? (s/coll-of ::schar) x)]}
  (with-meta x {:type :SStr}))
  ([x & xs]
   (seq->sstr (apply list x xs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTES

(defmethod print-method SChar
  [x w]
  (pp/pprint {:char (-> x :char str symbol)
              :pos  (-> x :pos key->str symbol)}
             w)
  )
  #_(wr w
        ":char "
        (str (:char x))
        " :pos "
        (key->str (:pos x)))
  ;)

(defmethod pp/simple-dispatch SChar
  [x]
  (print x))


(defmethod print-method :SStr
  [x w]
  (print-method {:str (reduce (fn [a b] (.concat a b))
                              (for [s x] (str (:char s))))
                 :pos (reduce (fn [a b] (.concat a b))
                              (for [s x] (key->str (:pos s))))
                 }
                w)
)
  #_(apply wr
         w
         ":str "
         (for [s x]
           (str (:char s))))
  #_(apply wr w "\n:pos "
         (for [s x]
           (key->str (:pos s))))
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASES

(def x (SChar. \a :s))
(def xxx (seq->sstr [x x x x x]))

(defn -main []

  )