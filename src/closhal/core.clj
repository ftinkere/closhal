(ns closhal.core
(:require [clojure.pprint :as pp]
          [clojure.test :as t]
          [clojure.spec.alpha :as s])
(:use     [clojure.java.io :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

;; infix form
(defn i [a f & s]
  (apply f a s))

;; :key -> "key"
(defn key->str
  [k]
  (-> k
      str
      (subs 1)))

;; alias for .write for Writer w with any number of strings to be write
(defn wr
  ([w x]
   (.write w x))
  ([w x & xs]
   (wr w x)
   (apply wr w xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPES

;; all possible positions of schar
(s/def ::poses #{:f :m :e :s})
(s/def ::char char?)

;; SChar type, store char and pos of it in the word
(defrecord SChar [char pos])
(defn schar [c p]                                           ;; constructor
  {:pre [(s/valid? ::char c)
         (s/valid? ::poses p)]}
  (->SChar c p))
(s/def ::schar #(instance? SChar %))                        ;; spec for check

;; Add meta type of SStr to seq of SChars, can attempt one seq or elements, do not mix
(defn seq->sstr
  ([x]
   {:pre [(s/valid? (s/coll-of ::schar) x)]}
  (with-meta x {:type :SStr}))
  ([x & xs]
   (seq->sstr (apply list x xs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTES

;; print SChar as map
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


;; print SStr as map of string with chars and string of poses it's char's
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

;; tested defs
(def x (SChar. \a :s))
(def xxx (seq->sstr [x x x x x]))

(defn -main []

  )