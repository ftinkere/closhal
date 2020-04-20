(ns closhal.core
  (:require [clojure.pprint :as pp]
            [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as st])
  (:use [clojure.java.io :refer :all])
  (:import (clojure.lang Keyword)
           (java.util Collection)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

;; infix form
(defn i [a f & s]
  (apply f a s))

;; :key -> "key"
(defn key->str
  [^Keyword k]
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


(defn window
  ([coll]
   (window nil coll))
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLES

;; code of table UTF. 0x2800 -- Braille
(def code-table 0x2800)

(def liters-codes (range 0x0020 0x0095 4))
(def cyr-liters (list \а \б \т \с
                      \л \в \д \з
                      \р \х \м \г
                      \н \п \к \ш
                      \ф \ч \ж \ц
                      \v \w \g \j
                      \А \Е \И \О \У))

;; TODO Специальынй символы и препинание, а так же гласные

(def code->lit-map (->> (zipmap liters-codes cyr-liters)
                        vec
                        flatten
                        (apply sorted-map)))
(def lit->code-map (let [mp (zipmap cyr-liters liters-codes)]
                     (->> mp
                          vec
                          flatten
                          (apply sorted-map-by
                                 (fn [a b]
                                   (compare (mp a) (mp b)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPES

;; all possible positions of schar
(def poses {:s 0 :f 1 :m 2 :e 3})
(s/def ::poses (set (keys poses)))
(s/def ::char char?)

(defn offset->pos [^Number x]
  {:pre [(#{0 1 2 3} x)]}
  (let [code->pos-map (zipmap (vals poses) (keys poses))]
    (code->pos-map x)
    ))

;; SChar type, store char and pos of it in the word
(defrecord SChar [^Character char ^Keyword pos])
(defn schar [^Character c ^Keyword p]                       ;; constructor
  {:pre [(s/valid? ::char c)
         #_((set cyr-liters) c)
         (or (nil? p) (s/valid? ::poses p))]}
  (->SChar c p))
(s/def ::schar #(instance? SChar %))                        ;; spec for check

;; Add meta type of SStr to seq of SChars, can attempt one seq or elements, do not mix
(defn seq->sstr
  ([^Collection coll]
   {:pre [(s/valid? (s/coll-of ::schar) coll)]}
   (with-meta coll {:type :SStr}))
  ([^SChar x & xs]
   (seq->sstr (apply list x xs))))

;; КОСТЫЛЬ ДЛЯ PPRINT!
(defrecord SStr [sstr])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactions

(defn code->lit [^Number c]
  (code->lit-map c))

(defn lit->code [^Character l]
  (lit->code-map l))

(defn code->schar [^Character c ^Keyword p]
  (schar (code->lit c) p))

(defn schar->code [^SChar sc]
  (+ (if-let [code (lit->code (:char sc))]
       code
       (int (:char sc)))
     (or (poses (:pos sc))
         0)))

(defn schar->echar [^SChar sc]
  (char (let [code (schar->code sc)
              offset (if (nil? (lit->code (:char sc)))
                       0 code-table)]
          (+ offset (schar->code sc)))))

(defn char->echar [^Character c]
  (char (+ code-table (lit->code c))))

(defn chpos [^Character sc ^Keyword p]
  {:pre  [(or (nil? p) (poses p))]
   :post [(s/valid? ::schar %)]}
  (assoc sc :pos p))

(defn echar->schar [^Character ech]
  (let [code (int ech)
        pos-offset (i code mod 4)
        ch (i code - pos-offset code-table)]
    (schar (code->lit ch) (offset->pos pos-offset))))

(defn echar->lit [^Character ech]
  (:char (echar->schar ech)))

(defn str->bad-sstr [str]
  (seq->sstr
    (reduce (fn [coll a]
              (let [f #(schar % :s)
                    coll (if (char? coll)
                           [(f coll)]
                           coll)
                    ]
                (conj coll (f a))))
            (seq str))))

(defn up-but-a [str]
  (st/join (map #(if (#{\е \и \о \у} %)
                   (st/upper-case %)
                   %)
                str)))
(defn spacing [str]
  (st/join (map #(if (= \space %)
                   "  "
                   %)
                str)))

(defn pre-work [^String str]
  (-> str
      st/lower-case
      up-but-a
      spacing))

;; TODO Правильные позиции и обработка гласных, экранов, спец обозначений в кириллице

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTES

(def ^:dynamic
  *print-end* false)

;; print number in base 16
(defn pb [x]
  (binding [pp/*print-base* 16] (pp/pprint x)))

;; print SChar as map
(defmethod print-method SChar
  [x w]
  (if *print-end*
    (.write w (str (schar->echar x)))
    (pp/pprint {:char (-> x :char str symbol)
                :pos  (if (nil? (:pos x))
                        nil
                        (-> x :pos key->str symbol))}
               w)
    )
  )
#_(wr w
      ":char "
      (str (:char x))
      " :pos "
      (key->str (:pos x)))
;)

(defmethod pp/simple-dispatch SChar
  ([x]
   (binding [*print-end* true]
     (print-method x *out*)))
  ([x w]
   (binding [*print-end* true]
     (print-method x w))))


;; print SStr as map of string with chars and string of poses it's char's
(defmethod print-method :SStr
  [x w]
  (if *print-end*
    (let []
      (doseq [s x]
        (print-method s w))
      (.write w "\n"))
    (pp/pprint {:str (reduce (fn [a b] (.concat a b))
                             (for [s x] (str (:char s))))
                :pos (reduce (fn [a b] (.concat a b))
                             (for [s x] (key->str (:pos s))))
                }
               w)))

(defn printe [x]
  (binding [*print-end* true]
    (print x)))




;; DO NOT USE
(defmethod pp/simple-dispatch SStr
  ([x]
   (binding [*print-end* true]
     (print-method (seq->sstr (:sstr x)) *out*)))
  ([x w]
   (binding [*print-end* true]
     (print-method (seq->sstr (:sstr x)) w))))

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
(def x (schar \б :s))
(def y (schar \п :f))
(def xxx (seq->sstr [x x y x y y]))

(defn -main []

  )