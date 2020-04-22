(ns closhal.core
  (:require [clojure.pprint :as pp]
            [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as st])
  (:use [clojure.java.io :refer :all])
  (:import (clojure.lang Keyword)
           (java.util Collection)
           (java.lang ProcessBuilder)
           (java.util.regex Pattern)))


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

(defn map-str [fn coll]
  (st/join (map fn coll)))

(defn map-if [pred fn coll]
  (map #(if (pred %)
          (fn %)
          %) coll))

(defn map-if-str [pred fn coll]
  (st/join (map-if pred fn coll)))

(defn replace-adv [^String str reg chg ^String to]
  (if-let [fnd (re-find reg str)]
    (if-let [b (re-find (re-pattern chg) fnd)]
      (st/replace str
                  reg
                  (st/replace fnd
                              chg
                              to))
      str)
    str))

(defn replace-adv-many [str reg-list]
  (if (= 0 (count reg-list))
    str
    (replace-adv-many
      (apply replace-adv str (first reg-list))
      (rest reg-list))
    ))
(defn replace-many [str reg-list]
  (if (= 0 (count reg-list))
    str
    (replace-many
      (apply st/replace str (first reg-list))
      (rest reg-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLES

;; code of table UTF. 0x2800 -- Braille
(def code-table 0x2800)

(def liters-codes (range 0x0020 0x0099 4))
(def cyr-liters (list \а \б \т \с
                      \л \в \д \з
                      \р \х \м \г
                      \н \п \к \ш
                      \ф \ч \ж \ц
                      \v \w \g \j
                      \А \Е \И \О \У \ь))

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
  {:pre [(or (nil? x) (#{0 1 2 3} x))]}
  (let [code->pos-map (zipmap (vals poses) (keys poses))]
    (code->pos-map x)
    ))

;; SChar type, store char and pos of it in the word
(defrecord SChar [^Character char ^Keyword pos])
(defn schar
  ([^Character c ^Keyword p]
   {:pre [(s/valid? ::char c)
          #_((set cyr-liters) c)
          (or (nil? p) (s/valid? ::poses p))]}
   (->SChar c p))
  ([^Character c]
   (schar c nil)))

(s/def ::schar #(instance? SChar %))                        ;; spec for check

;; Add meta type of SStr to seq of SChars, can attempt one seq or elements, do not mix
(defn seq->sstr
  ([^Collection coll]
   #_{:pre [(s/valid? (s/coll-of ::schar) coll)]}
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

(defn code->schar
  ([^Number c ^Keyword p]
   (schar (code->lit c) p))
  ([^Number c]
   (code->schar c nil)))

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



(defn- up-vols [str]
  (map-if-str #{\а \е \и \о \у}
              st/upper-case
              str))

(defn- spacing [str]
  (map-if-str #{\space}
              (fn [_] "  ")
              str))


(defn replacing-first [str]
  (replace-many str [
                     ["ы" "и"]
                     ["э" "е"]
                     ["ё" "ьо"]
                     ["ю" "ьу"]
                     ["я" "ьа"]
                     ["й" "ьи"]
                     ["дж" "g"]
                     ["дз" "j"]
                     ["кс" "v"]
                     ["пс" "w"]
                     ["д.ж" "дж"]
                     ["д.з" "дз"]
                     ["к.с" "кс"]
                     ["п.с" "пс"]
                     ["ъ" "-"]
                     ]))

(defn replacing-second [str]
  (st/trim (replace-adv-many (st/join [" " str " "]) [
                                                      [#"\S-\S" "-" " - "] ; пробелы рядом с дефисом для обработки А как на концах слова
                                                      [#"\sА" "А" "а"] ; А в начале слова, после пробела, меняем на аль
                                                      [#"А\s" "А" "а"] ; Тоже в конце слова, меняем А на аль
                                                      [#"\sА\s" "А" "а"] ; А как отдельное слово тоже на аль
                                                      [#"\s/А" "/" ""] ; При экране / перед А в начале слова просто убираем экран, А остаётся А, на аль не меняем
                                                      [#"/а\s" "/а" "А"] ; Меняем аль с экраном в конце слова на А, т.к. выше А заменилась на аль в конце слова
                                                      [#"\s/А\s" "/А" "А"] ; А остаётся А при записи с экраном / в отдельной позиции
                                                      [#" - " " - " "-"] ; Убираем пробелы слева и справа от дефиса
                                                      ;[#"\s-А" "-А" "-/а"] ; Пробел дефис А     -> экран / аль, чтобы её не съело в следующем
                                                      ;[#"\s-а" "-а" "-А"]  ; Пробел дефис аль   -> А
                                                      ;[#"\s-/а" "/а" "а"]  ; Убираем экран
                                                      [#"\s-/А" "/А" "а"]
                                                      [#"\S-а" "-а" "-/А"] ; Непробел дефис аль -> экран / А
                                                      [#"\S-А" "-А" "-а"] ; Непробел дефис А   -> аль
                                                      [#"\S-/А" "/А" "А"] ; Убираем экран
                                                      ])))

(defn pre-work [^String str]
  (-> str
      st/lower-case
      replacing-first
      up-vols
      replacing-second
      spacing))

;; TODO Правильные позиции и обработка гласных, экранов, спец обозначений в кириллице


(defn str->bad-sstr [^String str]
  (seq->sstr
    (reduce (fn [coll a]
              (let [f #(schar % :s)
                    coll (if (char? coll)
                           [(f coll)]
                           coll)
                    ]
                (conj coll (f a))))
            (seq (pre-work str)))))

(defn enpos [ps ch ft]
  (cond
    (not ((set cyr-liters) (:char ch))
         ) (chpos ch nil)

    (or (= \space (:char (first ft)))
        (= \- (:char (first ft)))
        (empty? ft)
        ) (chpos ch :e)
    :else (chpos ch :f)
    )
  )

(defn enpose [sstr]
  (seq->sstr
    (loop [ps []
           ch (first sstr)
           ft (rest sstr)]
      (if (nil? ch)
        ps
        (recur (conj ps (enpos ps ch ft))
               (first ft)
               (rest ft)))
      )
    )
  )

(defn expos [ps ch ft]
  (cond
    (and
      (or (and (= \- (:char (last ps)))
               (or (= \space (:char (second (reverse ps))))
                   (nil? (:pos (second (reverse ps))))))
          (and (= \- (:char (first ft)))
               (or (= \space (:char (second ft)))
                   (nil? (:pos (second ft))))))

      ) (chpos ch :m)

    (and (= \ь (:char (last ps)))
         (:pos ch)
         ) (chpos ch (:pos (last ps)))

    (and (nil? (:pos (first ft)))
         (= :m (:pos (last ps)))
         ) (chpos ch :e)

    (and (or (= :f (:pos (last ps)))
             (= :m (:pos (last ps))))
         (= :f (:pos ch))
         (not (nil? (:pos (first ft))))
         ) (chpos ch :m)

    (and (or (nil? (:pos (last ps)))
             (= :e (:pos (last ps))))
         (or (= \- (:char (first ft)))
             (= \space (:char (first ft))))
         (:pos ch)
         ) (chpos ch :s)

    :else ch
    )
  )

(defn expose [sstr]
  (seq->sstr
    (loop [ps []
           ch (first sstr)
           ft (rest sstr)]
      (if (nil? ch)
        ps
        (recur (conj ps (expos ps ch ft))
               (first ft)
               (rest ft)))
      )
    )
  )

(defn remex [sstr]
  (seq->sstr
    (map #(if (= \. (:char %))
            (schar \̥  (:pos nil))
            %)
      (filter (fn [ch] (not= \- (:char ch))) sstr))
    ))

(defn pose [sstr]
  ((comp remex expose enpose) sstr))

(defn str->sstr [str]
  (seq->sstr (pose (str->bad-sstr str))))

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
                             (for [s x] (key->str (or (:pos s) :n))))
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