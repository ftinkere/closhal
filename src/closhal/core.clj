(ns closhal.core
  (:require [clojure.pprint :as pp]
            [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as st])
  (:use [clojure.java.io :refer :all])
  (:import (clojure.lang Keyword Var)
           (java.util Collection)
           (java.lang ProcessBuilder)
           (java.util.regex Pattern))
  (:gen-class))


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


;; [:a :b :c :d :e] -> '('(nil :a :b) '(:a :b :c) '(:b :c :d) '(:c :d :e) '(:d :e nil))
(defn window
  ([coll]
   (window nil coll))
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))


;;Применяет map посимвольно к строке и возврощает строку
(defn map-str [fn coll]
  (st/join (map fn coll)))


;; Применяет map к coll с функцией fn, которая применяется при условии выполнения функции pred к элементу
(defn map-if [pred fn coll]
  (map #(if (pred %)
          (fn %)
          %) coll))


;; Применяет map-if к строке посимвольно
(defn map-if-str [pred fn coll]
  (st/join (map-if pred fn coll)))


;; Заменяет в строке str в подстроке reg элемент chg на to
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

;; Применяет последовательно список аргументов reg-list к replace-adv к исходной строке str. Задаёт список правил замены
(defn replace-adv-many [str reg-list]
  (if (= 0 (count reg-list))
    str
    (replace-adv-many
      (apply replace-adv str (first reg-list))
      (rest reg-list))
    ))

;; Применяет последовательно список аргументов reg-list к clojure.string/replace к исодной строке str
(defn replace-many [str reg-list]
  (if (= 0 (count reg-list))
    str
    (replace-many
      (apply st/replace str (first reg-list))
      (rest reg-list))))


(defn st-first [str]
  (if (i (.length str) > 0)
    (first (.substring str 0 1))
    nil))
(defn st-rest [str]
  (if (i (.length str) > 0)
    (.substring str 1)
    ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLES

;; code of table UTF. 0x2800 (0x87f0)-- Braille
;; Код добавки. Требуется для замены таблицы Брайля на таблицу личного пользования в случае чего
(def code-table 0x2800)


;; Список кодов отдельных символов
(def liters-codes (range 0x0020 0x009D 4))
;; Список буквенных символов
(def cyr-liters (list \а \б \т \с
                      \л \в \д \з
                      \р \х \м \г
                      \н \п \к \ш
                      \ф \ч \ж \ц
                      \q \w \g \j
                      \А \Е \И \О \У \Й \ь \:))
(def lits (set (take 30 cyr-liters)))

;; Ассоциативный массив, где ключи это код, а значения это соответствующий буквенный символ. Отсортирован по кодам
(def code->lit-map (->> (zipmap liters-codes cyr-liters)
                        vec
                        flatten
                        (apply sorted-map)))
;; Ассоциативный массив, где ключи это буквенные символы, а значения соответствующие им коды. Отсортирован по кодам
(def lit->code-map (let [mp (zipmap cyr-liters liters-codes)]
                     (->> mp
                          vec
                          flatten
                          (apply sorted-map-by
                                 (fn [a b]
                                   (compare (mp a) (mp b)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPES

;; Мапа, ключи это кейворды для позиции schar, а значения это прибавка к коду.
(def poses {:s 0 :f 1 :m 2 :e 3})

;;объявление типов в core.spec
(s/def ::poses (set (keys poses)))
(s/def ::char char?)

;; Получение кейворда poses по прибавке к коду
(defn offset->pos [^Number x]
  {:pre [(or (nil? x) (#{0 1 2 3} x))]}
  (let [code->pos-map (zipmap (vals poses) (keys poses))]
    (code->pos-map x)
    ))

;; Тип SChar для хранения символа и позиции. Позиции могут быть nil и ключи из poses
(defrecord SChar [^Character char ^Keyword pos])
;; Конструктор типа. Проверяет входные значения
(defn schar
  ([^Character c ^Keyword p]
   {:pre [(s/valid? ::char c)
          #_((set cyr-liters) c)
          (or (nil? p) (s/valid? ::poses p))]}
   (->SChar c p))
  ([^Character c]
   (schar c nil)))

;; core.spec объявление для проверки на SChar
(s/def ::schar #(instance? SChar %))

;; Костыль, чтобы список из элементов SChar обрабатывался отдельно за счёт мета данных типа
(defn seq->sstr
  ([^Collection coll]
   #_{:pre [(s/valid? (s/coll-of ::schar) coll)]}
   (with-meta (filter #(not (nil? %)) coll) {:type :SStr}))
  ([^SChar x & xs]
   (seq->sstr (apply list x xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactions

;; Получает по коду соответствующий буквенный символ TODO: можно добавить поддержку добавок позиции
(defn code->lit [^Number c]
  (code->lit-map c))

;; Получает код буквенного символа из таблицы. Не поддерживает другие символы.
;; Использовать функцию code->schar в норме
(defn lit->code [^Character l]
  (lit->code-map l))

;; По полученному коду буквенного символа получает schar.
;; Принимает только коды отдельных символов и может принимать кейворд позиции, иначе будет nil
(defn code->schar
  ([^Number c ^Keyword p]
   (schar (code->lit c) p))
  ([^Number c]
   (code->schar c nil)))

;; Получает код по schar. Код содержит прибавку позиции, если позиция nil, то прибавка 0
(defn schar->code [^SChar sc]
  (+ (if-let [code (lit->code (:char sc))]
       code
       (int (:char sc)))
     (or (poses (:pos sc))
         0)))

;; Получает символ по char, который идёт на вывод. Если не является буквеным символом, то
;; выводится сам изначальный символ. Иначе символ с прибавкой таблицы. По умолчанию таблица Брайля.
(defn schar->echar [^SChar sc]
  (char (let [code (schar->code sc)
              offset (if (nil? (lit->code (:char sc)))
                       0 code-table)]
          (+ offset (schar->code sc)))))

;; Хз зачем, получение выводимого символа по буквенному символу из таблицы. Другие выдадут nil
(defn lit->echar [^Character c]
  (char (+ code-table (lit->code c))))

;; Возвращает schar с тем же символом, но новой позицией
(defn chpos [^Character sc ^Keyword p]
  {:pre  [(or (nil? p) (poses p))]
   :post [(s/valid? ::schar %)]}
  (assoc sc :pos p))

;; Попытка получить по конечному символу schar. Непредсказуема для символов вне буквенных символов в таблице
(defn echar->schar [^Character ech]
  (let [code (int ech)
        pos-offset (i code mod 4)
        ch (i code - pos-offset code-table)]
    (schar (code->lit ch) (offset->pos pos-offset))))

;; Отбрасывает позицию schar, который вернёт echar->schar и возвращает только символ
(defn echar->lit [^Character ech]
  (:char (echar->schar ech)))

;; Декодирует
(defn estr->str [^String estr]
  (reduce (fn [coll a]
            (let [cd (int a)
                  depos (i cd mod 4)
                  f (if (and (i cd > 0x2800)
                             (i cd < 0x2900))
                      (echar->lit a)
                      a)
                  coll (if (char? coll) [f] coll)
                  ]
              (conj coll f)
              )
            )
          estr)
  )


;; Делает все гласные в строке большими
(defn- up-vols [str]
  (map-if-str #{#_\а \е \и \о \у \й}
              st/upper-case
              str))

;; Удваивает пробелы. Нужно для визуального различения слов в конечной строке
(defn- spacing [str]
  (map-if-str #{\space}
              (fn [_] "  ")
              str))

;; По правилам [что на-что] меняет все вхождения в строке
(defn replacing-first [str]
  (replace-many str [
                     ["ы" "и"]
                     ["э" "е"]
                     ["ё" "ьо"]
                     ["ю" "ьу"]
                     ["я" "ьа"]
                     #_["й" "иь"]
                     ["щ" "шь"]
                     ["дж" "g"]
                     ["дз" "j"]
                     ["кс" "q"]
                     ["пс" "w"]
                     ["д.ж" "дж"]
                     ["д.з" "дз"]
                     ["к.с" "кс"]
                     ["п.с" "пс"]
                     ["ъ" "-"]
                     ["его" "агр"]
                     ]))

;; По правилам [где что на-что] меняет все вхождения в строке.
(defn replacing-second [str]
  (st/trim (replace-adv-many (st/join [" " str " "]) [
                                                      [#"-" "-" " - "] ; пробелы рядом с дефисом для обработки А как на концах слова
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


;; Обработка базовой строки в вид, пригодный для обработки
(defn pre-work [^String str]
  (-> str
      st/lower-case
      replacing-first
      up-vols
      #_replacing-second
      #_spacing))


;; Преобразует строку в sstr, массив schar с мета данными типа, все позиции равны :s
(defn str->bad-sstr
  ([] nil)
  ([^String str]
   (if (if (not (char? str))
         (empty? str))
     []
     (seq->sstr
       (if (or (char? str)
               (= 1 (count str)))
         [(schar (if (char? str) str (first str)) nil)]
         (reduce (fn [coll a]
                   (let [f #(schar % nil)
                         coll (if (char? coll)
                                [(f coll)]
                                coll)
                         ]
                     (conj coll (f a))))
                 (seq (pre-work str)))))))

  )


(defn pose-gen [pose-fn]
  (fn [sstr]
    (seq->sstr
      (loop [ps [(schar \space)]
             ch (first sstr)
             ft (conj (vec (rest sstr)) (schar \space))]
        (if (nil? ch)
          ps
          (recur (conj ps (pose-fn ps ch ft))
                 (first ft)
                 (rest ft)))
        )
      )
    )
  )

;; Принимает массив предшествующих символов, символ schar, и массив следующих за ним символов
;; Возвращает необходимую позицию для данного символа
;; Делает первый прогон для обработки в минимальный режим, проставляет только nil, :f и :e
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
(def enpose (pose-gen enpos))

;; Принимает массив предшествующих символов, символ schar, и массив следующих за ним символов
;; Возвращает необходимую позицию для данного символа
;; Делает второй прогон. Проставляет позиции :m и :s на основе минимальной конфигурации на основе :f, :e, nil
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

    ;(and (or (= \ь (:char ch))
    ;         (= \: (:char ch)))
    ;     (= :e (:pos (first ft)))
    ;     ) (chpos ch :e)

    ;(and (or (= \ь (:char (first ft)))
    ;         (= \: (:char (first ft))))
    ;     (or (= :s (:pos (second ft)))
    ;         (nil? (:pos (second ft))))
    ;     ) (chpos ch (:pos (first ft)))

    (and (= :m (:pos ch))
         (#{:e :s} (:pos (last ps)))
         (#{:e :m} (:pos (first ft)))
         (:pos ch)
         ) (chpos ch :f)
    (and (= :m (:pos ch))
         (#{:e :s} (:pos (last ps)))
         (or (#{:s} (:pos (first ft)))
             (nil? (:pos (first ft))))
         (:pos ch)
         ) (chpos ch :s)
    (and (= :e (:pos ch))
         (or (#{:e :s} (:pos (last ps)))
             (nil? (:pos (last ps))))
         ) (chpos ch :s)

    (and (or (= \ь (:char (last ps)))
             (= \: (:char (last ps))))
         (:pos ch)
         (not (nil? (:pos (first ft))))
         (not= :s (:pos (first ft)))
         ) (chpos ch (:pos (last ps)))

    (and (nil? (:pos (first ft)))
         (or
           (= :f (:pos (last ps)))
           (= :m (:pos (last ps))))
         ) (chpos ch :e)

    (and (or (= :f (:pos (last ps)))
             (= :m (:pos (last ps))))
         (= :f (:pos ch))
         (not (nil? (:pos (first ft))))
         ) (chpos ch :m)

    (and (or (nil? (:pos (last ps)))
             (= :e (:pos (last ps))))
         (or (= \- (:char (first ft)))
             (nil? (:pos (first ft))))
         (:pos ch)
         ) (chpos ch :s)

    :else ch
    )
  )
(def expose (pose-gen expos))

(comment

  [#"-" "-" " - "]                                          ; пробелы рядом с дефисом для обработки А как на концах слова
  [#"\sА" "А" "а"]                                          ; А в начале слова, после пробела, меняем на аль
  [#"А\s" "А" "а"]                                          ; Тоже в конце слова, меняем А на аль
  [#"\sА\s" "А" "а"]                                        ; А как отдельное слово тоже на аль
  [#"\s/А" "/" ""]                                          ; При экране / перед А в начале слова просто убираем экран, А остаётся А, на аль не меняем
  [#"/а\s" "/а" "А"]                                        ; Меняем аль с экраном в конце слова на А, т.к. выше А заменилась на аль в конце слова
  [#"\s/А\s" "/А" "А"]                                      ; А остаётся А при записи с экраном / в отдельной позиции
  [#" - " " - " "-"]                                        ; Убираем пробелы слева и справа от дефиса
  ;[#"\s-А" "-А" "-/а"] ; Пробел дефис А     -> экран / аль, чтобы её не съело в следующем
  ;[#"\s-а" "-а" "-А"]  ; Пробел дефис аль   -> А
  ;[#"\s-/а" "/а" "а"]  ; Убираем экран
  [#"\s-/А" "/А" "а"]
  [#"\S-а" "-а" "-/А"]                                      ; Непробел дефис аль -> экран / А
  [#"\S-А" "-А" "-а"]                                       ; Непробел дефис А   -> аль
  [#"\S-/А" "/А" "А"]                                       ; Убираем экран

  )

(defn dpos [ps ch ft]
  (cond
    (#{\-} (:char ch)
     ) nil
    :else ch
    )
  )
(def dpose (pose-gen dpos))

(defn apos [ps ch ft]
  (cond
    (and (#{\ь \:} (:char (first ft)))
         (= :e (:pos (first ft)))
         (not (#{:f :s} (:pos ch)))
         ) (chpos ch :e)

    (and (= \А (:char ch))
         (or (and (= :f (:pos ch))
                  (nil? (:pos (last ps)))
                  (not (nil? (last ps))))
             (and (= :e (:pos ch))
                  (#{:e :s} (:pos (last ps)))
                  (#{:f :s} (:pos (first ft)))))
         (not= \/ (:char (last ps)))
         ) (schar \а :s)

    (and (= \А (:char ch))
         (not= \/ (:char (last ps)))
         (#{:m :f} (:pos (last ps)))
         (or (#{:s :f} (:pos (first ft)))
             (and (nil? (:pos (first ft)))
                  (not (nil? (first ft)))))
         ) (schar \а :e)

    (#{\/} (:char ch)
     ) nil

    :else ch)
  )
(def apose (pose-gen apos))

(defn ьpos [ps ch ft]
  (cond

    (#{\ь \:} (:char ch)
     ) (chpos ch :s)

    :else ch
    )
  )
(def ьpose (pose-gen ьpos))

;; применяет последовательно enpose, expose и remex к sstr
(defn pose [sstr]
  (let [ps (comp ьpose apose expose apose expose dpose expose enpose)
        trim (fn [s]
               (let [len (.size s)]
                 (loop [rindex len]
                   (if (zero? rindex)
                     ""
                     (if (Character/isWhitespace (:char (nth s (dec rindex))))
                       (recur (dec rindex))
                       ;; there is at least one non-whitespace char in the string,
                       ;; so no need to check for lindex reaching len.
                       (loop [lindex 0]
                         (if (Character/isWhitespace (:char (nth s lindex)))
                           (recur (inc lindex))
                           (->> s (drop lindex) (take (i rindex - lindex)))))))))
               )]
    (-> (ps sstr) trim))
  )

;; Преобразует базовую строку в sstr
(defn str->sstr
  ([] nil)
  ([str]
   (if (empty? str)
     []
     (seq->sstr (pose (str->bad-sstr str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTES

;; переменная для корректировки вывода символов schar в виде читаемом и виде финальном
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

;; Печатает с переменной *print-end*
(defn printe [x]
  (binding [*print-end* true]
    (print x)))


;;;;; TEST

(defn sym-sstr
  ([] '())
  ([c p & more]
   (seq->sstr
     (conj (apply sym-sstr more) (schar c p))
     ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New methods                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tr-word
  "Преобразует строку одного слова без пробелов с поддержкой дефисов"

  ([^String str]
   (tr-word str true))
  ([^String str first?]
   {:pre [(not (st/includes? str " "))]}
   (if first?

     (let [str (pre-work str)
           open-left? (st/starts-with? str "-")
           open-right? (st/ends-with? str "-")
           open (fn [sstr]
                  (let [fir-rig (drop-while #(not (lits (:char %))) sstr)
                        fir-lef (take-while #(not (lits (:char %))) sstr)
                        fir-out (rest fir-rig)
                        fir-one (first fir-rig)

                        sec-lef (reverse (drop-while #(not (lits (:char %))) (reverse sstr)))
                        sec-rig (reverse (take-while #(not (lits (:char %))) (reverse sstr)))
                        sec-one (last sec-lef)

                        mid-out (drop-last (+ 1 (count sec-rig))
                                           fir-out)

                        up-f (cond

                               (not open-left?) fir-one

                               (and open-left?
                                    (= \а (:char fir-one))
                                    (= :s (:pos fir-one))
                                    (#{:f :s} (:pos (first mid-out))) ;; up-m :f to :m
                                    ) (schar \А :m)         ;; -аз -аза обрабатывает в конечную часть
                               ;; исправляя а-з а-за

                               (and open-left?
                                    (= :s (:pos fir-one))
                                    ) (chpos fir-one :e)    ;; -т-ка

                               (and open-left?
                                    (= :f (:pos fir-one))
                                    ) (chpos fir-one :m)

                               :else (chpos fir-one nil)
                               )

                        up-m (cond
                               (and open-left?
                                    (= \а (:char fir-one))
                                    (= :s (:pos fir-one))
                                    (#{:f :s} (:pos (first mid-out)))
                                    ) (seq->sstr
                                        (flatten
                                          (conj []
                                                (chpos (first mid-out) (if (= :f (:pos (first mid-out))) :m :e))
                                                (rest mid-out)
                                                )))
                               :else mid-out
                               )
                        up-s (cond

                               (not open-right?) sec-one

                               (and open-right?
                                    (= \а (:char sec-one))
                                    (#{:e :s} (:pos sec-one))
                                    ) (schar \А (if (= :e (:pos sec-one)) :m :f))

                               (and open-right?
                                    (= :s (:pos sec-one))
                                    ) (chpos sec-one :f)

                               (and open-right?
                                    (= :e (:pos sec-one))
                                    ) (chpos sec-one :m)

                               :else (chpos sec-one nil)
                               )

                        ]
                    (seq->sstr

                      (flatten
                        (conj []
                              fir-lef
                              up-f
                              up-m
                              up-s
                              sec-rig
                              ))
                      )
                    )
                  )                                         ;; BUG (tr "-м-")
           ]
       (seq->sstr
         (open (apply conj []
                      (flatten (map #(tr-word % false)
                                    (st/split str #"-")))))))

     (loop [ps []
            ch (st-first str)
            ft (st-rest str)]
       (let [filt (fn [x] (filter #(if (instance? SChar %) (lits (:char %)) (lits %)) x))]
         (cond

           (nil? ch) ps

           (and (empty? (filt ps))
                (empty? (filt ft))
                ) (recur (conj ps (schar ch :s))
                         (st-first ft)
                         (st-rest ft))

           (and (= \а ch)
                (or (empty? (filt ps))
                    (#{:s :e} (:pos (last (filt ps)))))
                ) (recur (if (= \/ (:char (last ps)))
                           (conj (vec (butlast ps)) (schar \А :f))
                           (conj ps (schar \а :s)))
                         (st-first ft)
                         (st-rest ft))

           (and (lits ch)
                (or (empty? (filt ps))
                    (#{:s :e} (:pos (last (filt ps)))))
                (not= \а ch)

                ) (recur (conj ps (schar ch (if (empty? (filt ft)) :s :f)))
                         (st-first ft)
                         (st-rest ft))

           (and (= \а ch)
                (#{:f :m} (:pos (last (filt ps))))
                (not (empty? (filt ft)))
                ) (recur (if (= \/ (:char (last ps)))
                           (conj (vec (butlast ps)) (schar \а :e))
                           (conj ps (schar \А :m)))
                         (st-first ft)
                         (st-rest ft))

           (and (lits ch)
                (#{:f :m} (:pos (last (filt ps))))
                (not (empty? (filt ft)))
                ) (recur (conj ps (schar ch :m))
                         (st-first ft)
                         (st-rest ft))

           (and (lits ch)
                (not (empty? (filt ps)))
                (empty? (filt ft))
                ) (recur (if (and (= \а ch)
                                  (= \/ (:char (last ps))))
                           (conj (vec (butlast ps)) (schar \а :e))
                           (conj ps (schar ch :e)))
                         (st-first ft)
                         (st-rest ft))

           :else (recur (conj ps (schar ch))
                        (st-first ft)
                        (st-rest ft))
           ))
       )
     ))
  )

(defn st-join                                               ;; ONLY SYMS IN STR!
  ([coll]
   (seq->sstr
     (flatten
       (apply conj [] coll)))
   )
  ([sep coll]
   (seq->sstr
     (flatten (let [sep (if (or (string? sep) (char? sep))
                          (str->bad-sstr sep)
                          sep)]
                (if (= 1 (count coll))
                  (first coll)
                  (loop [ps (vector (first coll))
                         ft (vec (next coll))]
                    (if ft
                      (recur (conj ps sep (first ft))
                             (next ft)
                             )
                      ps)
                    ))))))
  )

(defn tr-line [line]
  (st-join " "
           (map tr-word (st/split line #"\s"))))

(defn tr [text]
  (st-join "\n"
           (map tr-line (st/split-lines text))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASES

;; tested defs
(def x (schar \б :s))
(def y (schar \п :f))
(def xxx (seq->sstr [x x y x y y]))

(defn -main [& args]

  (doseq [arg args]
    (let [arg (if (nil? arg) "" arg)
          c-flag (= \d (first arg))
          sstr (if c-flag
                 (estr->str (rest arg))
                 (tr (if (char? arg) (str arg) arg)))]
      (if (not c-flag)
        (printe sstr))
      (print sstr))
    )
  (loop []
    (if-let [str-in (read-line)]
      (do
        (if (or (= ":exit" str-in)
                (= ":q" str-in)
                (= ":quit" str-in))
          (System/exit 0)
          )
        (let [str-in (if (nil? str-in) "" str-in)
              c-flag (= \d (first str-in))
              sstr (if c-flag
                     (estr->str (rest str-in))
                     (str->sstr (if (char? str-in) (str str-in) str-in)))]
          (if (not c-flag)
            (printe sstr))
          (print sstr)
          (recur)))
      (recur)))
  )


