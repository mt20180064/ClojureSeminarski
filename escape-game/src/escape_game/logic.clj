(ns logic)

(defn my-into
  [target additions]
  (apply conj additions target))

(def igraci [{:ime "Nina" :iskustvo 4 :timskiIgrac true :snalazljivost 5 :raspolozenje 5 :tema false :strah false :kompetentnost false :par nil }
             {:ime "Anis" :iskustvo 4 :timskiIgrac true :snalazljivost 4 :raspolozenje 2 :tema true :strah true :kompetentnost true :par nil }
             {:ime "Điri" :iskustvo 5 :timskiIgrac false :snalazljivost 1 :raspolozenje 5 :tema false :strah false :kompetentnost true :par nil }
             {:ime "Joča" :iskustvo 2 :timskiIgrac true :snalazljivost 3 :raspolozenje 1 :tema true :strah true :kompetentnost false :par nil }
             {:ime "Maša" :iskustvo 5 :timskiIgrac false :snalazljivost 4 :raspolozenje 2 :tema false :strah false :kompetentnost true :par nil }
             {:ime "Nemica" :iskustvo 1 :timskiIgrac true :snalazljivost 3 :raspolozenje 2 :tema false :strah true :kompetentnost false }
             {:ime "Roko" :iskustvo 5 :timskiIgrac false :snalazljivost 1 :raspolozenje 3 :tema false :strah false :kompetentnost true :par nil }
             {:ime "Roki" :iskustvo 4 :timskiIgrac true :snalazljivost 4 :raspolozenje 1 :tema false :strah true :kompetentnost false :par nil }
             {:ime "Teo" :iskustvo 1 :timskiIgrac true :snalazljivost 2 :raspolozenje 5 :tema false :strah true :kompetentnost false :par nil }
             {:ime "Nelsi" :iskustvo 3 :timskiIgrac false :snalazljivost 3 :raspolozenje 4 :tema true :strah true :kompetentnost false :par nil  }
             {:ime "Beli" :iskustvo 5 :timskiIgrac false :snalazljivost 1 :raspolozenje 2 :tema true :strah false :kompetentnost true :par nil }
             {:ime "Cole" :iskustvo 4 :timskiIgrac true :snalazljivost 5 :raspolozenje 1 :tema true :strah true :kompetentnost true :par nil }
             {:ime "Bebinger" :iskustvo 1 :timskiIgrac true :snalazljivost 4 :raspolozenje 5 :tema false :strah false :kompetentnost false :par nil}
             ])


;(def tim1 (atom #{}))
;(def tim2 (atom #{}))

(def duzina (count igraci))
;duzina



(defn check-topic
  [igrac]
  (if (get igrac :tema)
    2
    0)
 )


(defn check-fear
  [igrac]
  (if (get igrac :strah) 
   -1
    2))

(defn sum-good-attributes
  [igrac]
(+ (get igrac :raspolozenje) (get igrac :iskustvo) (get igrac :snalazljivost)))


(defn check-competitive
  [igrac]
  (if (= (get igrac :kompetentnost) true)
    true
    false))

(defn sum-everything
  [igrac]
  (assoc igrac :ukupno (+ (sum-good-attributes igrac) (check-fear igrac) (check-topic igrac))))


(defn sort-everything
 [igraci duzina]
  (loop [i 0 sortiran []]
    (if (= duzina i)
      sortiran
   (recur (inc i) (sort-by :ukupno (my-into [(sum-everything (nth igraci i))] sortiran))))))


(defn team-added
  [tim i]
  (if (clojure.core/even? i)
    1
    2))

(defn give-teams
  [n lista]
  (loop [i 0 list-with-teams '()]
    (if (= i n )
      list-with-teams
      (recur (inc i) (my-into [(update (nth lista i) :tim team-added i)] list-with-teams))
      )
    ))
duzina
 
(defn stamp
  [n lista]
  (loop [i 0 s '()]
    (if (= i n) 
      s
    (recur(inc i) (my-into [(get (nth lista i) :ime)] s)))))
(stamp duzina igraci)

(stamp duzina (sort-everything igraci duzina))

(defn pravljenjeEkipa
  [n lista]
  (let [sortirana (give-teams n (sort-everything lista n))]
    (println sortirana)
    (loop  [i 0 j 1 t1 #{(if (clojure.core/odd? n)
                           (get (last sortirana) :ime)
                           "")} t2 #{}]
      (if (or (= n j) (= n i) (= n (+ (count t2) (count t2))))
        (println "evo prvog tima" t1 "  evo drugog tima" t2)
        (recur (+ 1 (inc i))
               (+ 1 (inc j))
               (my-into [(get (nth sortirana i) :ime)] t1)
               (my-into [(get (nth sortirana j) :ime)] t2))))))

(pravljenjeEkipa duzina igraci)


(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))
;da probam clojure script
;(defn take-game
 ; []

  ;(Integer. (get-input 20)
  ;  ))

(defn ask-for-details 
  []

  (println "Zdravo! Da bismo odredili najbolju podelu, potrebno je da unesete neke podatke vezane za vasu ekipu i sobu.
              Da li je soba horor karaktera?
              1-da
              2-ne")
  (println(get-input))
  (if (= "1" (get-input))
    (println "odmah se ide na one algoritme gde se racuna strah kao faktor")
    (println "prelazi se tamo gde strah nije faktor"))
  (println "U redu. Da li je soba linearna?
              1-da
              2-ne")
  (get-input)

  (println "Da li je fokus sobe na zagonetkama (1) ili mehanizmima (2)?")
  (get-input)

  (println "Koja je tezina sobe?
                  1-teska
                  2-srednja
                  3-laka")
  (get-input))
(ask-for-details)

;smisliti sta da se uradi sa kompetitivnost i timski igrac
;dodati opciju za slucaj da ima nesto nepredvidjeno veliko dobro ili lose
;kao sto je npr eto da ne zna jezik
;kad vidim kako se radi sa bazom napraviti da se igraci cuvaju u bazi
;da bi moglo da se importuje sledeci put kad bude isti igrac
;napraviti da se kriterijumi menjaju u zavisnosti od vrste i duzine sobe
;kada sve to uradim moze se preci na GUI
;da li raditi i za druge igre ili je ovo dovoljno?