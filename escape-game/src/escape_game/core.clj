
(ns escape-game.core
 
  (:gen-class))


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


(defn collect-data-to-determine
  []
  (let [score 0]
 (println "Zdravo! Da bismo odredili najbolju podelu, potrebno je da unesete neke podatke vezane za vasu ekipu i sobu.
               Da li je soba horor karaktera?
               1-da
               2-ne")
 (if (= "1" (get-input))
   (println (+ score 1))
   (println (+ score 2)))
 (println "U redu. Da li je soba linearna?
               1-da
               2-ne")
 (if (= "1" (get-input))
   (println (+ score 1))
    (println (+ score 2)))
 (println "Da li je fokus sobe na zagonetkama (1) ili mehanizmima (2)?")
 (if (= "1" (get-input))
   (println (+ score 1))
    (println (+ score 2)) )(println score)))
  (defn -main
    [& args] 
     (println "koliko vas igra?") 
     (get-input)
    (println "nas predlog je da se podelite u _ ekipa. Zelite li drugacije? 
              1-da
              2-ne")
    (get-input)
    (println "Sada nekoliko podataka o sobi i predlozicemo vam tim.")
    (collect-data-to-determine)
    (println "Ovo je ono sto imamo od podataka o igracima: djisgjodg
              da li postoji nesto vazno sto treba naznaciti u vezi nekog
              od igraca, a da nije pomenuto?
              1-ne
              2-unos")
  (if (= "2" (get-input)) 
    ( do(println "unesite:") (get-input))
    (println "Za nekoliko sekundi videcete nas predlog timova!"))
)
    
  
  

