
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
   (println (inc score))
   (println (inc score) (inc score)))
 (println "U redu. Da li je soba linearna?
               1-da
               2-ne")
(if (= "1" (get-input))
  (println (inc score))
  (println (inc score) (inc score)))
 (println "Da li je fokus sobe na zagonetkama (1) ili mehanizmima (2)?")
(if (= "1" (get-input))
  (println (inc score))
  (println (inc score) (inc score))) (println score)))
 

(defn podaci []
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
    (do (println "unesite:") (get-input))
    (println "Za nekoliko sekundi videcete nas predlog timova!")))

(defn prompt [question]
  (println question)
  (flush)
  (read-line))





(defn get-user-info []
  {:name (prompt "Tell me about yourself. Name?")
   :experience (Integer. (prompt "How experienced from 1 (not at all) to 5 (expert) are you with escape rooms?"))
   :adroit (Integer. (prompt "How adroit from 1 (not at all) to 5 (extremely) you find yourself?"))
   :mood (Integer. (prompt "How would you describe your mood today from 1 (bad) to 5 (cheerfull and excited) you find yourself?"))
   :teamplayer ( Integer. (prompt "Do you find yourself a team player? 1-yes, 2-no"))
   :frightened (Integer. (prompt "Are you scared of the jumpscares and creepy details? 1-yes, 2-no"))
   :theme (Integer. (prompt "Does the theme of the room interest you or is related to you anyhow that can be an advangate? 1-yes, 2-no"))
   :competitiveness (Integer. (prompt "Do you find yourself competitive? 1-yes, 2-no")) 
   }
  )

(def ^:dynamic *players* [])


(defn players-vector-maker []
   (let [num-users (Integer. (prompt "How many users?"))
        user-info (doall (repeatedly num-users get-user-info))]
    (alter-var-root #'*players*
                    (fn [current-players]
                      (concat current-players user-info)))
    (println "OK! Thank you.")
    (println "User information stored in memory:")
    (prn *players*)))

(defn -main [& args]
 (players-vector-maker))
    
  
  

