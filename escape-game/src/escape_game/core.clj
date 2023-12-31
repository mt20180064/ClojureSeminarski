
(ns escape-game.core
 
  (:require [clojure.string :as string])
  (:require [escape_game.logic]))




;u ovom namespace-u dolazimo do podataka o sobi i o igracima na osnovu kojih
;ce algoritam vrsiti procenu kako bi bilo najoptimalnije podeliti igrace u odredjeni broj ekipa
;escape room 

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
   :summary 0
   }
  )

(defn calculate-divisions [num-players]
  (let [cases [[2 10 2] [6 15 3] [8 20 4] [10 25 5] [12 30 6] [14 35 7]]
        matching-cases (filter #(when (<= (first %) num-players (second %)) %) cases)
        divisions (map #(nth % 2) matching-cases)]
    (if (seq divisions)
      divisions
      (throw (RuntimeException. "No matching division found")))))


(defn prompt-for-division [divisions]
  (let [chosen-division (prompt (str "You can divide into " (string/join ", " divisions) ". Which option would you like? Type one of these numbers."))]
    (try
      (Integer. chosen-division)
      (catch NumberFormatException _
        (throw (RuntimeException. "Invalid division option"))))))


(defn get-room-info [num-players]
  (let [division (prompt-for-division (calculate-divisions num-players))]
    {:horror (Integer. (prompt "Now about the room. Is the one you are playing classified as horror? 1-yes, 2-no"))
     :linear (Integer. (prompt "Is it linear or not? 1-yes, 2-no"))
     :tech (Integer. (prompt "Is it full of high-tech mechanisms or more old school based on riddles? (ask your game-master) 1-mechanisms, 2-riddles"))
     :knowledge (Integer. (prompt "Does it require knowledge about the topic? 1-yes, 2-no"))
     :division division}))

(println "(println "(println "")")")(def players (atom []))
(def room (atom {}))

(defn room-data-maker [num-players]
  (swap! room (fn [_] (get-room-info num-players)))
  (println "OK! Thank you.")
  (println "Room information stored in memory:")
  (prn @room))

(defn players-vector-maker []
  (let [num-users (Integer. (prompt "How many users?"))
        user-info (doall (repeatedly num-users get-user-info))]
    (swap! players #(vec (concat % user-info)))
    (println "OK! Thank you.")
    (println "User information stored in memory:")
    (prn @players)))

(def room-for-test 
  {:tech 1, :division 2})

(defn add-sum-to-player [vector-of-players key-sum zero-value]
  (map #(assoc % key-sum zero-value) vector-of-players))

(defn add-sum-to-atom [players]
  (swap! players #(add-sum-to-player % :key-sum 0)))

(defn algorythms-by-divisions [number-of-divisions]
  (cond
    (= number-of-divisions 2 ) (logic/pravljenjeEkipa (count @players) @players)
    (= number-of-divisions 3) (println "alg za 3") 
    (= number-of-divisions 4) (println "alg za 4")
    (= number-of-divisions 5) (println "alg za 5")
    (= number-of-divisions 6) (println "alg za 6")
    (= number-of-divisions 7) (println "alg za 7")
    ))

(defn -main [& args]
  (println "da li radi ")
  (players-vector-maker)
  (room-data-maker (count @players))
 ; (algorythms-by-divisions (get @room :division))
  )
    
  
  

