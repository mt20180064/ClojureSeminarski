(ns escape-game.core
  (:require [clojure.string :as string])
  (:require [escape_game.logic]))

(defn prompt [question]
  (println question)
  (flush)
  (read-line))

(defn valid-input? [input min max]
  (and (integer? input) (<= min input max)))

(defn get-valid-input [question min max]
  (loop []
    (let [input (Integer. (prompt question))]
      (if (valid-input? input min max)
        input
        (do (println (str "Please enter a valid number between " min " and " max "."))
            (recur))))))

(defn get-user-info []
  (let [name (prompt "Tell me about yourself. Name?")
        experience (get-valid-input "How experienced from 1 (not at all) to 5 (expert) are you with escape rooms?" 1 5)
        adroit (get-valid-input "How adroit from 1 (not at all) to 5 (extremely) you find yourself?" 1 5)
        mood (get-valid-input "How would you describe your mood today from 1 (bad) to 5 (cheerful and excited)?" 1 5)
        teamplayer (get-valid-input "Do you find yourself a team player? 1-yes, 2-no" 1 2)
        frightened (get-valid-input "Are you scared of the jumpscares and creepy details? 1-yes, 2-no" 1 2)
        theme (get-valid-input "Does the theme of the room interest you or is related to you anyhow that can be an advantage? 1-yes, 2-no" 1 2)
        competitiveness (get-valid-input "Do you find yourself competitive? 1-yes, 2-no" 1 2)]
    {:name name
     :experience experience
     :adroit adroit
     :mood mood
     :teamplayer teamplayer
     :frightened frightened
     :theme theme
     :competitiveness competitiveness
     :summary 0}))

(defn calculate-divisions [num-players]
  (let [cases [[2 10 2] [6 15 3] [8 20 4] [10 25 5] [12 30 6] [14 35 7]]
        matching-cases (filter #(when (<= (first %) num-players (second %)) %) cases)
        divisions (map #(nth % 2) matching-cases)]
    (if (seq divisions)
      divisions
      (throw (RuntimeException. "No matching division found")))))

(defn prompt-for-division [divisions]
  (loop []
    (let [chosen-division (prompt (str "You can divide into "
                                       (clojure.string/join ", " divisions)
                                       ". Which option would you like? Type one of these numbers."))]
      (if-let [division-choice (try (Integer/parseInt chosen-division)
                                    (catch NumberFormatException _ nil))]
        (if (contains? (set divisions) division-choice)
          division-choice
          (do (println "Please enter a valid division number from the options provided.")
              (recur)))
        (do (println "Invalid input. Please enter a number.")
            (recur))))))

(defn get-room-info [num-players]
  (let [division (prompt-for-division (calculate-divisions num-players))]
    {:horror (get-valid-input "Now about the room. Is the one you are playing classified as horror? 1-yes, 2-no" 1 2)
     :linear (get-valid-input "Is it linear or not? 1-yes, 2-no" 1 2)
     :tech (get-valid-input "Is it full of high-tech mechanisms or more old school based on riddles? (ask your game-master) 1-mechanisms, 2-riddles" 1 2)
     :knowledge (get-valid-input "Does it require knowledge about the topic? 1-yes, 2-no" 1 2)
     :division division}))

(defn room-data-maker [num-players]
  (let [room-data (get-room-info num-players)]
    (println "OK! Thank you.")
    (println "Room information stored in memory:")
    (prn room-data)
    room-data))

(defn players-vector-maker []
  (flush)
  (let [num-users (get-valid-input "How many users?" 2 35)
        user-info (doall (repeatedly num-users get-user-info))]
    (println "OK! Thank you.")
    (println "User information stored in memory:")
    (prn user-info)
    user-info))

(defn choose-and-run-algorithm [players room-data num-teams]
  (let [algorithm (cond
                    (<= num-teams 4)
                    (cond
                      (= (:linear room-data) 1) (if (< (count players) 8)
                                                  'logic/create-and-print-players-across-teams
                                                  'logic/create-and-print-k-means-teams)
                      (= (:linear room-data) 2) 'logic/create-and-print-balanced-teams
                      :else 'logic/create-and-print-randomly-made-teams)
                    (<= num-teams 7)
                    (cond
                      (= (:linear room-data) 1) (if (< (count players) 8)
                                                  'logic/create-and-print-divisions-by-score
                                                  'logic/create-and-print-round-robin-teams)
                      (= (:linear room-data) 2) 'logic/create-and-print-divisions-by-score
                      :else 'logic/create-and-print-randomly-made-teams)
                    :else 'logic/create-and-print-randomly-made-teams)]

    (println "Used algorithm is:" algorithm)
    ((resolve algorithm) players room-data num-teams)))

(defn -main
  [& args]
  (let [players (players-vector-maker)
        room-data (room-data-maker (count players))]
    (choose-and-run-algorithm players room-data (:division room-data))))

  



    
  
  

