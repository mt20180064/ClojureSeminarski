(ns escape-game.logic-test
  (:require [clojure.test :refer :all]
            [logic :refer :all]))


(def players
  [{:name "Nina" :experience 1 :teamplayer 1 :adroit 3 :mood 5 :theme 2 :frightened 2 :competitiveness 2}
   {:name "Anis" :experience 4 :teamplayer 1 :adroit 4 :mood 2 :theme 1 :frightened 1 :competitiveness 1}
   {:name "Điri" :experience 5 :teamplayer 2 :adroit 1 :mood 5 :theme 2 :frightened 2 :competitiveness 1}
   {:name "Joča" :experience 2 :teamplayer 1 :adroit 3 :mood 1 :theme 1 :frightened 1 :competitiveness 2}
   {:name "Maša" :experience 5 :teamplayer 2 :adroit 4 :mood 2 :theme 2 :frightened 2 :competitiveness 1}
   {:name "Nemica" :experience 1 :teamplayer 1 :adroit 3 :mood 2 :theme 2 :frightened 1 :competitiveness 2}
   {:name "Roko" :experience 5 :teamplayer 2 :adroit 1 :mood 3 :theme 2 :frightened 2 :competitiveness 1}
   {:name "Roki" :experience 4 :teamplayer 1 :adroit 4 :mood 1 :theme 2 :frightened 1 :competitiveness 2}
   {:name "Teo" :experience 1 :teamplayer 1 :adroit 2 :mood 5 :theme 2 :frightened 1 :competitiveness 2}
   {:name "Nelsi" :experience 3 :teamplayer 2 :adroit 3 :mood 4 :theme 1 :frightened 1 :competitiveness 2}
   {:name "Beli" :experience 5 :teamplayer 2 :adroit 1 :mood 2 :theme 1 :frightened 2 :competitiveness 1}
   {:name "Cole" :experience 4 :teamplayer 1 :adroit 5 :mood 1 :theme 1 :frightened 1 :competitiveness 1}
   {:name "Bebinger" :experience 1 :teamplayer 1 :adroit 4 :mood 5 :theme 2 :frightened 2 :competitiveness 1}])


(def combination-of-rooms 
  [{:horror 1 :linear 1 :tech 1 :knowledge 1}
   {:horror 1 :linear 1 :tech 1 :knowledge 2}
   {:horror 1 :linear 1 :tech 2 :knowledge 1}
   {:horror 1 :linear 2 :tech 1 :knowledge 2}
   {:horror 2 :linear 1 :tech 1 :knowledge 1}
   {:horror 1 :linear 2 :tech 1 :knowledge 2}
   {:horror 1 :linear 2 :tech 2 :knowledge 1}
   {:horror 2 :linear 2 :tech 1 :knowledge 1}
   {:horror 2 :linear 1 :tech 2 :knowledge 1}
   {:horror 2 :linear 1 :tech 1 :knowledge 2}
   {:horror 1 :linear 1 :tech 2 :knowledge 2}
   {:horror 2 :linear 2 :tech 1 :knowledge 2}
   {:horror 2 :linear 2 :tech 2 :knowledge 1}
   {:horror 2 :linear 1 :tech 2 :knowledge 2}
   {:horror 1 :linear 2 :tech 2 :knowledge 2}
   {:horror 2 :linear 2 :tech 2 :knowledge 2}])


(deftest test-round-robin-distribute
  (let [long-list-of-players (concat players players players players) 
        num-teams-list [2 3 4 5 6 7]] 
    (doseq [num-teams num-teams-list
            room-data combination-of-rooms]
      (let [selected-players (take (* num-teams 3) long-list-of-players)] 
        (testing (str "Testing with subset of players, " num-teams " teams, and room data " room-data)
          (let [teams (logic/round-robin-distribute selected-players room-data num-teams)]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams)))))) 
    (doseq [num-teams num-teams-list
            room-data combination-of-rooms]
      (testing (str "Testing with full list of players, " num-teams " teams, and room data " room-data)
        (let [teams (when (>= (count long-list-of-players) (* 2 num-teams))
                      (logic/round-robin-distribute long-list-of-players room-data num-teams))]
          (is (= num-teams (count teams)))
          (is (not-any? empty? teams))
          (when (< (count long-list-of-players) (* 2 num-teams))
            (is (nil? (logic/round-robin-distribute long-list-of-players room-data num-teams)))))))))

(deftest test-divide-players-into-teams
  (let [long-list-of-players (concat players players players players)
        num-teams-list [2 3 4 5 6 7]]

    (doseq [num-teams num-teams-list]
      (let [assignments (logic/distribute-players-randomly long-list-of-players nil num-teams) 
            selected-players (take (* num-teams 3) long-list-of-players)]

        (testing (str "Testing with subset of players and " num-teams " teams")
          (let [teams (logic/divide-players-into-teams assignments selected-players num-teams)]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams))))

        (testing (str "Testing with full list of players and " num-teams " teams")
          (let [teams (when (>= (count long-list-of-players) (* 2 num-teams))
                        (logic/divide-players-into-teams assignments long-list-of-players num-teams))]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams))))))))

(deftest test-divide-players-into-teams-by-score
  (let [long-list-of-players (concat players players players players)
        num-teams-list [2 3 4 5 6 7]]

    (doseq [num-teams num-teams-list]
      (let [adjusted-weights {:experience (rand)
                              :teamplayer (rand)
                              :adroit (rand)
                              :mood (rand)
                              :theme (rand)
                              :frightened (rand)
                              :competitiveness (rand)}
            selected-players (take (* num-teams 3) long-list-of-players)]

        (testing (str "Testing with subset of players and " num-teams " teams")
          (let [teams (logic/divide-players-into-teams-by-score selected-players num-teams adjusted-weights)]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams))))

        (testing (str "Testing with full list of players and " num-teams " teams")
          (let [teams (when (>= (count long-list-of-players) (* 2 num-teams))
                        (logic/divide-players-into-teams-by-score long-list-of-players num-teams adjusted-weights))]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams))))))))

(deftest test-sort-everything
  (let [long-list-of-players (concat players players players)
        player-counts [5 10 15 (count long-list-of-players)]]
    (doseq [player-count player-counts]
      (let [selected-players (take player-count long-list-of-players)]
        (testing (str "Testing sort-everything with " player-count " players")
          (let [sorted-players (logic/sort-everything selected-players)]
            (is (= player-count (count sorted-players)))
            (is (apply <= (map :summary sorted-players)))))))))


(deftest test-distribute-players-across-teams
  (let [long-list-of-players (concat players players players)
        num-teams-list [2 3 4 5 6 7]]

    (doseq [num-teams num-teams-list]
      (let [selected-players (take (* 3 num-teams) long-list-of-players)
            sorted-players (logic/sort-everything selected-players)]

        (testing (str "Testing distribute-players-across-teams with sorted " (count sorted-players) " players and " num-teams " teams")
          (let [teams (logic/distribute-players-across-teams sorted-players nil num-teams)]
            (is (= num-teams (count teams)))
            (is (not-any? empty? teams))))))))

(deftest test-calculate-coef
  (let [sample-players (take 5 (shuffle players))] 

    (doseq [player sample-players
            room-data combination-of-rooms]
      (testing (str "Testing calculate-coef with player " (:name player) " and room data " room-data)
        (let [coef (logic/calculate-coef player room-data)] 
          (is (number? coef)))))))

(def long-list-of-players
  [concat players players players])

(deftest test-calculate-threshold
  (let [sample-players (take 10 (shuffle long-list-of-players)) 
        num-teams 2]

    (doseq [room-data combination-of-rooms]
      (let [teams (logic/distribute-players-randomly sample-players room-data num-teams)
            threshold (logic/calculate-threshold room-data teams)]

        (testing (str "Testing calculate-threshold with room data " room-data) 
          (is (number? threshold))
          (is (>= threshold 0))))))) 


(deftest test-adjust-weights-based-on-room
  (let [random-room-configs (take 5 (shuffle combination-of-rooms))] 

    (doseq [room-data random-room-configs]
      (testing (str "Testing adjust-weights-based-on-room with room data " room-data)
        (let [adjusted-weights (logic/adjust-weights-based-on-room room-data)
              base-weights {:experience 0.4, :adroit 0.35, :mood 0.3, :teamplayer 0.25, :competitiveness 0.2, :theme 0.15, :frightened 0.1}] 
          (is (= (keys base-weights) (keys adjusted-weights))) 
          )))))















