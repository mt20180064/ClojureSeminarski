(ns escape-game.core-test
  (:require [clojure.test :refer :all]
            [escape-game.core :refer :all]))
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


(deftest test-choose-and-run-algorithm
  (let [long-list-of-players (concat players players players) 
        num-teams-list [2 3 4 5 6 7] 
        random-room-configs (take 15 (shuffle combination-of-rooms))] 

    (doseq [num-teams num-teams-list
            room-data random-room-configs]
      (testing (str "Testing choose-and-run-algorithm with num-teams=" num-teams " and room data " room-data)
        (try
          (escape-game.core/choose-and-run-algorithm long-list-of-players room-data num-teams)
          (is true) 
          (catch Exception e
            (is false (str "Failed with error: " e))))))))


