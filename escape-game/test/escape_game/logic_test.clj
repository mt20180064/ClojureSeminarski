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
