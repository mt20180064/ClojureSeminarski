(ns logic)

(defn my-into
  [target additions]
  (apply conj additions target))

(def igraci [{:name "Nina" :experience 4 :teamplayer 1 :adroit 5 :mood 5 :theme 2 :frightened 2 :competitiveness 2 }
             {:name "Anis" :experience  4 :teamplayer 1 :adroit 4 :mood 2 :theme 1 :frightened  1 :competitiveness 1 }
             {:name "Điri" :experience  5 :teamplayer 2 :adroit 1 :mood 5 :theme  2 :frightened 2 :competitiveness 1 }
             {:name "Joča" :experience  2 :teamplayer 1 :adroit 3 :mood 1 :theme  1 :frightened  1 :competitiveness 2 }
             {:name "Maša" :experience  5 :teamplayer 2 :adroit 4 :mood 2 :theme  2 :frightened  2 :competitiveness 1 }
             {:name "Nemica" :experience 1 :teamplayer 1 :adroit 3 :mood 2 :theme  2 :frightened  1 :competitiveness 2 }
             {:name "Roko" :experience 5 :teamplayer 2 :adroit 1 :mood 3 :theme  2 :frightened  2 :competitiveness 1 }
             {:name "Roki" :experience  4 :teamplayer 1 :adroit 4 :mood 1 :theme  2 :frightened  1 :competitiveness 2 }
             {:name"Teo" :experience  1 :teamplayer 1 :adroit 2 :mood 5 :theme  2 :frightened  1 :competitiveness 2 }
             {:name "Nelsi" :experience  3 :teamplayer 2 :adroit 3 :mood 4 :theme  1 :frightened  1 :competitiveness 2  }
             {:name "Beli" :experience  5 :teamplayer 2 :adroit 1 :mood 2 ::theme  1 :frightened  2 :competitiveness 1  }
             {:name "Cole" :experience  4 :teamplayer 1 :adroit 5 :mood 1 :theme  1 :frightened 1 :competitiveness 1  }
             ;{:name "Bebinger" :experience  1 :teamplayer 1 :adroit 4 :mood 5 :theme  2 :frightened  2 :competitiveness 1 }
             ])

(defn group-competitive-players [players]
  (let [grouped-players (group-by #(= (:competitiveness %) 1) players)]
    (concat (grouped-players true) (grouped-players false))))

(group-competitive-players igraci)

 
(def duzina (count igraci))
;duzina



(defn check-topic
  [igrac]
  (if (get igrac :theme)
    2
    0)
 )


(defn check-fear
  [igrac]
  (if (= 1 (get igrac :frightened)) 
   -1
    2))

(defn sum-good-attributes
  [igrac]
(+ (get igrac :mood) (get igrac :experience) (get igrac :adroit)))


(defn check-competitive
  [igrac]
  (if (= (get igrac :competitiveness) 1)
    -1
    1))

(defn check-team-player
  [igrac]
  (if (= (get igrac :teamplayer) 1)
    2
    0))

(defn sum-everything
  [igrac]
  (assoc igrac :summary (+ (sum-good-attributes igrac) (check-fear igrac) (check-topic igrac) (check-competitive igrac) (check-team-player igrac))))


(defn sort-everything
 [igraci duzina]
  (loop [i 0 sortiran []]
    (if (= duzina i)
      sortiran
   (recur (inc i) (sort-by :summary (my-into [(sum-everything (nth igraci i))] sortiran))))))


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
 


(defn pravljenjeEkipa
  [n lista]
  (let [sortirana (give-teams n (sort-everything lista n))] 
    (loop  [i 0 j 1 t1 #{(if (clojure.core/odd? n)
                           (get (last sortirana) :name)
                           "")} t2 #{}]
      (if (or (= n j) (= n i) (= n (+ (count t2) (count t2))))
        (println "evo prvog tima" t1 "  evo drugog tima" t2)
        (recur (+ 1 (inc i))
               (+ 1 (inc j))
               (my-into [(get (nth sortirana i) :name)] t1)
               (my-into [(get (nth sortirana j) :name)] t2))))))

(pravljenjeEkipa duzina igraci)




