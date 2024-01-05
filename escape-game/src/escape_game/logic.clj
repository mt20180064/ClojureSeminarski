(ns logic
  (:require [clojure.string]
            ))

(defn my-into
  [target additions]
  (apply conj additions target))

(def igraci
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

(defn print-teams [balanced-teams]
  (let [format-team (fn [team]
                      (clojure.string/join ", " (map :name team)))]
    (loop [idx 0
           teams balanced-teams
           result ""]
      (if (empty? teams)
        result
        (let [team-str (format-team (first teams))
              new-result (str result "Team " (inc idx) ": " team-str "\n")]
          (recur (inc idx) (rest teams) new-result))))))

(defn check-topic [igrac]
  (if (get igrac :theme)
    2
    0))

(defn check-fear [igrac]
  (if (= 1 (get igrac :frightened))
    -1
    2))

(defn sum-good-attributes [igrac]
  (+ (get igrac :mood) (get igrac :experience) (get igrac :adroit)))

(defn check-competitive [igrac]
  (if (= (get igrac :competitiveness) 1)
    -1
    1))

(defn check-team-player [igrac]
  (if (= (get igrac :teamplayer) 1)
    2
    0))

(defn sum-everything [igrac]
  (assoc igrac :summary (+ (sum-good-attributes igrac) (check-fear igrac) (check-topic igrac) (check-competitive igrac) (check-team-player igrac))))

(defn sort-everything [igraci]
  (loop [i 0 sortiran []]
    (if (= (count igraci) i)
      sortiran
      (recur (inc i) (sort-by :summary (my-into [(sum-everything (nth igraci i))] sortiran))))))
 
(defn distribute-players-across-teams [players room-data num-teams]
  (sort-everything players)
  (let [total-players (count players)]
    (loop [front 0
           back (dec total-players)
           teams (vec (repeat num-teams []))]
      (cond
        (> front back) teams
        (= front back)
        (let [random-team (rand-int num-teams)]
          (update teams random-team conj (nth players front)))
        :else
        (let [team-index (mod front num-teams)
              updated-teams (-> teams
                                (update team-index conj (nth players front))
                                (update team-index conj (nth players back)))]
          (recur (inc front) (dec back) updated-teams))))))

(defn create-and-print-players-across-teams [players room-data num-teams]
  (let [few-teams (distribute-players-across-teams players room-data num-teams)]
    (doseq [team (map-indexed (fn [idx team]
                                (str "Team " (inc idx) ": " (clojure.string/join ", " (map :name team))))
                              few-teams)]
      (println team))))

;sada cu da pokusam da napravim algoritam za podelu u tri ekipe. 
;ideja mi je da u odnosu na ovaj za podelu na dva unapredim algoritam tako sto ce se 
;uzimati u obzir i kakva je soba. To cu da postignem tako sto 
;cu uvesti pojam koeficijenta igraca na koji ce razlicite vrednosti vezane za sobu razlicito
;ucitaci na vrednost atributa igraca tj doprinos svakog od njih konacnom koeficijentu

(defn calculate-coef [player room-data]
  (let [base-coef (+ (:experience player) (:adroit player) (:mood player))
        horror-adjust (cond
                        (and (= 1 (:horror room-data)) (= 1 (:frightened player))) -4
                        (and (= 1 (:horror room-data)) (= 2 (:frightened player))) -1
                        :else 0)
        knowledge-adjust (if (and (= 1 (:knowledge room-data)) (= 1 (:theme player))) 3 0)
        tech-adjust (if (and (= 1 (:tech room-data)) (> (:experience player) 3) (= 1 (:theme player))) 1 0)
        linear-adjust (if (and (= 2 (:linear room-data)) (= 1 (:teamplayer player))) 1 0)]
    (+ base-coef horror-adjust knowledge-adjust tech-adjust linear-adjust)))

(defn add-coefs-to-players [players room-data]
  (map (fn [player]
         (assoc player :coef (calculate-coef player room-data)))
       players))

(defn divide-players [players num-teams]
  (let [ensure-coef (fn [player] (if (nil? (:coef player)) (assoc player :coef 0) player))
        sorted-players (sort-by :coef > (map ensure-coef players))
        initial-teams (vec (repeat num-teams []))]
    (reduce (fn [teams player]
              (let [team-sizes (map count teams)
                    min-size (apply min team-sizes)
                    min-index (first (keep-indexed #(when (= %2 min-size) %1) team-sizes))
                    updated-teams (update teams min-index conj player)]
                updated-teams))
            initial-teams
            sorted-players)))

;ovo do sada je izostavilo uticaj toga da ti je igrac timski igrac ili kompetitivan
;jer ti faktori imaju znacaja tek nakon sto je inicijalna podela vec napravljena, zato sto sami od sebe
;nisu vazni koliko i kada se porede sa ostalima. Dakle, sada dodajem te faktore u algoritam tako sto
;timski igraci bolje igraju kada su zajedno, dok kompetitivni igraci nisu losiji ako su sami, ali losiji su
;ako su dva u istom timu. To cu implementirati preko uticaja na koeficijent
(defn adjust-coefs-for-traits [team]
  (let [teamplayer-count (count (filter #(= 1 (:teamplayer %)) team))
        competitive-count (count (filter #(= 1 (:competitiveness %)) team))]
    (map (fn [player]
           (let [teamplayer-adjust (if (> teamplayer-count 1) 1 0)
                 competitive-adjust (if (> competitive-count 1) -1 0)]
             (update player :coef + teamplayer-adjust competitive-adjust)))
         team)))

(defn calculate-threshold [room-data teams]
  (let [base-threshold 8
        team-size-factor (if (> (count (first teams)) 5) 1 0.8)
        linear-factor (if (= 1 (:linear room-data)) 1.2 1)
        horror-factor (if (= 1 (:horror room-data)) 0.8 1)]
    (int (* base-threshold team-size-factor linear-factor horror-factor))))

(defn is-balanced [teams threshold]
  (let [team-coefs (map #(reduce + (map :coef %)) teams)
        max-coef (apply max team-coefs)
        min-coef (apply min team-coefs)]
    (<= (- max-coef min-coef) threshold)))

(defn redivide-if-unbalanced [teams original-players threshold]
  (if (is-balanced teams threshold)
    teams
    (let [updated-players (apply concat (map adjust-coefs-for-traits teams))]
      (divide-players updated-players (count teams)))))

;sledecu funkciju pravim da bih lakse pozvala ceo ovaj algoritam iz drugog namespace-a
(defn create-and-print-balanced-teams [players room-data num-teams]
  (let [players-with-coef (add-coefs-to-players players room-data)
        divided-teams (divide-players players-with-coef num-teams)
        threshold (calculate-threshold room-data divided-teams)
        balanced-teams (redivide-if-unbalanced divided-teams players-with-coef threshold)]
    (doseq [team (map-indexed (fn [idx team]
                                (str "Team " (inc idx) ": " (clojure.string/join ", " (map :name team))))
                              balanced-teams)]
      (println team))))

;sada implementiram k-means algoritam
(defn scale-value [x]
  (/ (- x 1) 4))

(defn euclidean-distance [vec1 vec2]
  (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) vec1 vec2))))

(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn mean [vectors]
  (map average (apply map vector vectors)))

(defn extract-features [player]
  [(scale-value (player :experience))
   (scale-value (player :adroit))
   (scale-value (player :mood))
   (scale-value (player :theme))])

;funkcija map prolazi kroz igrace i onda svakog od njih dodeljuje centroidu kom je najblizi 
;(if to obavlja unutar funkcije reduce koja vektor sa mapama igraca
;transformise tako sto deli igrace u klastere)
(defn assign-players-to-nearest-centroid [players centroids]
  (let [player-assignments (map (fn [player]
                                  (let [player-features (extract-features player)]
                                    (try
                                      (reduce (fn [a b]
                                                (let [centroid-a (centroids a)
                                                      centroid-b (centroids b)]
                                                  (if (< (euclidean-distance player-features centroid-a)
                                                         (euclidean-distance player-features centroid-b))
                                                    a b)))
                                              (keys centroids))
                                      (catch Exception e
                                        (println "Exception for player:" player "Error:" e)
                                        nil))))
                                players)]
    (zipmap players player-assignments)))

;grupise igrace na osnovu indeksa njima dodeljenog klastera i onda za svaku grupu
;racuna prosecnu vrednost relevantnih atributa pa na osnovu toga pravi nove centroide
(defn update-centroids [players assignments k]
  (let [grouped-players (group-by #(get assignments %) players)]
    (into {} (map (fn [i]
                    [i (mean (vec (map extract-features (get grouped-players i))))])
                  (range k)))))

;ovu funkciju sam delom prepisala sa gitHuba (deo gde se dodeljuju inicijalni centroidi), prilagodivsi je mojim kriterijumima i do sada napravljenim funkcijama
(defn k-means-players [players k]
  (let [initial-centroids (into {} (map-indexed (fn [i _] [i (extract-features (nth players i))]) (range k)))
        max-iterations 50]
    (loop [centroids initial-centroids
           n 0]
      (let [assignments (assign-players-to-nearest-centroid players centroids)
            new-centroids (update-centroids players assignments k)]
        (if (or (= centroids new-centroids) (>= n max-iterations))
          {:centroids centroids, :assignments assignments}
          (recur new-centroids (inc n)))))))
;max-iterations sam postavila na 50 (nasumicno) da bih izbegla rizik od overflow, testiranjem cu kasnije utvrditi da li treba da bude neki drugi broj

;ova fukcija treba da podeli igrace u timove na osnovu rezultata k-meansa.Prvo ce svakom timu dodeliti po jednog igraca iz svakog klastera
;(nastavice sa takvom podelom sve dok ima dovoljno igraca u svim klasterima), a onda ce preostale igrace podeliti ravnomervno tako da timovi imaju
;isto ili priblizno isti broj igraca. 
(defn divide-players-into-teams [assignments players num-teams]
  (let [cluster-groups (group-by #(get assignments %) players)]
    (let [team-assignments (reduce (fn [teams cluster-group]
                                     (reduce (fn [teams player]
                                               (let [team-index (->> teams
                                                                     (map-indexed (fn [idx team] [idx (count team)]))
                                                                     (sort-by second)
                                                                     (first)
                                                                     (first))]
                                                 (update teams team-index conj (:name player))))
                                             teams
                                             (second cluster-group)))
                                   (vec (repeat num-teams []))
                                   (vals cluster-groups))]
      team-assignments)))
;pravim funkcijju koja objedinjuje sve ovo da bih je lakse pozvala iz drugog namespace-a
(defn create-and-print-k-means-teams [players room-data num-teams]
  (let [k-means-result (k-means-players players num-teams)
        assignments (:assignments k-means-result)
        teams (divide-players-into-teams assignments players num-teams)
        formatted-teams (map-indexed (fn [idx team]
                                       (str "Team " (inc idx) ": " (clojure.string/join ", " team)))
                                     teams)]
    (doseq [team formatted-teams]
      (println team))))
 
;sledeci algoritam delice igrace na osnovu vrednosti njihovih atributa, ali ce njima prethodno biti
;dodati tezinski koeficijenti koji zavise i od tipa sobe koji se igra, prvo definisemo inicijalne koeficijente
;razlika u odnosu na vec prethodno implementirani algoritam je sto je taj radio sa ukupnim koeficijenotm,
;a sada varira svaka od vrednosti pojedinacno

(defn adjust-weights-based-on-room [room-data]
  (let [base-weights {:experience 0.4, :adroit 0.35, :mood 0.3, :teamplayer 0.25, :competitiveness 0.2, :theme 0.15, :frightened 0.1}]
    (-> base-weights
        (update :adroit #(if (= (:tech room-data) 1) (* % 1.3) %))
        (update :teamplayer #(if (= (:linear room-data) 2) (* % 1.5) %))
        (update :frightened #(if (= (:horror room-data) 1) (* % 1.6) %))
        (update :theme #(if (= (:knowledge room-data) 1) (* % 1.7) %))
        (update :experience #(if (= (:linear room-data) 1) (* % 1.4) %)))))
;za igraca racunamo skor na osnovu njihovih atributa i odgovarajucih tezinskih koeficijenata
(defn calculate-player-score [player weights]
  (reduce + (map (fn [[attr weight]] (* (get player attr 0) weight)) weights)))

(defn divide-players-into-teams-by-score [players num-teams adjusted-weights]
  (let [player-scores (map #(calculate-player-score % adjusted-weights) players)
        sorted-players (map first (sort-by second > (zipmap players player-scores)))
        initial-teams (vec (repeat num-teams []))]
    (reduce (fn [teams player]
              (let [team-index (->> teams
                                    (map-indexed (fn [idx team] [idx (count team)]))
                                    (sort-by second)
                                    (first)
                                    (first))]
                (update teams team-index conj player)))
            initial-teams
            sorted-players)))

(defn create-and-print-divisions-by-score [players room-data num-teams]
  (let [adjusted-weights (adjust-weights-based-on-room room-data)
        team-assignments (divide-players-into-teams-by-score players num-teams adjusted-weights)]
    (doseq [team (map-indexed (fn [idx team]
                                (str "Team " (inc idx) ": " (clojure.string/join ", " (map :name team))))
                              team-assignments)]
      (println team))))

;algoritam koji cu sada implementirati je prilagodjeni Round Robin algoritam s tim sto cu na 
;specifican nacin soritrati igrace. Umesto da se rangiranje vrsi na osnovu zbira vrednosti njihovih atributa
;ili na osnovu nekih konkretnih atributa, vrste sobe, itd., ovaj put ce se za boljeg 
;igraca smatrati onaj cije su vrednosti atributa medjusobno najslicnije. 
;Da bih to ostvarila za pocetak mi treba funkcija za izracunavanje standardne devijacije:
(defn standard-deviation [numbers]
  (let [mean (double (/ (reduce + numbers) (count numbers)))
        sum-squared-diffs (reduce (fn [acc n] (+ acc (Math/pow (- n mean) 2))) 0 numbers)]
    (Math/sqrt (/ sum-squared-diffs (count numbers)))))

;posto ona ukljucuje deljenje, bolje da pretvorim celobrojne vrednosti atributa (sve) u decimalne

(defn player-attributes-list [player]
  [(double (:experience player))
   (double (:teamplayer player))
   (double (:adroit player))
   (double (:mood player))
   (double (:theme player))
   (double (:frightened player))
   (double (:competitiveness player))])

(defn round-robin-distribute [players room-data num-teams]
  (let [player-std-devs (map (fn [player] [player (standard-deviation (player-attributes-list player))]) players)
        sorted-players (map first (sort-by second player-std-devs))
        initial-teams (vec (repeat num-teams []))]
    (reduce (fn [teams player]
              (let [team-index (->> teams
                                    (map-indexed (fn [idx team] [idx (count team)]))
                                    (sort-by second)
                                    (first)
                                    (first))]
                (update teams team-index conj player)))
            initial-teams
            sorted-players)))

(defn create-and-print-round-robin-teams [players room-data num-teams]
  (let [round-robin-teams (round-robin-distribute players room-data num-teams)]
    (doseq [team (map-indexed (fn [idx team]
                                (str "Team " (inc idx) ": " (clojure.string/join ", " (map :name team))))
                              round-robin-teams)]
      (println team))))


;ubacicu i algoritam za nasumicnu raspodelu kako bih kasnije poredila rezultate
;jedino o cemu algoritam vodi racuna je da bude jednak ili priblizno jednak broj igraca u svim timovima (obezbedjeno mod funkcijom)
(defn distribute-players-randomly [players room-data num-teams]
  (let [shuffled-players (shuffle players)
        initial-teams (vec (repeat num-teams []))]
    (reduce (fn [teams player]
              (let [team-index (->> teams
                                    (map-indexed (fn [idx team] [idx (count team)]))
                                    (sort-by second)
                                    (first)
                                    (first))]
                (update teams team-index conj player)))
            initial-teams
            shuffled-players)))

(defn create-and-print-randomly-made-teams [players room-data num-teams]
  (let [randomly-teams (distribute-players-randomly players room-data num-teams)]
    (doseq [team (map-indexed (fn [idx team]
                                (str "Team " (inc idx) ": " (clojure.string/join ", " (map :name team))))
                              randomly-teams)]
      (println team))))


























 
 








