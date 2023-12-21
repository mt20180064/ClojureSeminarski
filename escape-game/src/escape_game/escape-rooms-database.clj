(ns escape-rooms-database
(:require [clojure.java.jdbc :as jdbc]))

(def db-spec
  {:classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "mem:test"
   :user "mini"
   :password ""})

(defn get-db-connection []
  (jdbc/get-connection db-spec))

(defn fetch-data [db]
  (jdbc/with-db-connection [conn db]
    (jdbc/query conn ["SELECT * FROM escape-rooms"]))
  )

(defn insert-data [db data]
  (jdbc/with-db-connection [conn db]
    (jdbc/insert! conn
                  :your_table
                  {:column1 (:value1 data)
                   :column2 (:value2 data)
                   ;; Add more columns as needed
                   })))

;;(defn -main []
 ; (let [data (fetch-data)]
   ; (println "Fetched data:" data)))