;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Ship [rooms])

(defn create-initial-ship [num-players]
  (Ship. {:red-up []
	  :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players))))
	  :blue-up []}))

(defn find-player [ship player]
  (first (first
	  (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ship-structure
  (let [ship (create-initial-ship 4)]
    (is (= :white-up (find-player ship :player1)))
    (is (= :white-up (find-player ship :player4)))))
