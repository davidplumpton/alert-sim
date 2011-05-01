;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Ship [rooms])

(defn create-initial-ship []
  (Ship. {:red-up [] :white-up [:player1] :blue-up []}))

(defn find-player [ship player]
  (first (first
	  (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ship-structure
  (let [ship (create-initial-ship)]
    (is (= :white-up (find-player ship :player1)))))
