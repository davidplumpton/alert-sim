;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Ship [rooms])

(def *order* [:red-up :white-up :blue-up])


(defn create-initial-ship [num-players]
  (Ship. {:red-up []
	  :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players))))
	  :blue-up []}))

(defn find-player [ship player]
  (first (first
	  (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship)))))

(defn which-room [where direction]
  (cond
   (and (= where :red-up) (= direction :left)) :red-up
   (and (= where :red-up) (= direction :right)) :white-up
   (and (= where :white-up) (= direction :left)) :red-up
   (and (= where :white-up) (= direction :right)) :blue-up
   (and (= where :blue-up) (= direction :left)) :white-up
   (and (= where :blue-up) (= direction :right)) :blue-up))

(defn move-player [ship player direction]
  (let [ship-rooms (:rooms ship)
	position (find-player ship player)
	removed-rooms (reduce (fn [m room] (assoc m room (remove #{player} (get ship-rooms room)))) {} *order*)
	destination-room (which-room position direction)
	destination-contents (destination-room ship)
	updated-rooms (assoc removed-rooms destination-room (conj destination-contents player))]
    (Ship. updated-rooms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ship-structure
  (let [ship (create-initial-ship 4)]
    (is (= :white-up (find-player ship :player1)))
    (is (= :white-up (find-player ship :player4)))
    (is (not (= :white-up (find-player ship :player5))))))

(deftest player-move
  (let [ship (create-initial-ship 4)
	ship-with-player2-left (move-player ship :player2 :left)]
    (is (= :red-up (find-player ship-with-player2-left :player2)))
    (is (= :white-up (find-player ship-with-player2-left :player3)))))

(deftest room-movements
  (is (= :red-up (which-room :red-up :left)))
  (is (= :red-up (which-room :white-up :left)))
  (is (= :white-up (which-room :blue-up :left)))
  (is (= :white-up (which-room :red-up :right)))
  (is (= :blue-up (which-room :white-up :right)))
  (is (= :blue-up (which-room :blue-up :right))))