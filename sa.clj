;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Ship [rooms threats])

(def *rooms* [:red-up :white-up :blue-up :red-down :white-down :red-down])

(defn create-initial-ship [num-players]
  (Ship.
   (assoc
       (reduce (fn [m r] (assoc m r [])) {} *rooms*)
     :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players)))))
   {}))

(defn find-player [ship player]
  (first (first
	  (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship)))))

(defn- move-grav-lift [zone deck]
  (keyword (str zone "-" (if (= deck "up") "down" "up"))))

(defn- move-left [zone deck]
  (keyword (str (case zone "red" "red" "white" "red" "blue" "white") "-" deck)))

(defn- move-right [zone deck]
   (keyword (str (case zone "red" "white" "white" "blue" "blue" "blue") "-" deck)))
  
(defn which-room [where direction]
  (let [[zone deck] (.split (name where) "-")]
    (cond
     (= direction :left) (move-left zone deck)
     (= direction :right) (move-right zone deck)
     (= direction :updown) (move-grav-lift zone deck)
     true (throw (str "Bad direction " direction)))))

(defn move-player [ship player direction]
  (let [ship-rooms (:rooms ship)
	position (find-player ship player)
	removed-rooms (reduce conj {} (for [[k v] ship-rooms] [k (remove #{player} v)]))
	destination-room (which-room position direction)
	destination-contents (destination-room ship)
	updated-rooms (assoc removed-rooms destination-room (conj destination-contents player))]
    (assoc ship :rooms updated-rooms)))

(defn add-threat [ship time new-threat]
  (assoc-in ship [:threats time] new-threat))

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
  (is (= :red-down (which-room :white-down :left)))
  (is (= :white-up (which-room :blue-up :left)))
  (is (= :white-up (which-room :red-up :right)))
  (is (= :blue-up (which-room :white-up :right)))
  (is (= :blue-up (which-room :blue-up :right)))
  (is (= :blue-down (which-room :white-down :right)))
  (is (= :red-down (which-room :red-up :updown)))
  (is (= :white-up (which-room :white-down :updown)))
  (is (= :blue-up (which-room :blue-down :updown))))

(deftest threats
  (let [ship (create-initial-ship 4)]
    (is (= 0 (count (:threats ship))))
    (let [ship-with-threat (add-threat ship 5 :fighter)]
      (is (= [5 :fighter] (first (:threats ship-with-threat)))))))
