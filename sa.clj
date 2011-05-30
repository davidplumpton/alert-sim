;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Ship [rooms threats trajectories])

(def rooms [:red-up :white-up :blue-up :red-down :white-down :red-down])

;; Commas indicate range boundaries
(def trajectories [
     [:z :_ :_ :_ :x, :_ :_ :_ :_ :_]
     [:z :_ :_ :_ :_, :_ :_ :x :_ :_, :_]
     [:z :_ :y :_ :_, :_ :_ :x :_ :_, :_ :_]
     [:z :_ :_ :_ :y, :_ :_ :_ :x :_, :_ :_ :_]
     [:z :_ :_ :_ :_, :_ :y :_ :_ :_, :x :_ :_ :_]
     [:z :_ :y :_ :_, :_ :y :_ :_ :x, :_ :_ :_ :_ :_]
     [:z :_ :_ :_ :y, :_ :_ :y :_ :_, :_ :x :_ :_ :_ :_]])

(defn create-initial-ship
  "Create the starting position, specifying the number of players"
  ([num-players]
     (create-initial-ship num-players trajectories))
  ([num-players trajectories]
  (Ship.
   (assoc
       (apply assoc {} (interleave rooms (repeat [])))
     :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players)))))
   {}
   (apply assoc {} (interleave [:red :white :blue :internal] trajectories)))))

(defn find-player
  "Find the room a player is in"
  [ship player]
  (ffirst (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship))))

(defn- move-grav-lift
  "Determine which room the grav-lift leads to"
  [zone deck]
  (keyword (str zone "-" (if (= deck "up") "down" "up"))))

(defn- move-left
  "Determine which room is left of here"
  [zone deck]
  (keyword (str (case zone "red" "red" "white" "red" "blue" "white") "-" deck)))

(defn- move-right
  "Determine which room is right here"
  [zone deck]
  (keyword (str (case zone "red" "white" "white" "blue" "blue" "blue") "-" deck)))
  
(defn which-room
  "Determine which room is the destination give a room and a direction"
  [where direction]
  (let [[zone deck] (.split (name where) "-")]
    (cond
     (= direction :left) (move-left zone deck)
     (= direction :right) (move-right zone deck)
     (= direction :updown) (move-grav-lift zone deck)
     true (throw (str "Bad direction " direction)))))

(defn move-player
  "Update the ship to move a player in some direction"
  [ship player direction]
  (let [ship-rooms (:rooms ship)
	position (find-player ship player)
	removed-rooms (update-in ship-rooms [position] #(remove #{player} %))
	destination-room (which-room position direction)
	destination-contents (destination-room removed-rooms)
	updated-rooms (assoc removed-rooms destination-room (conj destination-contents player))]
    (assoc ship :rooms updated-rooms)))

(defn create-threat [number type zone range]
  {:number number :type type :zone zone :range range})

(defn threat-number [threat]
  (:number threat))

(defn threat-type [threat]
  (:type threat))

(defn threat-zone [threat]
  (:zone threat))

(defn threat-range [threat]
  (:range threat))

(defn add-threat
  "Add a new threat at some time and zone"
  [ship step zone new-threat]
  (assoc-in ship [:threats step] new-threat))

(defn add-turn
  "Add a turn for a player"
  [turns step player turn]
  (assoc-in turns [step player] turn))

(defn turn-player
  "Given a turn find the player"
  [turn]
  (first turn))

(defn turn-direction
  "Given a turn find the direction"
  [turn]
  (second turn))

(defn get-turn
  "Find a specific turn or turns at a certain step"
  ([turns step]
     (get turns step))
  ([turns step player]
     (get-in turns [step player])))

(defn play-step
  "Play all the turns at a certain step"
  [ship turns step]
  (reduce
   (fn [ship turn] (move-player ship (turn-player turn) (turn-direction turn)))
   ship (get-turn turns step)))

(defn- pp-rooms [ship rooms]
  (apply str (for [room rooms]
	       (let [player-str (apply str (for [player (room (:rooms ship))] (.substring (name player) 6)))
		     room-str (str player-str (.substring "      " (count player-str)))]
		     (str "| " room-str)))))

(defn- pp-external-trajectories [ship]
  (let [traj-red (get-in ship [:trajectories :red])
	traj-white (get-in ship [:trajectories :white])
	traj-blue (get-in ship [:trajectories :blue])]
  ))

(defn- pp-internal-trajectory [ship]
  (apply str (map #(cond
		    (= :x %) "X "
		    (= :y %) "Y "
		    (= :z %) "Z "
		    (= :_ %) "- ") (reverse (get-in ship [:trajectories :internal])))))

(def divider (str (apply str (take 3 (repeat "+-------"))) "+\n"))

(defn pp
  "Pretty print ship"
  [ship]
  (print
   (str
    (pp-external-trajectories ship)
    "   red    white   blue\n"
    divider
    (pp-rooms ship [:red-up :white-up :blue-up]) "|\n"
    divider
    (pp-rooms ship [:red-down :white-down :blue-down]) "|\n"
    divider
    (pp-internal-trajectory ship))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ship-structure
  (let [ship (create-initial-ship 4)]
    (is (= :white-up (find-player ship :player1)))
    (is (= :white-up (find-player ship :player4)))
    (is (not= :white-up (find-player ship :player5)))))

(deftest player-move
  (let [ship (create-initial-ship 4)
	ship-with-player2-left (move-player ship :player2 :left)]
    (is (= :red-up (find-player ship-with-player2-left :player2)))
    (is (= :white-up (find-player ship-with-player2-left :player3)))
    (is (= :white-up (find-player (move-player ship-with-player2-left :player2 :right) :player3)))))

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
  (let [ship (create-initial-ship 4)
	ship-with-threat (add-threat ship 5 :red :fighter)
	threat (create-threat 3 :energy-cloud :blue 8)]
    (is (= 0 (count (:threats ship))))
    (is (= [5 :fighter] (first (:threats ship-with-threat))))
    (is (= 3 (threat-number threat)))
    (is (= :blue (threat-zone threat)))
    (is (= 8 (threat-range threat)))
    (is (= :energy-cloud (threat-type threat)))))

(defn- create-sample-turns []
  (reduce
   (fn [m [step player turn]] (add-turn m step player turn))
   {} [[1 :player1 :left] [1 :player2 :right] [2 :player1 :updown] [2 :player3 :left] [3 :player1 :right]]))

(deftest add-turn-should-work-for-multiple-steps
  (let [ship (create-initial-ship 4)
	turns (create-sample-turns)]
    (is (= :left (get-turn turns 1 :player1)))
    (is (= :right (get-turn turns 1 :player2)))
    (is (= :updown (get-turn turns 2 :player1)))
    (is (= :right (get-turn turns 3 :player1)))))

(deftest play-step-should-work
  (let [ship (create-initial-ship 4)
	turns (create-sample-turns)
	ship-after-step-1 (play-step ship turns 1)]
    (is (= :red-up (find-player ship-after-step-1 :player1)))
    (is (= :blue-up (find-player ship-after-step-1 :player2)))
    (is (= :white-up (find-player ship-after-step-1 :player3)))))

(deftest trajectories-should-default
  (let [ship (create-initial-ship 4)]
    (is (= (nth trajectories 0) (get-in ship [:trajectories :red])))))
