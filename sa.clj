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
       (apply assoc {} (interleave *rooms* (repeat [])))
     :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players)))))
   {}))

(defn find-player [ship player]
  (ffirst (filter
	   (fn [[room contents]] (some #{player} contents))
	   (:rooms ship))))

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
	removed-rooms (update-in ship-rooms [position] #(remove #{player} %))
	destination-room (which-room position direction)
	destination-contents (destination-room ship)
	updated-rooms (assoc removed-rooms destination-room (conj destination-contents player))]
    (assoc ship :rooms updated-rooms)))

(defn add-threat [ship time zone new-threat]
  (assoc-in ship [:threats time] new-threat))

(defn add-turn [turns step player turn]
  (assoc-in turns [step player] turn))

(defn turn-player [turn]
  (first turn))

(defn turn-direction [turn]
  (second turn))

(defn get-turn
  ([turns step]
     (get turns step))
  ([turns step player]
     (get-in turns [step player])))

(defn play-step [ship turns step]
  (reduce
   (fn [ship turn] (move-player ship (turn-player turn) (turn-direction turn)))
   ship (get-turn turns step)))

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
  (let [ship (create-initial-ship 4)
	ship-with-threat (add-threat ship 5 :red :fighter)]
    (is (= 0 (count (:threats ship))))
    (is (= [5 :fighter] (first (:threats ship-with-threat))))))

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
