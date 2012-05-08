;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011-2012

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(defrecord Board [rooms threats trajectories])

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

(def threats
  {:e1-01 [:pulse-ball 5 1 2 2 4 [:all-zones 1] [:all-zones 1] [:all-zones 2]]
   :e1-02 [:destroyer 5 2 2 2 4 [:attack 1] [:attack 1] [:attack 2] :double-damage]
   :e1-03 [:steath-fighter 4 2 4 2 4 [:reveal] [:attack 2] [:attack 2] :stealth-until-reveal]
   :e1-04 [:energy-cloud 5 3 2 2 4 [:drain-all-shields] [:other-zones 2] [:other-zones 2] :no-shield-if-pulse-cannon-hits]
   :e1-05 [:gunship 5 2 2 2 4 [:attack 2] [:attack 2] [:attack 3]]
   :e1-06 [:cryoshield-fighter 4 1 3 2 4 [:attack 1] [:attack 1] [:attack 2]]
   :e1-07 [:fighter 5 2 3 2 4 [:attack 1] [:attack 1] [:attack 2]]
   :e1-08 [:armored-grappler 4 3 2 2 4 [:attack 1] [:repair 1] [:attack 4]]
   :e1-09 [:amoeba 8 0 2 2 4 [:repair 2] [:repair 2] [:attack 5] :no-rockets]
   :e1-10 [:meteroid 5 0 5 [] [] [:attack-hit-points] :no-rockets]
   :se1-01 [:frigate 7 2 2 4 8 [:attack 2] [:attack 3] [:attack 4]]})

(defn create-initial-board
  "Create the starting position, specifying the number of players"
  ([num-players]
     (create-initial-board num-players trajectories))
  ([num-players trajectories]
  (Board.
   (assoc
       (apply assoc {} (interleave rooms (repeat [])))
     :white-up (vec (map (fn [num] (keyword (str "player" num))) (range 1 (inc num-players)))))
   []
   (apply assoc {} (interleave [:red :white :blue :internal] trajectories)))))

(defn find-player
  "Find the room a player is in"
  [board player]
  (ffirst (filter
           (fn [[room contents]] (some #{player} contents))
           (:rooms board))))

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
  "Update the board to move a player in some direction"
  [board player direction]
  (let [board-rooms (:rooms board)
        position (find-player board player)
        removed-rooms (update-in board-rooms [position] #(remove #{player} %))
        destination-room (which-room position direction)
        destination-contents (destination-room removed-rooms)
        updated-rooms (assoc removed-rooms destination-room (conj destination-contents player))]
    (assoc board :rooms updated-rooms)))

(defn create-threat [number type zone distance id]
  {:number number :type type :zone zone :distance distance :id id})

(defn add-threat
  "Add a new threat at some time and zone"
  [board new-threat]
  (assoc board :threats (conj (:threats board) new-threat)))

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

(defn player-turn
  "Play all the turns at a certain step"
  [board turns step]
  (reduce
   (fn [board turn] (move-player board (turn-player turn) (turn-direction turn)))
   board (get-turn turns step)))

(defn threats-move
  "All threats move"
  [board]
    (assoc board :threats
      (map (fn [threat] (assoc threat :distance 7)) (:threats board))))

(defn- pp-rooms [board rooms]
  (apply str (for [room rooms]
               (let [player-str (apply str (for [player (room (:rooms board))] (.substring (name player) 6)))
                     room-str (str player-str (.substring "      " (count player-str)))]
                     (str "| " room-str)))))

(defn- trajectory-element-str
  [trajectory index length]
    (let [elem (nth trajectory index " ")]
      (case elem :x "X" :y "Y" :z "Z" :_ "-" " ")))

(defn- pp-external-trajectories
  "Format a string representing the external trajectories"
  [board]
  (let [[red white blue] (map #(get-in board [:trajectories %]) [:red :white :blue])
        length (max (count red) (count white) (count blue))
        gap "        "]
    (apply str
           (for [i (reverse (range length))] (str (trajectory-element-str red i length) gap
                                        (trajectory-element-str white i length) gap
                                        (trajectory-element-str blue i length) "\n")))))

(defn- pp-internal-trajectory
  "Format a string representing the internal trajectory"
  [board]
  (let [internal (get-in board [:trajectories :internal])]
    (apply str (interleave
                (for [i (range (dec (count internal)) -1 -1)] (trajectory-element-str internal i (count internal)))
                (repeat " ")))))

(def divider (str (apply str (take 3 (repeat "+-------"))) "+\n"))

(defn pp
  "Pretty print board"
  [board]
  (print
   (str
    (pp-external-trajectories board)
    "   red    white   blue\n"
    divider
    (pp-rooms board [:red-up :white-up :blue-up]) "|\n"
    divider
    (pp-rooms board [:red-down :white-down :blue-down]) "|\n"
    divider
    (pp-internal-trajectory board) "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest board-structure
  (let [board (create-initial-board 4)]
    (is (= :white-up (find-player board :player1)))
    (is (= :white-up (find-player board :player4)))
    (is (not= :white-up (find-player board :player5)))))

(deftest player-move
  (let [board (create-initial-board 4)
        board-with-player2-left (move-player board :player2 :left)]
    (is (= :red-up (find-player board-with-player2-left :player2)))
    (is (= :white-up (find-player board-with-player2-left :player3)))
    (is (= :white-up (find-player (move-player board-with-player2-left :player2 :right) :player3)))))

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
  (let [board (create-initial-board 4)
        threat1 (create-threat 3 :energy-cloud :blue 8 :e1-04)
        threat2 (create-threat 5 :fighter :red 9 :e1-07)
        threats [threat1 threat2]
        board-with-threats (reduce add-threat board threats)]
    (is (= 0 (count (:threats board))))
    (is (= 2 (count (:threats board-with-threats))))
    (is (= :energy-cloud (:type (first (:threats board-with-threats)))))
    (is (= 3 (:number threat1)))
    (is (= :blue (:zone threat1)))
    (is (= 9 (:distance threat2)))
    (is (= :fighter (:type threat2)))))

(defn- create-sample-turns []
  (reduce
   (fn [m [step player turn]] (add-turn m step player turn))
   {} [[1 :player1 :left] [1 :player2 :right] [2 :player1 :updown] [2 :player3 :left] [3 :player1 :right]]))

(deftest add-turn-should-work-for-multiple-steps
  (let [board (create-initial-board 4)
        turns (create-sample-turns)]
    (is (= :left (get-turn turns 1 :player1)))
    (is (= :right (get-turn turns 1 :player2)))
    (is (= :updown (get-turn turns 2 :player1)))
    (is (= :right (get-turn turns 3 :player1)))))

(deftest player-turn-should-work
  (let [board (create-initial-board 4)
        turns (create-sample-turns)
        board-after-step-1 (player-turn board turns 1)]
    (is (= :red-up (find-player board-after-step-1 :player1)))
    (is (= :blue-up (find-player board-after-step-1 :player2)))
    (is (= :white-up (find-player board-after-step-1 :player3)))))

(deftest trajectories-should-default
  (let [board (create-initial-board 4)]
    (is (= (nth trajectories 0) (get-in board [:trajectories :red])))))

(deftest threats-should-move
  (let [board (create-initial-board 4)
        threat (create-threat 1 :fighter :red 10 :e1-07)
        board-with-threat (add-threat board threat)
        updated (threats-move board-with-threat)]
    (is (= 10 (:distance threat)))
    (is (= 7 (:distance (first (:threats updated)))))))
