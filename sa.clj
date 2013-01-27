;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011-2012

(ns sa
  [:refer-clojure]
  [:use clojure.test])

(def ^:dynamic *rooms* [:red-up :white-up :blue-up :red-down :white-down :red-down])

;; Commas indicate range boundaries
(def ^:dynamic *tracks* {
     :t1 [:z :_ :_ :_ :x, :_ :_ :_ :_ :_]
     :t2 [:z :_ :_ :_ :_, :_ :_ :x :_ :_, :_]
     :t3 [:z :_ :y :_ :_, :_ :_ :x :_ :_, :_ :_]
     :t4 [:z :_ :_ :_ :y, :_ :_ :_ :x :_, :_ :_ :_]
     :t5 [:z :_ :_ :_ :_, :_ :y :_ :_ :_, :x :_ :_ :_]
     :t6 [:z :_ :y :_ :_, :_ :y :_ :_ :x, :_ :_ :_ :_ :_]
     :t7 [:z :_ :_ :_ :y, :_ :_ :y :_ :_, :_ :x :_ :_ :_ :_]})

(def ^:dynamic *threats*
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

(def ^:dynamic *threat-fields*
  [:name :health :shield :velocity :survive-points :kill-points :x-action :y-action :z-action])

(def ^:dynamic *ship-layout*
  {:red-up {:left :red-up :change :red-down :right :white-up}
  :white-up {:left :red-up :change :white-down :right :blue-up}
  :blue-up {:left :white-up :change :blue-down :right :blue-up}
  :red-down {:left :red-down :change :red-up :right :white-down}
  :white-down {:left :red-down :change :white-up :right :blue-down}
  :blue-down {:left :white-down :change :blue-up :right :blue-down}})

(defn create-game
  "Create the starting position, specifying the number of players"
  [num-players]
  {:num-players {:number num-players}
   :round {:number 1}
   :player1 {:type :player :room :white-up}
   :player2 {:type :player :room :white-up}
   :player3 {:type :player :room :white-up}
   :player4 {:type :player :room :white-up}
   :red-shield {:type :shield :power 1 :max 2}
   :white-shield {:type :shield :power 1 :max 3}
   :blue-shield {:type :shield :power 1 :max 2}
   :red-reactor {:type :reactor :power 2 :max 3}
   :white-reactor {:type :reactor :power 3 :max 5}
   :blue-reactor {:type :reactor :power 2 :max 3}
   :rods {:number 3}
   :computer {:number 3}
   :missiles {:number 3}
   :red-track {:type :track :track (:t1 *tracks*)}
   :white-track {:type :track :track (:t2 *tracks*)}
   :blue-track {:type :track :track (:t3 *tracks*)}
   :internal-track {:type :track :track (:t4 *tracks*)}})

(defn move-to-room
  "Find the room to move to"
  [room direction]
  (get-in *ship-layout* [room direction]))

(defn player-move
  "Carry out an action by a player"
  [game player action]
  (let [from-room (:room (player game))
        to-room (move-to-room from-room action)]
    (assoc-in game [player :room] to-room)))

(defn find-by-type
  "Returns a sequence of all objects in the game of a specified type.
  :id will be automatically assigned."
  [game selector]
  (let [matching-pairs (filter (fn [pair] (= (:type (second pair)) selector)) (seq game))]
    (for [pair matching-pairs] (assoc (second pair) :id (first pair)))))

(defn add-threat
  "Add a specified threat into the game"
  [game threat-id track-id turn]
  (let [threat (apply assoc {} :type :threat :track track-id :position (dec (count (:track (track-id game)))) :turn turn (interleave *threat-fields* (threat-id *threats*)))]
    (assoc game threat-id threat)))

(defn advance-threat
  "Move a specified threat. Carry out any actions."
  [game threat-id]
  (let [threat (threat-id game)
        track (:track ((:track threat) game))
        position (:position threat)
        velocity (:velocity threat)]
    (if (> position 8)
      (assoc-in game [threat-id :position] (- position velocity))
      (assoc-in (assoc-in game [threat-id :position] (- position velocity)) [:red-shield :power] 0))))

(defn threat-attack
  [game threat-id action]
  (assoc-in game [:red-shield :power] 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def game4 (create-game 4))

(defn- create-game-with-threat []
  (add-threat game4 :e1-07 :red-track 2))

(deftest create-game-should-have-players
  (are [x y] (= x y)
    4 (:number (:num-players game4))
    1 (:number (:round game4))
    :white-up (:room (:player1 game4))
    :white-up (:room (:player4 game4))))

(deftest create-game-should-have-tracks
  (are [x y] (= x y)
    (:t1 *tracks*) (:track (:red-track game4))
    (:t2 *tracks*) (:track (:white-track game4))
    (:t3 *tracks*) (:track (:blue-track game4))
    (:t4 *tracks*) (:track (:internal-track game4))))

(deftest create-game-should-have-shields
  (let [rs (:red-shield game4)
        ws (:white-shield game4)
        bs (:blue-shield game4)]
    (are [x y] (= x y)
      1 (:power rs)
      1 (:power ws)
      1 (:power bs)
      2 (:max rs)
      3 (:max ws)
      2 (:max bs))))

(deftest create-game-should-have-reactors
  (let [rr (:red-reactor game4)
        wr (:white-reactor game4)
        br (:blue-reactor game4)]
    (are [x y] (= x y)
      2 (:power rr)
      3 (:power wr)
      2 (:power br)
      3 (:max rr)
      5 (:max wr)
      3 (:max br))))

(deftest create-game-should-have-various-things
  (are [x y] (= x y)
    3 (:number (:rods game4))
    3 (:number (:missiles game4))
    3 (:number (:computer game4))))

(deftest move-to-room-should-return-correct-room
  (is (= :white-up (move-to-room :blue-up :left)))
  (is (= :red-up (move-to-room :red-up :left)))
  (is (= :red-down (move-to-room :red-up :change)))
  (is (= :blue-up (move-to-room :blue-down :change)))
  (is (= :blue-down (move-to-room :blue-down :right))))

(deftest player-move-should-change-player-location
  (let [after (player-move game4 :player1 :left)
        after2 (player-move game4 :player2 :change)
        after3 (player-move game4 :player3 :right)]
    (is (= :red-up (:room (:player1 after))))
    (is (= :white-down (:room (:player2 after2))))
    (is (= :blue-up (:room (:player3 after3))))
    (is (= :white-up (:room (:player4 after3))))))

(deftest find-by-type-should-find-correct-objects
  (are [x y] (= x y)
    3 (count (find-by-type game4 :shield))
    4 (count (find-by-type game4 :player))
    3 (count (find-by-type game4 :reactor))
    0 (count (find-by-type game4 :threat))))

(deftest add-threat-should-work
  (let [with-threat (add-threat game4 :e1-07 :red-track 2)
        threat (first (find-by-type with-threat :threat))
        track (:red-track with-threat)]
    (are [x y] (= x y)
      :e1-07 (:id threat)
      :fighter (:name threat)
      :red-track (:track threat)
      2 (:turn threat)
      3 (:velocity threat)
      (dec (count (:track track))) (:position threat)))) 

(deftest threat-advance-should-work
  (let [with-threat (create-game-with-threat)
        threat-before (:e1-07 with-threat)
        after-advance-1 (advance-threat with-threat :e1-07)
        threat-after-1 (:e1-07 after-advance-1)
        after-advance-2 (advance-threat after-advance-1 :e1-07)
        threat-after-2 (:e1-07 after-advance-2)]
    (are [x y] (= x y)
      9 (:position threat-before)
      6 (:position threat-after-1)
      3 (:position threat-after-2))))

(deftest threat-action-should-work
  (let [with-threat (create-game-with-threat)
        after-advance-1 (advance-threat with-threat :e1-07)
        after-advance-2 (advance-threat after-advance-1 :e1-07)]
    (are [x y] (= x y)
      1 (:power (:red-shield after-advance-1))
      1 (:power (:white-shield after-advance-1))
      1 (:power (:blue-shield after-advance-1))
      0 (:power (:red-shield after-advance-2))
      1 (:power (:white-shield after-advance-2))
      1 (:power (:blue-shield after-advance-2)))))
