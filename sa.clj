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
   :red-track {:type :track :track-layout (:t1 *tracks*)}
   :white-track {:type :track :track-layout (:t2 *tracks*)}
   :blue-track {:type :track :track-layout (:t3 *tracks*)}
   :internal-track {:type :track :track-layout (:t4 *tracks*)}})

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
  (let [threat (apply assoc {} :type :threat :track track-id :position (dec (count (:track-layout (track-id game)))) :turn turn (interleave *threat-fields* (threat-id *threats*)))]
    (assoc game threat-id threat)))

(defn find-threat-actions
  "Return a list of any actions the threat crosses while advancing."
  [game threat-id start end]
  (if (< start 7) [:todo] []))

(defn advance-threat
  "Move a specified threat. Carry out any actions."
  [game threat-id]
  (let [threat (threat-id game)
        track (:track-layout ((:track threat) game))
        position (:position threat)
        velocity (:velocity threat)
	end-position (- position velocity)
        actions (find-threat-actions game threat-id position end-position)]
    (if (seq actions)
      (assoc-in (assoc-in game [threat-id :position] end-position) [:red-shield :power] 0)
      (assoc-in game [threat-id :position] (- position velocity)))))

(defn threat-attack
  "A threat makes an attack"
  [game threat-id force]
  (assoc-in game [:red-shield :power] 0))
