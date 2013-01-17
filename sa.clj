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

(defn create-game
  "Create the starting position, specifying the number of players"
  [num-players]
  {:num-players num-players})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-create-game
  (let [game (create-game 4)]
    (is (= 4 (:num-players game)))))

