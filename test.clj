;;; Simulator for the board game "Space Alert"
;;; David Plumpton 2011-2013

(ns sa
  [:refer-clojure]
  [:use clojure.test])

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
