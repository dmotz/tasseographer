#!/usr/local/bin/planck

(ns tasseographer.core
  (:require [planck.core    :refer [slurp]]
            [planck.shell   :refer [sh]]
            [clojure.string :refer [split-lines]]))


(def min-len 3)

(def ascii->hex
  {"e" "e"
   "t" "7"
   "i" "1"
   "o" "0"
   "a" "a"
   "s" "5"
   "d" "d"
   "f" "f"
   "g" "6"
   "z" "2"
   "c" "c"
   "b" "b"})

(def hex->ascii (apply hash-map (mapcat reverse ascii->hex)))

(def hex-chars (set (keys ascii->hex)))


(defn hex? [s]
  (every? hex-chars s))


(defn long? [s]
  (>= (count s) min-len))


(defn add-to-trie [trie [x & xs]]
  (let [v (trie x)]
    (assoc
      trie
      x
      (if (seq xs)
        (add-to-trie (if v v {}) xs)
        (if v
          (assoc v :end true)
          {:end true})))))


(defn in-trie? [trie [x & xs]]
  (let [v (trie x)]
    (if (nil? v)
      false
      (if (not (seq xs))
        (:end v)
        (recur v xs)))))


(defn find-matches [trie coll hash]
  (let [hashv (vec hash)
        len   (count hash)]
    (loop [start   0
           end     min-len
           matches []
           cand    nil]
      (if (or (>= (+ start min-len) len) (> end len))
        (concat coll matches)
        (let [frag (subvec hashv start end)]
          (if (in-trie? trie frag)
            (recur start (inc end) matches frag)
            (recur end (+ end min-len) (if cand (conj matches cand) matches) nil)))))))


(let [trie
      (->>
        (slurp "/usr/share/dict/words")
        split-lines
        (filter hex?)
        (filter long?)
        (map (partial map ascii->hex))
        (reduce add-to-trie {}))

      hashes
      (->>
        (sh "git" "log" \.)
        :out
        split-lines
        (filter #(.startsWith % "commit "))
        (map (partial drop 7))
        (reduce (partial find-matches trie) [])
        (map (partial map hex->ascii))
        (map (partial apply str))
        (map println)
        dorun)])
