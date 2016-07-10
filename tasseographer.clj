#!/usr/local/bin/planck

(ns tasseographer.core
  (:require [planck.core    :refer [slurp *command-line-args*]]
            [planck.shell   :refer [sh]]
            [clojure.string :refer [split-lines]])
  (:require-macros [planck.shell :refer [with-sh-dir]]))


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

(def hex? (partial every? hex-chars))

(def min-len 3)


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
        (if (empty? matches)
          coll
          (conj coll [hash (flatten matches)]))
        (let [frag (subvec hashv start end)]
          (if (in-trie? trie frag)
            (recur start (inc end) matches frag)
            (recur
              end
              (+ end min-len)
              (if cand
                (conj
                  (conj
                    matches
                    (repeat (- start (reduce + (map count matches))) " "))
                  cand)
                matches)
              nil)))))))


(with-sh-dir (or (first *command-line-args*) \.)
  (let [trie
        (->>
          (slurp "/usr/share/dict/words")
          split-lines
          (transduce
            (comp
              (filter hex?)
              (filter long?)
              (map (partial map ascii->hex)))
            add-to-trie
            {}))

        hashes
        (->>
          (sh "git" "log" \.)
          :out
          split-lines
          (transduce
            (comp
              (filter #(.startsWith % "commit "))
              (map (partial drop 7)))
            (partial find-matches trie)
            [])
          (map
            (fn [[hex matches]]
              (println (apply str hex))
              (print "\u001b[33m")
              (println
               (apply
                 str
                 (map
                   (fn [c]
                     (if (= c " ") " " (hex->ascii c)))
                   matches)))
              (print "\u001b[0m")
              (println)))
          dorun)]))
