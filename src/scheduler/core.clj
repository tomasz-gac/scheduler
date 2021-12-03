(ns scheduler.core
  (:require [clojure.core.logic :as l])
  (:require [clojure.core.logic.fd :as fd]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn worko
  "Defines work relationship. Work start must be before end, and have a duration."
  [s d e]
  (l/all (fd/< s e) (fd/+ s d e)))

(l/defne processo
  "Defines a process. Process is a relationship between three lists that define a series of work.
  In a list of work, next work cannot start, before previous was finished."
  [starts ends]
         ([[] []])
         ([[s] [e]])
         ([[s . s2 . sd] [e . e2 . ee]]
          (fd/<= s2 e)
          (processo (l/llist s2 sd) (l/llist e2 ee))))

(defn keyorfresho [map key]
  (let [v (key map)]
    (if (nil? v)
      (fn [x] (l/fresh [s] (l/== s x)))
      (fn [x] (l/== v x)))))

(defn extract-work
  [work]
  (l/fne [x]
    ([[s d e]]
     ((keyorfresho work :start) s)
     ((keyorfresho work :duration) d)
     ((keyorfresho work :end) e))))