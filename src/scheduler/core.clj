(ns scheduler.core
  (:require [clojure.core.logic :as l])
  (:require [clojure.core.logic.fd :as fd]))

(l/defne worko
  "Defines work relationship. Work start must be before end, and have a duration."
  [work] ([[s d e]] (l/all (fd/< s e) (fd/+ s d e))))

(l/defne processo
  "Defines a process goal.
  Process is a list of work, in which next work cannot start before previous has ended"
  [works]
         ([[]])
         ([[a]])
         ([[a b . d]]
          (l/matche [a b] ([ [_ _ e] [s _ _] ] (fd/<= e s)))
          (processo (l/llist b d))))

(defn keyorfresho
  "Returns a goal that assigns a value from a map at a given key,
  or a fresh value if the key is missing"
  [map key]
  (let [v (key map)]
    (if (nil? v)
      (fn [x] (l/fresh [s] (l/== s x)))
      (fn [x] (l/== v x)))))

(defn is-work
  "Returns a goal that unifies x with work data specified by the supplied map.
  Work is a list [start duration end] that corresponds with map values at those keys."
  [work]
  (l/fne [x]
    ([[s d e]]
     ((keyorfresho work :start) s)
     ((keyorfresho work :duration) d)
     ((keyorfresho work :end) e)
     )))

(defn is-process
  "Returns a goal that unifies x with process data specified by the supplied collection of maps.
  Process is a list of work data, as defined by is-work."
  ([[p & process]]
   (if (empty? process)
     (fn [x] l/succeed)
     (l/fne [x]
            ([[a . d]] ((is-work p) a) ((is-process process) d))))))



