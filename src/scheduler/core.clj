(ns scheduler.core
  (:require [clojure.core.logic :as l])
  (:require [clojure.core.logic.fd :as fd]))

(l/defne worko
  "Defines work relationship. Work start must be before end, and have a duration."
  [work] ([[s d e]]
          (l/all (fd/< s e) (fd/+ s d e))))

(l/defne happens-before
         "Defines a happens-before relationship between two works."
         [before after]
         ([[_ _ e] [s _ _]] (fd/<= e s)))

(l/defne non-overlapping
         [a b]
         ([[s1 _ e1] [s2 _ e2]]
          (l/conde
            [(fd/>= s2 e1)]
            [(fd/>= s1 e2)])))

(l/defne processo
  "Defines a process goal.
  Process is a list of work, in which next work cannot start before previous has ended"
  [works]
  ([[]])
  ([[a]])
  ([[a b . d]]
   (l/matche [a b] ([ [_ _ e] [s _ _] ] (fd/<= e s)))
   (processo (l/llist b d))))

(defn key-or-fresho
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
     ((key-or-fresho work :start) s)
     ((key-or-fresho work :duration) d)
     ((key-or-fresho work :end) e)
     (is-work [s d e]))))



(defn is-process
  "Returns a goal that unifies x with process data specified by the supplied collection of maps.
  Process is an ordered list of work data, as defined by is-work."
  ([[p & process]]
   (if (empty? process)
     (l/fne [x] ([_] ((is-work p) x)))
     (l/fne [x] ([[a . [d]]] ((is-work p) a) ((is-process process) d))))))
