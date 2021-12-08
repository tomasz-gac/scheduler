(ns scheduler.core
  (:require [clojure.core.logic :as l])
  (:require [clojure.core.logic.fd :as fd]))

(l/defne stripo
  "Defines time strip relationship. Time strip start must be before end, and have a duration."
  [work] ([[start duration end _]]
          (fd/+ start duration end)))

(l/defne happens-beforo
         "Defines a happens-before relationship between consecutive time strips in a list."
         [elements]
         ([[]])
         ([[a]])
         ([[a b . d]]
          (l/matche [a b] ([ [_ _ e _] [s _ _ _] ] (fd/<= e s)))
          (happens-beforo (l/llist b d))))

(l/defne non-overlappo
  "Two time strips must not overlap in time, if they share the same space"
  [a b]
  ([[_ _ _ sp1] [_ _ _ sp2]]
   (l/conde
     [(fd/== sp1 sp2)
      (l/conde
        [(happens-beforo [a b])]
        [(happens-beforo [b a])])]
     [(fd/!= sp1 sp2)])))

(defn is-stripo
  "Returns a goal that unifies x with time strip data specified by the supplied map.
  Time strip is a list [start duration end space] that corresponds with map values at those keys."
  [work]
  (l/fne [x]
         ([[start duration end space]]
          ((:start work) start)
          ((:duration work) duration)
          ((:end work) end)
          ((:space work) space)
          (stripo [start duration end space]))))

(l/defne constrain-spaceo
  "This constraint will assure that all time strips within the argument
  obey non-overlapping relationship with each other. Quadratic."
  [strips]
  ([[]])
  ([[w . ws]]
   (let [space' (l/defne space' [c a]
                  ([_ []])
                  ([_ [f . d]]
                   (non-overlappo c f)
                   (space' c d)))]
     (l/all (space' w ws) (constrain-spaceo ws)))))

(defn is-process
  "Returns a goal that unifies x with process data specified by the supplied collection of maps.
  Process is an ordered list of work data, as defined by is-work."
  ([[p & process]]
   (if (empty? process)
     (fn [x] (l/fresh [res]
                      ((is-stripo p) res)
                      (l/conso res [] x)))
     (l/fne [x] ([[a . d]]
                 ((is-stripo p) a)
                 ((is-process process) d))))))

(defn preprocess-strip
  "Preprocesses time strip data. Changes numeric values to core.logic goals,
  or uses default to introduce fresh variables. :space key is required in work-map"
  [work-map default]
  (letfn [(extract [key map default]
            (let [v (key map default)]
              (cond
                (number? v) (fn [x] (l/== x v))
                (fn? v) v)))]
    {:start    (extract :start work-map default)
     :duration (extract :duration work-map default)
     :end      (extract :end work-map default)
     :space    (extract :space work-map default)}))

(defn process-data
  ([m default]
   (map preprocess-strip (repeat m) (repeat default))))

(defn process
  ([n] (process n 1))
  ([n space]
   (take n (process-data {:duration 1 :space space}
                         #(fd/in % (fd/interval 0 500))))))
