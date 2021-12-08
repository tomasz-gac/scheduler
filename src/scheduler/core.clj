(ns scheduler.core
  (:require [clojure.core.logic :as l])
  (:require [clojure.core.logic.fd :as fd]))

(l/defne happens-beforo
         "Defines a happens-before relationship between consecutive time strips in a list."
         [elements]
         ([[]])
         ([[a]])
         ([[a b . d]]
          (l/fresh [end start]
                   (l/featurec a {:end end})
                   (l/featurec b {:start start})
                   (fd/<= end start)
                   (happens-beforo (l/llist b d)))))


(defn non-overlappo
  "Two time strips must not overlap in time, if they share the same space"
  [a b]
  (l/fresh [sa sb]
           (l/featurec a {:space sa})
           (l/featurec b {:space sb})
           (l/conde
             [(fd/== sa sb)
              (l/conde
                [(happens-beforo [a b])]
                [(happens-beforo [b a])])]
             [(fd/!= sa sb)])))

(defn is-strip
  "Returns a goal that unifies x with time strip data specified by the supplied map.
  Time strip is a list [start duration end space] that corresponds with map values at those keys."
  [work]
  (fn [x]
    (l/fresh [start duration end space]
             ((:start work) start)
             ((:duration work) duration)
             ((:end work) end)
             ((:space work) space)
             (fd/< start end)
             (fd/+ start duration end)
             (l/== x {:start start
                      :duration duration
                      :end end
                      :space space}))))

(defn is-process
  "Returns a goal that unifies x with process data specified by the supplied collection of maps.
  Process is an ordered list of work data, as defined by is-work."
  ([[p & process]]
   (if (empty? process)
     (fn [x] (l/fresh [res]
                      ((is-strip p) res)
                      (l/conso res [] x)))
     (l/fne [x] ([[a . d]]
                 ((is-strip p) a)
                 ((is-process process) d))))))

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
                      ((is-strip p) res)
                      (l/conso res [] x)))
     (l/fne [x] ([[a . d]]
                 ((is-strip p) a)
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
     :space    (fn [x] (l/== x (:space work-map)))}))

