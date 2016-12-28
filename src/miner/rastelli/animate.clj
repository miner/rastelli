(ns miner.rastelli.animate
  (:require [miner.rastelli.util :as util]
            [miner.rastelli.pattern :as p]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as qm]
            [quil.core :as q]))




;; Quil/Processing coord system is upper left (0,0) x inc horiz right, y inc down.


(def basic-ball-colors [[254 1 1] [1 254 1] [1 1 254] [254 127 1] [54 54 127]
                        [1 127 1] [54 127 254] [54 254 127]  [127 127 127]])

(defn ball-colors [n]
  (util/modth basic-ball-colors n))

(def tickfactor 60)

(def ball-size 30)
(def arc-stroke (quot ball-size 4))
(def hand-under (quot ball-size 2))

(def flight-factor 0.65)  ;; of window width
(def hand-factor 0.2)   ;; of flight-factor
(def y-border (* 2 ball-size))

(def dwell-time 0.5)  ;; relative to beat, in real life, typically 0.5 to 0.8

(assert (< (+ flight-factor hand-factor) 1.0))


;; FIXME: This would be better to use than the current linear estimate.
;; physical height relative to a 3 for toss height t
(defn ht3 [t]
  (if (< t 3)
    0.0
    (/ (* (dec t) (dec t)) 4.0)))


(defn rl-beat [beat]
  (if (odd? beat)
    :left
    :right))

;; Y heights should not really scale linearly, but it's easier.  Real physics requires
;; quadratic changes in heights.  Also, we're using eliptical paths (because that's what's built
;; into Quil/Processing), but physics would use parabolic paths.

;; could use ht3 instead

;; long
(defn calc-height-factor [window-height max-throw-height]
  (quot (- window-height y-border y-border) (- (max 3 max-throw-height) 2)))

;; Would have to change for ht3
(defn ball-apex [throw-height hfactor]
  (if (< throw-height 3)
    0
    (* hfactor (- throw-height 2))))

(defn calc-yoffset [hfactor maxh]
  (ball-apex maxh hfactor))

(defn calc-flight-distance [window-width]
  (long (* window-width flight-factor)))


;; ellipse y = b sqrt(1 - (x/a)2)
;; a = radius height, b = radius width

(defn ellipse-y ^long [^long x a b]
  (if (>= (Math/abs x) a)
    0
    (let [xa (/ (double x) a)]
      (long (* b (Math/sqrt (- 1.0 (* xa xa))))))))

(defn ball-y [x flight-distance ymax]
  (let [a (quot flight-distance 2)]
    (ellipse-y x a ymax)))

(defn eball-xy [ball flight-distance hfactor yoffset time]
  (let [rl (rl-beat (:start ball))
        fd flight-distance
        bh (:height ball 0)
        apex (ball-apex bh hfactor)
        hz (* hand-factor flight-distance)
        t (- (/ (double time)  tickfactor) (:start ball))
        ft (- bh dwell-time)
        [x y]  (cond
                   (zero? bh) nil

                   (and (= bh 1) (<= t ft))
                   (let [x1 (- (quot fd 2) hz (* (/ t ft) (- fd hz)))]
                     (case rl
                       :right [x1 0.0]
                       :left [(- x1) 0.0]))

                   (= bh 2)
                   ;; basically a hold, but with a little pop up
                   (let [x1 (/ fd 2)
                         d2 (/ dwell-time 2.0)
                         y1 (let [ft4 (/ ft 4.0)
                                  ft2 (* ft4 2.0)]
                              (cond
                                  (> t ft2) 0.0
                                  (<= ft4 t ft2)  (* ball-size (/ (- ft2 t) ft4))
                                  (< t ft4) (* ball-size (/ t ft4))
                                  :else 0.0)) ]
                     (case rl
                       :right [(- x1 hz) y1]
                       :left [(- hz x1) y1]))

                 ;; dwelling in hand
                 (> t ft)
                 (let [dx (* 4 hz (- t ft))
                       dy (- (ball-y dx (* 4 hz) ball-size))]
                   (if (or (and (= rl :right) (odd? bh)) (and (= rl :left) (even? bh)))
                     [(+ (- (quot fd -2) hz) dx) dy]
                     [(- (+ (quot fd 2) hz) dx) dy]))

                 ;; flying
                 (odd? bh)
                 (let [dx (long (* (/ t ft) fd))
                       x0 (if (= rl :right) (- (quot fd 2) dx) (+ (quot fd -2) dx))
                       y0 (ball-y x0 fd apex)]
                   ;;(println "eball-xy odd" x1 y1)
                   (if (= rl :right) [(- x0 hz) y0] [(+ x0 hz) y0]))

                 ;; even throw
                 :else 
                 (let [dx (* (/ t ft) 2 hz)
                       x0 (if (= rl :right)
                            (- hz dx)
                            (- dx hz))
                       y1 (ball-y x0 (* 2 hz) apex)
                       x1 (if (= rl :right)
                            (- (quot fd 2) x0)
                            (- (quot fd -2) x0))]
                   [x1 y1]))]

    (when (and x y)
      ;;(println "pre-off" x y)
      [(long x) (long (- yoffset y))])))



(defn landed? [ball beat]
  (>= beat (+ (:height ball) (:start ball))))


(defn update-throw [state beat]
  ;; not fully implemented
  (let [height (util/modth (:pattern state) beat)]
    (assoc state :balls (map (fn [b] (if (landed? b beat)
                                       (assoc b :start beat :height height)
                                       b))
                           (:balls state)))))

 
(defn update-state [state]
  (let [qwidth (q/width)
        qheight (q/height)
        state (if (and (= qwidth (:width state))
                       (= qheight (:height state)))
                state
                (let [hmax (apply max (:pattern state))
                      hfactor (calc-height-factor qheight hmax)
                      new-state (assoc state
                                       :width qwidth
                                       :height qheight
                                       :height-factor hfactor
                                       :yoffset (calc-yoffset hfactor hmax)
                                       :flight-distance
                                       (calc-flight-distance  qwidth))]
                  ;;(println "resize state = " new-state)
                  new-state))]
    (if (:running? state)
      (let [tick (inc (:tick state))
            new-state    (assoc state :tick tick)]
        (if (util/zmod? tick tickfactor)
          (update-throw new-state (quot tick tickfactor))
          new-state))
      state)))


(defn draw-zero-maybe [state]
  (let [beat (quot (:tick state) tickfactor)]
    ;(println "dzm " beat) (flush)
    (when (zero? (util/modth (:pattern state) beat))
      ;(print "X") (flush)
      (q/push-style)
      (q/text-size 32)
      (q/fill 1)
      (let [hand (if (even? beat) :right :left)
            flight-distance (:flight-distance state)
            yoffset (:yoffset state)]
        (q/text-char \X  (if (= hand :right)
                           (quot (- flight-distance ball-size) 2)
                           (quot (+ flight-distance ball-size) -2))
                     (+ yoffset (quot ball-size 2))))
      (q/pop-style))))

;; Color doesn't depend on hand.
;; Better to use same orbit in same trace color.

(defn draw-toss-arc [orbi hand ball-height flight-distance hfactor yoffset]
  (let [color (apply q/color (ball-colors orbi))
        xhand (* hand-factor flight-distance)
        fd2 (quot flight-distance 2)]
    (q/push-style)
    (q/stroke color 25)
    (q/stroke-weight arc-stroke)
    (cond
        ;; (zero? ball-height)  nil
        
        #_ (= ball-height 2)
        #_ (q/text-char \X  (if (= hand :right)
                           (quot (- flight-distance ball-size) 2)
                           (quot (+ flight-distance ball-size) -2))
            (+ yoffset (quot ball-size 2)))

        (= ball-height 1)
        (q/line (- xhand (quot flight-distance 2)) yoffset
            (- (quot flight-distance 2) xhand) yoffset)
        
        (> ball-height 2)
        (let [xoff (if (odd? ball-height) 0 (+ xhand (quot flight-distance 2)))
              x (if (= hand :right) (- xoff xhand) (- xhand xoff))
              wid (if (odd? ball-height) flight-distance (* 2 xhand))
              ht (* 2 (ball-apex ball-height hfactor))]
          ;;(println "Draw-toss" x y wid ht)
          (q/no-fill)
          (q/arc x yoffset wid ht (- q/PI) 0.0 :open)))
    (q/pop-style)))

(defn draw-label [state]
  (q/push-style)
  (q/fill 1)
  (q/stroke (q/color 255 255 255) 50)
  (q/text-size 32)
  (q/text (p/pattern-str (:pattern state)) 0 (+ (:yoffset state) 40))
  (q/pop-style))
            

(defn draw-state [state]
  (q/push-style)
  (q/background 255)
  (q/fill 1)
  (q/stroke 2)
  (let [time (:tick state)
        beat (quot time tickfactor)
        hfactor (:height-factor state)
        yoffset (:yoffset state)
        flight-distance (:flight-distance state)]

    ;; (when (:running? state) (println "\nstate" state))

    (q/with-translation [(quot (q/width) 2), y-border]

      (when (:orbits? state)
        (doseq [[i orb] (map-indexed (fn [i orb] [i (if (odd? (count orb)) (into orb orb) orb)])
                                     (p/orbits (:pattern state)))]
          (doseq [ball-height (set (take-nth 2 orb))]
            (draw-toss-arc i :right ball-height flight-distance hfactor yoffset))
          (doseq [ball-height (set (take-nth 2 (rest orb)))]
            (draw-toss-arc i :left ball-height flight-distance hfactor yoffset))))

      (draw-label state)
      
      (doseq [b (:balls state)]
        (when-let [[x y] (eball-xy b flight-distance hfactor yoffset time)]
          ;; (when (:running? state) (println "ball" (:color b) x y))
          (apply q/fill (ball-colors (:color b)))
          (q/ellipse x y ball-size ball-size)))

      (q/pop-style)
      (draw-zero-maybe state))))
        


(defn clicked [state mouse-info]
  (println "\nclicked at:" (q/millis))
  (println "mouse at:" mouse-info)
  (println "pattern: " (:pattern state))
  (println "orbits:  " (p/orbits (:pattern state)))
  (pprint state)
  (-> state
      (update :orbits? not)))

(defn key-pressed [state key-info]
  ;;(println "\nkey:" (q/key-as-keyword) key-info)
  (case (long (:key-code key-info))
    (32) (update state :running? not)
    (9, 10, 13) (update state :orbits? not)
    state))

(defn setup-with [pattern]
  (let [balls (mapv (fn [i beat]
                      {:color i :height (util/modth pattern beat) :start beat})
                    (range)
                    (p/ball-starts pattern))]
    (fn []
      (q/frame-rate tickfactor)
      {:balls balls
       :height 0 
       :width 0 
       :flight-distance 0
       :yoffset 0
       :height-factor 0.0
       :pattern pattern
       :orbits? true
       :running? true
       :tick 0})))

(defn animate
  ([] (animate [9 7 5 3 1]))
  ([pat]
   (let [pat (p/as-pattern pat)]
     (when-not (p/siteswap? pat)
       (throw (ex-info "Illegal pattern" {:illegal pat})))
     (q/defsketch juggling
       :host "host"
       :title "Jeeves Juggler"
       :size [600  720]
       :setup (setup-with pat)
       :update update-state
       :draw draw-state
       :mouse-clicked clicked
       :key-pressed key-pressed
       :features [:resizable]
       :middleware [qm/fun-mode])
     {:pattern pat
      :numballs (p/numballs pat)
      :orbits (p/orbits pat)})))
