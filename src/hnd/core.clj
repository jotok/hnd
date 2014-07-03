(ns hnd.core
  (:import [java.awt Color Dimension Frame GraphicsEnvironment]
           [java.awt.event WindowListener]))

;; model constants

(def default-initial-score 20)
(def default-p-dove 0.5)
(def default-split-at (* 2 default-initial-score))
(def default-p-occupied 0.3)

(def default-game-outcomes {[:hawk :hawk] [-2 -2]
                            [:hawk :dove] [1 -2]
                            [:dove :hawk] [-2 1]
                            [:dove :dove] [2 2]})

;; gui constants

(def gui-nrow 40)
(def gui-ncol 40)
(def gui-width 600)
(def gui-height 600)
(def gui-refresh-delay 200)
(def gui-dove-rgb (into [] (map float [0.149020 0.545098 0.823529])))
(def gui-hawk-rgb (into [] (map float [0.796078 0.294118 0.086275])))
(def gui-steps 80)

;; model constructors

(defn make-random-agent 
  "Make an agent which is a dove with probability p and a hawk
  otherwise."  
  [& {:keys [initial-score p-dove] 
      :or {initial-score default-initial-score 
           p-dove default-p-dove}}] 
  {:score initial-score 
   :type (if (< (rand) p-dove) :dove :hawk)})

(defn make-grid
  "Construct an nrow x ncol grid of agents. A cell in the grid will be
  occupied with probability p-occupied, and the function make-agent-fn
  will be called to construct every agent."  
  [nrow ncol & {:keys
  [p-occupied make-agent-fn]
                :or {p-occupied default-p-occupied 
                     make-agent-fn #(make-random-agent)}}]
  (->> (for [r (range (* nrow ncol))]
         (if (< (rand) p-occupied)
           (ref (make-agent-fn))
           (ref :empty)))
       (into [])))

(defn make-ca
  "Construct a CA using the default parameters."
  [nrow ncol]
  {:nrow nrow :ncol ncol :grid (make-grid nrow ncol)})

;; functions to get the cell neighborhood

(defn sub->ind [ca i j]
  (+ (* (:ncol ca) i) j))

(defn ind->sub [ca r]
  [(quot r (:ncol ca)) (mod r (:ncol ca))])

(defn get-cell 
  "Return the cell at (i (mod nrow), j (mod ncol))."
  [ca i j]
  (let [i (mod i (:nrow ca))
        j (mod j (:ncol ca))
        r (sub->ind ca i j)]
    ((:grid ca) r)))

(defn get-neighborhood 
  "Return the Von Neumann neighborhood at (i, j) in the following
  order: center, north, east, south, west."  
  [ca i j] 
  {:c (get-cell ca i j)
   :n (get-cell ca (dec i) j)
   :e (get-cell ca i (inc j))
   :s (get-cell ca (inc i) j)
   :w (get-cell ca i (dec j))})

(defn occupied? 
  "Returns true if the cell referent is not :empty."
  [cell]
  (not= @cell :empty))

(defn neighborhoods-view 
  "Generate a list of neighborhoods whose center matches a filter
  function."  
  [filter-fn]
  (fn [ca]
    (let [indices (->> (:grid ca)
                       (map vector (iterate inc 0))
                       (filter (fn [[ix cell]] (filter-fn cell)))
                       (map first)
                       (map #(ind->sub ca %)))]
      (for [[i j] indices] (get-neighborhood ca i j)))))

(def occupied-neighborhoods
  "Return a list of neighborhoods whose center is occupied."
  (neighborhoods-view occupied?))

(def empty-neighborhoods
  (neighborhoods-view (complement occupied?)))

(defn abundant? 
  "Returns true if the cell is occupied and the score of the occupant
is greater than or equal to default-split-at."  
  [cell]
  (and (occupied? cell)
       (>= (:score @cell) default-split-at)))

(def abundant-neighborhoods
  (neighborhoods-view abundant?))

(defn active? 
  "Returns true if the cell is occupied and the cell occupant has
  proposed an action in any direction."
  [cell]
  (and (occupied? cell)
       (:propose @cell)))

(def active-neighborhoods
  (neighborhoods-view active?))

;; Update rules

(defn list-occupied-neighbors
  "Return a list of occupied neighboring cells. The list may be
empty. The resulting list is of the form [[k v] [k2 v2] ...] where k
is a direction and v is a cell."  
  [nbhd]
  (filter (fn [[k v]] (and (not= k :c) (occupied? v)))
          nbhd))

(defn propose-out!
  "Propose a direction to add in. The direction will point to a
  neighbor included by neighbor-filter-fn."  
  [occupied-nbhd neighbor-filter-fn]
  (let [center (:c occupied-nbhd)
        neighbors (neighbor-filter-fn occupied-nbhd)]
    (if-not (empty? neighbors)
      (let [n (rand-nth neighbors)]
        (dosync (alter center assoc :propose (n 0)))))))

(defn active-neighbor?
  "Returns true if the neighboring (occupied) cell has proposed an
  action in the direction of the center cell."  
  [[neighbor-dir neighbor-cell]]
  (let [neighbor-proposed-dir  (:propose @neighbor-cell)]
    (#{[:n :s] [:e :w] [:s :n] [:w :e]} [neighbor-dir neighbor-proposed-dir])))

(defn find-active-neighbors
  "Return a list of active neighbors."
  [nbhd]
  (->> (list-occupied-neighbors nbhd)
       (filter active-neighbor?)
       (map second)))

(defn find-opponents
  "This function is used in the game-playing step. Start with the list
  of active neighbors, and add the neighbor in the direction of
  proposed action from the center cell. Returns a set to ensure no
  opponent appears twice."  
  [active-nbhd]
  (let [center (:c active-nbhd)
        opp-first ((:propose @center) active-nbhd)
        opp-rest (find-active-neighbors active-nbhd)]
    (into #{opp-first} opp-rest)))

(defn add-score
  "Returns an agent who's score is incremented by the given amount."
  [agent x]
  (assoc agent :score (+ (:score agent) x)))

(defn game-outcome
  "Returns the outcome of a game between the given agents."
  [agent-1 agent-2]
  (default-game-outcomes [(:type agent-1) (:type agent-2)]))

(defn play-games!
  "Play all pending games at the center cell."
  [active-nbhd]
  (let [center (:c active-nbhd)
        opponents (find-opponents active-nbhd)
        outcomes (for [opp opponents] (game-outcome @center @opp))]
    (dosync
     (doseq [[score-1 score-2] outcomes]
       (alter center add-score score-1))
     (alter center dissoc :propose))))

(defn play-step!
  "Perform a single game-playing step."
  [ca]
  (doall
   (pmap #(propose-out! % list-occupied-neighbors) (occupied-neighborhoods ca)))
  (doall
   (pmap play-games! (active-neighborhoods ca)))
  ca)

;; Movement rules

(defn list-empty-neighbors
  "Return a list of empty neighboring cells."
  [nbhd]
  (filter (fn [[k v]] (and (not= k :c) (not (occupied? v))))
          nbhd))

(defn complete-move!
  "Choose an active neighbor to move to the (empty) center cell."
  [empty-nbhd]
  (let [center (:c empty-nbhd)
        movers (find-active-neighbors empty-nbhd)]
    (dosync
     (doseq [mcell movers]
       (alter mcell dissoc :propose))
     (if-not (empty? movers)
       (let [mcell (rand-nth movers)]
         (ref-set center @mcell)
         (ref-set mcell :empty))))))

(defn move-step!
  "Perform a single move step."
  [ca]
  (doall
   (pmap #(propose-out! % list-empty-neighbors) (occupied-neighborhoods ca)))
  (doall
   (pmap complete-move! (empty-neighborhoods ca)))
  ca)

;; Birth-Death rules
;; Should rewrite this section to by DRY

(defn cull!
  "Remove an agent if their score falls to 0."
  [cell]
  (when (<= (:score @cell) 0)
    (dosync (ref-set cell :empty))))

(defn thin-the-herd! 
  "Remove all agents whose score has fallen to 0."
  [ca]
  (doall (pmap cull! (filter occupied? (:grid ca))))
  ca)

(defn split-agent 
  "Create two agents with the score of the original agent split
  between them."  
  [agent]
  (let [current-score (:score agent)]
    [(assoc agent :score (quot (inc current-score) 2))
     (assoc agent :score (quot current-score 2))]))

(defn complete-split!
  "Choose an active neighbor to split into the (empty) center cell."
  [empty-nbhd]
  (let [center (:c empty-nbhd)
        splitters (find-active-neighbors empty-nbhd)]
    (dosync
     (doseq [spcell splitters]
       (alter spcell dissoc :propose))
     (if-not (empty? splitters)
       (let [spcell (rand-nth splitters)
             [parent child] (split-agent @spcell)]
         (ref-set center child)
         (ref-set spcell parent))))))

(defn split-step!
  "Perform a single split step."
  [ca]
  (doall
   (pmap #(propose-out! % list-empty-neighbors) (abundant-neighborhoods ca)))
  (doall
   (pmap complete-split! (empty-neighborhoods ca)))
  ca)

;; step

(defn step!
  "Execute one model step."
  [ca]
  (-> ca
      play-step!
      thin-the-herd!
      split-step!
      move-step!))

;; GUI

(defn get-alpha [cell]
  (let [alpha (float (/ (:score @cell) default-split-at))]
    (cond (> alpha 1) (float 1.0)
          (< alpha 0) (float 0.0)
          :else alpha)))

(defn draw-ca [ca g g-width g-height]
  (let [grid-width (quot g-width (:ncol ca))
        grid-height (quot g-height (:nrow ca))]
    (.clearRect g 0 0 g-width g-height)
    (doseq [i (range (:nrow ca)) j (range (:ncol ca))]
      (let [cell (get-cell ca i j)]
        (if (occupied? cell)
          (let [type (:type @cell)
                rgb (if (= type :dove) gui-dove-rgb gui-hawk-rgb)
                alpha (get-alpha cell)
                x (* i grid-width)
                y (* j grid-height)]
            (.setColor g (Color. (rgb 0) (rgb 1) (rgb 2) alpha))
            (.fillRect g x y grid-width grid-height)))))))

(defn make-window-closer [frame]
  (proxy [WindowListener] []
    (windowActivated [e])
    (windowClosing [e]
      (doto frame
        (.setVisible false)
        (.dispose)))
    (windowDeactivated [e])
    (windowIconified [e])
    (windowOpened [e])
    (windowClosed [e])))

(defn make-frame [width height]
  (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        gs (.getDefaultScreenDevice ge)
        gc (.getDefaultConfiguration gs)
        frame (Frame. gc)
        window-closer (make-window-closer frame)]
    (doto frame
      (.setIgnoreRepaint true)
      (.setTitle "Hawk and Dove Game")
      (.setSize (Dimension. width height))
      (.setVisible true)
      (.createBufferStrategy 2)
      (.addWindowListener window-closer))))
      
(defn -main [& args]
  (let [ca (make-ca gui-nrow gui-ncol)
        frame (make-frame gui-width gui-height)
        strategy (.getBufferStrategy frame)]
    (dotimes [_ gui-steps]
      (let [ts (System/currentTimeMillis)
            g (.getDrawGraphics strategy)]
        (step! ca)
        (draw-ca ca g (.getWidth frame) (.getHeight frame))
        (.dispose g)
        (.show strategy) 
        (Thread/sleep (max 0 (- gui-refresh-delay
                                (- (System/currentTimeMillis) ts))))))))
