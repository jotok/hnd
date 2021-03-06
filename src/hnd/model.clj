(ns hnd.model)

(def default-initial-score 20)
(def default-p-dove 0.5)
(def default-split-at (* 2 default-initial-score))
(def default-p-occupied 0.3)

(def default-payoffs {[:hawk :hawk] [-2 -2]
                      [:hawk :dove] [1 -2]
                      [:dove :hawk] [-2 1]
                      [:dove :dove] [2 2]})

;; constructors

(defn make-random-agent 
  "Make an agent which is a dove with probability p and a hawk otherwise."  
  [& {:keys [initial-score p-dove] 
      :or {initial-score default-initial-score 
           p-dove default-p-dove}}] 
  {:score initial-score 
   :type (if (< (rand) p-dove) :dove :hawk)})

(defn make-grid
  "Construct an nrow x ncol grid of agents. A cell in the grid will be occupied with
  probability p-occupied, and the function make-agent-fn will be called to construct
  every agent." 
  [nrow ncol & {:keys
                [p-occupied make-agent-fn]
                :or {p-occupied default-p-occupied 
                     make-agent-fn #(make-random-agent)}}]
  (->> (for [r (range (* nrow ncol))]
         (if (< (rand) p-occupied)
           (make-agent-fn)
           {}))
       (into [])))

(defn make-ca
  "Construct a CA of the given dimensions using the default parameters."
  [nrow ncol]
  {:nrow nrow :ncol ncol :grid (make-grid nrow ncol)})

;; accessing and updating the CA

(defn sub->ind [ca i j]
  (+ (* (:ncol ca) i) j))

(defn ind->sub [ca r]
  [(quot r (:ncol ca)) (mod r (:ncol ca))])

(defn get-cell
  [ca r]
  (let [r (mod r (* (:nrow ca) (:ncol ca)))]
    (get-in ca [:grid r])))

(defn get-neighborhood
  "Returns the Von Neumann neighborhood at index r."
  [ca r]
  {:c (get-cell ca r)
   :n (get-cell ca (- r (:ncol ca)))
   :e (get-cell ca (inc r))
   :s (get-cell ca (+ r (:ncol ca)))
   :w (get-cell ca (dec r))})

(defn filter-cell-indices
  "Return a list of indices of cells which satisfy the predicate."
  [ca pred]
  (->> (:grid ca)
       (map vector (iterate inc 0))
       (filter (fn [[_ cell]] (pred cell)))
       (map first)))

(defn filter-cell-neighborhoods
  "Return a list of neighborhoods whose centers satisfy the predicate. The values inthe
  list are of the form [r cell], where r is the index of the cell."
  [ca pred]
  (for [r (filter-cell-indices ca pred)]
    [r (get-neighborhood ca r)]))

(defn filter-neighbor-dir
  "Returns a list of directions whose corresponding cells satisfy the given predicate.
  dir-cell-pred should be a function of a direction-cell pair."
  [nbhd dir-cell-pred]
  (->> (dissoc nbhd :c)
       (filter dir-cell-pred)
       (map first)))

(defn filter-neighbor-cell-dir
  "Returns a list of directions whose corresponding cells satisfy the given predicate.
  cell-pred should be a function of a cell."
  [nbhd cell-pred]
  (filter-neighbor-dir nbhd (fn [[dir cell]] (cell-pred cell))))

(defn center-propose-action-dir
  "Adds the key :propose-action-dir to the center cell with a value
  corresponding to a randomly chosen neighboring cell that satisfies
  cell-pred. If no neighbors satisfy cell-pred, the center cell is not
  altered. Returns the center cell."
  [nbhd cell-pred]
  (if-let [s (seq (filter-neighbor-cell-dir nbhd cell-pred))]
    (assoc (:c nbhd) :propose-action-dir (rand-nth s))
    (:c nbhd)))

(defn wants-to-act-on-center?
  "Returns true if the neighbor in direction dir is proposing to act on center."
  [[dir cell]]
  (#{[:n :s] [:s :n] [:e :w] [:w :e]} [dir (:propose-action-dir cell)]))

(defn center-accept-action-dir
  "Adds the key :accept-action-dir to the center cell with a value
  corresponding to a randomly chosen neighboring cell for which
  wants-to-act-on-center? is true. If no neighbors want to act on the
  center cell, it is not altered. Returns the center cell."
  [nbhd]
  (if-let [s (seq (filter-neighbor-dir nbhd wants-to-act-on-center?))]
    (assoc (:c nbhd) :accept-action-dir (rand-nth s))
    (:c nbhd)))

(defn wants-action-from-center?
  "Returns true if the neighbor in direction dir is requesting an action from center."
  [[dir cell]]
  (#{[:n :s] [:s :n] [:e :w] [:w :e]} [dir (:accept-action-dir cell)]))

(defn update-grid
  "Given a list of index-cell pairs, replace the cells in grid with
  the corresponding values in ix-cells."
  [grid ix-cells]
  (reduce (fn [g [ix cell]] (assoc g ix cell)) grid ix-cells))

(defn update-ca
  "For each cell in ca satisfying apply-to, call update-fn on the
  cell neighbohood and update ca with the new value."
  [ca  update-fn & {:keys [apply-to] :or {apply-to (constantly true)}}]
  (let [ix-nbhds (filter-cell-neighborhoods ca apply-to)
        ix-cells (for [[ix nbhd] ix-nbhds] [ix (update-fn nbhd)])]
    (assoc ca :grid (update-grid (:grid ca) ix-cells))))

(def not-empty? (complement empty?))

;; Game rules

(defn add-score
  "Add the score to the given agent."
  [agent x]
  (assoc agent :score (+ (:score agent) x)))

(defn game-payoff
  "Returns the outcome of a game between the given agents."
  [agent-1 agent-2]
  (default-payoffs [(:type agent-1) (:type agent-2)]))

(defn game-opponents
  "Returns a seq of game opponents."
  [agent-nbhd]
  (let [center (:c agent-nbhd)
        opponent-dir (filter-neighbor-dir agent-nbhd wants-to-act-on-center?)
        opponents (into #{} (for [dir opponent-dir] (dir agent-nbhd)))]
    (conj opponents ((:propose-action-dir center) agent-nbhd))))

(defn complete-games
  "Play all opponents, update score, and remove :propose-action-dir key."
  [agent-nbhd]
  (let [center (:c agent-nbhd)
        opponents (game-opponents agent-nbhd)
        payoffs (for [opp opponents] (game-payoff center opp))]
    (-> (reduce (fn [agent [score _]] (add-score agent score)) center payoffs)
        (dissoc :propose-action-dir))))

(defn game-step
  "Perform a single game-playing step."
  [ca]
  (-> ca
      (update-ca #(center-propose-action-dir % not-empty?) :apply-to not-empty?)
      (update-ca complete-games :apply-to :propose-action-dir)))

;; Movement rules

(defn complete-move
  "Update the center cell with any movement specified in the
  neighborhood."
  [nbhd]
  (let [accept-action-dir (:accept-action-dir (:c nbhd))
        target-dir (filter-neighbor-dir nbhd wants-action-from-center?)
        new-cell (cond accept-action-dir (accept-action-dir nbhd)
                       (seq target-dir) {}
                       :else (:c nbhd))]
    (dissoc new-cell :propose-action-dir)))

(defn move-step
  [ca]
  (-> ca
      (update-ca #(center-propose-action-dir % empty?) :apply-to not-empty?)
      (update-ca center-accept-action-dir :apply-to empty?)
      (update-ca complete-move)))

;; Birth-Death rules

(defn cull
  "If the cell score is 0 or less, return an empty cell, otherwise
  return the original cell."  
  [cell]
  (if (and (:score cell)
           (<= (:score cell) 0))
    {}
    cell))

(defn thin-the-herd
  "Replace any cells with a score of 0 or less with an empty cell."
  [ca]
  (let [new-grid (into [] (map cull (:grid ca)))]
    (assoc ca :grid new-grid)))

(defn split-agent 
  "Create two agents with the score of the original agent split
  between them."  
  [agent]
  (let [current-score (:score agent)]
    [(assoc agent :score (quot (inc current-score) 2))
     (assoc agent :score (quot current-score 2))]))

(defn abundant? 
  "Returns true if the cell is occupied and the score of the occupant is greater than or
  equal to threshold."  
  [cell & {:keys [threshold] :or {threshold default-split-at}}]
  (and (:score cell) 
       (>= (:score cell) threshold)))

(defn complete-split
  "Update the center cell with any splits specified by the neighborhood."
  [nbhd]
  (let [accept-action-dir (:accept-action-dir (:c nbhd))
        target-dir (filter-neighbor-dir nbhd wants-action-from-center?)
        new-cell (cond accept-action-dir 
                       (let [[_ child] (split-agent (accept-action-dir nbhd))]
                         child)
                       (seq target-dir) 
                       (let [[parent _] (split-agent (:c nbhd))]
                         parent)
                       :else (:c nbhd))]
    (dissoc new-cell :propose-action-dir)))

(defn death-and-birth-step
  [ca]
  (-> ca
      thin-the-herd
      (update-ca #(center-propose-action-dir % empty?) :apply-to abundant?)
      (update-ca center-accept-action-dir :apply-to empty?)
      (update-ca complete-split)))

;; model step

(defn step
  "Execute one model step."
  [ca]
  (-> ca game-step death-and-birth-step move-step))


