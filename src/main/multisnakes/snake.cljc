(ns multisnakes.snake)

(def directions #{:up :down :left :right})

(def direction-delta
  {:up    [0 -1]
   :down  [0 1]
   :left  [-1 0]
   :right [1 0]})

(defn move-position [pos direction]
  (let [[x y]   pos
        [dx dy] (get direction-delta direction [0 0])]
    [(+ x dx) (+ y dy)]))

(defn get-blocked-positions
  ([b snake future-rounds]
   (let [ss (filter #(not= (:id snake) (:id %))
                    ;; (drop-last  future-rounds)
                    (vals (:snakes b)))]
     (apply concat (map (fn [s]
                          (if-not (:dead? s)
                            (drop-last future-rounds (:positions s))
                            (:positions s)))
                        ss))))

  ([b snake] (get-blocked-positions b snake 1)))

(defprotocol PSnake
  (move [this direction grow?])
  (hit? [this position])
  (dead? [this blocked-positions])
  (get-head [this])
  (get-tail [this]))

(defprotocol PReset
  (reset [this]))

(defprotocol PBounded
  (in-bounds? [this w h]))

(defrecord ^:export Snake [id positions]
  PSnake
  (move [this direction grow?]
    (let [head     (first positions)
          new-head (move-position head direction)]
      (-> this
          (assoc :previos-direction direction)
          (update :positions
                  (fn [pos-list]
                    (concat
                     [new-head]
                     (if-not grow?
                       (butlast pos-list)
                       pos-list)))))))

  (get-head [this] (first (:positions this)))

  (get-tail [this] (rest (:positions this)))

  (hit? [this position]
    ((set (:positions this)) position))

  (dead? [this blocked-positions]
    (or (get this :dead? false)
        ((into #{} (concat blocked-positions (get-tail this))) (get-head this))))

  PBounded
  (in-bounds? [this w h]
    (every? #(and (< (first %) w)
                  (< (second %) h))
            (:positions this)))
  PReset
  (reset [this]
    (-> this
        (dissoc :dead?)
        (update :positions #(take 1 %)))))

(def NEXT-SNAKE-ID (atom 1000))

(defn ^export create-snake
  ([positions id]
   (map->Snake
    {:id id
     :positions positions}))
  ([positions]
   (create-snake positions (swap! NEXT-SNAKE-ID inc)))
  ([]
   (create-snake  [[0 0]])))

(defprotocol PBoard
  (play [this snake-id direction])
  (game-over? [this]))

(defn hit-target? [position direction target-position]
  (= (move-position position direction) target-position))

(def sum (partial reduce +))

(defn new-target
  [w h snakes]
  (when-not (= (* w h) (sum  (mapv #(count (:positions %)) (vals snakes))))
    (let [free-positions (->  (for [i     (range w)
                                    j     (range h)
                                    :when (not-any? #(hit? % [i j])  (vals snakes))]
                                [i j])
                              shuffle)]
      (first free-positions))))

(defn update-board-target-fn [need-new-target?]
  (fn [board]
    (if need-new-target?
      (do
        (-> board
            (assoc :previous-target (:target-position board)
                   :target-position (new-target (:width board) (:height board) (:snakes board)))))
      board)))

(defrecord ^:export Board [width height snakes target-position]
  PBoard
  (play [this snake-id direction]
    (let [snake            (get-in this  [:snakes snake-id])
          need-new-target? (hit-target? (first (:positions snake))
                                        direction
                                        (:target-position this))
          new-snake        (move snake direction need-new-target?)
          new-snake        (if (dead? new-snake (get-blocked-positions this new-snake))
                             (assoc new-snake :dead? true)
                             new-snake)
          target-updater   (update-board-target-fn need-new-target?)]
      (-> this
          (assoc-in [:snakes snake-id] new-snake)
          ;; (update :target-position
          ;;         (fn [target]
          ;;           (if need-new-target?
          ;;             (new-target (:width this) (:height this) (:snakes this))
          ;;             target)))
         target-updater))) 
  (game-over? [this]
    (every?
     #(dead? % (get-blocked-positions this %))
     (vals (:snakes this))))
  Object
  (toString [this] (str "#Board" (into {} (for [[k v] this] [k v]))))
  PReset
  (reset [this]
    (-> this
        (update :snakes
                (fn [snakes]
                  (into {}
                        (for [[snake-id snake] snakes]
                          [snake-id (reset snake)]))))
        (dissoc :game-over?))))

(defn create-board
  ([w h snakes target]
   (map->Board {:width w
                :height h
                :snakes (cond
                          (map? snakes) snakes
                          (coll? snakes) (->>
                                          (for [s snakes]
                                            [(:id s) s])
                                          (into {}))
                          (= Snake (type snakes)) {(:id snakes) snakes})
                :target-position target}))
  ([w h snakes]
   (create-board w h snakes (new-target w h snakes)))
  ([w h]
   (create-board w h [(create-snake)])))

(defn target-direction
  [position target-position]
  (let [[x1 y1] position
        [x2 y2] target-position
        [dx dy] [(- x2 x1) (- y2 y1)]
        opts    [(when (pos? dx) :right)
                 (when (neg? dx) :left)
                 (when (pos? dy) :down)
                 (when (neg? dy) :up)]
        opts (filter some?  opts)]
    (-> opts shuffle first)))

(defn direction-positions [pos direction]
  (let [[x y] pos]
    (condp = direction
      :up    (drop 1
                   (for [i (range)] [x (- y i)]))
      :down  (drop 1
                   (for [i (range)] [x (+ y i)]))
      :right (drop 1
                   (for [i (range)] [(+ x i) y]))
      :left  (drop 1
                   (for [i (range)] [(- x i) y])))))

(defn neighbours [pos]
  (let [[x y] pos]
    (for [i [1 -1]
          j [1 -1]]
      [(+ i x) (+ j y)])))

(def MAX-AHEAD 50)

(defn positions-ahead [position direction]
  (take MAX-AHEAD
        (iterate #(move-position % direction) position)))

(defn danger-distances-by-direction
  [position dangers]
  (let [projections
        (into
         {}
         (map
          #(vector % (positions-ahead position %))
          directions))]
    (into {}
          (map
           (fn
             [[dir positions]]
             [dir
              (loop [res       0
                     p         (first positions)
                     remaining (rest positions)]
                (if (or (nil? p)
                        ((set dangers) p))
                  res
                  (recur (inc res) (first remaining) (rest remaining))))])
           projections))))

(def opposite-direction
  {:up    :down
   :down  :up
   :left  :right
   :right :left})

(def MAX-SEARCH-DEPTH 20)

(defn can-escape?
  [snake direction board]
  ;; (let []
  (loop [depth             1
         s                 [(move snake direction false)]
         blocked-positions (get-blocked-positions board snake depth)]
    (let [alives (filter (comp not #(dead? % blocked-positions)) s)]
      (if (empty? alives)
        (do
          (println (:id snake) " cannot escape")
          false)
        (if (= MAX-SEARCH-DEPTH depth)
          true
          (recur
           (inc depth)
           (flatten (map
                     (fn [s1]
                       (filter
                        (comp not #(dead? % blocked-positions))
                        (map
                         (fn [dir]
                           (move s1 dir false))
                         directions)))
                     alives))
           (get-blocked-positions board snake (inc depth))))))))

(defn target-directions
  [snake board]
  (let [target-position   (:target-position board)
        blocked-positions (get-blocked-positions board snake)
        position          (first (:positions snake))
        dangers           (concat
                           (-> (:positions snake) rest butlast)
                           blocked-positions)
        dangers-distances (danger-distances-by-direction position dangers)
        [x1 y1]           position
        [x2 y2]           target-position
        [dx dy]           [(- x2 x1) (- y2 y1)]
        ;; snake (create-snake (concat (:positions snake) blocked-positions))
        opts              [(when (pos? dx) :right)
                           (when (neg? dx) :left)
                           (when (pos? dy) :down)
                           (when (neg? dy) :up)]
        opts              (filter some? opts)
        opts              (filter (fn [o] (not= o (opposite-direction (:previos-direction snake))))  opts)
        opts              (filter #(can-escape? snake % board)
                                  opts)
        opts              (if-not (empty? opts)
                            opts
                            (do
                              (println "gotta choose least worse")
                              (filter #(can-escape? snake % board)
                                      directions)))]
    opts))

(defn distance [pos1 pos2]
  (let [[x1 y1] pos1
        [x2 y2] pos2]
    (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))))

(defn positions-taken [board]
  (let [ss (vals (:snakes board))]
    (apply concat (map :positions ss))))

(defn random-position [w h taken]
  (->> (for [i     (range w)
             j     (range h)
             :when (empty? ((set taken) [i j]))] [i j])
       shuffle
       first))


(defn play-round [board]
  (if-not (game-over? board)
    (let [snake-ids (keys (:snakes board))]
      (reduce
       (fn [result-board snake-id]
         (let [snake          (get-in result-board [:snakes snake-id])
               snake-is-dead? (dead? snake (get-blocked-positions result-board snake))
               play-direction (if-not snake-is-dead?
                                (->> (target-directions snake result-board)
                                     shuffle
                                     first)
                                (-> directions shuffle first))]
           (if-not snake-is-dead?
             (play result-board snake-id play-direction)
             result-board)))
       board
       snake-ids))
    (do
      (println "Game Over, no more rounds")
      board)))
