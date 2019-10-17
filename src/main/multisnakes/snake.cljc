(ns multisnakes.snake
  (:require [thi.ng.math.core :as math]
            [astar.core :as astar]))
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
   (let [ss (filterv #(not= (:id snake) (:id %))
                     (vals (:snakes b)))]
     (apply concat (map (fn [s]
                          (if-not (:dead? s)
                            (drop-last future-rounds (:positions s))
                            (:positions s)))
                        ss))))

  ([b snake] (get-blocked-positions b snake 1)))

(declare abs)

(defprotocol PSnake
  (move [this direction grow?])
  (hit? [this position])
  (dead? [this blocked-positions])
  (get-head [this])
  (get-tail [this]))

(defprotocol PGraph
  (graph [this snake-id pos])
  (dist [this from to])
  (heuristic-dist [this target-pos])
  (route [this snake-id from to]))

(defprotocol RoutePlanner
  (plan-route [this snake-id])
  (route-planned? [this snake-id])
  (planned-route-valid? [this snake-id])
  (step-route [this snake-id]))

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
    ((into #{} (:positions this)) position))

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
(extend-protocol PBoard
  nil
  (play [this a b] this)
  (game-over? [_] false))
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
      (-> board
          (assoc :previous-target (:target-position board)
                 :target-position (new-target (:width board) (:height board) (:snakes board))))
      board)))

(defn event [type snake-id text]
  {:type type
   :snake-id snake-id
   :message text})

(defn update-events [{:keys [snake-id died? captured-target?]}]
  (let [update-dead
        (fn [board]
          (if died?
            (update board
                    :events
                    conj
                    (event :snake-died snake-id
                           (str "snake " snake-id " has died")))
            board))
        update-captured
        (fn [board]
          (if captured-target?
            (update board
                    :events
                    conj
                    (event :capture-target snake-id
                           (str "snake " snake-id " has captured target")))
            board))]
    (comp update-dead update-captured)))

(defrecord Board [width height snakes target-position]
  PBoard
  (play [this snake-id direction]
    (let [snake            (get-in this  [:snakes snake-id])
          need-new-target? (hit-target? (get-head snake)
                                        direction
                                        (:target-position this))
          new-snake        (move snake direction need-new-target?)
          new-snake        (if (dead? new-snake (get-blocked-positions this new-snake))
                             (assoc new-snake :dead? true)
                             new-snake)
          events-updater   (update-events {:snake-id        snake-id
                                           :died?           (:dead? new-snake)
                                           :captured-target need-new-target?})
          target-updater   (update-board-target-fn need-new-target?)]
      (-> this
          (assoc-in [:snakes snake-id] new-snake)
          (target-updater)
          (events-updater))))

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
        (dissoc :game-over? :events))))
(defn create-board
  ([w h snakes target]
   (map->Board {:width           w
                :height          h
                :snakes          (cond
                                   (map? snakes)           snakes
                                   (coll? snakes)          (->>
                                                            (doall
                                                             (for [s snakes]
                                                               [(:id s) s]))
                                                            (into {}))
                                   (= Snake (type snakes)) {(:id snakes) snakes})
                :target-position target
                :events          []}))
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
    (for [i [1 0 -1]
          j [1 0 -1]
          :when (not= (abs i) (abs j))]
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
          ;; (println (:id snake) " cannot escape")
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
                              ;; (println "gotta choose least worse")
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
(defn pos->direction [from to]
  (let [[x1 y1] from
        [x2 y2] to
        [dx dy]    [(- x2 x1) (- y2 y1)]]
    (cond
      (zero? dx) (if (neg? dy) :up :down)
      (zero? dy) (if (neg? dx) :left :right))))

(defn play-round [board]
  (assert (= Board (type board)))
  (when-not  (nil? board)
    ;; (if-not (game-over? board)
    (let [snake-ids (vec
                     (shuffle
                      (keys (:snakes board))))]
      (reduce
       (fn [result-board snake-id]
         (let [snake          (get-in result-board [:snakes snake-id])
               snake-is-dead? (dead? snake (get-blocked-positions result-board snake))]
                 ;; r              (route result-board (get-head snake) (:target-position result-board))
                 ;; play-direction (if-not snake-is-dead?
                                  ;; (if-not (empty? r)
                                  ;; (->> r
                                       ;; first
                                       ;; (pos->direction (get-head snake))
                                  ;; (->>
                                   ;; (target-directions snake result-board)
                                   ;; shuffle
                                   ;; first
                                  ;; (-> directions shuffle first))
                 ;; play-direction (if (nil? play-direction) (-> directions shuffle first) play-direction)]
             ;; (println "route :" r)
           (if-not snake-is-dead?
             (if-let [new-board (step-route result-board snake-id)]
               new-board
               (let [play-direction (->> (target-directions snake result-board)
                                         shuffle
                                         first)
                     play-direction (if (nil? play-direction) (-> directions shuffle first) play-direction)]
                 (println "choosing least bad")
                 (play result-board snake-id play-direction)))
             result-board)))
       board
       snake-ids)
      #_(do
          (println "Game Over, no more rounds")
          board))))
    ;; board))


;; (defprotocol PGraph
  ;; (graph [this pos])
  ;; (dist [this from to])
  ;; (heuristic-dist [this target-pos])
  ;; (route [this from to]))

(defn abs [x]
  (if (neg? x) (* -1 x) x))

(defn get-graph [board snake-id pos]
  (let [snake (get-in board [:snakes snake-id]) 
        blocked (into #{} 
                      (concat (:positions snake)
                              (get-blocked-positions board snake)))]
    (->> (neighbours pos)
         (filter (complement blocked)))))

(extend-protocol PGraph
  Board
  (graph [this snake-id pos]
    (get-graph this snake-id pos))

  (dist [this from to]
    (let [[x1 y1] from
          [x2 y2] to]
      (+ (abs (- x2 x1)) (abs (- y2 y1)))))
  (heuristic-dist [this target-pos] 0)
  (route [this snake-id from to]
    (let [res (astar/route (partial graph this snake-id) (partial dist this) (partial heuristic-dist this) from to)]
      (println res)
      res)))

(defn execute-route [snake-id  board]
  (let [snake    (get-in board [:snakes snake-id])
        r        (:planned-route snake)
        next-pos (first r)
        next-dir (pos->direction (get-head snake) next-pos)]
    (-> (play board snake-id next-dir)
        (update-in [:snakes snake-id :planned-route] rest))))

(extend-protocol RoutePlanner
  Board
  (plan-route [this snake-id]
    (let [snake (get-in this [:snakes snake-id])
          r     (route this snake-id (get-head snake) (:target-position this))]
      (-> this
          (assoc-in [:snakes snake-id :planned-route] r)
          (assoc-in [:snakes snake-id :planned-route-target] (:target-position this)))))
  (route-planned? [this snake-id]
    (not-empty
     (get-in this [:snakes snake-id :planned-route])))
            
  (planned-route-valid? [this snake-id]
    (if (and
         (route-planned? this snake-id)
         (= (get-in this [:snakes snake-id :planned-route-target])
            (get this :target-position)))
      (let [snake (get-in this [:snakes snake-id])
            blocked (into #{}
                          (get-blocked-positions this snake))
            r       (get-in snake [ :planned-route])]
        (not-any?  blocked r))
      false))
  (step-route [this snake-id]
    (if (planned-route-valid? this snake-id)
      (execute-route snake-id this)
      (let [new-this  (plan-route this snake-id)]
            ;; new-board (assoc-in this [:snakes (:id new-this)] new-this)]
        (when  (planned-route-valid? new-this snake-id)
          (execute-route snake-id new-this))))))
