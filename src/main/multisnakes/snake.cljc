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

(defprotocol PSnake
  (move [this direction grow?])
  (hit? [this position])
  (dead? [this])
  (get-head [this])
  (get-tail [this]))
(defprotocol PBounded
  (in-bounds? [this w h]))

(defrecord ^:export Snake [positions]
  PSnake
  (move [this direction grow?]
    (let [head     (first positions)
          new-head (move-position head direction)]
      (-> this
          (update :positions
                  (fn [pos-list]
                    (concat
                     [new-head]
                     (if-not grow?
                       (butlast pos-list)
                       pos-list)))))))
  (get-head [this] (first positions))
  (get-tail [this] (rest positions))
  (hit? [this position]
    ((set positions) position))
  (dead? [this]
    (not= (count positions) (count (set positions))))
  PBounded
  (in-bounds? [this w h]
    (every? #(and (< (first %) w) (< (second %) h)) positions))
  Object
  (toString [this]
    (str "#Snake" (into {} (for [[k v] this] [k v])))))

(defn create-snake
  ([positions]

   (->Snake positions))
  ([] (create-snake [[0 0]])))

(defprotocol PBoard
  (play [this direction])
  (game-over? [this]))

(defn hit-target? [position direction target-position]
  (= (move-position position direction) target-position))

(defn new-target [w h snake]
  (when-not (= (* w h) (count (:positions snake)))
    (let [free-positions (->  (for [i     (range w)
                                    j     (range h)
                                    :when (not (hit? snake [i j]))]
                                [i j])
                              shuffle)]
      (first free-positions))))

(defrecord ^:export Board [width height snake target-position]
  PBoard
  (play [this direction]
    (let [need-new-target? (hit-target? (first (:positions snake))
                                        direction
                                        target-position)]
      (-> this
          (update :snake move direction need-new-target?)
          (update :target-position
                  (fn [target]
                    (if need-new-target?
                      (new-target width height (:snake this))
                      target))))))
  (game-over? [this] (dead? snake))
  Object
  (toString [this] (str "#Board" (into {} (for [[k v] this] [k v])))))

(defn create-board
  ([w h snake target]
   (map->Board {:width w
                :height h
                :snake snake
                :target-position target}))
  ([w h snake]
   (create-board w h snake (new-target w h snake)))
  ([w h]
   (create-board w h (create-snake [[0 0]]))))

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

#_(defn safe-dir? [snake direction]
   (let [p (first (:positions snake))]
        next-pos (first (direction-positions p direction))
    (not (contains? (into #{} (:positions snake)) next-pos))))

(defn neighbours [pos]
  (let [[x y] pos]
    (for [i [1 -1]
          j [1 -1]]
      [(+ i x) (+ j y)])))
(defn degrees-of-freedom [snake pos]
  (->> (neighbours pos)
       (reduce (fn [res p]
                 (if-not (hit? snake p)
                   (inc res)
                   res))
               0)))

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
  [snake direction]
  (let []
    (loop [depth MAX-SEARCH-DEPTH
           s     [(move snake direction false)]]
      (let [alives (filter (comp not dead?) s)]
        (if (empty? alives)
          false
          (if (zero? depth)
            true
            (recur
             (dec depth)
             (flatten (map
                       (fn [s1]
                         (filter
                          (comp not dead?)
                          (map
                           (fn [dir]
                             (move s1 dir false))
                           directions)))
                       alives)))))))))

(defn target-directions
  [snake target-position]
  (let [position          (first (:positions snake))
        dangers           (-> (:positions snake) rest butlast)
        dangers-distances (danger-distances-by-direction position dangers)
        [x1 y1]           position
        [x2 y2]           target-position
        [dx dy]           [(- x2 x1) (- y2 y1)]
        opts              [(when (pos? dx) :right)
                           (when (neg? dx) :left)
                           (when (pos? dy) :down)
                           (when (neg? dy) :up)]
        opts              (filter some?  opts)
        opts              (filter #(can-escape? snake %)
                                  opts)
        opts              (if-not (empty? opts)
                            opts
                            (do
                              (println "gotta choose least worse")
                              (filter #(can-escape? snake %)
                                      directions)))]
    opts))

(defn distance [pos1 pos2]
  (let [[x1 y1] pos1
        [x2 y2] pos2]
    (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))))

(defn snake-move-decide [snake target-position w h]
  (let [m       (* w h)
        p       (first (:positions snake))
        dirs    (map #(vector % (take m (direction-positions p %))) directions)
        dangers (map (fn [[d ds]]
                       (vector d
                               (any? #(= 1.0 (distance p %)) ds)))
                     dirs)]
    dangers))

(defn do-iterations [n]
  (loop [n n
         b (create-board 20 20)]
    (if (zero? n)
      b
      (recur (dec n)
             (play b
                   (target-direction
                    (first
                     (get-in b [:snake :positions]))
                    (:target-position b)))))))

