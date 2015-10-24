(ns fill-rectangle.core)

(def problem-factor
  {:box-width 2
   :box-height 3
   :box-count 20})

(defn next-cell
  " get next cell "
  [[x y]]
  (if (= y 0)
    [0 (inc x)]
    [(inc x) (dec y)]))

(defn cell-list
  " create infinite cell list
    not used"
  [start-cell-pos]
  (iterate next-cell start-cell-pos))

(defn get-cell-owner
  " get cell-owner at the position.
    return false if it is not occupied"
  [wall-map cell-pos]
  (get wall-map cell-pos false))

(defn max-width-height
  " get max width and height of the wall"
  [wall-map]
  (reduce (fn [[cur-x-max cur-y-max]
              [x y]]
            [(max cur-x-max x)
             (max cur-y-max y)])
          [0 0]
          (keys wall-map)))

(defn calc-psude-area
  " calculate psude area from max height and wihth
    not correct if the shape is not rectangle"
  [wall-map]
  (apply * (max-width-height wall-map)))

(defn succeed? [wall-map problem-factor]
  " check if the sum of box area and pseudo wall area is same or not
    this will same as if tha problem are solved"
  (= (* (:box-height problem-factor)
        (:box-width problem-factor)
        (:box-count problem-factor))
     (calc-psude-area wall-map)))

(defn cells-occuping [problem-factor [org-x org-y] orientation]
  (let [width-range (range 0 (:box-width problem-factor))
        height-range (range 0 (:box-height problem-factor))
        [x-range y-range] (if (= orientation :vertical)
                            [height-range width-range]
                            [width-range height-range])]
    (for [x x-range y y-range]
      [(+ org-x x) (+ org-y y)])))

(defn set-box [box-id wall-map problem-factor position orientation]
  (let [need-cell (cells-occuping problem-factor position orientation)]
    (if (apply = false (map (partial get-cell-owner wall-map)
                            need-cell))
      (reduce #(assoc %1 %2 box-id)
              wall-map
              need-cell)
      false)))






