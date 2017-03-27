(ns sudoku.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn with-index [coll]
  (map-indexed vector coll))

(print (str x " "))

(defn sudoku-print [coll printfn filler]
  (doall
   (for [[i x] (with-index coll)]
     (do
       (printfn x)
       (when (and (not= (dec (count coll)) i)
                  (= 2 (mod i 3)))
         (print filler))
       (when (= (dec (count coll)) i)
         (println " "))))))

(defn print-line [line]
  (sudoku-print line #(print (str % " ")) "| "))

(defn print-board [board]
  (println "========BOARD========")
  (sudoku-print board print-line "------+-------+------\n"))

(def board [[2 9 6 3 1 8 5 7 4]
            [5 8 4 9 7 2 6 1 3]
            [7 1 3 6 4 5 2 8 9]
            [6 2 5 8 9 7 3 4 1]
            [9 3 1 4 2 6 8 5 7]
            [4 7 8 5 3 1 9 2 6]
            [1 6 7 2 5 3 4 9 8]
            [8 5 9 7 6 4 1 3 2]
            [3 4 2 1 8 9 7 6 5]])

(print-board board)

(defn get-box [x y board]
  (assert (<= 0 x 9))
  (assert (<= 0 y 9))
  (let [get-row-col #(cond
                       (> (/ (inc %) 3) 2) 2
                       (> (/ (inc %) 3) 1) 1
                       :default 0)
        min-x (* 3 (get-row-col y))
        min-y (* 3 (get-row-col x))
        box-indices (for [row (range min-x (+ 3 min-x))
                          col (range min-y (+ 3 min-y))]
                       [row col])]
     (map #(get-in board %) box-indices)))

(defn get-boxes [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (get-box x y board)))

(defn get-col [col board]
  (nth (apply map vector board) col))

(defn get-row [row board]
  (nth board row))

(get-boxes board)

(defn solved? [board]
  (and
    (not-any? neg? (flatten board))
    (every? true? (map distinct? board))
    (every? true? (map #(distinct? (get-col % board)) (range 9)))
    (every? true? (map distinct? (get-boxes board)))))

(solved? board)

(defn available? [n board])
