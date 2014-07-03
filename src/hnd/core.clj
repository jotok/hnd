(ns hnd.core
  (:require [hnd.model :as model])
  (:import [java.awt Color Dimension Frame GraphicsEnvironment]
           [java.awt.event WindowListener]))

(def gui-nrow 40)
(def gui-ncol gui-nrow)
(def gui-width 600)
(def gui-height 600)
(def gui-refresh-delay 200)
(def gui-dove-rgb (into [] (map float [0.149020 0.545098 0.823529])))
(def gui-hawk-rgb (into [] (map float [0.796078 0.294118 0.086275])))
(def gui-steps 80)

(defn get-alpha [cell]
  (let [alpha (/ (:score cell) model/default-split-at)
        alpha (max 0 (min 1 alpha))]
    (float alpha)))

(defn draw-ca [ca g g-width g-height]
  (let [grid-width (quot g-width (:ncol ca))
        grid-height (quot g-height (:nrow ca))]
    (.clearRect g 0 0 g-width g-height)
    (doseq [i (range (:nrow ca)) j (range (:ncol ca))]
      (let [cell (model/get-cell ca (model/sub->ind ca  i j))]
        (if (model/not-empty? cell)
          (let [type (:type cell)
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
  (let [ca (atom (model/make-ca gui-nrow gui-ncol))
        frame (make-frame gui-width gui-height)
        strategy (.getBufferStrategy frame)]
    (dotimes [_ gui-steps]
      (let [ts (System/currentTimeMillis)
            g (.getDrawGraphics strategy)]
        (swap! ca model/step)
        (draw-ca @ca g (.getWidth frame) (.getHeight frame))
        (.dispose g)
        (.show strategy) 
        (Thread/sleep (max 0 (- gui-refresh-delay
                                (- (System/currentTimeMillis) ts))))))))
