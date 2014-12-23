 drawing.practice
  (:require [quil.core :as q]))

(def flake (ref nil))        ;; reference to snowflake image
(def background (ref nil))   ;; reference to blue background image

(defn setup []
    ;; loading two images
    (dosync
               (ref-set flake (q/load-image http://www.specialistpaints.com/image_uploads/large/White_Flake.jpg))
         (ref-set background (q/load-image http://www.photosinbox.com/download/grunge-blue-background.jpg )))

(defn draw []
    ;; drawing blue background and a snowflake on it
    (q/background-image @background)
    (q/image @flake 400 10))

(q/defsketch practice
    :title "Clara's Quil practice"
    :size [1000 1000]
    :setup setup
    :draw draw)
