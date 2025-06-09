(ql:quickload "sdl2")

(defpackage #:plantworld
  (:use :common-lisp)
  (:export :main))

(in-package :plantworld)

(defun make-world (x y)
  (make-array (list y x) :initial-element 0))

(defun get-cell-size (screen-y screen-x grid)
  (destructuring-bind (grid-y grid-x) (array-dimensions grid)
    (mapcar 'round
            (list (/ screen-y grid-y)
                  (/ screen-x grid-x)))))

(defun get-point-neighbours (x y &key (distance 1))
    (loop for dx from (- distance) to distance
          append (loop for dy from (- distance) to distance
                       unless (= dx dy 0) ; the point itself
                         collect (list (+ x dx) (+ y dy)))))

(defun hypotenuse (x y)
  (sqrt (+ (* x x) (* y y))))

(defun norm-vector (x y)
  (let ((length (hypotenuse x y)))
    (if (zerop length)
        (values 0 0)
        (values (/ x length) (/ y length)))))

(defun cast-ray (x y dx dy distance)
  "The DDA raycasting method."
  (multiple-value-bind (norm-x norm-y) (norm-vector dx dy)
    (flet
        ((initial-max (el norm) ; This computes how far you have to go
           (if (/= norm 0)      ; along the ray to reach the next grid line.
               (/ (+ el (if (> norm 0) norm 0) (- el)) norm)
               most-positive-single-float))
         
         (initial-delta (norm) ; Time to cross one unit in x or y
           (if (/= norm 0)     ; direction.
               (abs (/ 1 norm))
               most-positive-single-float))) ; Avoid division by zero.
      (loop
        :with curr-x = x
        :and curr-y  = y
        :and max-x   = (initial-max x norm-x)
        :and max-y   = (initial-max y norm-y)
        :and delta-x = (initial-delta norm-x)
        :and delta-y = (initial-delta norm-y)
        :and step-x  = (if (> norm-x 0) 1 -1)
        :and step-y  = (if (> norm-y 0) 1 -1)

        :for step :from 0 below distance
        :if (< max-x max-y)
          :do (incf max-x delta-x) (incf curr-x step-x)
        :else
          :do (incf max-y delta-y) (incf curr-y step-y)
        :collect (list curr-x curr-y step)))))

(defun test-cast-ray () nil)

(defun make-circle (x y radius n-points)
  "Compute a set of integer points on the circumference of a circle.
Resulting points have to be rounded to integers as we're dealing with
pixels here"
  (flet ((get-point (n)
           (let ((theta (/ (* 2 pi n) n-points)))
             (list (truncate (+ x (* radius (cos theta))))
                   (truncate (+ y (* radius (sin theta))))))))
    (loop :for i :from 0 :below n-points :collect (get-point i))))



(defun draw-circle (renderer x y radius &optional (n-points 10))
  ;; TODO modify this to work better
  (let ((point-objects (make-circle x y radius n-points)))
    (sdl2:render-draw-points renderer point-objects n-points)))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "plant world version zero"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun draw-grid-lines (renderer)
  (destructuring-bind (w h) (get-cell-size *screen-width* *screen-height* *world*)
    (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
    (loop :for i :from 0 :to *screen-height* :by h
          :for j :from 0 :to *screen-width* :by w
          :do (sdl2:render-draw-line renderer j 0 j *screen-height*)
              (sdl2:render-draw-line renderer 0 i *screen-width* i))))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *world* (make-world 10 10))
(defparameter *objects* (list))
(defparameter *lightsources* (list))

(defun run ()
   (with-window-renderer (window renderer)
     (livesupport:continuable
     (sdl2:with-event-loop (:method :poll)
       (:quit () t)
       (:idle ()
              ;; Clear screen
              (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
              (sdl2:render-clear renderer)
              
              ;; Grid lines for debugging
              (draw-grid-lines renderer)

              ;; Test circle
              (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
              (loop :for (x y) :in (make-circle 300 150 100 10)
                    :do (sdl2:render-draw-point renderer x y))

              ;; Some basic lightsources with 1 ray like in the Python version
              ;; along the top of the world shining straight down
              (destructuring-bind (world-x world-y) (array-dimensions *world*)
                (loop :for xy in (loop for i from 0 below world-x collect (list i 0))
                      :do ((destructuring-bind (x y) xy
                             (let ((ray-path (cast-ray x y 0 -1 world-y)))
                               (loop :for (rx ry step) :in ray-path :do (setf (aref *world* rx ry) step)))))))

              ;;(loop :for i :from 0 :below (world-x)
              ;;      :for j :from 0 :below (world-y)
                    
              
              (sdl2:render-present renderer))
       ;; Debugger continue to keep REPL alive on error
       (livesupport:update-repl-link)))))
  
;;;
;;;Compilation instructions for SBCL
;;;

(sb-ext:save-lisp-and-die "app"
  :toplevel #'main
  :executable t)
