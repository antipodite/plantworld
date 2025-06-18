(in-package :plantworld)

#|
  Lindenmeyer system implementation.
  "An 0L-System according to Prusinkiewicz and Lindenmayer (1996) as well as Hanan (1992)
  is defined as an ordered triplet G = 〈V, ω, P 〉, consisting of
  1. an alphabet V of symbols corresponding to the different types of plant modules,
  2. an initial nonempty string of symbols ω ∈ V + called the axiom,
  3. as well as a finite set of productions P ⊂ V × V ∗."
  - Nikole Leopold's thesis
  https://www.cg.tuwien.ac.at/research/publications/2017/LEOPOLD-2017-ALG/LEOPOLD-2017-ALG-thesis.pdf
|#

;; Currently unused. TODO check plants don't get too large once I know what the
;; effect on performance is. States up to at least 100,000 don't seem to be a
;; problem now code uses lists rather than strings
(defparameter *max-state-size* 1000000) 

(defclass base-plant ()
  ((axiom :initarg :axiom :accessor plant-axiom :type list)
   (rules :initarg :rules :accessor plant-rules :type hash)
   ;; (angle :initarg :angle :accessor plant-angle :type number)
   (state :accessor plant-state :initform '() :type list)
   (iters :accessor plant-iters :initform 0 :type integer))
  (:documentation "Base data structure storing L-system definition and state."))

(defgeneric grow-plant (obj)
  (:documentation "Generic function for doing one iteration of L-system and setf'ing result."))

(defgeneric draw-plant (obj &rest args)
  (:documentation "Generic function for drawing an L-system to screen."))


(defmethod initialize-instance :after ((plant base-plant) &key)
  "Make sure the types of the slots are correct.
We're operating on lists of single char strings internally, and degrees
should be converted to radians. Since this is the base class, we don't
do anything about the production rules as these are specified for each
type of L-system."
  (setf (plant-axiom plant) (as-list (plant-axiom plant)))
  ;;(setf (plant-angle plant) (degrees->radians (plant-angle plant)))
  (setf (plant-state plant) (as-list (plant-axiom plant))))


(defmethod print-object ((plant base-plant) stream)
  (print-unreadable-object (plant stream :type t)
    (format stream
            "Axiom: ~A; Rules: ~A; Angle: ~A° (~,2f rad); State length: ~A"
            (first (plant-axiom plant))
            (print-hash (plant-rules plant) nil)
            (radians->degrees (plant-angle plant))
            (plant-angle plant)
            (length (plant-state plant)))))


(defmethod draw-plant ((plant base-plant) &rest args)
  "Draw a plant of any type to screen."
  (destructuring-bind (renderer pos heading left up step) args
    (let
        ((lines (interpret-string (plant-state plant) (plant-angle plant) pos heading left up :step-size step)))
      (loop
        for xyz in lines do
          (destructuring-bind (x1 y1 z1 x2 y2 z2) (mapcar #'truncate (alexandria:flatten xyz))
            (declare (ignore z1 z2))
            (sdl2:render-draw-line renderer x1 y1 x2 y2))))))


(defclass plant (base-plant)
  ((angle :initarg :angle :accessor plant-angle :type number))
  (:documentation "The plain L-system, with no parametric behaviour or stochasticity.
Rules are stored as a hash table with symbols as keys and productions as values."))


(defmethod initialize-instance :after ((plant plant) &key)
  "Set up hash table for generic L-system with no stochastic productions."
  (setf (plant-angle plant) (degrees->radians (plant-angle plant)))
  (setf (plant-rules plant)
        (hash-create (mapcar (lambda (pair) (destructuring-bind (key val) pair
                                         (list key (as-list val))))
                             (plant-rules plant)))))


(defmethod grow-plant ((plant plant))
  (let ((new-state (mapcar (lambda (str) (or (gethash str (plant-rules plant))
                                        str))
                           (plant-state plant))))
    (setf (plant-state plant) (flatten new-state))
    (incf (plant-iters plant))))


(defclass stochastic-plant (base-plant)
  ((angle :initarg :angle :accessor plant-angle :type number))
  (:documentation "For stochastic L-systems.
Rules are specified as nested lists, e.g.:
'((\"F\" (0.33 \"F[+F]F[-F]F\") (0.33 \"F[+F]F\") (0.34 \"F[-F]F\")))
Rules are stored in a hash table where keys are symbols and values are lists
containing two element lists of (probability production)."))


(defmethod initialize-instance :after ((plant stochastic-plant) &key)
  "Set up hash table for stochastic L-system."
  ;; Sanity check probabilities sum to 1
  (let ((prob-sums (loop for (symbol productions) in (plant-rules plant)
                         for x = (loop for (probability production) in productions collect probability)
                         collect (reduce #'+ x))))
    (when
        (/= (reduce #'+ prob-sums) (length (plant-rules plant)))
      (error "Probabilities don't sum to 1 for each rule!"))
    (setf (plant-angle plant)
          (degrees->radians (plant-angle plant)))
    (setf (plant-rules plant)
          ;; Convert production strings to lists and generate hash table
          (hash-create (loop for (s ps) in (plant-rules plant)
                             for listified = (loop for (p res) in ps collect (list p (as-list res)))
                             collect (list s listified))))))


(defmethod grow-plant ((plant stochastic-plant))
  (let ((new-state (mapcar (lambda (str)
                             (let ((choices (gethash str (plant-rules plant))))
                               (if choices
                                   (roulette-select choices)
                                   str)))
                           (plant-state plant))))
    (setf (plant-state plant) (flatten new-state))
    (incf (plant-iters plant))))

;;; This is going to be much more complex. I'll need a tokeniser

(defclass parametric-plant (base-plant)
  ((parameters :initarg :params :accessor plant-params :type hash))
  (:documentation "A parametric L-system."))

(defmethod initialize-instance :after ((plant parametric-plant) &key)
  (setf (plant-rules plant)
        (hash-create (mapcar (lambda (pair) (destructuring-bind (key val) pair
                                         (list key (as-list val))))
                             (plant-rules plant)))))

;;(defparameter *para* (make-instance 'parametric-plant :


;;;
;;; Lindenmayer system turtle interpreter
;;;

(defun interpret-string (string angle initial-pos initial-heading
                         initial-left initial-up &key (step-size 1))
  (let
      ((position initial-pos) ; (x, y, z)
       (prev-pos nil)
       (heading  initial-heading)
       (up       initial-up)
       (left     initial-left)
       (stack    '())
       (lines   '()))
    (flet
        ((move ()
           (list  (+ (first position) (* (first heading) step-size))
                  (+ (second position) (* (second heading) step-size))
                  (+ (third position) (* (third heading) step-size))))
         (renormalise ()
           (setf heading (norm-vector heading))
           (setf left (norm-vector (cross-product up heading)))
           (setf up (norm-vector (cross-product heading left)))))
      (loop
        for instruction in string do
          (cond
            ;; Move forward one step
            ((string= instruction "F")
             (let ((new-position (move)))
               (setf prev-pos position)
               (setf position new-position)
               (push (list prev-pos new-position) lines)))
            ;; Move forward one step but don't draw
            ((string= instruction "f")
             (setf position (move)))
            ;; Yaw left
            ((string= instruction "+")
             (setf heading (rotate-vector heading up angle))
             (setf left (rotate-vector left up angle)))
            ;; Yaw right
            ((string= instruction "-")
             (setf heading (rotate-vector heading up (- angle)))
             (setf left (rotate-vector left up (- angle))))
            ;; Pitch up
            ((string= instruction "&")
             (setf heading (rotate-vector heading left angle))
             (setf up (rotate-vector up left angle)))
            ;; Pitch down
            ((string= instruction "^")
             (setf heading (rotate-vector heading left (- angle)))
             (setf up (rotate-vector up left (- angle))))
            ;; Roll left
            ((string= instruction "{")
             (setf left (rotate-vector left heading angle))
             (setf up (rotate-vector up heading angle)))
            ;; Roll right
            ((string= instruction "}")
             (setf left (rotate-vector left heading (- angle)))
             (setf up (rotate-vector up heading (- angle))))
            ;; Push to stack
            ((string= instruction "[")
             (push (list position heading left up) stack))
            ;; Pop from stack
            ((string= instruction "]")
             (if stack (destructuring-bind (p h l u) (pop stack)
                         (setf position p
                               heading h
                               left l
                               up u))
                 (error "Tried to pop from empty stack")))
            (t
             (error "Unrecognised instruction ~A" instruction)))
          (renormalise))
      lines)))

;;;;
;;;; Making a world for my plants to grow in
;;;;


;; (defun make-terrain (array cell-size scale)
;;   "First go at perlin noise terrain"
;;   (destructuring-bind (n m) (array-dimensions array)
;;       (loop for i below n do
;;         (loop for j below m do
;;           (setf (aref array i j) (noisy:noise (* i coord-scale) (* j coord-scale)))))))


(defun cast-ray (x y dx dy distance)
  "The DDA raycasting method."
  (let
      ((norm-x (norm-vector dx))
       (norm-y (norm-vector dy)))
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
        with curr-x = x
        and curr-y  = y
        and max-x   = (initial-max x norm-x)
        and max-y   = (initial-max y norm-y)
        and delta-x = (initial-delta norm-x)
        and delta-y = (initial-delta norm-y)
        and step-x  = (if (> norm-x 0) 1 -1)
        and step-y  = (if (> norm-y 0) 1 -1)

        for step from 0 below distance
        if (< max-x max-y)
          do (incf max-x delta-x) (incf curr-x step-x)
        else
          do (incf max-y delta-y) (incf curr-y step-y)
        collect (list curr-x curr-y step)))))

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

;;;;
;;;; Graphics
;;;;

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)
(defparameter *plants* '())

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

;;;;
;;;; Main loop
;;;;

;;; Some plants for testing
(setf *plants*
      (list (make-instance 'plant :axiom "F" :rules '(("F" "F[+F]F[-F]F")) :angle 25)
            (make-instance 'plant :axiom "F" :rules '(("F" "FF-[-F+F+F]+[+F-F-F]")) :angle 20)
            (make-instance 'stochastic-plant :axiom "F" :angle 25
                                             :rules '(("F" ((0.33 "F[+F]F[-F]F") (0.33 "F[+F]F") (0.34 "F[-F]F")))))))
;;; Grow them a bit
(loop for plant in *plants*
      do (loop for i below 5 do (grow-plant plant)))


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
               ;;(draw-grid-lines renderer)

               (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
               (loop for plant in *plants*
                     for x from 120 by 400
                     do (draw-plant plant renderer `(,x ,*screen-height* 0) '(0 -1 0) '(-1 0 0) '(0 0 1) 6))
               
               ;; Test circle
               ;; (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
               ;; (loop :for (x y) :in (make-circle 300 150 100 10)
               ;;       :do (sdl2:render-draw-point renderer x y))

               ;; Some basic lightsources with 1 ray like in the Python version
               ;; along the top of the world shining straight down
               ;; (destructuring-bind (world-x world-y) (array-dimensions *world*)
               ;;   (loop :for xy in (loop for i from 0 below world-x collect (list i 0))
               ;;         :do ((destructuring-bind (x y) xy
               ;;                (let ((ray-path (cast-ray x y 0 -1 world-y)))
               ;;                  (loop :for (rx ry step) :in ray-path :do (setf (aref *world* rx ry) step)))))))

               ;;(loop :for i :from 0 :below (world-x)
               ;;      :for j :from 0 :below (world-y)


               (sdl2:render-present renderer))
        ;; Debugger continue to keep REPL alive on error
        (livesupport:update-repl-link)))))
