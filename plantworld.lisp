(defpackage #:plantworld
  (:use :common-lisp)
  (:export :main)
  (:import-from :alexandria curry))

(in-package :plantworld)

;;;;
;;;; Lindenmeyer system implementation.
;;;; "An 0L-System according to Prusinkiewicz and Lindenmayer (1996) as well as Hanan (1992)
;;;; is defined as an ordered triplet G = 〈V, ω, P 〉, consisting of
;;;; 1. an alphabet V of symbols corresponding to the different types of plant modules,
;;;; 2. an initial nonempty string of symbols ω ∈ V + called the axiom,
;;;; 3. as well as a finite set of productions P ⊂ V × V ∗."
;;;; - Nikole Leopold's thesis
;;;; https://www.cg.tuwien.ac.at/research/publications/2017/LEOPOLD-2017-ALG/LEOPOLD-2017-ALG-thesis.pdf
;;;;

(defparameter *max-state-size* 100000)

(defun make-system (axiom rules angle)
   `((STRING . ,axiom)
     (RULES  . ,rules)
     (ANGLE  . ,(degrees->radians angle))))

(defun get-state (system)
  (cdr (assoc 'STRING system)))

(defun set-state (system state)
  (setf (cdr (assoc 'STRING system)) state))

(defun get-rules (system)
  (cdr (assoc 'RULES system)))

(defun get-angle (system)
  (cdr (assoc 'ANGLE system)))

(defun apply-rules (rules symbol)
  (or (cdr (assoc symbol rules :test #'string=))
      symbol))

(defun iter-system (system)
  "Apply a single iteration of given RULES to STRING"
  (let ((string (get-state system))
        (rules  (get-rules system)))
    (as-string
     (mapcar (curry #'apply-rules rules) (as-list string)))))

(defun grow-system (system n)
  (loop for i below n do
    (let ((state (iter-system system)))
      (if (>= (length state) *max-state-size*)
          (error "State size ~A too large" state)
          (set-state system state))
      system)))

(defparameter *0L-example*
  (make-system "F+F+F+F" '(("F" . "F+F-F-FF+F+F-F")) 90))

;;
;;; Interpreter
;;

(defun interpret-string (string angle initial-pos initial-heading
                         initial-left initial-up &key (step-size 1))
  (let
      ((position initial-pos) ; (x, y, z)
       (heading  initial-heading)
       (up       initial-up)
       (left     initial-left)
       (stack    '())
       (result   '()))
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
        for instruction across string
        for step-number from 1 do
          (cond
            ;; Move forward one step
            ((string= instruction "F")
             (let ((new-position (move)))
               (setf position (move))
               (push (list step-number instruction new-position heading up left) result)))
            ;; Move forward one step but don't draw
            ((string= instruction "f")
             (let ((new-position (move)))
               (setf position (move))
               (push (list step-number instruction new-position heading up left) result)))
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
      result)))

(defun draw-string (steps)
  "Return line segments as pairs of coordinates from the output
of INTERPRET-STRING"
  (let ((filtered (remove-if-not (lambda (s) (not (char= #\f (second s))))
                                 steps)))
    (loop for (a b) in (chunk 2 filtered) collect (list (third a) (third b)))))


;;;;
;;;; Grid geometry functions
;;;;

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
(defparameter *world* (make-world 10 10))
(defparameter *objects* (list))
(defparameter *lightsources* (list))

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

(defvar test-plant)
(defvar test-lines)
(setq test-plant (make-system "F" '(("F" . "F[+F]F[-F]F")) 25.7))
(grow-system test-plant 5)
(setq test-lines (draw-string (interpret-string (get-state test-plant)
                                                (get-angle test-plant)
                                                '(320 480 0)
                                                '(0 -1 0)
                                                '(-1 0 0 )
                                                '(0 0 1)
                                                :step-size 1)))
                                       
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
              (loop for xyz in test-lines do
                (destructuring-bind (x1 y1 z1 x2 y2 z2) (mapcar #'truncate (alexandria:flatten xyz))
                  (declare (ignore z1 z2))
                  (sdl2:render-draw-line renderer x1 y1 x2 y2)))

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
 
;;;
;;;Compilation instructions for SBCL
;;;

;;(sb-ext:save-lisp-and-die "app"
;;  :toplevel #'main
;;  :executable t)
