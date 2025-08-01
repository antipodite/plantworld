;;;;
;;;; Geometry functions
;;;;

(in-package :plantworld)


(defun degrees->radians (angle)
  (* angle (/ pi 180)))

(defun radians->degrees (rad)
  (/ rad (/ pi 180)))

(defun hypotenuse (x y)
  (sqrt (+ (* x x) (* y y))))

(defun vector-length (vector)
  (sqrt (reduce #'+ (loop for el in vector collect (* el el)))))

(defun norm-vector (vector)
  (let ((length (vector-length vector)))
    (loop for el in vector collect (/ el length))))

(defun cross-product (a b)
  (list (- (* (second a) (third b)) (* (third a) (second b)))
        (- (* (third a) (first b)) (* (first a) (third b)))
        (- (* (first a) (second b)) (* (second a) (first b)))))

(defun dot-product (a b)
  (reduce #'+ (loop for i in a for j in b collect (* i j))))

(defun multiply-scalar (vector scalar)
  (loop for i in vector collect (* i scalar)))

(defun add-vectors (a b)
  (loop for i in a for j in b collect (+ i j)))

(defun rotate-vector (vector axis angle)
  (add-vectors (add-vectors (multiply-scalar vector (cos angle))
                            (multiply-scalar (cross-product axis vector) (sin angle)))
               (multiply-scalar axis (* (dot-product axis vector)
                                        (- 1 (cos angle))))))

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
