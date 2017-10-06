;; TODO: Figure out how to support both little-endian and big-endian without nreverse.
;; TODO: Add generic function or macro for bigint-split-base/int-split-base
;; Allow encoding bigint as any generic sequence (list, vector, string, ...)

(defmacro bind (bindings &rest body)
  "A drop-in replacement for `let*'.
Supports destructuring using the same syntax as `cl-destructuring-bind'."
  ;; This works, but it could probably be optimized to generate only one `let*', instead of one for each binding
  (cl-destructuring-bind ((args expr) . bindings) bindings
    (let ((body (if bindings `((bind ,bindings ,@body)) body)))
      `(cl-destructuring-bind ,args ,expr ,@body))))

(cl-defun bigint-to-string (n &optional (base 10))
  (mapconcat
   (lambda (digit) (list (if (< digit 10)
                             (+ digit ?0)
                           (+ (- digit 10) ?A))))
   (bigint-split-base--inner n base)
   ""))

(defun bigint-split-base (n base)
  "Split a bigint (big-endian) into a new bigint (big-endian) with a different base."
  (cons (bigint-split-base--inner n base) base))

;; ;; little-endian (recursive, pure)
;; (defun bigint-split-base--inner (n base)
;;   (if (bigint-zerop n) nil
;;     (bind (((quotient . remainder) (bigint-div n base)))
;;       (cons remainder (bigint-split-base--inner quotient base)))))

;; ;; big-endian (recursive, pure)
;; (defun bigint-split-base--inner (n base &optional next)
;;   (if (bigint-zerop n) next
;;     (bind (((quotient . remainder) (bigint-div n base)))
;;       (bigint-split-base--inner quotient base (cons remainder next)))))

;; ;; TODO: See if gv-ref/gv-deref makes things simpler
;; ;; little-endian (iterative, impure)
;; (defun bigint-split-base--inner (n base)
;;   (bind ((result '(t . nil))
;;          (ptr result))
;;     (while (not (bigint-zerop n))
;;       (bind (((quotient . remainder) (bigint-div n base))
;;              (new-ptr (cons remainder nil)))
;;         (setq n quotient)
;;         (setcdr ptr new-ptr)
;;         (setq ptr new-ptr)))
;;     (cdr result)))

;; big-endian (iterative, impure)
(defun bigint-split-base--inner (n base)
  (bind ((result nil))
    (while (not (bigint-zerop n))
      (bind (((quotient . remainder) (bigint-div n base)))
        (setq n quotient)
        (setq result (cons remainder result))))
    result))

(cl-defun bigint-zerop ((digits . base))
  "Return t if zero."
  (-all-p 'zerop digits))

;; bigint-add

;; bigint-mul

(cl-defun bigint-div ((&whole dividend digits . base) divisor)
  "Divides bigint (big-endian) dividend with an int divisor.
Returns bigint quotient (big-endian) and int remainder."
  (bind (((quotient . remainder) (bigint-div--inner dividend divisor 0)))
    (cons (cons quotient base) remainder)))

(cl-defun bigint-div--inner (((digit . remainder) . base) divisor carry)
  ;; TODO: Use better variable names
  (bind (((qs . rs) (int-div (+ (* carry base) digit) divisor)))
    (if remainder
        (bind (((ql . rl) (bigint-div--inner (cons remainder base) divisor rs)))
          ;; TODO: Calculate (+ (* qs base) ql) instead of (cons qs ql) if result doesn't overflow `most-positive-fixnum'
          (cons (cons qs ql) rl))
      (cons (cons qs nil) rs))))

(defun int-div (dividend divisor)
  "Divides two integers.
Returns quotient and remainder."
  (cons (/ dividend divisor) (% dividend divisor)))
