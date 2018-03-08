;;; kaitai-bigint.el --- A big integer library designed for kaitai-mode

;; Copyright (C) 2017-2018 Kira Bruneau

;; Author: Kira Bruneau <kira.bruneau@gmail.com>
;; Keywords: bigint kaitai-mode kaitai

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'bind))

(require 'cl)

(cl-defun kaitai-bigint-to-string (n &optional (base 10))
  "Convert bigint (big-endian) N to a string."
  (mapconcat
   (lambda (digit) (list (if (< digit 10)
                             (+ digit ?0)
                           (+ (- digit 10) ?A))))
   (kaitai-bigint-to-base--inner n base)
   ""))

(defun kaitai-bigint-to-base (n base)
  "Convert bigint (big-endian) N to base BASE."
  (cons (kaitai-bigint-to-base--inner n base) base))

;; ;; little-endian (recursive)
;; (defun kaitai-bigint-to-base--inner (n base)
;;   (if (kaitai-bigint-zerop n) nil
;;     (bind (((quotient . remainder) (kaitai-bigint-div n base)))
;;       (cons remainder (kaitai-bigint-to-base--inner quotient base)))))

;; ;; big-endian (recursive)
;; (defun kaitai-bigint-to-base--inner (n base &optional next)
;;   (if (kaitai-bigint-zerop n) next
;;     (bind (((quotient . remainder) (kaitai-bigint-div n base)))
;;       (kaitai-bigint-to-base--inner quotient base (cons remainder next)))))

;; ;; TODO: See if gv-ref/gv-deref makes things simpler
;; ;; little-endian (iterative)
;; (defun kaitai-bigint-to-base--inner (n base)
;;   (bind ((result '(t . nil))
;;          (ptr result))
;;     (while (not (kaitai-bigint-zerop n))
;;       (bind (((quotient . remainder) (kaitai-bigint-div n base))
;;              (new-ptr (cons remainder nil)))
;;         (setq n quotient)
;;         (setcdr ptr new-ptr)
;;         (setq ptr new-ptr)))
;;     (cdr result)))

;; big-endian (iterative)
(defun kaitai-bigint-to-base--inner (n base)
  (bind ((result nil))
    (while (not (kaitai-bigint-zerop n))
      (bind (((quotient . remainder) (kaitai-bigint-div n base)))
        (setq n quotient)
        (setq result (cons remainder result))))
    result))

(cl-defun kaitai-bigint-zerop ((digits . base))
  "Return t if zero."
  (cl-every 'zerop digits))

;; kaitai-bigint-add

;; kaitai-bigint-mul

(cl-defun kaitai-bigint-div ((&whole dividend digits . base) divisor)
  "Divide bigint (big-endian) DIVIDEND with an integer DIVISOR.
Return (QUOTIENT . REMAINDER)."
  (bind (((quotient . remainder) (kaitai-bigint-div--inner dividend divisor 0)))
    (cons (cons quotient base) remainder)))

(cl-defun kaitai-bigint-div--inner (((digit . remainder) . base) divisor carry)
  ;; TODO: Use better variable names
  (bind (((qs . rs) (kaitai-smallint-div (+ (* carry base) digit) divisor)))
    (if remainder
        (bind (((ql . rl) (kaitai-bigint-div--inner (cons remainder base) divisor rs)))
          ;; TODO: Calculate (+ (* qs base) ql) instead of (cons qs ql) if result doesn't overflow `most-positive-fixnum'
          (cons (cons qs ql) rl))
      (cons (cons qs nil) rs))))

(defun kaitai-smallint-div (dividend divisor)
  "Divide DIVIDEND by DIVISOR.
Return (QUOTIENT . REMAINDER)."
  (cons (/ dividend divisor) (% dividend divisor)))

(provide 'kaitai-bigint)

;;; kaitai-bigint.el ends here
