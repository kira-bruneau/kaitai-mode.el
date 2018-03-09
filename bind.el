;;; bind.el --- A drop-in replacement for `let*' similar to metabang-bind for common lisp

;; Copyright (C) 2017-2018 Kira Bruneau

;; Author: Kira Bruneau <kira.bruneau@gmail.com>
;; Keywords: bind metabang-bind cl-destructuring-bind let*

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
  (require 'cl-macs))

(defmacro bind (bindings &rest body)
  "A drop-in replacement for `let*' similar to metabang-bind for Common Lisp.
Supports destructuring using the same syntax as `cl-destructuring-bind'."
  ;; This works, but it could probably be optimized to generate only one `let*', instead of one for each binding
  (declare (indent defun))
  (cl-destructuring-bind ((args expr) . bindings) bindings
    (let ((body (if bindings `((bind ,bindings ,@body)) body)))
      `(cl-destructuring-bind ,args ,expr ,@body))))

(provide 'bind)

;;; bind.el ends here
