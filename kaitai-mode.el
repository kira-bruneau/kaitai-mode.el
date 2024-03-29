;;; kaitai-mode.el --- Edit binary files in emacs using Kaitai Struct .ksy files

;; Copyright (C) 2017-2018 Kira Bruneau

;; Author: Kira Bruneau <kira.bruneau@gmail.com>
;; Keywords: kaitai struct ksy binary

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
  (require 'cl-macs)
  (require 'bind))

(require 'kaitai-bigint)

;;;###autoload
(defun kaitai-mode (&optional schema)
  "A mode for editing binary files as a kaitai struct.
If SCHEMA is nil, attempt to autodetect the appropriate schema"
  (interactive)
  (delay-mode-hooks
    (kill-all-local-variables)

    ;; TODO: Auto detect schema from source contents
    (setq kaitai--schema
          (if schema schema
            '(product
               (header . (product
                 (magic . (eq (power byte 4) (#x80 #x37 #x12 #x40)))
                 (clock . (uint (reverse (power byte 4))))
                 (pc . (uint (reverse (power byte 4))))
                 (release . (uint (reverse (power byte 4))))
                 (crc1 . (uint (reverse (power byte 4))))
                 (crc2 . (uint (reverse (power byte 4))))
                 (_ . (eq (power byte 8) 0))
                 (name . (str (power byte 20)))
                 (_ . (eq (power byte 7) 0))
                 (cartridge-code . (str (product
                   (manufacturer-code . (str byte))
                   (game-code . (str (power byte 2)))
                   (region-code . (str byte)))))
                 (_ . (eq (power byte 1) 0))))
              (boot-code . (power byte 4032))
              (code . (power byte))))
          major-mode 'kaitai-mode
          mode-name (format "Kaitai[%s]" "z64")
          buffer-read-only t)

    (use-local-map kaitai-mode-map)
    (kaitai--refresh)

    ;; relavent variables set in find-file:
    ;; (setq buffer-read-only read-only)))
    ;; (setq buffer-file-read-only read-only)
    ;; buffer-file-truename
    ;; buffer-file-number
    ;; buffer-file-name
    ;; default-directory

    ;; TODO: Hooks
    ;; (setq 'revert-buffer-function kaitai--revert-buffer)
    ;; (add-hook 'after-revert-hook 'image-after-revert-hook nil t)
    ;; (add-hook 'change-major-mode-hook 'convert-back-to-original-buffer)
    ;; (add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
    ;; (add-hook 'write-contents-functions kaitai--save-buffer nil t))
    )
  (run-mode-hooks 'kaitai-mode-hook))

(defvar kaitai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kaitai-toggle-expand-node)

    ;; emacs-like navigation keybindings
    ;; (define-key map (kbd "n") 'next-line)
    ;; (define-key map (kbd "p") 'previous-line)
    ;; (define-key map (kbd "e") 'forward-sexp)
    ;; (define-key map (kbd "a") 'backward-sexp)

    ;; vim-like: (querty: hjkl, colemak: hnei) navigation keybindings
    ;; (define-key map (kbd "n") 'next-line)
    ;; (define-key map (kbd "e") 'previous-line)
    ;; (define-key map (kbd "i") 'forward-sexp)
    ;; (define-key map (kbd "h") 'backward-sexp)
    ;; (define-key map (kbd "o") 'kaitai-toggle-expand-node)

    ;; (query: ijkl, colemak: unei) navigation keybindings
    (define-key map (kbd "e") 'next-line)
    (define-key map (kbd "u") 'previous-line)
    (define-key map (kbd "i") 'forward-sexp)
    (define-key map (kbd "n") 'backward-sexp)
    (define-key map (kbd "o") 'kaitai-toggle-expand-node)

    ;; search
    (define-key map (kbd "s") 'isearch-forward)
    (define-key map (kbd "r") 'isearch-backward)

    map))

(defun kaitai-toggle-expand-node ()
  "Expand / collapse the kaitai node at point."
  (interactive)
  (bind (((expand-state . size)
          (kaitai--toggle-expand-node-body
           kaitai--schema
           kaitai--expand-states
           (1- (line-number-at-pos (point))))))
    (setq kaitai--expand-states expand-state))
  (kaitai--refresh))

(defvar-local kaitai--source "/home/kira/Downloads/Super Mario 64 (U) [!].z64"
  "Source file that contains the raw data")

(defvar-local kaitai--schema nil
  "Schema used to render a tree from the underlying unibyte buffer")

(defvar-local kaitai--expand-states nil
  "Tree that models the current expand state of each of the nodes in the schema")

;;; Rendering Functions
(defun kaitai--refresh ()
  (let ((orig-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (kaitai--insert-node
     kaitai--schema
     (cons kaitai--source 0)
     kaitai--expand-states
     0)
    (goto-char orig-point)))

(defun kaitai--insert-node (schema stream expand-states depth)
  (pcase-exhaustive schema
    (`(product . ,schema)
     (kaitai--insert-product schema stream expand-states depth))
    (`(power . ,schema)
     ;; TODO: Allow 0 exponent in power
     (kaitai--insert-power schema stream expand-states depth))

    ((pred integerp)
     (kaitai--insert-uint schema stream))
    ('bool
     (kaitai-insert-bool stream))
    ('byte
     (kaitai--insert-byte stream))

    (`(uint ,schema)
     (kaitai--insert-uint schema stream))
    (`(str ,schema)
     (kaitai--insert-str schema stream))

    (`(reverse ,schema)
     (kaitai--insert-reverse schema stream))
    (`(eq . ,schema)
     (kaitai--insert-eq schema stream expand-states depth))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-product ((field . rest-product) stream expand-states depth)
  (bind (((expand-state . rest-expand-states)
          (if expand-states expand-states '((nil . nil) . nil)))
         (stream (kaitai--insert-field field stream expand-state depth)))
    (if (not rest-product) stream
      (kaitai--insert-newline depth)
      (kaitai--insert-product rest-product stream rest-expand-states depth))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-field ((name . schema) stream expand-state depth)
  (kaitai--insert-expand-symbol
   (bind (((expanded . expand-states) expand-state))
     (if expanded 'close 'open)))
  (insert " ") ;; TODO: don't insert space if node doesn't have a name
  (kaitai--insert-field-name name)
  (insert " ") ;; TODO: don't insert space if node doesn't have a summary
  (kaitai--insert-field-summary schema)
  (bind (((expanded . expand-states) expand-state))
    (if (not expanded) stream
      (let ((depth (1+ depth)))
        (kaitai--insert-newline depth)
        (kaitai--insert-node schema stream expand-states depth)))))

(defun kaitai--insert-expand-symbol (type)
  (insert
   (pcase-exhaustive type
     ('open "+")
     ('close "-")
     (_ " "))))

(defun kaitai--insert-field-name (name)
  (let ((start (point)))
    (insert (symbol-name name))
    (put-text-property start (point) 'face 'bold)))

(defun kaitai--insert-field-summary (schema)
  (let ((start (point)))
    ;; TODO: don't show product body in summary
    (insert (prin1-to-string schema))
    (put-text-property start (point) 'face 'shadow)))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-power ((base exponent) stream expand-states depth)
  (bind (((expand-state . rest-expand-states)
          (if expand-states expand-states '((nil . nil) . nil)))
         (stream (kaitai--insert-node base stream expand-states depth))
         (exponent (1- exponent)))
    (if (zerop exponent) stream
      (insert " ")
      (kaitai--insert-power (list base exponent) stream rest-expand-states depth))))

(defun kaitai--insert-bool (stream)
  (error "todo: requires sub-byte streaming"))

(defun kaitai--insert-byte (stream)
  (bind (((slice . stream) (kaitai--stream-read stream (kaitai--sizeof-byte))))
    (insert (format "0x%X" (aref slice 0)))
    stream))

(defun kaitai--insert-uint (schema stream)
  (bind (((slice . stream) (kaitai--stream-read stream (car (kaitai--sizeof-uint schema stream))))
         (uint (cons (string-to-list slice) 256))
         (hex-view (concat "0x" (kaitai-bigint-to-string uint 16)))
         (decimal-view (concat (kaitai-bigint-to-string uint 10))))
    (insert hex-view " = " decimal-view)
    stream))

(defun kaitai--insert-str (schema stream)
  (bind (((slice . stream) (kaitai--stream-read stream (car (kaitai--sizeof-str schema stream)))))
    (insert slice)
    stream))

(defun kaitai--insert-reverse (schema stream)
  (pcase-exhaustive schema
    (`(power . ,power)
     (error "todo: I am not sure how to implement this"))))

(cl-defun kaitai--insert-eq ((schema data) stream expand-states depth)
  (message "todo: enforce that data matches values in stream")
  (kaitai--insert-node schema stream expand-states depth))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))

;;; Sizeof Functions
(defun kaitai--sizeof-node (schema stream)
  (pcase-exhaustive schema
    (`(product . ,schema)
     (kaitai--sizeof-product schema stream))
    (`(power . ,schema)
     (kaitai--sizeof-power schema stream))

    ((pred integerp)
     (let ((size (kaitai--sizeof-raw schema)))
       (cons size (kaitai--stream-skip stream size))))
    ('bool
     (let ((size (kaitai--sizeof-bool)))
       (cons size (kaitai--stream-skip stream size))))
    ('byte
     (let ((size (kaitai--sizeof-byte)))
       (cons size (kaitai--stream-skip stream size))))

    (`(uint ,schema)
     (kaitai--sizeof-uint schema stream))
    (`(str ,schema)
     (kaitai--sizeof-str schema stream))

    (`(reverse ,schema)
     (kaitai--sizeof-reverse schema stream))
    (`(eq . ,schema)
     (kaitai--sizeof-reverse schema stream))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--sizeof-product (((name . schema) . rest-product) stream)
  (bind (((size . stream) (kaitai--sizeof-node schema stream)))
    (if (not rest-product) (cons size stream)
      (bind (((rest-size . stream) (kaitai--sizeof-product rest-product stream)))
        (cons (+ size rest-size) stream)))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--sizeof-power ((base exponent) stream)
  (if (zerop exponent) (cons 0 stream)
    (bind (((size . stream) (kaitai--sizeof-node base stream))
           ((rest-size . stream) (kaitai--sizeof-power (list base (1- exponent)) stream)))
      (cons (+ size rest-size) stream))))

(defun kaitai--sizeof-raw (size)
  (/ (logb size) 8))

(defun kaitai--sizeof-bool ()
  (error "todo: requires sub-byte streaming"))

(defun kaitai--sizeof-byte ()
  1)

(defun kaitai--sizeof-uint (schema stream)
  (kaitai--sizeof-node schema stream))

(defun kaitai--sizeof-str (schema stream)
  (kaitai--sizeof-node schema stream))

(defun kaitai--sizeof-reverse (schema stream)
  (kaitai--sizeof-node schema stream))

(cl-defun kaitai--sizeof-eq ((schema data) stream)
  (kaitai--sizeof-node schema stream))

;;; Toggle Expansion Functions
(defun kaitai--toggle-expand-node (node expand-state line)
  (bind (((expanded . expand-states) expand-state))
    (if (eq expanded (zerop line))
        (cons (cons nil expand-states) 1)
      (bind (((expand-states . size)
              (kaitai--toggle-expand-node-body node expand-states (1- line))))
        (cons (cons t expand-states) (1+ size))))))

(defun kaitai--toggle-expand-node-body (node expand-states line)
  (pcase-exhaustive node
    (`(product . ,product)
     (kaitai--toggle-expand-product product expand-states line))))

(defun kaitai--toggle-expand-product (product expand-states line)
  (bind ((((name . node) . rest-product) product)
         ((&optional (expand-state '(nil . nil)) . rest-expand-states) expand-states)
         ((expand-state . size) (kaitai--toggle-expand-node node expand-state line)))
    (if rest-product
        (bind (((rest-expand-states . rest-size)
                (kaitai--toggle-expand-product rest-product rest-expand-states (1- line))))
          (cons
           (cons expand-state rest-expand-states)
           (+ size rest-size)))
      (cons expand-state size))))

;;; Reading Functions
(cl-defun kaitai--stream-read ((filename . pos) size)
  (let ((end (+ pos size)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename nil pos end)
      (cons (buffer-string) (cons filename end)))))

(cl-defun kaitai--stream-skip ((filename . pos) size)
  (cons filename (+ pos size)))

(provide 'kaitai-mode)

;;; kaitai-mode.el ends here
