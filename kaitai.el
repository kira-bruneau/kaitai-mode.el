(defun kaitai-mode (&optional schema)
  "A mode for editing binary files as a kaitai struct"
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
  "Expand / collapse the kaitai node at point"
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
         (hex-view (concat "0x" (bigint-to-string uint 16)))
         (decimal-view (concat (bigint-to-string uint 10))))
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

;;; Bigint Library
(cl-defun bigint-to-string (n &optional (base 10))
  (mapconcat
   (lambda (digit) (list (if (< digit 10)
                             (+ digit ?0)
                           (+ (- digit 10) ?A))))
   (bigint-to-base--inner n base)
   ""))

(defun bigint-to-base (n base)
  "Convert a bigint (big-endian) into a new bigint (big-endian) with a different base."
  (cons (bigint-to-base--inner n base) base))

;; ;; little-endian (recursive, pure)
;; (defun bigint-to-base--inner (n base)
;;   (if (bigint-zerop n) nil
;;     (bind (((quotient . remainder) (bigint-div n base)))
;;       (cons remainder (bigint-to-base--inner quotient base)))))

;; ;; big-endian (recursive, pure)
;; (defun bigint-to-base--inner (n base &optional next)
;;   (if (bigint-zerop n) next
;;     (bind (((quotient . remainder) (bigint-div n base)))
;;       (bigint-to-base--inner quotient base (cons remainder next)))))

;; ;; TODO: See if gv-ref/gv-deref makes things simpler
;; ;; little-endian (iterative, impure)
;; (defun bigint-to-base--inner (n base)
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
(defun bigint-to-base--inner (n base)
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
  (bind (((qs . rs) (smallint-div (+ (* carry base) digit) divisor)))
    (if remainder
        (bind (((ql . rl) (bigint-div--inner (cons remainder base) divisor rs)))
          ;; TODO: Calculate (+ (* qs base) ql) instead of (cons qs ql) if result doesn't overflow `most-positive-fixnum'
          (cons (cons qs ql) rl))
      (cons (cons qs nil) rs))))

(defun smallint-div (dividend divisor)
  "Divides two integers.
Returns quotient and remainder."
  (cons (/ dividend divisor) (% dividend divisor)))

;;; Utility Functions
(defmacro bind (bindings &rest body)
  "A drop-in replacement for `let*' similar to metabang-bind for common lisp.
Supports destructuring using the same syntax as `cl-destructuring-bind'."
  ;; This works, but it could probably be optimized to generate only one `let*', instead of one for each binding
  (declare (indent defun))
  (cl-destructuring-bind ((args expr) . bindings) bindings
    (let ((body (if bindings `((bind ,bindings ,@body)) body)))
      `(cl-destructuring-bind ,args ,expr ,@body))))
