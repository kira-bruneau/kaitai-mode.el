(defun kaitai-mode (&optional schema)
  "A mode for editing binary files as a kaitai struct"
  (delay-mode-hooks
    (kill-all-local-variables)

    ;; TODO: Auto detect schema from source contents
    (setq kaitai--schema
          (if schema schema
            '(product
               (header . (product
                 (magic . (power byte 4)) ;; 0x80 0x37 0x12 0x40
                 (clock . (uint 4294967296))
                 (pc . (uint 4294967296))
                 (release . (uint 4294967296))
                 (crc1 . (uint 4294967296))
                 (crc2 . (uint 4294967296))
                 (_ . (power byte 8)) ;; zero
                 (name . (power byte 20))
                 (_ . (power byte 7)) ;; zero
                 (cartridge-id . (product
                   (game-id . (power byte 3))
                   (region . byte)))
                 (_ . (power byte 1)))) ;; zero
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
  (run-mode-hooks 'kaitai-mode-hook)))

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
  (cl-destructuring-bind (expand-state . size)
      (kaitai--toggle-expand-node-body
       kaitai--schema
       kaitai--expand-states
       (1- (line-number-at-pos (point))))
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
    (kaitai--insert-node-body
     kaitai--schema
     (cons kaitai--source 0)
     kaitai--expand-states
     0)
    (goto-char orig-point)))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-named-node ((name . node) stream expand-state depth)
  (kaitai--insert-expand-symbol
   (cl-destructuring-bind (expanded . expand-states) expand-state
     (if expanded 'close 'open)))
  (insert " ") ;; TODO: don't insert space if node doesn't have a name
  (kaitai--insert-node-name name)
  (insert " ") ;; TODO: don't insert space if node doesn't have a summary
  (kaitai--insert-node-summary node)
  (cl-destructuring-bind (expanded . expand-states) expand-state
    (when expanded
      (let ((depth (1+ depth)))
        (kaitai--insert-newline depth)
        (kaitai--insert-node-body node stream expand-states depth)))))

;; TODO: Use toggle/checkbox widget
(defun kaitai--insert-expand-symbol (type)
  (insert
   (pcase-exhaustive type
     ('open "+")
     ('close "-")
     (_ " "))))

(defun kaitai--insert-node-name (name)
  (let ((start (point)))
    (insert (symbol-name name))
    (put-text-property start (point) 'face 'bold)))

(defun kaitai--insert-node-summary (node)
  (pcase-exhaustive node
    (`(product . ,product))
    (`(power . ,power)
     (kaitai--insert-power-summary power))

    (`(reverse ,reverse)
     (kaitai--insert-reverse-summary reverse))

    ((pred integerp))
    ('bit)
    ('byte)

    (`(uint ,uint))
    (`(str ,str))))

(cl-defun kaitai--insert-power-summary ((base &optional exponent))
  (insert "[")
  (insert (prin1-to-string base))
  (when exponent
    (insert "; ")
    (insert (prin1-to-string exponent)))
  (insert "]"))

(defun kaitai--insert-reverse-summary (reverse)
  (insert "reversed(")
  (kaitai--insert-node-summary reverse)
  (insert ")"))

(defun kaitai--insert-node-body (node stream expand-states depth)
  (pcase-exhaustive node
    (`(product . ,product)
     (kaitai--insert-product product stream expand-states depth))
    (`(power . ,power)
     (kaitai--insert-power power stream expand-states depth))

    ((pred integerp)
     (kaitai--insert-uint node stream))
    ('bit
     (kaitai-insert-bit stream))
    ('byte
     (kaitai--insert-byte stream))

    (`(uint ,uint)
     (kaitai--insert-uint uint stream))
    (`(str ,str)
     (kaitai--insert-str str stream))

    (`(reverse ,reverse)
     (kaitai--insert-reverse reverse stream))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-product ((named-node . next-product) stream expand-states depth)
  (cl-destructuring-bind (expand-state . next-expand-states)
      (if expand-states expand-states '((nil . nil) . nil))
    (let ((stream (kaitai--insert-named-node named-node stream expand-state depth)))
      (if (not next-product) stream
        (kaitai--insert-newline depth)
        (kaitai--insert-product next-product stream next-expand-states depth)))))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-power ((base exponent) stream expand-states depth)
  (cl-destructuring-bind (expand-state . next-expand-states)
      (if expand-states expand-states '((nil . nil) . nil))
    (let ((stream (kaitai--insert-node-body base stream expand-states depth))
          (exponent (1- exponent)))
        (if (zerop exponent) stream
          (insert " ")
          (kaitai--insert-power (list base exponent) stream next-expand-states depth)))))

(defun kaitai--insert-bit (stream)
  (error "todo: requires sub-byte streaming"))

(defun kaitai--insert-byte (stream)
  (cl-destructuring-bind (slice . stream) (kaitai--read-slice stream 1)
    (insert (format "0x%X" (aref slice 0)))
    stream))

(defun kaitai--insert-uint (schema stream)
  (message "todo: support non integer schema, subbyte-streaming")
  (cl-destructuring-bind (slice . stream) (kaitai--read-slice stream (/ (logb schema) 8))
    (message "todo: convert entire slice into a number, requires bigint division and modulo")
    (insert (int-to-string (aref slice 0)))
    stream))

(defun kaitai--insert-str (schema stream)
  (message "todo: support non integer schema, subbyte-streaming")
  (cl-destructuring-bind (slice . stream) (kaitai--read-slice stream schema)
    (insert slice)
    stream))

(defun kaitai--insert-reverse (reverse stream)
  (pcase-exhaustive reverse
    (`(product . ,product)
     (kaitai--insert-product (reverse product) stream expand-states depth))
    (`(power . ,power)
     (error "todo: I am not sure how to implement this"))))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))

;;; Reading Functions

(cl-defun kaitai--read-slice ((filename . pos) len)
  (let ((end (+ pos len)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename nil pos end)
      (cons (buffer-string) (cons filename end)))))

;;; Toggle Expansion Functions
(defun kaitai--toggle-expand-node (node expand-state line)
  (cl-destructuring-bind (expanded . expand-states) expand-state
    (if (eq expanded (zerop line))
        (cons (cons nil expand-states) 1)
      (cl-destructuring-bind (expand-states . size)
          (kaitai--toggle-expand-node-body node expand-states (1- line))
        (cons (cons t expand-states) (1+ size))))))

(defun kaitai--toggle-expand-node-body (node expand-states line)
  (pcase-exhaustive node
    (`(product . ,product)
     (kaitai--toggle-expand-product product expand-states line))))

(defun kaitai--toggle-expand-product (product expand-states line)
  (cl-destructuring-bind ((name . node) . rest-product) product
    (cl-destructuring-bind (&optional (expand-state '(nil . nil)) . rest-expand-states)
        expand-states
      (cl-destructuring-bind (expand-state . size)
          (kaitai--toggle-expand-node node expand-state line)
        (if rest-product
            (cl-destructuring-bind (rest-expand-states . rest-size)
                (kaitai--toggle-expand-product rest-product rest-expand-states (1- line))
              (cons
               (cons expand-state rest-expand-states)
               (+ size rest-size)))
          (cons expand-state size))))))

;; TODO: Create kaitai--bind: a multi-value version of cl-destructuring-bind
