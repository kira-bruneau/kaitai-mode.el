;; heirarchal structure modes: outline-mode, org-mode, neotree
;; non text modes: hexl-mode, archive-mode, image-mode
;; TODO: Integrate with vlf package to support large files

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
                 (clock . (power byte 4))
                 (pc . (power byte 4))
                 (release . (power byte 4))
                 (crc1 . (power byte 4))
                 (crc2 . (power byte 4))
                 (_ . (power byte 8)) ;; zero
                 (name . (power byte 20))
                 (_ . (power byte 7)) ;; zero
                 (cartridge-id . (product
                   (game-id . (power byte 3))
                   (region . (power byte 1))))
                 (_ . (power byte 1)))) ;; zero
              (boot-code . (power byte 4032))
              (code . (power byte))))
          major-mode 'kaitai-mode
          mode-name (format "Kaitai[%s]" "z64"))

    (use-local-map kaitai-mode-map)

    ;; TODO: Hooks
    ;; (setq 'revert-buffer-function kaitai--revert-buffer)
    ;; (add-hook 'after-revert-hook 'image-after-revert-hook nil t)
    ;; (add-hook 'change-major-mode-hook 'convert-back-to-original-buffer)
    ;; (add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
    ;; (add-hook 'write-contents-functions kaitai--save-buffer nil t)

    ;; (rename-buffer (concat " " (buffer-name)))
    (kaitai--refresh)
    (setq buffer-read-only t))
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
  "Tree that models the current expand state of each of the expandable nodes in the schema")

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
  "Insert a named expandable node, expand-state is ignored if the node is not expandable"
  (let ((expandable (kaitai--node-expandable-p node)))
    (kaitai--insert-expand-symbol
     (when expandable
       (cl-destructuring-bind (expanded . expand-states) expand-state
         (if expanded 'close 'open))))
    (insert " ") ;; TODO: don't insert space if node doesn't have a name
    (kaitai--insert-node-name name)
    (insert " ") ;; TODO: don't insert space if node doesn't have a summary
    (kaitai--insert-node-summary node)
    (when expandable
      (cl-destructuring-bind (expanded . expand-states) expand-state
        (when expanded
          (let ((depth (1+ depth)))
            (kaitai--insert-newline depth)
            (kaitai--insert-node-body node stream expand-states depth)))))))

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
    ((pred stringp) (kaitai--insert-leaf node))))

(cl-defun kaitai--insert-power-summary ((base &optional exponent))
  (insert "[")
  (insert (prin1-to-string base))
  (when exponent
    (insert "; ")
    (insert (prin1-to-string exponent)))
  (insert "]"))

;; TODO: Use tail call optimization to efficently express this recursive function
(defun kaitai--insert-node-body (node stream expand-states depth)
  (pcase-exhaustive node
    (`(product . ,product)
     (kaitai--insert-product product stream expand-states depth))
    (`(power . ,power)
     (kaitai--insert-power power stream))))

;; TODO: Use tail call optimization to efficently express this recursive function
(defun kaitai--insert-product (product stream expand-states depth)
  (cl-destructuring-bind ((&whole named-node name . node) . next-product) product
    (cl-destructuring-bind (expand-state . next-expand-states)
        (if (kaitai--node-expandable-p node)
            (if expand-states expand-states '((nil . nil) . nil))
          (cons nil expand-states))
      (let ((stream (kaitai--insert-named-node named-node stream expand-state depth)))
        (when next-product
          (kaitai--insert-newline depth)
          (kaitai--insert-product next-product stream next-expand-states depth))))))

(cl-defun kaitai--insert-power ((base &optional size) (filename . pos))
  (let ((end (pcase-exhaustive base
               ('byte (if size (+ pos size))))))
    (let* ((slice (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally filename nil pos end)
                    (buffer-string)))
           (hex-view (concat "[" (mapconcat (lambda (byte) (format "0x%X" byte)) slice ", ") "]"))
           (decimal-view (concat "[" (mapconcat (lambda (byte) (number-to-string byte)) slice ", ") "]")))
      (insert slice " = " hex-view " = " decimal-view))
    (cons filename end)))

(defun kaitai--insert-leaf (leaf)
  (insert leaf))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))

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
    (if (kaitai--node-expandable-p node)
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
                (cons expand-state size))))
      (if rest-product
          (cl-destructuring-bind (rest-expand-states . rest-size)
              (kaitai--toggle-expand-product rest-product expand-states (1- line))
            (cons rest-expand-states (1+ rest-size)))
        '(nil . 1)))))

;;; Utility Functions
(defun kaitai--node-expandable-p (node)
  "Used to compress the expand-state tree to exclude non-expandable nodes"
  (pcase-exhaustive node
    (`(product . ,product) t)
    (`(power . ,power) t)
    (_ nil)))

;; TODO: Create kaitai--bind: a multi-value version of cl-destructuring-bind
