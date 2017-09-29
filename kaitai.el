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
                 (magic . (contents 0x80 0x37 0x12 0x40))
                 (clock . "= 0xF = 15")
                 (pc . "= 0x80080000 = 2148007936")
                 (release . "= 0x144B = 5195")
                 (crc1 . "= 0x5354631C = 1398039324")
                 (crc2 . "= 0x3A2DEF0 = 61005552")
                 (reserved1 . (contents 0 0 0 0 0 0 0 0))
                 (name . "= ZELDA MAJORA'S MASK")
                 (reserved2 . (contents 0 0 0 0 0 0 0))
                 (id . (product
                   (manufacturer . "= N")
                   (game-id . "= ZS")
                   (region . "= USA (0x45 = 69)")))
                 (reserved3 . (contents 0))))
              (boot-code . "= [3, 160, 72, 32, 141, 40, 240, 16, ...]")
              (code . "= [60, 8, 128, 10, 37, 8, 149, 0, ...]")))
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
    (kaitai--refresh))
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

(defvar-local kaitai--schema nil
  "Schema used to render a tree from the underlying unibyte buffer")

(defvar-local kaitai--expand-states nil
  "Tree that models the current expand state of each of the expandable nodes in the schema")

;;; Rendering Functions
(defun kaitai--refresh ()
  (let ((orig-point (point)))
    (erase-buffer)
    (kaitai--insert-node-body kaitai--schema kaitai--expand-states 0)
    (goto-char orig-point)))

;; TODO: Use tail call optimization to efficently express this recursive function
(cl-defun kaitai--insert-named-node ((name . node) expand-state depth)
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
            (kaitai--insert-node-body node expand-states depth)))))))

;; TODO: Use toggle/checkbox widget
(defun kaitai--insert-expand-symbol (type)
  (insert
   (pcase type
     ('open "+")
     ('close "-")
     (_ " "))))

(defun kaitai--insert-node-name (name)
  (insert (symbol-name name)))

(defun kaitai--insert-node-summary (node)
  (pcase node
    (`(product . ,product))
    (`(contents . ,contents)
     (kaitai--insert-contents contents))
    ((pred stringp) (kaitai--insert-leaf node))
    (node (error "invalid node: %s" node))))

;; TODO: Use tail call optimization to efficently express this recursive function
(defun kaitai--insert-node-body (node expand-states depth)
  (pcase node
    (`(product . ,product)
     (kaitai--insert-product product expand-states depth))
    ;; TODO: error should be clearer that `contents' and `string' are valid nodes, but don't have a body
    (node (error "invalid node: %s" node))))

;; TODO: Use tail call optimization to efficently express this recursive function
(defun kaitai--insert-product (product expand-states depth)
  (cl-destructuring-bind ((&whole named-node name . node) &rest next-product) product
    (cl-destructuring-bind (expand-state &rest next-expand-states)
        (if (kaitai--node-expandable-p node)
            (if expand-states expand-states '((nil . nil) . nil))
          (cons nil expand-states))
      (kaitai--insert-named-node named-node expand-state depth)
      (when next-product
        (kaitai--insert-newline depth)
        (kaitai--insert-product next-product next-expand-states depth)))))

(defun kaitai--insert-contents (contents)
  (insert "= [")
  ;; TODO: Possibly use recursion (maybe with y-combinator) to get rid of the code duplication
  (cl-destructuring-bind (content &rest contents) contents
    (insert (prin1-to-string content))
    (dolist (content contents)
      (insert ", ")
      (insert (prin1-to-string content))))
  (insert "]"))

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
      (cl-destructuring-bind (sub-expand-states . sub-size)
          (kaitai--toggle-expand-node-body node expand-states (1- line))
        (cons (cons t sub-expand-states) (1+ sub-size))))))

(defun kaitai--toggle-expand-node-body (node expand-states line)
  (pcase node
    (`(product . ,product)
     (kaitai--toggle-expand-product product expand-states line))
    ;; TODO: error should be clearer that `contents' and `string' are valid nodes, but don't have a body
    (node (error "invalid node: %s" node))))

(defun kaitai--toggle-expand-product (product expand-states line)
  (if product ;; NOTE: It may be too permissive to allow a product to be empty
      (cl-destructuring-bind ((name . node) &rest next-product) product
        ;; TODO: Try to get rid of the if checks through refactoring
        (let (expandable (kaitai--node-expandable-p node))
          (cl-destructuring-bind (expand-state . size)
              (if expandable
                  (cl-destructuring-bind (&optional (expand-state '(nil . nil)) &rest next-expand-states) expand-states
                    (kaitai--toggle-expand-node node expand-state line))
                '(nil . 1))
            (cl-destructuring-bind (next-expand-state . next-size)
                (kaitai--toggle-expand-product next-product (if expandable (cdr expand-states) expand-states) (1- line))
              (cons
               (if expandable (cons expand-state next-expand-state) next-expand-state)
               (+ size next-size))))))
    '(nil . 0)))

;;; Utility Functions
(defun kaitai--node-expandable-p (node)
  "Used to compress the expand-state tree to exclude non-expandable nodes"
  (pcase node
    (`(product . ,product) t)
    (_ nil)))

;; I don't think I actually need this
(defun kaitai--unwrap-or (value default)
  "Returns value if if non nil, otherwise returns default."
  (if value value default))

;; TODO: Create kaitai--bind: a multi-value version of cl-destructuring-bind
