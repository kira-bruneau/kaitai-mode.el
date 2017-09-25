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
    (define-key map (kbd "TAB") 'kaitai-toggle-expand-node)
    (define-key map (kbd "RET") 'kaitai-toggle-expand-node)
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

(cl-defun kaitai--insert-named-node ((name . node) expand-state depth)
  "Insert a named expandable node"
  ;; TODO: If any of the insertions are empty, don't insert newline/space between them
  (kaitai--insert-expand-symbol
   (when expand-state
     (cl-destructuring-bind (expanded . expand-states) expand-state
       (if expanded 'close 'open))))
  (insert " ")
  (kaitai--insert-node-name name)
  (insert " ")
  (kaitai--insert-node-summary node)
  (when expand-state
    (cl-destructuring-bind (expanded . expand-states) expand-state
      (when expanded
        (let ((depth (1+ depth)))
          (kaitai--insert-newline depth)
          (kaitai--insert-node-body node expand-states depth))))))

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
    (node (error "unexpected node: %s" node))))

(defun kaitai--insert-node-body (node expand-states depth)
  (pcase node
    (`(product . ,product)
     (kaitai--insert-product product expand-states depth))
    (`(contents . ,contents))
    ((pred stringp))
    (node (error "unexpected node: %s" node))))

(defun kaitai--insert-product (product expand-states depth)
  ;; Possibly use cl-loop instead??
  (cl-do
      ((product product (cdr product))
       (expand-states expand-states
                      (if (kaitai--node-expandable-p (cadr product))
                          (cdr expand-states)
                        expand-states))
       (newline nil t))
      ((not product))
    (when newline
      (kaitai--insert-newline depth))
    (let* ((named-node (car product))
           (expand-state
            (if (kaitai--node-expandable-p (cdr named-node))
                (kaitai--unwrap-or (car expand-states) '(nil . nil))
              nil)))
      (kaitai--insert-named-node named-node expand-state depth))))

(defun kaitai--insert-contents (contents)
  (insert "= [")
  ;; Possibly use cl-loop instead??
  (let ((content (car contents)))
    (when content
      (insert (prin1-to-string content))
      (dolist (content (cdr contents))
        (insert ", ")
        (insert (prin1-to-string content)))))
  (insert "]"))

(defun kaitai--insert-leaf (leaf)
  (insert leaf))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))

;;; Toggle Expansion Functions
(cl-defun kaitai--toggle-expand-node (node expand-state line)
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
    (node (error "unexpected node: %s" node))))

(defun kaitai--toggle-expand-product (product expand-states line)
  (if product ;; NOTE: It may be too permissive to allow a product to be empty
      (cl-destructuring-bind ((name . node) &rest next-product) product
        (if (kaitai--node-expandable-p node)
            ;; TODO: Get rid of optional when things are stable
            (cl-destructuring-bind (&optional (expand-state '(nil . nil)) &rest next-expand-states) expand-states
                (cl-destructuring-bind (expand-state . size)
                    (kaitai--toggle-expand-node node expand-state line)
                  (cl-destructuring-bind (next-expand-state . next-size)
                      (kaitai--toggle-expand-product next-product next-expand-states (1- line))
                    (cons
                     (cons expand-state next-expand-state)
                     (+ size next-size)))))
          ;; TODO: Figure out a way to refactor this to remove code duplication
          (cl-destructuring-bind (next-expand-state . next-size)
              (kaitai--toggle-expand-product next-product expand-states (1- line))
            (cons next-expand-state (1+ next-size)))))
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
