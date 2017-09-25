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
              (:header
               (product
                (:magic (contents 0x80 0x37 0x12 0x40))
                (:clock "= 0xF = 15")
                (:pc "= 0x80080000 = 2148007936")
                (:release "= 0x144B = 5195")
                (:crc1 "= 0x5354631C = 1398039324")
                (:crc2 "= 0x3A2DEF0 = 61005552")
                (:reserved1 (contents 0 0 0 0 0 0 0 0))
                (:name "= ZELDA MAJORA'S MASK")
                (:reserved2 (contents 0 0 0 0 0 0 0))
                (:id
                 (product
                  (:manufacturer "= N")
                  (:game-id "= ZS")
                  (:region "= USA (0x45 = 69)")))
                (:reserved3 (contents 0))))
              (:boot-code "= [3, 160, 72, 32, 141, 40, 240, 16, ...]")
              (:code "= [60, 8, 128, 10, 37, 8, 149, 0, ...]"))))

    (setq major-mode 'kaitai-mode)
    (setq mode-name "Kaitai")
    ;; (setq mode-name (format "Kaitai[%s]" schema-name))

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
    (define-key map (kbd "TAB") 'kaitai-toggle-expand)
    (define-key map (kbd "RET") 'kaitai-toggle-expand)
    map))

(defun kaitai-toggle-expand ()
  "Expand / collapse the kaitai node at point"
  (interactive)
  ;; TODO: walk through both kaitai--schema & kaitai--expand-state to find the node to expand
  ;; Then toggle the associated node in kaitai--expand-state
  (line-number-at-pos (point))
  (kaitai--refresh))

(defvar-local kaitai--schema nil
  "Schema used to render a tree from the underlying unibyte buffer")

(defvar-local kaitai--expand-states nil
  "Tree that models the current expand state of each of the expandable nodes in the schema")

;;; Rendering Functions
(defun kaitai--refresh ()
  (erase-buffer)
  (kaitai--insert-node kaitai--schema kaitai--expand-states 0))

(defun kaitai--insert-node (node expand-states depth)
  (pcase node
    (`(product . ,product)
     (kaitai--insert-product product expand-states depth))
    (`(contents . ,contents))
    (node (error "unexpected node: %s" node))))

(defun kaitai--insert-node-summary (node)
  (pcase node
    (`(product . ,product))
    (`(contents . ,contents)
     (kaitai--insert-contents contents))
    ((pred stringp) (kaitai--insert-leaf node))
    (node (error "unexpected node: %s" node))))

(defun kaitai--insert-product (product expand-states depth)
  ;; Possibly use cl-loop instead??
  (cl-do
      ((product product (cdr product))
       (expand-states expand-states
                      (if (kaitai--node-expandable-p (cl-caddr product))
                          (cdr expand-states)
                        expand-states))
       (newline nil t))
      ((not product))
    (when newline
      (kaitai--insert-newline depth))
    (let* ((assoc (car product))
           (expand-state
            (if (kaitai--node-expandable-p (cadr assoc))
                (kaitai--unwrap-or (car expand-states) '(nil nil))
              nil)))
      (kaitai--insert-assoc assoc expand-state depth))))

(defun kaitai--insert-assoc (assoc expand-state depth)
  (cl-destructuring-bind (id node) assoc
    (kaitai--insert-expand-symbol
     (when expand-state
       (if (car expand-state) 'close 'open)))
    (insert " ")
    (kaitai--insert-id id)
    (insert " ")
    (kaitai--insert-node-summary node)
    (when expand-state
      (cl-destructuring-bind (expanded expand-state) expand-state
        (when expanded
          (let ((depth (1+ depth)))
            (kaitai--insert-newline depth)
            (kaitai--insert-node node expand-state depth)))))))

;; TODO: Use toggle/checkbox widget
(defun kaitai--insert-expand-symbol (type)
  (insert
   (cond
    ((eq type 'open) "+")
    ((eq type 'close) "-")
    (t " "))))

(defun kaitai--insert-id (id)
  (insert (substring (symbol-name id) 1)))

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
