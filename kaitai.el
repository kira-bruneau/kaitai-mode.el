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
                  (:game-id "= NZS")
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

    (rename-buffer (concat " " (buffer-name)))
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

(defvar-local kaitai--expand-state nil
  "Tree that models the current expand state of each of the nodes in the schema")

(defun kaitai--refresh ()
  (erase-buffer)
  (kaitai--insert-node kaitai--schema kaitai--expand-state 0))

(defun kaitai--insert-node (node expand-state depth)
  (pcase node
    (`(product . ,product)
     (kaitai--insert-product product expand-state depth))
    (`(contents . ,contents))
    (node (error "unexpected node: %s" node))))

(defun kaitai--insert-node-summary (node)
  (pcase node
    (`(product . ,product))
    (`(contents . ,contents)
     (kaitai--insert-contents contents))
    (node (kaitai--insert-leaf node))))

(defun kaitai--node-expandable-p (node)
  (pcase node
    (`(product . ,product) t)
    (`(contents . ,contents) nil)
    (node nil)))

(defun kaitai--insert-product (product expand-state depth)
  (kaitai--insert-assoc (car product) (car expand-state) depth)
  (setq product (cdr product))
  (setq expand-state (cdr expand-state))
  (while product
    (kaitai--insert-newline depth)
    (kaitai--insert-assoc (car product) (car expand-state) depth)
    (setq product (cdr product))
    (setq expand-state (cdr expand-state))))

(defun kaitai--insert-assoc (assoc expand-state depth)
  (cl-destructuring-bind (id node) assoc
    (cl-destructuring-bind (expanded child-expand-state) expand-state
      (kaitai--insert-expand-symbol
       (when (kaitai--node-expandable-p node)
         (if expanded 'close 'open)))
      (insert-char ?\s)
      (kaitai--insert-id id)
      (insert-char ?\s)
      (kaitai--insert-node-summary node)
      (when (and (kaitai--node-expandable-p node) expanded)
        (let ((depth (1+ depth)))
          (kaitai--insert-newline depth)
          (kaitai--insert-node node node-expand-state depth))))))

(defun kaitai--insert-expand-symbol (type)
  (insert-char
   (cond
    ((eq type 'open) ?+)
    ((eq type 'close) ?-)
    (t ?\s))))

(defun kaitai--insert-id (id)
  (insert (substring (symbol-name id) 1)))

(defun kaitai--insert-contents (contents)
  (insert-char ?=)
  (insert-char ?\s)
  (insert-char ?\[)

  (let ((content (car contents)))
    (when content
      (insert (prin1-to-string content))
      (dolist (content (cdr contents))
        (insert ", ")
        (insert (prin1-to-string content)))))

  (insert-char ?\]))

(defun kaitai--insert-leaf (leaf)
  (insert leaf))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))
