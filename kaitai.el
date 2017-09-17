;; References: org-mode, neotree
;; TODO: Integrate with vlf package to support large files

(define-derived-mode kaitai-mode fundamental-mode "Kaitai"
  ;; TODO: Store the original contents of the buffer as a unibyte buffer,
  ;;       use it to generate leaves of tree
  ;; TODO: Automatically detect kaitai--schema from contents of buffer
  (erase-buffer)
  (kaitai--insert-node kaitai--schema kaitai--expand-state 0))

(defvar kaitai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'kaitai-toggle-expand)
    (define-key map (kbd "RET") 'kaitai-toggle-expand)
    map))

;; TODO: Obtain leaf data from original buffer
;; TODO: Consider replacing product alist with plist
(defvar kaitai--schema
  '(product
    (:header
     (product
      (:magic (contents "NES\x1a"))
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
    (:code "= [60, 8, 128, 10, 37, 8, 149, 0, ...]")))

(defvar kaitai--expand-state nil)

(defun kaitai-toggle-expand ()
  "Expand / collapse the kaitai node at point"
  (interactive)
  ;; TODO: walk through both kaitai--schema & kaitai--expand-state to find the node to expand
  ;; Then toggle the associated node in kaitai--expand-state
  (line-number-at-pos (point)))

(defun kaitai--insert-node (node expand-state depth)
  (if (listp node)
      (let ((type (car node))
            (args (cdr node)))
        (cond
         ((eq type 'product)
          (unless (zerop depth)
            (kaitai--insert-newline depth))
          (kaitai--insert-product args expand-state depth))
         ((eq type 'contents)
          (kaitai--insert-contents args))
         t (error "unexpected type: %s" type)))
    (kaitai--insert-leaf node)))

(defun kaitai--node-expandable-p (node)
  (and (listp node) (eq (car node) 'product)))

(defun kaitai--insert-product (product expand-state depth)
  (when product
    (kaitai--insert-assoc (car product) (car expand-state) depth)
    (setq product (cdr product))
    (setq expand-state (cdr expand-state))
    (while product
      (kaitai--insert-newline depth)
      (kaitai--insert-assoc (car product) (car expand-state) depth)
      (setq product (cdr product))
      (setq expand-state (cdr expand-state)))))

(defun kaitai--insert-assoc (assoc expand-state depth)
  (let ((id (car assoc))
        (node (cadr assoc)))
    (kaitai--insert-expand-symbol
     (if (kaitai--node-expandable-p node)
         (if expand-state 'close 'open)))
    (insert-char ?\s)
    (kaitai--insert-id id)
    (insert-char ?\s)
    (kaitai--insert-node node expand-state (1+ depth))))

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
