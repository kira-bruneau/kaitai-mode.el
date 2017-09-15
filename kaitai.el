;; References: org-mode, neotree
;; TODO: Integrate with vlf package to support large files

(define-derived-mode kaitai-mode fundamental-mode "Kaitai"
  ;; TODO: Store the original contents of the buffer as a unibyte buffer,
  ;;       use it to generate leaves of tree
  ;; TODO: Automatically detect kaitai-schema from contents of buffer
  (erase-buffer)
  (kaitai--insert-body kaitai-schema 0))

(defvar kaitai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'kaitai-toggle-expand)
    (define-key map (kbd "RET") 'kaitai-toggle-expand)
    map))

;; TODO: Obtain leaf data from original buffer
(defvar kaitai-schema
  '(product z64
     (header
      (product header
        (magic "= [128, 55, 18, 64]")
        (clock "= 0xF = 15")
        (pc "= 0x80080000 = 2148007936")
        (release "= 0x144B = 5195")
        (crc1 "= 0x5354631C = 1398039324")
        (crc2 "= 0x3A2DEF0 = 61005552")
        (reserved1 "= [0, 0, 0, 0, 0, 0, 0, 0]")
        (name "= ZELDA MAJORA'S MASK")
        (reserved2 "= [0, 0, 0, 0, 0, 0, 0]")
        (id
         (product id
           (game-id "= NZS")
           (region "= USA (0x45 = 69)")))
        (reserved3 "= [0]")))
     (boot-code "= [3, 160, 72, 32, 141, 40, 240, 16, ...]")
     (code "= [60, 8, 128, 10, 37, 8, 149, 0, ...]")))

(defvar kaitai--expand-state nil)

(defun kaitai-toggle-expand ()
  "Expand / collapse the kaitai product at point"
  (interactive)
  ;; TODO: walk through both kaitai-schema & kaitai--expand-state to find the product to expand
  ;; Then toggle the associated product in kaitai--expand-state
  (line-number-at-pos (point)))

(defun kaitai--insert-body (body depth)
  (if (listp body)
      (let ((type (car body))
            (id (cadr body))
            (args (cddr body)))
        (if id (insert "[" (capitalize (symbol-name id)) "]"))
        (kaitai--insert-newline depth)
        (cond
         ((eq type 'product)
          (kaitai--insert-product args depth))
         t (error "unexpected type: %s" type)))
    (kaitai--insert-leaf body)))

(defun kaitai--insert-product (product depth)
  (kaitai--insert-node (car product) depth)
  (dolist (node (cdr product))
    (kaitai--insert-newline depth)
    (kaitai--insert-node node depth)))

(defun kaitai--insert-node (node depth)
  (let ((id (car node))
        (body (cadr node)))
    (kaitai--insert-expand-symbol 'close)
    (insert-char ?\s)
    (kaitai--insert-id id)
    (insert-char ?\s)
    (kaitai--insert-body body (1+ depth))))

(defun kaitai--insert-expand-symbol (type)
  (insert-char
   (cond
    ((eq type 'open) ?+)
    ((eq type 'close) ?-)
    (t ?\s))))

(defun kaitai--insert-id (id)
  (insert (symbol-name id)))

(defun kaitai--insert-leaf (leaf)
  (insert leaf))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))
