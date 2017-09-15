;; References: org-mode, neotree
;; TODO: Integrate with vlf package to support large files

(define-derived-mode kaitai-mode fundamental-mode "Kaitai"
  ;; TODO: Store the original contents of the buffer as a unibyte buffer,
  ;;       use it to generate leaf nodes of tree
  ;; TODO: Automatically detect kaitai-schema from contents of buffer
  (erase-buffer)
  (kaitai--insert-body kaitai-schema 0))

(defvar kaitai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'kaitai-toggle-expand)
    (define-key map (kbd "RET") 'kaitai-toggle-expand)
    map))

;; TODO: Obtain leaf node data from original buffer
(defvar kaitai-schema
  '(z64
    ((header
      (header
       ((magic "= [128, 55, 18, 64]")
        (clock "= 0xF = 15")
        (pc "= 0x80080000 = 2148007936")
        (release "= 0x144B = 5195")
        (crc1 "= 0x5354631C = 1398039324")
        (crc2 "= 0x3A2DEF0 = 61005552")
        (reserved1 "= [0, 0, 0, 0, 0, 0, 0, 0]")
        (name "= ZELDA MAJORA'S MASK")
        (reserved2 "= [0, 0, 0, 0, 0, 0, 0]")
        (id
         (id
          ((gameId "= NZS")
           (region "= USA (0x45 = 69)"))))
        (reserved3 "= [0]"))))
     (bootCode "= [3, 160, 72, 32, 141, 40, 240, 16, ...]")
     (code "= [60, 8, 128, 10, 37, 8, 149, 0, ...]"))))

(defvar kaitai--expand-state nil)

(defun kaitai-toggle-expand ()
  "Expand / collapse the kaitai struct node at point"
  (interactive)
  ;; TODO: walk through both kaitai-schema & kaitai--expand-state to find the node to expand
  ;; Then toggle the associated node in kaitai--expand-state
  (line-number-at-pos (point)))

(defun kaitai--insert-body (body depth)
  (cond
   ((kaitai--body-expandable-p body)
    (let ((id (nth 0 body))
          (seq (nth 1 body)))
      (insert "[" (capitalize id) "]")
      (kaitai--insert-newline depth)
      (kaitai--insert-seq seq depth)))
   (t
    (kaitai--insert-leaf body))))

(defun kaitai--body-expandable-p (body)
  (listp body))

(defun kaitai--insert-seq (seq depth)
  (kaitai--insert-node (car seq) depth)
  (dolist (node (cdr seq))
    (kaitai--insert-newline depth)
    (kaitai--insert-node node depth)))

(defun kaitai--insert-node (node depth)
  (let* ((id (nth 0 node))
         (body (nth 1 node)))
    (kaitai--insert-expand-symbol
     (if (kaitai--body-expandable-p body) 'close))
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
  (insert id))

(defun kaitai--insert-leaf (leaf)
  (insert leaf))

(defun kaitai--insert-newline (depth)
  (newline)
  (insert-char ?\s (* depth 2)))
