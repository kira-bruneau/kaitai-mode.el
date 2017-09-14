;; References: org-mode, neotree
;; TODO: Integrate with vlf package
;; TODO: Create an index from each .ksy "contents" fields to auto detect the format

(defvar kaitai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kaitai-toggle-node)
    map))

(defvar kaitai-schema nil
  "Schema used to parse file")

(define-derived-mode kaitai-mode fundemental-mode "Kaitai"
  ;; 1) Store the buffer contents somewhere hidden (eg. hidden buffer)
  ;; 2) Set the encoding to UTF-8
  ;; 3) Re-render tree based on schema + file contents

  (erase-buffer)
  (kaitai--insert-seq
   '(("header" "[Header]"
      (("magic" "= [128, 55, 18, 64]")
        ("clock" "= 0xF = 15")
        ("pc" "= 0x80080000 = 2148007936")
        ("release" "= 0x144B = 5195")
        ("crc1" "= 0x5354631C = 1398039324")
        ("crc2" "= 0x3A2DEF0 = 61005552")
        ("reserved1" "= [0, 0, 0, 0, 0, 0, 0, 0]")
        ("name" "= ZELDA MAJORA'S MASK")
        ("reserved2" "= [0, 0, 0, 0, 0, 0, 0]")
        ("id" "[Id]"
         (("gameId" "= NZS")
           ("region" "= USA (0x45 = 69)")))
        ("reserved3" "= [0]")))
     ("bootCode" "= [3, 160, 72, 32, 141, 40, 240, 16, ...]")
     ("code" "= [60, 8, 128, 10, 37, 8, 149, 0, ...]")) 0))

(defun kaitai-toggle-expand ()
  "Expand / collapse the kaitai struct node at point"
  (interactive)
  (message "toggle-node"))

(defun kaitai--insert-seq (seq depth)
  (kaitai--insert-seq-element (car seq) depth)
  (dolist (element (cdr seq))
    (insert "\n")
    (kaitai--insert-seq-element element depth)))

(defun kaitai--insert-seq-element (element depth)
  (let ((id (nth 0 element))
        (summary (nth 1 element))
        (seq (nth 2 element)))
    (insert-char ?\s (* depth 2))
    (kaitai--insert-expand-symbol (if seq 'close))
    (insert " ")
    (kaitai--insert-id (nth 0 element))
    (insert " ")
    (kaitai--insert-summary (nth 1 element))
    (when seq
      (insert "\n")
      (kaitai--insert-seq (nth 2 element) (1+ depth)))))

(defun kaitai--insert-expand-symbol (type)
  (cond
   ((eq type 'open) (insert "+"))
   ((eq type 'close) (insert "-"))
   (t (insert " "))))

(defun kaitai--insert-id (id)
  (insert id))

(defun kaitai--insert-summary (summary)
  (insert summary))

;; Create a new buffer that is based on another buffer

;; Okay, I want to translate this into a view that I can edit


;; This is a view on top of the original text... It doesn't necessarily replace it

;; Kaitai mode should auto detect the format from the file extension / the magic
;; If the format is ambiguious, ask the user to diambiguated it


;; Any changes in this buffer should affect changes in the other buffer
