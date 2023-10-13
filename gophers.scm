(import (owl io))
(import (owl ff))
(import (owl sys))
(import (owl lazy))
(import (owl syscall))
(import (owl terminal))

(define x-padding 8)
(define border-n  "─")
(define border-e  "│")
(define border-s  "─")
(define border-w  "│")
(define border-ne "╮")
(define border-se "╯")
(define border-nw "╭")
(define border-sw "╰")

(define (term-get-key)
  (tuple->list (car ((terminal-input empty)))))

(define (deep=? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((eq? (car l1) (car l2)) (deep=? (cdr l1) (cdr l2)))
    (else #f)))

(define gopher-types
  `((0    . text-file)
    (1    . submenu)
    (2    . ccso-nameserver)
    (3    . err)
    (4    . binhex-file)
    (5    . dos-file)
    (6    . uuencode-file)
    (7    . full-text-search)
    (8    . telnet)
    (9    . bin-file)
    (+    . mirror)
    (g    . gif-file)
    (I    . image-file)
    (T    . telnet-3270)
    (:    . bitmap-flie)
    (,#\; . movie-file)
    (<    . sound-file)
    (d    . document-file)
    (h    . html-file)
    (i    . info)
    (p    . image-file)
    (r    . rtf-file)
    (s    . wav-file)
    (P    . pdf-file)
    (X    . xml-file)))

(define (term-set x y c)
  (write-bytes stdout (set-cursor '() x y))
  (write-bytes stdout (string->bytes c)))

(define (term-clear-line x y len)
  (term-set x y (make-string len 32)))

(define (cls)
  (write-bytes stdout (clear-screen '(97))))

(define (draw-border title x1 y1 x2 y2)
  (term-set x1 y1 border-nw)
  (term-set x2 y1 border-ne)
  (term-set x2 y2 border-se)
  (term-set x1 y2 border-sw)
  (for-each (λ (x) (term-set x y1 border-n)
                   (term-set x y2 border-s)) (iota (+ x1 1) 1 x2))

  (for-each (λ (y) (term-set x1 y border-e)
                   (term-set x2 y border-w)) (iota (+ y1 1) 1 y2)))

  ;tb_print(x1 + 3, y1, TB_BLUE, TB_DEFAULT, title);

; host path -> list of lines
(define (gopher-get host path)
  (define c (open-connection (resolve-host host) 70))
  (print-to (fd->port c) path)
  (force-ll (lines c)))

; list-of-lines -> parsed document
(define (gopher-parse ll)
  (define r (string->regex "c/\t/"))
  (filter (λ (x) (eq? (length x) 5))
          (map (λ (s) (append
                         (list (substring s 0 1))
                         (r (substring s 1 (string-length s))))) ll)))

(define (gui-end)
  (cls)
  (write-bytes stdout (cursor-show '()))
  (exit-owl 0))

; man what the hell
(define (term-size)
  (lets ((w h ll (get-terminal-size (terminal-input empty))))
        `((w . ,w) (h . ,h))))

; prompt "" -> str
(define (modal-get-str prompt _text)
  (define tw (cdr (assq 'w (term-size))))
  (define th (cdr (assq 'h (term-size))))
  (define w  (floor (/ tw 2)))
  (define h  (floor (/ th 2)))
  (define x1 (floor (/ w 2)))
  (define y1 (floor (/ h 2)))
  (define x2 (floor (- tw (/ w 2))))
  (define y2 (floor (- th (/ h 2))))

  (if (string=? _text "")
    (for-each (λ (y) (term-clear-line x1 y (- x2 x1))) (iota y1 1 y2)))

  (term-set (+ 1 x1) (floor (avg `(,y1 ,y2))) _text)

  (draw-border prompt x1 y1 x2 y2)

  (define c (term-get-key))
  (cond
    ((deep=? c '(enter)) (begin (cls) _text))
    ((deep=? c '(backspace))
     (begin
       (term-clear-line (+ 1 x1) (floor (avg `(,y1 ,y2))) (string-length _text))
       (modal-get-str prompt (substring _text 0 (- (string-length) 2)))))
    (else
      (modal-get-str prompt (string-concatenate
                              `(,_text ,(make-string 1 (cadr c)))))))
  )

(define (gui-loop page cursor)
  (define term-sz (term-size))
  (define lines-fit (- (cdr (assq 'h term-sz)) 2))
  (define max-width (- (cdr (assq 'w term-sz)) 2))
  (define scroll (max (- cursor lines-fit -1) 0))

  (draw-border "" 1 1 (cdr (assq 'w term-sz)) (cdr (assq 'h term-sz)))
  (for-each
    (λ (y)
       (define not-S (list-ref (list-ref page (+ y scroll)) 1))
       (define S (if (eq? (string-length not-S) 0) "   " not-S))
       (term-clear-line 2 (+ y 2) max-width)
       (if (eq? (+ y scroll) cursor) (write-bytes stdout (font-reverse '())))
       (term-set (+ 2 x-padding)
                 (+ y 2)
                 (substring S 0 (min (string-length S)
                                     (- max-width 2 (* x-padding 2)))))
       (if (eq? (+ y scroll) cursor) (write-bytes stdout (font-normal '()))))
    (iota 0 1 (min lines-fit (length page))))

  (define c (term-get-key))
  (cond
    ((or (deep=? '(ctrl c) c)
         (deep=? '(key 113) c)) (gui-end))
    ((deep=? '(key 106) c) (gui-loop page (if (eq? cursor (- (length page) 1))
                                            cursor (+ 1 cursor))))
    ((deep=? '(key 107) c) (gui-loop page (max 0 (- cursor 1))))
    ((deep=? '(key 103) c) (gui-loop
                             (gopher-parse
                               (gopher-get (modal-get-str "go: " "") "/"))
                             0))
    (else
      (gui-loop page cursor))))

(define (gui-start host path)
  (define page (gopher-parse (gopher-get host path)))

  (set-terminal-rawness #t)
  (cls)
  (write-bytes stdout (cursor-hide '()))
  (gui-loop page 0)
)

(lambda (args)
  (gui-start "gopher.floodgap.com" "/")
)
