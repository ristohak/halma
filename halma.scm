;;;;
;;;; halma.scm - Halma board game
;;;;
;;;; Author: Risto Hakala <risto.m.hakala@gmail.com>
;;;;
;;;; Requires MzScheme and Xdraw.
;;;;
;;;; Start the game by evaluating (halma). Use mouse to play. You
;;;; can stop the game by hitting 'q' or clicking 'quit'. 2 player
;;;; game is set as default.

;;; Ohjelman ydin on pelilautaa kuvaava listarakenne. Lista koostuu
;;; pelilaudan ruuduista, eli pareista, joissa on tieto ruudun
;;; sijainnista (koordinaattipari) ja sis‰llˆst‰. Pelin edetess‰ 
;;; nappuloita siirret‰‰n ja n‰in myˆs ruutujen sis‰ltˆ‰ muutetaan 
;;; listassa. Ohjelmaa hallitaan t‰ysin hiirell‰. Kun xdraw-
;;; ikkunaa klikataan, klikkauksen sijainti tutkitaan ja sijainnista 
;;; riippuen tapahtuu muutoksia sek‰ ikkunassa ett‰ pelilautaa 
;;; kuvaavassa listassa. Listarakenteen muuttaminen ja ikkunaan 
;;; piirt‰minen on jaettu t‰ysin erillisiksi toimenpiteiksi 
;;; ohjelman rakenteessa.

;;;
;;; Pelilaudan ja ruutujen hallinta
;;;

;; Ruutujen luonti
;;
;; Jokaisessa ruudussa on tieto sen sijainnista ja sis‰llˆst‰ eli
;; statuksesta.  Ruutu voi olla tyhj‰, jolloin sen status on 'empty 
;; tai sitten se voi sis‰lt‰‰ esimerkiksi 1. pelaajan pelinappulan, 
;; jolloin sen status olisi 'player-1. Ruutujen sijainti ilmoitetaan 
;; koordinaattiparina.

(define (make-square x y status)
  (cons (cons x y) status))

(define (make-empty-square x y)
  (make-square x y 'empty))

;; MAKE-EMPTY-BOARD: Luo tyhj‰n n x n -sivuisen pelilaudan
;;
;; Lauta on lista tyhji‰ ruutuja.

(define (make-empty-board n)
  (define (iter-board x y)
    (cond ((> y n) '())
	  ((> x n) (iter-board 1 (+ y 1)))
	  (else (cons (make-empty-square x y)
		      (iter-board (+ x 1) y)))))
  (iter-board 1 1))

;; MAKE-BOARD: Olio, jolla k‰sitell‰‰n pelilautaa ja sen ruutuja
;;
;; Oliota hallitaan l‰hett‰m‰ll‰ sille seuraavia viestej‰:
;;  * (board '2-player-board) Luo pelilaudan ja asettaa nappulat
;;      kahdelle pelaajalle
;;  * (board '4-player-board) Luo pelilaudan ja asettaa nappulat
;;      nelj‰lle pelaajalle
;;  * (board 'square pos) Noutaa ruudun, jonka sijainti on pos
;;  * (board 'square-status pos) Noutaa ruudun statuksen
;;  * (board 'square-empty? pos) Tutkii onko ruutu tyhj‰
;;  * (board 'status-same? pos-1 pos-2) Tutkii onko ruuduilla sama
;;      status
;;  * (board 'set-status! pos status) Muuttaa ruudun statuksen
;;  * (board 'set-status-to-empty! pos) Tyhjent‰‰ ruudun
;;  * (board 'move-piece! pos-1 pos-2) Siirt‰‰ nappulaa
;;  * (board 'move-1-possible? pos-1 pos-2) Tutkii voiko nappulan
;;      siirt‰‰ viereiseen ruutuun
;;  * (board 'move-2-possible? pos-1 pos-2) Tutkii voiko nappulan
;;      siirt‰‰ toiseen ruutuun toisen nappulan yli, eli voiko
;;      nappula hyp‰t‰
;;  * (board 'field-status field) Noutaa alueen statuksen. Jos alue 
;;      sis‰lt‰‰ eri tyyppisi‰ nappuloita/tyhji‰ ruutuja, 
;;      palautettavaa arvoa ei ole m‰‰ritelty. Annettava alue 
;;      field on lista koordinaattipareja
;;  * (board 'set-field-status field status) Muuttaa alueen 
;;      jokaisen ruudun statuksen
;;
;; Ruutujen sijainti pos annetaan koordinaattiparina.

(define (make-board)
  (let ((board #f))
    (define (2-player-board)
      (set! board (make-empty-board 16))
      (set-field-status! (2-player-home-field-1) 'player-1)
      (set-field-status! (2-player-home-field-2) 'player-2))
    (define (4-player-board)
      (set! board (make-empty-board 16))
      (set-field-status! (4-player-home-field-1) 'player-1)
      (set-field-status! (4-player-home-field-2) 'player-2)
      (set-field-status! (4-player-home-field-3) 'player-3)
      (set-field-status! (4-player-home-field-4) 'player-4))
    (define (square pos)
      (assoc pos board))
    (define (square-status pos)
      (cdr (square pos)))
    (define (square-empty? pos)
      (eq? (square-status pos) 'empty))
    (define (status-same? pos-1 pos-2)
      (eq? (square-status pos-1) (square-status pos-2)))
    (define (set-status! pos status)
      (set-cdr! (square pos) status))
    (define (set-status-to-empty! pos)
      (set-status! pos 'empty))
    (define (move-piece! pos-1 pos-2)
      (set-status! pos-2 (square-status pos-1))
      (set-status-to-empty! pos-1))
    (define (move-1-possible? pos-1 pos-2 player)
      (let ((x-1 (car pos-1))
	    (y-1 (cdr pos-1))
	    (x-2 (car pos-2))
	    (y-2 (cdr pos-2)))
	(and (square pos-1)
             (square pos-2)
             (eq? (square-status pos-1) player)
	     (square-empty? pos-2)
	     (or (and (= (abs (- x-2 x-1)) 1)
		      (= (abs (- y-2 y-1)) 1))
		 (and (= (abs (- x-2 x-1)) 1)
		      (= (abs (- y-2 y-1)) 0))
		 (and (= (abs (- x-2 x-1)) 0)
		      (= (abs (- y-2 y-1)) 1))))))
    (define (move-2-possible? pos-1 pos-2 player)
      (let ((x-1 (car pos-1))
	    (y-1 (cdr pos-1))
	    (x-2 (car pos-2))
	    (y-2 (cdr pos-2)))
	(and (square pos-1)
             (square pos-2)
             (eq? (square-status pos-1) player)
	     (square-empty? pos-2)
             (or (and (= (abs (- x-2 x-1)) 2)
		      (= (abs (- y-2 y-1)) 2)
		      (not (square-empty? (cons (/ (+ x-1 x-2) 2) 
                                                (/ (+ y-1 y-2) 2)))))
		 (and (= (abs (- x-2 x-1)) 2)
		      (= (abs (- y-2 y-1)) 0)
		      (not (square-empty? (cons (/ (+ x-1 x-2) 2) 
                                                y-1))))
		 (and (= (abs (- x-2 x-1)) 0)
		      (= (abs (- y-2 y-1)) 2)
		      (not (square-empty? (cons x-1 
                                                (/ (+ y-1 y-2) 2)))))))))
    (define (field-status field)
      (cond ((null? (cdr field)) 
	     (square-status (car field)))
	    ((status-same? (car field) (cadr field)) 
	     (field-status (cdr field)))))
    (define (set-field-status! field status)
      (for-each (lambda (pos) 
		  (set-status! pos status)) 
		field))
    (define (dispatch m . args)
      (cond ((eq? m '2-player-board)
	     (2-player-board))
	    ((eq? m '4-player-board)
	     (4-player-board))
            ((eq? m 'square) 
	     (square (car args)))
	    ((eq? m 'square-status) 
	     (square-status (car args)))
	    ((eq? m 'square-empty?) 
	     (square-empty? (car args)))
	    ((eq? m 'status-same?) 
	     (status-same? (car args) (cadr args)))
	    ((eq? m 'set-status!) 
	     (set-status! (car args) (cadr args)))
	    ((eq? m 'set-status-to-empty!) 
	     (set-status-to-empty! (car args)))
	    ((eq? m 'move-piece!) 
	     (move-piece! (car args) (cadr args)))
	    ((eq? m 'move-1-possible?)
	     (move-1-possible? (car args) (cadr args) (caddr args)))
            ((eq? m 'move-2-possible?)
	     (move-2-possible? (car args) (cadr args) (caddr args)))
	    ((eq? m 'field-status)
	     (field-status (car args)))
	    ((eq? m 'set-field-status!)
	     (set-field-status! (car args) (cadr args)))
	    (else 
	     (error "MAKE-BOARD: object got an invalid message:" m))))
    dispatch))

;; Pelaajien kotileirit
;;
;; Leirit ovat lista koordinaattejapareja, jotka ilmoittavat leiriin
;; kuuluvien ruutujen sijainnin.

(define (2-player-home-field-1)
  (list (cons 16 16) (cons 15 16) (cons 14 16) (cons 13 16) (cons 12 16)
	(cons 16 15) (cons 15 15) (cons 14 15) (cons 13 15) (cons 12 15)
	(cons 16 14) (cons 15 14) (cons 14 14) (cons 13 14)
	(cons 16 13) (cons 15 13) (cons 14 13)
	(cons 16 12) (cons 15 12)))

(define (2-player-home-field-2)
  (list (cons 1 1) (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)
	(cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2) (cons 5 2)
	(cons 1 3) (cons 2 3) (cons 3 3) (cons 4 3)
	(cons 1 4) (cons 2 4) (cons 3 4)
	(cons 1 5) (cons 2 5)))

(define (4-player-home-field-1)
  (list (cons 16 16) (cons 15 16) (cons 14 16) (cons 13 16)
	(cons 16 15) (cons 15 15) (cons 14 15) (cons 13 15)
	(cons 16 14) (cons 15 14) (cons 14 14)
	(cons 16 13) (cons 15 13)))

(define (4-player-home-field-2)
  (list (cons 1 1) (cons 2 1) (cons 3 1) (cons 4 1)
	(cons 1 2) (cons 2 2) (cons 3 2) (cons 4 2)
	(cons 1 3) (cons 2 3) (cons 3 3)
	(cons 1 4) (cons 2 4)))

(define (4-player-home-field-3)
  (list (cons 16 1) (cons 15 1) (cons 14 1) (cons 13 1)
	(cons 16 2) (cons 15 2) (cons 14 2) (cons 13 2)
	(cons 16 3) (cons 15 3) (cons 14 3)
	(cons 16 4) (cons 15 4)))

(define (4-player-home-field-4)
  (list (cons 1 16) (cons 2 16) (cons 3 16) (cons 4 16)
	(cons 1 15) (cons 2 15) (cons 3 15) (cons 4 15)
	(cons 1 14) (cons 2 14) (cons 3 14)
	(cons 1 13) (cons 2 13)))


;;;
;;; Grafiikan hallinta
;;;

;; MAKE-DRAWER: Olio, jolla hallitaan pelin grafiikkaa, eli 
;; pelilautaa ja muita alueita xdraw-ikkunassa
;;
;; MAKE-DRAWER hyv‰ksyy seuraavia viestej‰:
;;  * (drawer 'close) Sulkee ikkunan
;;  * (drawer '2-player-board) Piirt‰‰ pelilaudan ja nappulat 
;;      kahdelle pelaajalle
;;  * (drawer '4-player-board) Piirt‰‰ pelilaudan ja nappulat
;;      nelj‰lle pelaajalle
;;  * (drawer 'place-piece! pos player) Piirt‰‰ pelaajan nappulan
;;      ikkunaan. Sijainti pos annetaan pelilaudan koortinaatteina
;;  * (drawer 'erase-area! pos width height) Tyhjent‰‰ alueen 
;;      ikkunassa. Sijainti pos on sijainti xdraw-ikkunassa
;;  * (drawer 'erase-square! pos) Tyhjent‰‰ pelilaudan ruudun
;;  * (drawer 'move-piece! pos-1 pos-2) Siirt‰‰ nappulaa ikkunassa
;;  * (drawer 'show-number! player) N‰ytt‰‰ pelaajan vuoronumeron
;;  * (drawer 'cover-numbers!) Piilottaa pelaajien vuoronumerot
;;  * (drawer 'get-input) Noutaa k‰ytt‰j‰n antaman inputin. Palauttaa 
;;      #f:n, jos painetaan q:ta (lopettamista varten), muuten 
;;      palautetaan hiirenklikkauksen sijainti ikkunassa
;;      (koordinaattiparina)
;;
;; Pelilaudan ruutuja k‰sittelev‰t proseduurit ottavat sijainnin pos 
;; pelilaudan ruudun koordinaatteina.

(define (make-drawer)
  (let ((xo #f)
        (board #f))
    (define (init)
      (set! xo (new-xdraw "-geometry" "480x645" "-title" "Halma"))
      (xo 'send 'reset)
      (xo 'send 'ReportButtonEvents 'on 'off)
      (xo 'send 'ReportKeyEvents 'on 'off)
      (xo 'send 'ReadFileToPixmap "bg1.xpm" "board-1")
      (xo 'send 'ReadFileToPixmap "bg2.xpm" "board-2")
      (xo 'send 'ReadFileToPixmap "p1.xpm" "player-1")
      (xo 'send 'ReadFileToPixmap "p2.xpm" "player-2")
      (xo 'send 'ReadFileToPixmap "p3.xpm" "player-3")
      (xo 'send 'ReadFileToPixmap "p4.xpm" "player-4")
      (xo 'send 'ReadFileToPixmap "ncover.xpm" "number-cover"))
    (define (close)
      (xo 'exit))
    (define (2-player-board)
      (set! board 'board-1)
      (xo 'send 'CopyFromPixmap "board-1" 0 0)
      (for-each (lambda (pos) (place-piece! pos 'player-1))
                (2-player-home-field-1))
      (for-each (lambda (pos) (place-piece! pos 'player-2))
                (2-player-home-field-2)))
    (define (4-player-board)
      (set! board 'board-2)
      (xo 'send 'CopyFromPixmap "board-2" 0 0)
      (for-each (lambda (pos) (place-piece! pos 'player-1))
                (4-player-home-field-1))
      (for-each (lambda (pos) (place-piece! pos 'player-2))
                (4-player-home-field-2))
      (for-each (lambda (pos) (place-piece! pos 'player-3))
                (4-player-home-field-3))
      (for-each (lambda (pos) (place-piece! pos 'player-4))
                (4-player-home-field-4)))
    (define (place-piece! pos player)
      (let ((pos-x (car (board-pos->window-pos pos)))
            (pos-y (cdr (board-pos->window-pos pos)))
            (piece (symbol->string player)))
        (xo 'send 'CopyFromPixmap piece pos-x pos-y)))
    (define (erase-area! pos width height)
      (let ((pos-x (car pos))
	    (pos-y (cdr pos))
	    (board (symbol->string board)))
      (xo 'send 'CopyAreaFromPixmap board pos-x pos-y width height pos-x pos-y)))
    (define (erase-square! pos)
      (let ((pos (board-pos->window-pos pos))
            (piece-size 16))
	(erase-area! pos piece-size piece-size)))
    (define (move-piece! pos-1 pos-2 player)
      (place-piece! pos-2 player)
      (erase-square! pos-1))
    (define (show-player-number! player)
      (let ((pos-n-1 (cons 445 505))
            (pos-n-2 (cons 15 115))
            (pos-n-3 (cons 445 115))
            (pos-n-4 (cons 15 505))
            (n-size 20))
        (cond ((eq? player 'player-1)
               (erase-area! pos-n-1 n-size n-size))
              ((eq? player 'player-2)
               (erase-area! pos-n-2 n-size n-size))
              ((eq? player 'player-3)
               (erase-area! pos-n-3 n-size n-size))
              ((eq? player 'player-4)
               (erase-area! pos-n-4 n-size n-size)))))
    (define (cover-player-numbers!)
      (let ((pos-x 14)
	    (pos-y 117))
	(xo 'send 'CopyFromPixmap "number-cover" pos-x pos-y)))
    (define (get-input)
      (define (button-press-event? event)
        (and (pair? event)
             (= (length event) 6)
             (eq? (car event) 'event)
             (eq? (cadr event) 'ButtonPress)))
      (define (key-press-event? event)
        (and (pair? event)
             (= (length event) 6)
             (eq? (car event) 'event)
             (eq? (cadr event) 'KeyPress)))
      (define (bpe-pos bp-event)
        (let ((bpe-x (caddr bp-event))
              (bpe-y (cadddr bp-event)))
          (cons bpe-x bpe-y)))
      (define (quit? event)
        (if (string=? (list-ref event 5) "q")
            'quit))
      (define (loop)
        (let ((event (xo 'wait-for-event)))
          (cond ((button-press-event? event)
                 (bpe-pos event))
                ((key-press-event? event)
                 (if (quit? event)
                     #f
                     (loop)))
                (else
                 (loop)))))
      (loop))
    (define (dispatch m . args)
      (cond ((eq? m 'close)
             (close))
            ((eq? m '2-player-board)
             (2-player-board))
            ((eq? m '4-player-board)
             (4-player-board))
            ((eq? m 'place-piece!)
             (place-piece! (car args) (cadr args)))
            ((eq? m 'erase-area!)
             (erase-area! (car args) (cadr args) (caddr args)))
            ((eq? m 'erase-square!)
             (erase-square! (car args) (cadr args)))
            ((eq? m 'move-piece!)
             (move-piece! (car args) (cadr args) (caddr args)))
            ((eq? m 'show-player-number!)
             (show-player-number! (car args)))
            ((eq? m 'cover-player-numbers!)
             (cover-player-numbers!))
            ((eq? m 'get-input) 
             (get-input))
            (else 
             (error "MAKE-DRAWER: object got an invalid message:" m))))
    (init)
    dispatch))

;; Ikkunan alueiden tutkiminen ja koordinaattimuunnokset

(define (window-pos->board-pos pos)
  (let ((square-size 25)
        (margin-x 40)
        (margin-y 120)
        (pos-x (car pos))
        (pos-y (cdr pos)))
    (cons (quotient (- (- pos-x 1) (- margin-x square-size)) square-size)
          (quotient (- (- pos-y 1) (- margin-y square-size)) square-size))))

(define (board-pos->window-pos pos)
  (let ((square-size 25)
	(square-center 5)
	(margin-x 40)
	(margin-y 120)
	(pos-x (car pos))
	(pos-y (cdr pos)))
    (cons (+ (* (- pos-x 1) square-size) margin-x square-center)
	  (+ (* (- pos-y 1) square-size) margin-y square-center))))

(define (inside-area? pos area)
  (let* ((start-pos (car area))
         (end-pos (cadr area))
         (start-pos-x (car start-pos))
         (start-pos-y (cdr start-pos))
         (end-pos-x (car end-pos))
         (end-pos-y (cdr end-pos))
         (pos-x (car pos))
         (pos-y (cdr pos)))
    (and (>= pos-x start-pos-x)
         (>= pos-y start-pos-y)
         (<= pos-x end-pos-x)
         (<= pos-y end-pos-y))))

(define (inside-area-board? pos)
  (inside-area? pos (board-area)))

(define (inside-area-quit? pos)
  (inside-area? pos (quit-area)))

(define (inside-area-2-player-game? pos)
  (inside-area? pos (2-player-game-area)))

(define (inside-area-4-player-game? pos)
  (inside-area? pos (4-player-game-area)))

(define (inside-area-4-player-team-game? pos)
  (inside-area? pos (4-player-team-game-area)))

(define (inside-area-next-turn? pos)
  (inside-area? pos (next-turn-area)))

;; Aktiiviset alueet xdraw-ikkunassa (suunnikkaan muotoisen alueen 
;; alku- ja loppukoordinaatit)

(define (make-area start-pos-x start-pos-y end-pos-x end-pos-y)
  (list (cons start-pos-x start-pos-y) (cons end-pos-x end-pos-y)))

(define (board-area)
  (make-area 40 120 441 520))

(define (quit-area)
  (make-area 310 592 341 600))

(define (2-player-game-area)
  (make-area 97 570 243 579))

(define (4-player-game-area)
  (make-area 97 593 243 601))

(define (4-player-team-game-area)
  (make-area 112 606 178 614))

(define (next-turn-area)
  (make-area 310 570 383 576))


;;;
;;; P‰‰ohjelma
;;;

;; HALMA: P‰‰rutiini. Lataa pelilaudan eri pelimoodeille ja 
;; ohjaa pelin tapahtumia
;;
;; HALMA-objektin sis‰ll‰ on eri muuttujia (mm. players, 
;; team-mode, player, turn-count), joissa on tieto sen 
;; hetkisest‰ pelitilanteesta. Muuttujia muutetaan tarvittaessa.
;;
;; 'board' on pelilautaobjekti ja 'drawer' hoitaa xdraw-ikkunaan 
;; piirt‰misen. 
;;
;; Perusasetuksena ladataan kaksinpeli.

(define (halma)
  (let ((drawer (make-drawer))
	(board (make-board))
        (players #f)
	(team-mode #f)
        (player #f)
        (turn-count #f))
    (define (close)
      (drawer 'close))
    (define (2-player-game)
      (board '2-player-board)
      (drawer '2-player-board)
      (set! players 2)
      (set! team-mode #f)
      (set! player 'player-1)
      (set! turn-count 0)
      (drawer 'cover-player-numbers!)
      (drawer 'show-player-number! player)
      (loop #f #f))
    (define (4-player-game)
      (board '4-player-board)
      (drawer '4-player-board)
      (set! players 4)
      (set! team-mode #f)
      (set! player 'player-1)
      (set! turn-count 0)
      (drawer 'cover-player-numbers!)
      (drawer 'show-player-number! player)
      (loop #f #f))
    (define (4-player-team-game)
      (board '4-player-board)
      (drawer '4-player-board)
      (set! players 4)
      (set! team-mode #t)
      (set! player 'player-1)
      (set! turn-count 0)
      (drawer 'cover-player-numbers!)
      (drawer 'show-player-number! player)
      (loop #f #f))
    (define (player-list)
      (list 'player-1 'player-2 'player-3 'player-4))
    (define (next-player)
      (let* ((next-turn (+ turn-count 1))
             (next-player-index (modulo next-turn players))
             (next-player (list-ref (player-list) next-player-index)))
        (set! turn-count next-turn)
        (set! player next-player)
        (drawer 'cover-player-numbers!)
        (drawer 'show-player-number! player)))
    (define (correct-piece? pos)
      (let ((pos (window-pos->board-pos pos)))
        (eq? (board 'square-status pos) player)))
    (define (move-1-possible? pos-1 pos-2)
      (let ((pos-1 (window-pos->board-pos pos-1))
            (pos-2 (window-pos->board-pos pos-2)))
      (board 'move-1-possible? pos-1 pos-2 player)))
    (define (move-2-possible? pos-1 pos-2)
      (let ((pos-1 (window-pos->board-pos pos-1))
            (pos-2 (window-pos->board-pos pos-2)))
      (board 'move-2-possible? pos-1 pos-2 player)))
    (define (move-piece! pos-1 pos-2)
      (let ((pos-1 (window-pos->board-pos pos-1))
            (pos-2 (window-pos->board-pos pos-2)))
        (board 'move-piece! pos-1 pos-2 player)
        (drawer 'move-piece! pos-1 pos-2 player)))
    (define (solved?)
      (cond ((and (= players 2)
                  (eq? (board 'field-status (2-player-home-field-2)) 'player-1))
             "PLAYER-1 WON!")
            ((and (= players 2)
                  (eq? (board 'field-status (2-player-home-field-1)) 'player-2))
             "PLAYER-2 WON!")
            ((and (= players 4)
                  (not team-mode)
                  (or (eq? (board 'field-status (4-player-home-field-2)) 'player-1)
                      (eq? (board 'field-status (4-player-home-field-3)) 'player-1)
                      (eq? (board 'field-status (4-player-home-field-4)) 'player-1)))
             "PLAYER-1 WON!")
            ((and (= players 4)
                  (not team-mode)
		  (or (eq? (board 'field-status (4-player-home-field-1)) 'player-2)
		      (eq? (board 'field-status (4-player-home-field-3)) 'player-2)
		      (eq? (board 'field-status (4-player-home-field-4)) 'player-2)))
             "PLAYER-2 WON!")
            ((and (= players 4)
                  (not team-mode)
		  (or (eq? (board 'field-status (4-player-home-field-1)) 'player-3)
		      (eq? (board 'field-status (4-player-home-field-2)) 'player-3)
		      (eq? (board 'field-status (4-player-home-field-4)) 'player-3)))
             "PLAYER-3 WON!")
	    ((and (= players 4)
                  (not team-mode)
		  (or (eq? (board 'field-status (4-player-home-field-1)) 'player-4)
		      (eq? (board 'field-status (4-player-home-field-2)) 'player-4)
		      (eq? (board 'field-status (4-player-home-field-3)) 'player-4)))
             "PLAYER-4 WON!")
            ((and (= players 4)
                  team-mode
                  (eq? (board 'field-status (4-player-home-field-2)) 'player-1)
                  (eq? (board 'field-status (4-player-home-field-1)) 'player-2))
             "TEAM-1 WON!")
            ((and (= players 4)
                  team-mode
                  (eq? (board 'field-status (4-player-home-field-4)) 'player-3)
                  (eq? (board 'field-status (4-player-home-field-3)) 'player-4))
             "TEAM-2 WON!")
            (else
             #f)))
    (define (quit? input)
      (eq? input 'quit))
    ;; LOOP on p‰‰looppi, joka ohjailee pelin tapahtumia. LOOP 
    ;; tulkitsee drawerilta saamansa inputin ja l‰hett‰‰ 
    ;; tarvittavan k‰skyn ohjelman muihin osiin, kuten piirt‰j‰‰n 
    ;; ja itse board-objektiin. Ikkunaan klikkauksen seurauksena 
    ;; tutkitaan onko klikkaus osunut pelilautaan vai muuhun 
    ;; ikkunan osaan. Ensimm‰isella klikkauksella olisi pelatessa
    ;; tarkoitus valita siirrett‰v‰ nappula pelilaudasta. Toisella 
    ;; valitaan ruutu, johon nappula olisi tarkoitus siirt‰‰. 
    ;; Nappula siirret‰‰n, jos se on mahdollista. Jokaisella 
    ;; klikkauksella voidaan vaihtaa pelimoodia, lopettaa tai
    ;; vaihtaa vuoroa. has-moved? ilmoittaa onko nappulaa 
    ;; siirretty jo samalla vuorolla (eli onko hyp‰tty).
    (define (loop first has-moved?)
      (cond ((solved?)
             (begin (display (solved?))
                    (newline)
                    (cond ((= players 2)
                           (2-player-game))
                          ((and (= players 4)
                                (not team-mode))
                           (4-player-game))
                          ((and (= players 4)
                                team-mode)
                           (4-player-team-game)))))
            (first
             (let ((second (drawer 'get-input)))
               (if second
                   (if has-moved?
                       (cond ((not (inside-area-board? second))
                              (cond ((inside-area-quit? second)
                                     'quit)
                                    ((inside-area-2-player-game? second)
                                     (2-player-game))
                                    ((inside-area-4-player-game? second)
                                     (4-player-game))
				    ((inside-area-4-player-team-game? second)
                                     (4-player-team-game))
                                    ((inside-area-next-turn? second)
                                     (begin (next-player)
                                            (loop #f #f)))
                                    (else 
                                     (begin (next-player)
                                            (loop #f #f)))))
                             ((inside-area-board? second)
                              (cond ((move-2-possible? first second)
                                     (begin (move-piece! first second)
                                            (loop second #t)))
                                    (else 
                                     (begin (next-player)
                                            (if (correct-piece? second)
                                                (loop second #f)
                                                (loop #f #f)))))))
                       (cond ((not (inside-area-board? second))
                              (cond ((inside-area-quit? second)
                                     'quit)
                                    ((inside-area-2-player-game? second)
                                     (2-player-game))
                                    ((inside-area-4-player-game? second)
                                     (4-player-game))
				    ((inside-area-4-player-team-game? second)
                                     (4-player-team-game))
                                    ((inside-area-next-turn? second)
                                     (begin (next-player)
                                            (loop #f #f)))
                                    (else 
                                     (loop #f #f))))
                             ((inside-area-board? second)
                              (cond ((move-1-possible? first second)
                                     (begin (move-piece! first second)
                                            (next-player)
                                            (loop #f #f)))
                                    ((move-2-possible? first second)
                                     (begin (move-piece! first second)
                                            (loop second #t)))
                                    (else
                                     (loop #f #f))))))
                   'quit)))
            (else 
             (let ((first (drawer 'get-input)))
               (if first
                   (cond ((not (inside-area-board? first))
                          (cond ((inside-area-quit? first)
                                 'quit)
                                ((inside-area-2-player-game? first)
                                 (2-player-game))
                                ((inside-area-4-player-game? first)
                                 (4-player-game))
				((inside-area-4-player-team-game? first)
				 (4-player-team-game))
                                ((inside-area-next-turn? first)
                                 (begin (next-player)
                                        (loop #f #f)))
                                (else 
                                 (loop #f #f))))
                         ((inside-area-board? first)
                          (if (correct-piece? first)
                              (loop first #f)
                              (loop #f #f))))
                   'quit)))))
    (2-player-game)
    (close)))