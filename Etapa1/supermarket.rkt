#lang racket
(require racket/match)
(require racket/trace)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))

;(define C1 (empty-counter 1))
;(define C2 (empty-counter 2))
;(define C3 (empty-counter 3))
;(define C4 (empty-counter 4))

; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (make-counter 
   (match C
     [(counter index tt queue)
      index])
   (+
    (match C
     [(counter index tt queue)
      tt])
    minutes)
   (match C
     [(counter index tt queue)
      queue])))

;(define (tt+ C minutes)
;   (struct-copy counter C
;                [tt (+ (counter-tt C) minutes)]))

; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic

(define (minimum-helper counters acc)
  (cond
    [(null? counters)
     acc]
    [(< (counter-tt (car counters)) (counter-tt acc))
     (minimum-helper (cdr counters) (car counters))]
    [else
     (minimum-helper (cdr counters) acc)]))

(define (min counters)
  (cond
    [(null? counters)
      #f]
    [else
      (minimum-helper (cdr counters) (car counters))]))

(define (min-tt counters)
  (cons (counter-index (min counters)) (counter-tt (min counters))))

;(trace minimum)
;(trace mymin)

;(min-tt (list (tt+ C1 1) (tt+ C2 32) C4))
;(min-tt (list (tt+ C1 99999999999) (tt+ C2 9999999) (tt+ C3 999999999)))

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (struct-copy counter C
               [tt (+ (counter-tt C) n-items)]
               [queue (append(counter-queue C) (list (cons name n-items)))]))

;(cons 'ana '11)
;
;(add-to-counter C3 'ana 11)
;(make-counter 3 11 '((ana . 11)))
;
;(add-to-counter (add-to-counter (add-to-counter C2 'ana 11) 'gogu 4) 'leo 12)
;(make-counter 2 27 '((ana . 11) (gogu . 4) (leo . 12)))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește


(define (helper pair requests name n-items C1 C2 C3 C4)
  (cond
    [(= (car pair) (counter-index C1)) (serve requests (add-to-counter C1 name n-items) C2 C3 C4)]
    [(= (car pair) (counter-index C2)) (serve requests C1 (add-to-counter C2 name n-items) C3 C4)]
    [(= (car pair) (counter-index C3)) (serve requests C1 C2 (add-to-counter C3 name n-items) C4)]
    [(= (car pair) (counter-index C4)) (serve requests C1 C2 C3 (add-to-counter C4 name n-items))]))

(define (solve-name-items requests name n-items C1 C2 C3 C4 flag)
  (if flag
      (helper (min-tt (list C1 C2 C3 C4)) requests name n-items C1 C2 C3 C4)
      (helper (min-tt (list C2 C3 C4)) requests name n-items C1 C2 C3 C4)))

  
(define (solve-delay-index-minutes requests index minutes C1 C2 C3 C4)
  (cond
    [(= index (counter-index C1)) (serve requests (tt+ C1 minutes) C2 C3 C4)]
    [(= index (counter-index C2)) (serve requests C1 (tt+ C2 minutes) C3 C4)]
    [(= index (counter-index C3)) (serve requests C1 C2 (tt+ C3 minutes) C4)]
    [(= index (counter-index C4)) (serve requests C1 C2 C3 (tt+ C4 minutes))]))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (solve-delay-index-minutes (cdr requests) index minutes C1 C2 C3 C4)]
        [(list name n-items)         (if (<= n-items ITEMS)
                                         (solve-name-items (cdr requests) name n-items C1 C2 C3 C4 #t)
                                         (solve-name-items (cdr requests) name n-items C1 C2 C3 C4 #f))])))
;(trace solve-name-items)
;(trace solve-delay-index-minutes)
;(trace serve)

;(serve '((ana 12) (mia 2) (mara 4) (delay 1 2) (ion 7) (gigi 5) (delay 1 5) (delay 3 10) (lia 5) (dan 3)) C1 C2 C3 C4)
;(list
;   (make-counter 1 14 '((mia . 2) (gigi . 5)))
;   (make-counter 2 15 '((ana . 12) (dan . 3)))
;   (make-counter 3 14 '((mara . 4)))
;   (make-counter 4 12 '((ion . 7) (lia . 5)))
;  )

;(serve '((ana 12) (delay 3 10) (delay 3 10) (maria 3)) C1 C2 C3 C4)
;(list
;   (make-counter 1 3 '((maria . 3)))
;   (make-counter 2 12 '((ana . 12)))
;   (make-counter 3 20 '())
;   (make-counter 4 0 '())
;  )

;(serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (dan 3)) C1 C2 C3 C4)
;(list
;    (make-counter 1 7 '((mia . 2) (gigi . 5)))
;    (make-counter 2 12 '((ana . 12)))
;    (make-counter 3 7 '((mara . 4) (dan . 3)))
;    (make-counter 4 7 '((ion . 7))))

;(serve '((ana 3)) C1 C2 C3 C4)
;(list
;      (make-counter 1 3 '((ana . 3)))
;      C2
;      C3
;      C4
;    )
;
;(serve '((ana 12)) C1 C2 C3 C4)
;(list
;    C1
;    (make-counter 2 12 '((ana . 12)))
;    C3
;    C4
;  )
;
;(serve '((delay 1 10) (delay 2 5) (delay 2 7) (delay 4 2)) C1 C2 C3 C4)
;(list
;    (make-counter 1 10 '())
;    (make-counter 2 12 '())
;    C3
;    (make-counter 4 2 '())
;  )

;(serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (dan 3)) C1 C2 C3 C4)
;(list
;    (make-counter 1 7 '((mia . 2) (gigi . 5)))
;    (make-counter 2 12 '((ana . 12)))
;    (make-counter 3 7 '((mara . 4) (dan . 3)))
;    (make-counter 4 7 '((ion . 7)))
;   )
;
;(serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (delay 1 5) (delay 3 100) (lia 343) (dan 3)) C1 C2 C3 C4)
;(list
;    (make-counter 1 15 '((mia . 2) (gigi . 5) (dan . 3)))
;    (make-counter 2 12 '((ana . 12)))
;    (make-counter 3 104 '((mara . 4)))
;    (make-counter 4 350 '((ion . 7) (lia . 343)))
;  )
;
;(serve '((ana 1) (mia 2) (mara 4) (ion 2332) (gigi 3232) (delay 1 5) (delay 3 10) (lia 32) (dan 33232)) C1 C2 C3 C4)
;(list
;    (make-counter 1 6 '((ana . 1)))
;    (make-counter 2 3234 '((mia . 2) (gigi . 3232)))
;    (make-counter 3 33278 '((mara . 4) (lia . 32) (dan . 33232)))
;    (make-counter 4 2332 '((ion . 2332)))
;  )
;
;(serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (delay 1 5) (delay 3 10) (lia 3) (dan 3)) C1 C2 C3 C4)
;(list
;    (make-counter 1 12 '((mia . 2) (gigi . 5)))
;    (make-counter 2 12 '((ana . 12)))
;    (make-counter 3 14 '((mara . 4)))
;    (make-counter 4 13 '((ion . 7) (lia . 3) (dan . 3)))
;  )
;
