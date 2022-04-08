#lang racket
(require "supermarket.rkt")

; ignorați următoarele linii de cod...
(define show-defaults 999) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define nopoints #f) (define name-ex '(testul testele trecut capitolul))
(define default-results `(#f 0 () your-code-here)) (define (default-result r) (set! default-results (cons r default-results))) (define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define exerciții 'string)
(define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define n-exercs -1) (define default-returns '()) (define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (sunt n s) (set! n-exercs n)) (define s-a string-append)
(define (p . L) (map (λ (e) (display e) (when (> (string-length (format "~a" e)) 0) (display " "))) L) (newline)) (define (p-n-ex) (format "[~a]" (if nopoints (string-join (list (symbol->string (cadddr name-ex)) (number->string n-ex) "/" (number->string n-exercs))) n-ex)))
(define (epart ep% pfix full) (if (< (caddr ep%) 1) (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (if (and nopoints (not full)) "" (number->string n-ex)) (symbol->string (cadr ep%))) (if (and nopoints (not full)) "" (s-a pfix (if full "" (s-a (symbol->string (car name-ex)) " ")) (number->string n-ex)))))
(define (whengood ep%) (let [(pts (* p-ex (caddr ep%)))] (and (if prepend (printf "+~v: " pts) (printf "~a[OK] " (p-n-ex))) (if nopoints (p (epart ep% "" #f) "rezolvat") (p (epart ep% "" #f) "rezolvat: +" pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts)))))
(define (whenbad ep% gvn expcd msg) (and (when (member gvn default-results) (set! default-returns (cons (epart ep% "" #t) default-returns))) (when (or (not (member gvn default-results)) (<= (length default-returns) show-defaults)) (bad-res ep% gvn expcd msg))))
(define (bad-res ep% gvn expcd msg) (p (if prepend "+0.0:" (format "~a[--]" (p-n-ex))) (epart ep% "la " #f) 'rezultatul gvn msg expcd))
(define (check-conds e gvn conds) (or (null? conds) (let ([r ((car conds) gvn)]) (if (eq? r #t) (check-conds e gvn (cdr conds)) (whenbad e gvn "" (or r "nu îndeplinește condiția"))))))
(define (check-part part per given main-test expected . conds) (let* ([e (list n-ex part per)] [p? (pair? (cdr main-test))] [p (if p? (car main-test) identity)] [t ((if p? cadr car) main-test)] [m ((if p? cddr cdr) main-test)]) (when (eq? #t (check-conds e given conds)) (if (t (p given) expected) (whengood e) (whenbad e (p given) expected m)))))
(define (check given main-test expected . conds) (apply check-part '- 1 given main-test expected conds))
(define the cons) (define is (cons equal? "diferă de cel așteptat")) (define in (cons member "nu se află printre variantele așteptate"))
(define same-set-as (cons (λ (x y) (apply equal? (map list->seteqv (list x y)))) "nu este aceeași mulțime cu"))
(define same-unique (cons (λ (x y) (and (apply = (map length (list x y))) ((car same-set-as) x y))) "nu sunt aceleași rezultate cu"))
(define (sumar) (when (and (not (null? default-returns)) (< show-defaults (length default-returns))) (p "... rezultatul implicit dat la" (cadr name-ex) (reverse default-returns))) (when (not nopoints) (p 'total: total 'puncte)))
(define (mark-helper) (printf "---~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p (car e-p) ': (cadr e-p) "puncte. total 1 -" (car e-p) ': (+ t (cadr e-p))) (+ t (cadr e-p))) 0 all) (newline))


(sunt 5 exerciții)

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))

(exercițiul 1 : 10 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .05 (counter-index C1) is 1)
  (check-part 'b .05 (counter-index C2) is 2)
  (check-part 'c .05 (counter-index C3) is 3)
  (check-part 'd .05 (counter-index C4) is 4)
  (check-part 'e .4 (map counter-tt (list C1 C2 C3 C4)) is (make-list 4 0))
  (check-part 'f .4 (map counter-queue (list C1 C2 C3 C4)) is (make-list 4 null))
)

(exercițiul 2 : 10 puncte)
(when (and (andmap struct? (list C1 C2 C3 C4))
           (andmap counter? (map (lambda (x) (tt+ x 0)) (list C1 C2 C3 C4))))
  (check-part 'a .25 (counter-tt (tt+ C1 1)) is 1)
  (check-part 'b .25 (counter-tt (tt+ (tt+ C1 3433) 5465)) is 8898)
  (check-part 'c .25 (counter-tt (tt+ (tt+ C1 232) 32)) is 264)
  (check-part 'd .25 (counter-tt (tt+ (tt+ (tt+ C1 7876) 98787) 3243)) is 109906)
)

(exercițiul 3 : 20 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .25 (min-tt (list (tt+ C1 1) (tt+ C2 32) C4)) is '(4 . 0))
  (check-part 'b .25 (min-tt (list (tt+ C1 1) (tt+ C2 32) (tt+ C3 32))) is '(1 . 1))
  (check-part 'c .25 (min-tt (list (tt+ C1 99999999999) (tt+ C2 9999999) (tt+ C3 999999999))) is '(2 . 9999999))
  (check-part 'd .25 (min-tt (list (tt+ C1 99999999999) (tt+ C2 9999999) (tt+ C3 999999999) (tt+ C4 42))) is '(4 . 42))
)

(exercițiul 4 : 20 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .5 (add-to-counter C3 'ana 11) is (make-counter 3 11 '((ana . 11))))
  (check-part 'b .5 (add-to-counter (add-to-counter (add-to-counter C2 'ana 11) 'gogu 4) 'leo 12) is
      (make-counter 2 27 '((ana . 11) (gogu . 4) (leo . 12))))
)

(exercițiul 5 : 60 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .125 (serve '((ana 3)) C1 C2 C3 C4) is (list
      (make-counter 1 3 '((ana . 3)))
      C2
      C3
      C4
    ))

  (check-part 'b .125 (serve '((ana 12)) C1 C2 C3 C4) is (list
    C1
    (make-counter 2 12 '((ana . 12)))
    C3
    C4
  ))

  (check-part 'c .125 (serve '((delay 1 10) (delay 2 5) (delay 2 7) (delay 4 2)) C1 C2 C3 C4) is (list
    (make-counter 1 10 '())
    (make-counter 2 12 '())
    C3
    (make-counter 4 2 '())
  ))

  (check-part 'd .125 (serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (dan 3)) C1 C2 C3 C4) is (list
    (make-counter 1 7 '((mia . 2) (gigi . 5)))
    (make-counter 2 12 '((ana . 12)))
    (make-counter 3 7 '((mara . 4) (dan . 3)))
    (make-counter 4 7 '((ion . 7)))
   ))

  (check-part 'e .125 (serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (delay 1 5) (delay 3 100) (lia 343) (dan 3)) C1 C2 C3 C4) is (list
    (make-counter 1 15 '((mia . 2) (gigi . 5) (dan . 3)))
    (make-counter 2 12 '((ana . 12)))
    (make-counter 3 104 '((mara . 4)))
    (make-counter 4 350 '((ion . 7) (lia . 343)))
  ))

  (check-part 'f .125 (serve '((ana 1) (mia 2) (mara 4) (ion 2332) (gigi 3232) (delay 1 5) (delay 3 10) (lia 32) (dan 33232)) C1 C2 C3 C4) is (list
    (make-counter 1 6 '((ana . 1)))
    (make-counter 2 3234 '((mia . 2) (gigi . 3232)))
    (make-counter 3 33278 '((mara . 4) (lia . 32) (dan . 33232)))
    (make-counter 4 2332 '((ion . 2332)))
  ))

  (check-part 'g .125 (serve '((ana 12) (mia 2) (mara 4) (ion 7) (gigi 5) (delay 1 5) (delay 3 10) (lia 3) (dan 3)) C1 C2 C3 C4) is (list
    (make-counter 1 12 '((mia . 2) (gigi . 5)))
    (make-counter 2 12 '((ana . 12)))
    (make-counter 3 14 '((mara . 4)))
    (make-counter 4 13 '((ion . 7) (lia . 3) (dan . 3)))
  ))

  (check-part 'h .125 (serve '((ana 12) (mia 2) (mara 4) (delay 1 2) (ion 7) (gigi 5) (delay 1 5) (delay 3 10) (lia 5) (dan 3)) C1 C2 C3 C4) is (list
   (make-counter 1 14 '((mia . 2) (gigi . 5)))
   (make-counter 2 15 '((ana . 12) (dan . 3)))
   (make-counter 3 14 '((mara . 4)))
   (make-counter 4 12 '((ion . 7) (lia . 5)))
  ))
)
(sumar)

; (displayln "Grila de notare")
; (mark-helper) ; afișează care este grila de notare, poate fi util în funcție de maniera de notare. NU trebuie inclus în sursă.
