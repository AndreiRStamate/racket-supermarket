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


(sunt 6 exerciții)

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

; TODO: modificat punctaje

(exercițiul 1 : 5 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .05 (counter-index C1) is 1)
  (check-part 'b .05 (counter-index C2) is 2)
  (check-part 'c .05 (counter-index C3) is 3)
  (check-part 'd .05 (counter-index C5) is 5)
  (check-part 'e .2 (map counter-tt (list C1 C2 C3 C4 C5)) is '(0 0 0 0 12))
  (check-part 'f .3 (map counter-et (list C1 C2 C3 C4 C5)) is '(0 0 0 0 8))
  (check-part 'g .3 (map counter-queue (list C1 C2 C3 C4 C5)) is '(() () () () ((remus . 6) (vivi . 4))))
)

(exercițiul 2 : 20 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .2 (update (lambda (x) x) '() 1) is '())
  (check-part 'b .2 (update (lambda (x) C3) (list C1 C2 C3 C4) -1) is (list C1 C2 C3 C4))
  (check-part 'c .3 (update (lambda (x) C2) (list C1 C2 C3 C4 C5) 4) is (list C1 C2 C3 C2 C5))
  (check-part 'd .3 (update (lambda (x) (struct-copy counter x [tt 20])) (list C1 C2 C3) 3) is (list C1 C2 (counter 3 20 0 '())))
)

(exercițiul 3 : 7.5 puncte)
(when (and (procedure? min-tt) (andmap struct? (list C1 C2 C3 C4)))
  (check-part 'a .25 (min-tt (list (counter 3 2 2 '(()))
                                   (counter 4 6 1 '((ana . 1) (geo 5))))) is '(3 . 2))
  (check-part 'b .25 (min-tt (list (counter 1 3 1 '((dan . 1) (lia . 2)))
                                   (counter 2 9 5 '((ana . 1) (geo . 3) (ema . 1)))
                                   (counter 3 2 2 '((leo . 2))))) is '(3 . 2))
  (check-part 'c .25 (min-tt (list (counter 1 12 4 '((mara . 4) (dan . 3) (gigi . 5)))
                                   (counter 2 9 4 '((florin . 3) (ana . 5)))
                                   (counter 3 4 4 '((mia . 4)))
                                   (counter 4 15 10 '((eliza . 1) (gabi . 3) (cristi . 2))))) is '(3 . 4))
  (check-part 'd .25 (min-tt (list (counter 1 12 4 '((mara . 4) (dan . 3) (gigi . 5)))
                                   (counter 2 10 8 '((florin . 8) (ana . 2)))
                                   (counter 3 10 4 '((mia . 2) (nicu . 6)))
                                   (counter 4 15 10 '((eliza . 1) (gabi . 3) (cristi . 2))))) is '(2 . 10))
)

(exercițiul 4 : 7.5 puncte)
(when (and (procedure? min-et) (andmap struct? (list C1 C2 C3 C4)))
  (check-part 'a .25 (min-et (list (counter 3 2 2 '(()))
                                   (counter 4 6 1 '((ana . 1) (geo 5))))) is '(4 . 1))
  (check-part 'b .25 (min-et (list (counter 1 3 1 '((dan . 1) (lia . 2)))
                                   (counter 2 9 5 '((ana . 1) (geo . 3) (ema . 1)))
                                   (counter 3 2 2 '((leo . 2))))) is '(1 . 1))
  (check-part 'c .25 (min-et (list (counter 1 12 4 '((mara . 4) (dan . 3) (gigi . 5)))
                                   (counter 2 9 4 '((florin . 3) (ana . 5)))
                                   (counter 3 4 4 '((mia . 4)))
                                   (counter 4 15 10 '((eliza . 1) (gabi . 3) (cristi . 2))))) is '(1 . 4))
  (check-part 'd .25 (min-et (list (counter 1 12 4 '((mara . 4) (dan . 3) (gigi . 5)))
                                   (counter 2 10 8 '((florin . 8) (ana . 2)))
                                   (counter 3 10 4 '((mia . 2) (nicu . 6)))
                                   (counter 4 15 10 '((eliza . 1) (gabi . 3) (cristi . 2))))) is '(1 . 4))
)

(exercițiul 5 : 20 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .25 (remove-first-from-counter (counter 3 2 2 '((mara . 2)))) is (counter 3 0 0 '()))
  (check-part 'b .25 (remove-first-from-counter (counter 1 11 11 '((mara . 7)))) is (counter 1 0 0 '()))
  (check-part 'c .25 (remove-first-from-counter (make-counter 2 113 100 '((ana . 100) (dan . 4) (gigi . 9)))) is (make-counter 2 13 4 '((dan . 4) (gigi . 9))))
  (check-part 'd .25 (remove-first-from-counter (make-counter 4 42 22 '((mara . 12) (ana . 1) (gigi . 10) (dan . 9)))) is (make-counter 4 20 1 '((ana . 1) (gigi . 10) (dan . 9))))
)

(exercițiul 6 : 60 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  ; Test doar cu add.
  (check-part 'a (/ 1. 12)
    (serve '((ana 6) (bogdan 2) (cristi 4) (dana 4) (eliza 2) (florin 6) (gabi 1))
           (list C1 C2)
           (list C3 C4)) is
    (list (counter 1 5 2 '((bogdan . 2) (eliza . 2) (gabi . 1)))
          (counter 2 4 4 '((cristi . 4)))
          (counter 3 6 6 '((ana . 6)))
          (counter 4 10 4 '((dana . 4) (florin . 6)))))
  ; Test doar cu delay.
  (check-part 'b (/ 1. 12)
    (serve '((delay 3 11) (delay 2 2) (delay 2 4) (delay 5 10) (delay 3 1))
           (list C1 C2)
           (list C3 C4 C5)) is
    (list C1
          (counter 2 6 6 '())
          (counter 3 12 12 '())
          C4
          (counter 5 22 18 '((remus . 6) (vivi . 4)))))
  ; Test cu add și delay.
  (check-part 'c (/ 1. 12)
    (serve '((delay 1 4) (ana 1) (bogdan 4) (cristi 6) (dana 1) (eliza 2) (delay 3 3) (florin 1))
           (list C1)
           (list C2 C3 C4 C5)) is
    (list (counter 1 5 5 '((florin . 1)))
          (counter 2 4 1 '((ana . 1) (dana . 1) (eliza . 2)))
          (counter 3 7 7 '((bogdan . 4)))
          (counter 4 6 6 '((cristi . 6)))
          C5))
  ; Test cu add și remove.
  (check-part 'd (/ 1. 12)
    (serve '((ana 10) (bogdan 5) (cristi 5) (remove-first) (remove-first) (remove-first))
           (list C1)
           (list C2 C3 C4 C5)) is
    (list (counter 1 0 0 '())
          (counter 2 10 10 '((ana . 10)))
          (counter 3 0 0 '())
          (counter 4 0 0 '())
          (counter 5 4 4 '((vivi . 4)))))
  ; Test cu remove cu toate cozile goale.
  (check-part 'e (/ 1. 12)
    (serve '((remove-first) (ana 1) (delay 2 3) (delay 1 5) (remove-first) (delay 1 4) (remove-first))
           (list C1)
           (list C2)) is
    (list (counter 1 4 4 '())
          (counter 2 3 3 '())))
  ; Test cu remove și delay.
  (check-part 'f (/ 1. 12)
    (serve '((delay 1 5) (remove-first) (delay 2 10) (remove-first))
           (list C1 (counter 2 5 5 '((leo . 5))))
           (list (counter 3 13 4 '((ana . 3) (bogdan . 8) (lia . 1))) C4 C5)) is
     (list (counter 1 5 5 '())
           (counter 2 15 15 '((leo . 5)))
           (counter 3 1 1 '((lia . 1)))
           C4
           C5))
  ; Test doar cu ensure.
  (check-part 'g (/ 1. 12)
    (serve '((ensure 10) (ensure 5))
           (list C1)
           (list (counter 2 12 12 '((ana . 12))) (counter 3 10 6 '((geo . 6) (mia . 4))) (counter 4 6 6 '()))) is
     (list C1
           (counter 2 12 12 '((ana . 12)))
           (counter 3 10 6 '((geo . 6) (mia . 4)))
           (counter 4 6 6 '())
           (counter 5 0 0 '())
           (counter 6 0 0 '())))
  ; Test cu add și ensure.
  (check-part 'h (/ 1. 12)
    (serve '((ana 10) (bogdan 1) (cristi 4) (dana 3) (eliza 5) (ensure 4))
           (list C1)
           (list C2 C3)) is
     (list (counter 1 9 1 '((bogdan . 1) (dana . 3) (eliza . 5)))
           (counter 2 10 10 '((ana . 10)))
           (counter 3 4 4 '((cristi . 4)))
           (counter 4 0 0 '())
           (counter 5 0 0 '())
           (counter 6 0 0 '())))
  ; Test cu add, delay și ensure.
  (check-part 'i (/ 1. 12)
    (serve '((delay 2 3) (ana 2) (bogdan 3) (ensure 2) (cristi 5)
             (delay 1 3) (dana 3) (delay 3 5) (eliza 7) (ensure 5))
           (list C1)
           (list C2 C3)) is
     (list (counter 1 5 5 '((ana . 2)))
           (counter 2 6 6 '((dana . 3)))
           (counter 3 8 8 '((bogdan . 3)))
           (counter 4 12 5 '((cristi . 5) (eliza . 7)))
           (counter 5 0 0 '())
           (counter 6 0 0 '())
           (counter 7 0 0 '()))))
  ; Teste cu toate operațiile.
  (check-part 'j (/ 1. 12)
    (serve '((delay 2 1) (ana 2) (bogdan 3) (remove-first) (ensure 2) (cristi 4)
             (delay 1 3) (dana 3) (remove-first) (delay 3 5) (eliza 7) (ensure 5))
           (list C1)
           (list C2 C3)) is
     (list (counter 1 7 7 '((cristi . 4)))
           (counter 2 11 4 '((dana . 3) (eliza . 7)))
           (counter 3 5 5 '())
           (counter 4 0 0 '())
           (counter 5 0 0 '())))
  (check-part 'k (/ 1. 12)
    (serve '((delay 2 1) (ana 2) (bogdan 3) (remove-first) (ensure 2) (cristi 4)
             (delay 1 3) (dana 3) (remove-first) (delay 3 5) (eliza 7) (ensure 5))
           (list C1 C2 C3)
           (list C4 C5)) is
     (list
           (counter 1 7 7 '((cristi . 4)))
           (counter 2 1 1 '())
           (counter 3 5 5 '())
           (counter 4 3 3 '((dana . 3)))
           (counter 5 12 8 '((remus . 6) (vivi . 4)))
           (counter 6 7 7 '((eliza . 7)))
           (counter 7 0 0 '())
           (counter 8 0 0 '())))
  (check-part 'l (/ 1. 12)
    (serve '((delay 1 3) (ana 2) (bogdan 3) (remove-first) (ensure 3) (cristi 6)
             (delay 1 3) (dana 3) (remove-first) (eliza 7) (ensure 7))
           (list C1)
           (list C2)) is
     (list
           (counter 1 9 9 '((dana . 3)))
           (counter 2 13 6 '((cristi . 6) (eliza . 7)))
           (counter 3 0 0 '())
           (counter 4 0 0 '())))

(sumar)