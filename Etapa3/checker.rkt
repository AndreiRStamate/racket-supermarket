#lang racket
(require "queue.rkt" "supermarket.rkt")

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


(sunt 10 exerciții)

(define Q1 (make-queue '() '() 0 0))
(define Q2 (make-queue '(1 2) '(5 4 3) 2 3))
(define Q3 (make-queue '() '(3 2 1 0) 0 4))
(define Q4 (make-queue '(1) '() 1 0))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))

(exercițiul 1 : 5 puncte)
(when (struct? empty-queue)
  (check-part 'a .25 (queue-left empty-queue) is '())
  (check-part 'b .25 (queue-right empty-queue) is '())
  (check-part 'c .25 (queue-size-l empty-queue) is 0)
  (check-part 'd .25 (queue-size-r empty-queue) is 0)
  )

(exercițiul 2 : 5 puncte)
(check-part 'a .25 (queue-empty? Q1) is #t)
(check-part 'b .25 (queue-empty? Q2) is #f)
(check-part 'c .25 (queue-empty? Q3) is #f)
(check-part 'd .25 (queue-empty? Q4) is #f)

(exercițiul 3 : 5 puncte)
(check-part 'a .25 (enqueue 'a Q1) is (queue '() '(a) 0 1))
(check-part 'b .25 (enqueue 'b Q2) is (queue '(1 2) '(b 5 4 3) 2 4))
(check-part 'c .25 (enqueue 'c Q3) is (queue '() '(c 3 2 1 0) 0 5))
(check-part 'd .25 (enqueue 'd Q4) is (queue '(1) '(d) 1 1))
  
(exercițiul 4 : 15 puncte)
(check-part 'a .2 (dequeue Q2) is (queue '(2) '(5 4 3) 1 3))
(check-part 'b .6 (dequeue Q3) is (queue '(1 2 3) '() 3 0))
(check-part 'c .2 (dequeue Q4) is (queue '() '() 0 0))
  
(exercițiul 5 : 10 puncte)
(check-part 'a .4 (top Q2) is 1)
(check-part 'b .6 (top Q3) is 0)
 
(exercițiul 6 : 5 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a .05 (counter-index C1) is 1)
  (check-part 'b .05 (counter-index C5) is 5)
  (check-part 'c .1 (map counter-tt (list C1 C2 C3 C4 C5)) is '(0 0 0 0 12))
  (check-part 'd .1 (map counter-et (list C1 C2 C3 C4 C5)) is '(0 0 0 0 8))
  (check-part 'e .7 (map counter-queue (list C1 C2 C3 C4 C5)) is
              (list empty-queue empty-queue empty-queue empty-queue
                    (queue '((remus . 6) (vivi . 4)) '() 2 0)))
  )

(exercițiul 7 : 5 puncte)
(when (andmap struct? (list C1))
  (check-part 'a .25 ((add-to-counter 'ana 10) C1) is
              (counter 1 10 10 (queue '() '((ana . 10)) 0 1)))
  (check-part 'b .25 ((add-to-counter 'bogdan 1) (counter 1 2 2 (queue '((ana . 2)) '() 1 0))) is
              (counter 1 3 2 (queue '((ana . 2)) '((bogdan . 1)) 1 1)))
  (check-part 'c .25 ((add-to-counter 'cristi 3) (counter 1 2 2 (queue '() '((ana . 2)) 0 1))) is
              (counter 1 5 2 (queue '() '((cristi . 3) (ana . 2)) 0 2)))
  (check-part 'd .25 ((add-to-counter 'dana 4) (counter 1 6 3 (queue '((ana . 2)) '((cristi . 2) (bogdan . 1)) 1 2))) is
              (counter 1 10 3 (queue '((ana . 2)) '((dana . 4) (cristi . 2) (bogdan . 1)) 1 3)))
  )

(exercițiul 8 : 10 puncte)
(check-part 'a .25 (remove-first-from-counter (counter 3 2 2 (queue '((mara . 2)) '() 1 0))) is
            (counter 3 0 0 empty-queue))
(check-part 'b .25 (remove-first-from-counter (counter 1 11 11 (queue '((mara . 7)) '() 1 0))) is
            (counter 1 0 0 empty-queue))
(check-part 'c .25 (remove-first-from-counter (counter 2 113 100 (queue '((ana . 100) (dan . 4) (gigi . 9)) '() 3 0))) is
            (counter 2 13 4 (queue '((dan . 4) (gigi . 9)) '() 2 0)))
(check-part 'd .25 (remove-first-from-counter (counter 4 42 22 (queue '() '((dan . 9) (gigi . 10) (ana . 1) (mara . 12)) 0 4))) is
            (counter 4 20 1 (queue '((ana . 1) (gigi . 10) (dan . 9)) '() 3 0)))
  

(exercițiul 9 : 10 puncte)
(when (andmap struct? (list C1))
  (check-part 'a .25 ((pass-time-through-counter 3) C1) is C1)
  (check-part 'b .25 ((pass-time-through-counter 10) (counter 1 4 4 empty-queue)) is
              (counter 1 0 0 empty-queue))
  (check-part 'c .25 ((pass-time-through-counter 3) (counter 1 7 4 (queue '((ana . 2)) '((bogdan . 3)) 1 1))) is
              (counter 1 4 1 (queue '((ana . 2)) '((bogdan . 3)) 1 1)))
  (check-part 'd .25 ((pass-time-through-counter 4) (counter 1 7 4 (queue '() '((bogdan . 3) (ana . 2)) 0 2))) is
              (counter 1 3 0 (queue '() '((bogdan . 3) (ana . 2)) 0 2)))
  )

(exercițiul 10 : 50 puncte)
(when (andmap struct? (list C1 C2 C3 C4))
  (check-part 'a 0.1
              (serve '(2 (ana 14) 6)
                     (list C1)
                     (list C2 C3)) is
                                   (list
                                    '()
                                    (counter 1 0 0 (queue '() '() 0 0))
                                    (counter 2 8 8 (queue '() '((ana . 14)) 0 1))
                                    (counter 3 0 0 (queue '() '() 0 0))))
  (check-part 'b 0.1
              (serve '(10)
                     (list C1)
                     (list (counter 2 12 5 (make-queue '() '((geo . 7) (lia . 5)) 0 2))
                           C3)) is
                                (list
                                 '((2 . lia))
                                 (counter 1 0 0 (queue '() '() 0 0))
                                 (counter 2 2 2 (queue '((geo . 7)) '() 1 0))
                                 (counter 3 0 0 (queue '() '() 0 0))))
  (check-part 'c 0.1
              (serve '((lia 5) 3 (ana 2) 2 (mia 6) (geo 4) 5)
                     (list C1 C2)
                     (list C3 C4)) is
                                   (list
                                    '((1 . lia) (2 . ana) (1 . geo))
                                    (counter 1 0 0 (queue '() '() 0 0))
                                    (counter 2 0 0 (queue '() '() 0 0))
                                    (counter 3 1 1 (queue '() '((mia . 6)) 0 1))
                                    (counter 4 0 0 (queue '() '() 0 0))))
  (check-part 'd 0.1
              (serve '((lia 5) 3 (ana 2) 2 (mia 6) (geo 4) (cristi 3) 5)
                     (list C1)
                     (list C2)) is
                                (list
                                 '((1 . lia) (2 . ana) (1 . geo))
                                 (counter 1 2 2 (queue '((cristi . 3)) '() 1 0))
                                 (counter 2 1 1 (queue '() '((mia . 6)) 0 1))))
  (check-part 'e 0.1
              (serve '(5 (adi 1) 2)
                     (list (counter 1 10 10 empty-queue))
                     (list (counter 2 6 6 empty-queue))) is
                                                         (list
                                                          '((2 . adi)) (counter 1 3 3 (queue '() '() 0 0))
                                                          (counter 2 0 0 (queue '() '() 0 0))))			
  (check-part 'f 0.1
              (serve '(5 (ensure 2) 2 (ensure 1) 2)
                     (list (counter 1 9 9 empty-queue))
                     (list (counter 2 6 6 empty-queue) C3 C4 C5)) is
                                                                  (list
                                                                   '((5 . remus))
                                                                   (counter 1 0 0 (queue '() '() 0 0))
                                                                   (counter 2 0 0 (queue '() '() 0 0))
                                                                   (counter 3 0 0 (queue '() '() 0 0))
                                                                   (counter 4 0 0 (queue '() '() 0 0))
                                                                   (counter 5 3 3 (queue '((vivi . 4)) '() 1 0))
                                                                   (counter 6 0 0 (queue '() '() 0 0))
                                                                   (counter 7 0 0 (queue '() '() 0 0))))
  (check-part 'g 0.1
              (serve '(5 (delay 1 5) 2 (delay 2 3))
                     (list (counter 1 10 10 empty-queue))
                     (list (counter 2 6 6 empty-queue))) is
                                                         (list
                                                          '()
                                                          (counter 1 8 8 (queue '() '() 0 0))
                                                          (counter 2 3 3 (queue '() '() 0 0))))
  (check-part 'h 0.1
              (serve '((ana 1) 2 (delay 1 5) 3 (delay 2 2) (mia 2) 1 (diana 10) 1 
                               (ensure 3) 4 (delay 1 12) (ion 2) 5 (gabi 8)
                               (ensure 3) (lia 10) (dan 3) (ada 7) 4 (ensure 3))
                     (list C1)
                     (list C2 C3 C4)) is
                                      (list
                                       '((1 . ana) (3 . mia) (2 . ion) (4 . diana) (4 . dan))
                                       (counter 1 3 3 (queue '() '() 0 0))
                                       (counter 2 4 4 (queue '() '((gabi . 8)) 0 1))
                                       (counter 3 6 6 (queue '() '((lia . 10)) 0 1))
                                       (counter 4 0 0 (queue '() '() 0 0))
                                       (counter 5 3 3 (queue '() '((ada . 7)) 0 1))
                                       (counter 6 0 0 (queue '() '() 0 0))))
  (check-part 'i 0.1
              (serve '((ana 12) 4 (mia 2) 1 (mara 4) 1 (ensure 10) 10 (ion 7) 5 (gigi 5) 2
                                (delay 1 5) 3 (delay 3 10) 5 (ensure 10) (lia 3) (dan 3) (ada 7)
                                (nera 4) (ensure 5) (milu 8) (leo 1) 5)
                     (list C1)
                     (list C2 C3 C4)) is
                                      (list
                                       '((1 . mia) (3 . mara) (2 . ana) (2 . ion) (1 . gigi) (1 . lia) (2 . dan) (2 . leo))
                                       (counter 1 2 2 (queue '((nera . 4)) '() 1 0))
                                       (counter 2 0 0 (queue '() '() 0 0))
                                       (counter 3 0 0 (queue '() '() 0 0))
                                       (counter 4 2 2 (queue '() '((ada . 7)) 0 1))
                                       (counter 5 3 3 (queue '() '((milu . 8)) 0 1))))
  (check-part 'j 0.1
              (serve '(3 (ana 13) 2 (mia 2) 1 (mara 1) 1 (ensure 3) (ion 5) 2 (gigi 6) 4
                         (delay 1 8) (delay 3 10) (ensure 7) (lia 6) 1 (dan 7) 1 (ada 4)
                         (nera 4) 1 (ensure 7) (leo 4) 3)
                     (list C1 C2)
                     (list C3)) is
                                (list
                                 '((1 . mia) (2 . mara) (1 . ion) (2 . ada) (4 . lia))
                                 (counter 1 2 2 (queue '() '() 0 0))
                                 (counter 2 4 4 (queue '((nera . 4)) '() 1 0))
                                 (counter 3 13 7 (queue '() '((gigi . 6) (ana . 13)) 0 2))
                                 (counter 4 7 7 (queue '((dan . 7)) '() 1 0))
                                 (counter 5 1 1 (queue '() '((leo . 4)) 0 1))
                                 (counter 6 0 0 (queue '() '() 0 0))))
  )

(sumar)