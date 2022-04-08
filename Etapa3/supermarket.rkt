#lang racket
(require racket/match)
(require racket/trace)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)




; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (counter index 0 0 empty-queue))

(define Q1 (make-queue '() '() 0 0))
(define Q2 (make-queue '(1 2) '(5 4 3) 2 3))
(define Q3 (make-queue '() '(3 2 1 0) 0 4))
(define Q4 (make-queue '(1) '() 1 0))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))


(define (update f counters index)
  (if (null? counters)
      '()
      (cond
        [(= index (counter-index (car counters))) (cons (f (car counters)) (update f (cdr counters) index))]
        [else (cons (car counters) (update f (cdr counters) index))])))

(define tt+
  (lambda (C)
    (lambda (minutes)
      (struct-copy counter C
                   [tt (+ (counter-tt C) minutes)]
                   ))))

(define et+
  (lambda (C)
    (lambda (minutes)
      (struct-copy counter C
                   [et (+ (counter-et C) minutes)]
                   ))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (struct-copy counter C
                     [tt (+ (counter-tt C) items)]
                     [et (if (zero? (counter-et C))
                             items
                             (counter-et C))]
                     [queue (enqueue  (cons name items) (counter-queue C) )]
                     )))

(define (min-tt-et counters flag)
  (if flag (min-tt counters) (min-et counters)))

(define (min-tt-helper counters acc)
  (cond
    [(null? counters) acc]
    [(< (counter-tt (car counters)) (counter-tt acc))
     (min-tt-helper (cdr counters) (car counters))]
    (else
     (min-tt-helper (cdr counters) acc))))

(define (minim-tt counters)
  (if (null? counters)
      #f
      (min-tt-helper (cdr counters) (car counters))))

(define (min-tt counters)
  (cons (counter-index (minim-tt counters)) (counter-tt (minim-tt counters))))

(define (min-et-helper counters acc)
  (cond
    [(null? counters) acc]
    [(< (counter-et (car counters)) (counter-et acc))
     (min-et-helper (cdr counters) (car counters))]
    (else
     (min-et-helper (cdr counters) acc))))

(define (minim-et counters)
  (if (null? counters)
      #f
      (min-et-helper (cdr counters) (car counters))))

(define (min-et counters)
  (cons (counter-index (minim-et counters)) (counter-et (minim-et counters))))


(define (remove-first-from-counter C)   ; testată de checker
  (if (not(queue-empty? (dequeue(counter-queue C))))
      (struct-copy counter C
                   [tt (- (counter-tt C) (counter-et C))]
                   [et (cdr(top(dequeue(counter-queue C))))]
                   [queue (dequeue (counter-queue C))])
      
      (struct-copy counter C
                   [tt 0]
                   [et 0]
                   [queue (dequeue (counter-queue C))])))


;(remove-first-from-counter (counter 4 42 22 (queue '() '((dan . 9) (gigi . 10) (ana . 1) (mara . 12)) 0 4)))
;(counter 4 20 1 (queue '((ana . 1) (gigi . 10) (dan . 9)) '() 3 0))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                     [tt (if (<= minutes (counter-tt C))
                             (- (counter-tt C) minutes)
                             0)]
                     [et (if (<= minutes (counter-et C))
                             (- (counter-et C) minutes)
                             0)]
                     )))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (index-to-counter index counters)
  (if (null? counters)
      (empty-counter 1)
      (cond
        [(= index (counter-index (car counters))) (car counters)]
        [else (index-to-counter index (cdr counters))])))


(define (serve-name-items requests fast-counters slow-counters name n-items customers)
  
  (if (<= n-items ITEMS)
      (if (member (index-to-counter (car (min-tt (append fast-counters slow-counters))) (append fast-counters slow-counters)) fast-counters)
          (real-serve requests
                 (update (λ (C) (struct-copy counter C
                                             [tt (+ (counter-tt C) n-items)]
                                             [et (if (queue-empty? (counter-queue C))
                                                     (if (zero? (counter-et C))
                                                         n-items
                                                         (+ (counter-et C) n-items))
                                                     (counter-et C))]
                                             [queue (enqueue  (cons name n-items) (counter-queue C) )]
                                             ))
                         fast-counters
                         (car (min-tt fast-counters)))
                 slow-counters
                 customers)
          (real-serve requests
                 fast-counters
                 (update (λ (C) (struct-copy counter C
                                             [tt (+ (counter-tt C) n-items)]
                                             [et (if (queue-empty? (counter-queue C))
                                                     (if (zero? (counter-et C))
                                                         n-items
                                                         (+ (counter-et C) n-items))
                                                     (counter-et C))]
                                             [queue (enqueue  (cons name n-items) (counter-queue C) )]
                                             ))
                         slow-counters
                         (car (min-tt slow-counters)))
                 customers))
      (real-serve requests
             fast-counters
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) n-items)]
                                         [et (if (queue-empty? (counter-queue C))
                                                 (if (zero? (counter-et C))
                                                     n-items
                                                     (+ (counter-et C) n-items))
                                                 (counter-et C))]
                                         [queue (enqueue  (cons name n-items) (counter-queue C) )]
                                         ))
                     slow-counters
                     (car (min-tt slow-counters)))
             customers)))

(define (serve-delay requests fast-counters slow-counters index minutes customers)
  'yes
  (if (member (index-to-counter index (append fast-counters slow-counters)) fast-counters)
      (real-serve requests
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) minutes)]
                                         [et (+ (counter-et C) minutes)]
                                         ))
                     fast-counters
                     index)
             slow-counters
             customers)
      (real-serve requests
             fast-counters
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) minutes)]
                                         [et (+ (counter-et C) minutes)]
                                         ))
                     slow-counters
                     index)
             customers)))

(define (sum counters acc)
  (if (null? counters)
      acc
      (if (pair? counters)
          (sum (cdr counters) (+ acc (counter-tt (car counters))))
          acc)))

(define (ttmed counters)
  (/ (sum counters 0) (length counters)))

(define (ttmed-maths counters average)
  (if (integer? (/ (sum counters 0) average))
      (quotient (sum counters 0) average)
      (add1(quotient (sum counters 0) average))))

(define (add-slow-counters requests fast-counters slow-counters count index customers)
                   (if (<= count 0)
                       (real-serve requests fast-counters slow-counters customers)
                       (add-slow-counters requests fast-counters (append slow-counters (list(empty-counter index))) (sub1 count) (add1 index) customers))) 

(define (serve-ensure requests fast-counters slow-counters average index customers)
  (add-slow-counters requests fast-counters slow-counters (- (ttmed-maths (append fast-counters slow-counters) average) (+ (length fast-counters) (length slow-counters))) (add1(+ (length fast-counters) (length slow-counters))) customers))
  ;(if (> (ttmed (append fast-counters slow-counters)) average)
   ;   (serve-ensure requests fast-counters (append slow-counters (list(empty-counter index))) average (add1 index))
    ;  (serve requests fast-counters slow-counters)))


(define (pass-time-through-counter-and-remove minutes)
  (λ (C)
    (struct-copy counter C
                     [tt (if (<= minutes (counter-tt C))
                             (- (counter-tt C) minutes)
                             0)]
                     [et (if (<= minutes (counter-tt C))
                             (- (counter-tt C) minutes)
                             0)]
                     [queue (if (not(queue-empty? (counter-queue C)))
                                (if (>= minutes (cdr(top(counter-queue C))))
                                    (dequeue (counter-queue C))
                                    (counter-queue C))
                                (counter-queue C))]
                     )))


(define (count-customers minutes customers counters)
  (if (null? counters)
      customers
      (if (not(queue-empty? (counter-queue (car counters))))
          (if (>= minutes (cdr(top(counter-queue (car counters)))))
              (count-customers minutes (cons (cons (counter-index (car counters)) (car(top(counter-queue (car counters))))) customers) (cdr counters))
              (count-customers minutes customers (cdr counters)))
          (count-customers minutes customers (cdr counters)))))


(define (get-customers x counters customers)
  (count-customers x customers counters))

(define (pass-time-through-counters x counters)
  (map (pass-time-through-counter-and-remove x) counters))

(define (serve-x requests fast-counters slow-counters x customers)
   (real-serve
    requests
    (pass-time-through-counters x fast-counters)
    (pass-time-through-counters x slow-counters)
    (filter (λ (l) (not(null? l))) (get-customers x (append fast-counters slow-counters) customers))))

(define (real-serve requests fast-counters slow-counters customers)
  (if (null? requests)
      (append (list customers) fast-counters slow-counters)
      (match (car requests)
        
        [(list 'ensure average)      (serve-ensure (cdr requests) fast-counters slow-counters average (add1 (+ (length fast-counters) (length slow-counters))) customers)]
        [(list name n-items)         (serve-name-items (cdr requests) fast-counters slow-counters name n-items customers)]
        [(list 'delay index minutes) (serve-delay (cdr requests) fast-counters slow-counters index minutes customers)]
        [x                           (serve-x (cdr requests) fast-counters slow-counters x customers)])))

(define (serve requests fast-counters slow-counters)
  (real-serve requests fast-counters slow-counters '()))
;
;(trace count-customers)
;(trace get-customers)
;(trace real-serve)
;(trace serve-x)

(serve '((lia 5) 3 (ana 2) 2 (mia 6) (geo 4) 5)
       (list C1 C2)
       (list C3 C4))
(list
 '((1 . lia) (2 . ana) (1 . geo))
 (counter 1 0 0 (queue '() '() 0 0))
 (counter 2 0 0 (queue '() '() 0 0))
 (counter 3 1 1 (queue '() '((mia . 6)) 0 1))
 (counter 4 0 0 (queue '() '() 0 0)))
