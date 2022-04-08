#lang racket
(require racket/match)
(require racket/trace)
(require racket/math)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (counter index 0 0 '()))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 '((remus . 6) (vivi . 4))))

; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (if (null? counters)
      '()
      (cond
        [(= index (counter-index (car counters))) (cons (f (car counters)) (update f (cdr counters) index))]
        [else (cons (car counters) (update f (cdr counters) index))])))

; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define tt+
  (lambda (C)
    (lambda (minutes)
      (struct-copy counter C
                   [tt (+ (counter-tt C) minutes)]
                   ))))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (lambda (C)
    (lambda (minutes)
      (struct-copy counter C
                   [et (+ (counter-et C) minutes)]
                   ))))

; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define add-to-counter
  (lambda (C)
    (lambda (name)
      (lambda (n-items)
        (struct-copy counter C
                     [tt (+ (counter-tt C) n-items)]
                     [et (+ (counter-et C) n-items)]
                     [queue (append (counter-queue C) (list (cons name n-items)))]
                     )))))

(define (add-to-counter-fara-lambde C name n-items)
  (struct-copy counter C
               [tt (+ (counter-tt C) n-items)]
               [et (+ (counter-et C) n-items)]
               [queue (append (counter-queue C) (list (cons name n-items)))]
               ))
  


;(((add-to-counter C3) 'ana) 11)
;(make-counter 3 11 11 '((ana . 11)))

; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

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

(define (min-et-non-zero-helper counters acc)
  (cond
    [(null? counters) acc]
    [(< (counter-et (car counters)) (counter-et acc))
     (if (zero? (counter-et (car counters)))
         (min-et-non-zero-helper (cdr counters) acc)
         (if (equal? (counter-queue (car counters)) '())
             (min-et-non-zero-helper (cdr counters) acc)
             (min-et-non-zero-helper (cdr counters) (car counters))))]
    [else
     (min-et-non-zero-helper (cdr counters) acc)])
  
  )



(define (greater-than-zero counters)
  (cond
    [(null? counters) (empty-counter 1)]
    [(> (counter-et (car counters)) 0)
     (if (equal? (counter-queue (car counters)) '())
         (greater-than-zero (cdr counters))
         (car counters))]
    [else (greater-than-zero (cdr counters))]))


(define (minim-et-non-zero counters)
  (if (null? counters)
      #f
      (min-et-non-zero-helper (cdr counters) (greater-than-zero counters))))

(define (min-et-non-zero counters)
  (cons (counter-index (minim-et-non-zero counters)) (counter-et (minim-et-non-zero counters))))


;(min-et-non-zero (list C1 C5))

; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (if (not(null? (cdr (counter-queue C))))
      (struct-copy counter C
                   [tt (- (counter-tt C) (counter-et C))]
                   [et (cdr (car (cdr(counter-queue C))))]
                   [queue (cdr(counter-queue C))])
      (struct-copy counter C
                   [tt 0]
                   [et 0]
                   [queue (cdr(counter-queue C))])))


;  (remove-first-from-counter (counter 3 2 2 '((mara . 2))))
;(counter 3 0 0 '())
;  (remove-first-from-counter (counter 1 11 11 '((mara . 7))))
;(counter 1 0 0 '())
;  (remove-first-from-counter (make-counter 2 113 100 '((ana . 100) (dan . 4) (gigi . 9))))
;(make-counter 2 13 4 '((dan . 4) (gigi . 9)))
;  (remove-first-from-counter (make-counter 4 42 22 '((mara . 12) (ana . 1) (gigi . 10) (dan . 9))))
;(make-counter 4 20 1 '((ana . 1) (gigi . 10) (dan . 9)))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)

;(((add-to-counter C3) 'ana) 11)

(define (index-to-counter index counters)
  (if (null? counters)
      (empty-counter 1)
      (cond
        [(= index (counter-index (car counters))) (car counters)]
        [else (index-to-counter index (cdr counters))])))


(define (serve-name-items requests fast-counters slow-counters name n-items)
  
  (if (<= n-items ITEMS)
      (if (member (index-to-counter (car (min-tt (append fast-counters slow-counters))) (append fast-counters slow-counters)) fast-counters)
          (serve requests
                 (update (λ (C) (struct-copy counter C
                                             [tt (+ (counter-tt C) n-items)]
                                             [et (if (null? (counter-queue C))  (if (zero? (counter-et C)) n-items (+ (counter-et C) n-items)) (counter-et C))]
                                             [queue (append (counter-queue C) (list (cons name n-items)))]
                                             ))
                         fast-counters
                         (car (min-tt fast-counters)))
                 slow-counters)
          (serve requests
                 fast-counters
                 (update (λ (C) (struct-copy counter C
                                             [tt (+ (counter-tt C) n-items)]
                                             [et (if (null? (counter-queue C))  (if (zero? (counter-et C)) n-items (+ (counter-et C) n-items)) (counter-et C))]
                                             [queue (append (counter-queue C) (list (cons name n-items)))]
                                             ))
                         slow-counters
                         (car (min-tt slow-counters)))
                 ))
      (serve requests
             fast-counters
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) n-items)]
                                         [et (if (null? (counter-queue C))  (if (zero? (counter-et C)) n-items (+ (counter-et C) n-items)) (counter-et C))]
                                         [queue (append (counter-queue C) (list (cons name n-items)))]
                                         ))
                     slow-counters
                     (car (min-tt slow-counters))))))

(define (serve-delay requests fast-counters slow-counters index minutes)
  (if (member (index-to-counter index (append fast-counters slow-counters)) fast-counters)
      (serve requests
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) minutes)]
                                         [et (+ (counter-et C) minutes)]
                                         ))
                     fast-counters
                     index)
             slow-counters)
      (serve requests
             fast-counters
             (update (λ (C) (struct-copy counter C
                                         [tt (+ (counter-tt C) minutes)]
                                         [et (+ (counter-et C) minutes)]
                                         ))
                     slow-counters
                     index))))

(define (serve-remove-first requests fast-counters slow-counters)
  (if (equal? '() (counter-queue (index-to-counter (car (min-et-non-zero (append fast-counters slow-counters))) (append fast-counters slow-counters))))
      (serve requests fast-counters slow-counters)
      (if (member (index-to-counter (car (min-et-non-zero (append fast-counters slow-counters))) (append fast-counters slow-counters)) fast-counters)
      
          (serve requests
                 (update (λ (C) (if (not(null? (cdr (counter-queue C))))
                                    (struct-copy counter C
                                                 [tt (- (counter-tt C) (counter-et C))]
                                                 [et (cdr (car (cdr(counter-queue C))))]
                                                 [queue (cdr(counter-queue C))])
                                    (struct-copy counter C
                                                 [tt 0]
                                                 [et 0]
                                                 [queue (cdr(counter-queue C))])))
                             
                         fast-counters
                         (car (min-et-non-zero fast-counters)))
                 slow-counters)
          (serve requests
                 fast-counters
                 (update (λ (C) (if (not(null? (cdr (counter-queue C))))
                                    (struct-copy counter C
                                                 [tt (- (counter-tt C) (counter-et C))]
                                                 [et (cdr (car (cdr(counter-queue C))))]
                                                 [queue (cdr(counter-queue C))])
                                    (struct-copy counter C
                                                 [tt 0]
                                                 [et 0]
                                                 [queue (cdr(counter-queue C))])))
                         slow-counters
                         (car (min-et-non-zero slow-counters))))
          )))

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

(define (add-slow-counters requests fast-counters slow-counters count index)
                   (if (<= count 0)
                       (serve requests fast-counters slow-counters)
                       (add-slow-counters requests fast-counters (append slow-counters (list(empty-counter index))) (sub1 count) (add1 index)))) 

(define (serve-ensure requests fast-counters slow-counters average index)
  (add-slow-counters requests fast-counters slow-counters (- (ttmed-maths (append fast-counters slow-counters) average) (+ (length fast-counters) (length slow-counters))) (add1(+ (length fast-counters) (length slow-counters)))))
  ;(if (> (ttmed (append fast-counters slow-counters)) average)
   ;   (serve-ensure requests fast-counters (append slow-counters (list(empty-counter index))) average (add1 index))
    ;  (serve requests fast-counters slow-counters)))
                                             

(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (serve-ensure (cdr requests) fast-counters slow-counters average (add1 (+ (length fast-counters) (length slow-counters))))]
        [(list name n-items)         (serve-name-items (cdr requests) fast-counters slow-counters name n-items)]
        [(list 'delay index minutes) (serve-delay (cdr requests) fast-counters slow-counters index minutes)]
        [(list remove-first) (serve-remove-first (cdr requests) fast-counters slow-counters)])))
