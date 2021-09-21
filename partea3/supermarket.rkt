#lang racket
(require racket/match)
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
  (make-counter index 0 0 empty-queue))


(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))

(define (update f counters index)
  (cond
   ((null? counters) counters)
   ((equal? null index) counters)
   ((> index  0)  (map (lambda (x) (if (equal? (counter-index x) index) (f x) x)) counters))
   (else counters)))

(define tt+
  (lambda (minutes)
    (lambda (C)  
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-queue C)))))

(define et+
  (lambda (minutes)
    (lambda (C)  
      (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-queue C)))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if
       (and(equal? (queue-size-l (counter-queue C)) 0) (equal? (queue-size-r (counter-queue C)) 0))
       (make-counter (counter-index C) (+ (counter-tt C) items) items  (enqueue (cons name items) (counter-queue C)))
       (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (enqueue (cons name items) (counter-queue C))))))



;(define functie-mai-abstracta-careia-ii-veti-da-un-nume-sugestiv
 ; 'your-code-here)
;(define min-tt 'your-code-here)
;(define min-et 'your-code-here)


(define (min-template func counters)
  (cond
    ((equal? (length counters) 0) (cons null null))
    ((null? (cdr counters)) (cons (counter-index (car counters)) (func counters)))
    ((let ((min (car counters)))
       (if
        (and (< (counter-index min) (counter-index (car (cdr counters)))) (<= (func counters) (func (cdr counters))))
        (min-template func (cons min (cdr(cdr counters))))
        (min-template func (cdr counters))))))
  )

(define (min-tt counters)
  (min-template aux-tt counters)
   )
(define (min-et counters)
  (min-template aux-et counters)
)

(define (aux-tt counters)
  (counter-tt (car counters))
  )
(define (aux-et counters)
  (counter-et (car counters))

  )


(define (MySum L)
  (if (null? L)
      0
      (+ (cdr (first L)) (MySum (cdr L)))))

(define (rev L)
  (if (null? L)
      L
      (append (rev (cdr L))
              (list (car L)))))

(define (remove-first-from-counter C)   ; testată de checker
  (cond 
    ((equal? (length (append (queue-left (counter-queue C)) (queue-right (counter-queue C)))) 1) (make-counter (counter-index C) 0 0 (dequeue (counter-queue C))))
    ((equal? (queue-size-l (counter-queue C)) 0)
                  (make-counter
                           (counter-index C)
                           (MySum (cdr (rev (queue-right (counter-queue C)))))
                           (cdr (second (rev (queue-right (counter-queue C)))))
                           (dequeue (counter-queue C))))

    ((> (length (append (queue-left (counter-queue C)) (queue-right (counter-queue C)))) 1)
                  (make-counter
                           (counter-index C)
                           (MySum (cdr (append (queue-left (counter-queue C)) (queue-right (counter-queue C)))))
                           (cdr (second (append (queue-left (counter-queue C)) (queue-right (counter-queue C)))))
                           (dequeue (counter-queue C))))
    ))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      ((and (>= minutes (counter-tt C)) (>= minutes (counter-et C))) (make-counter (counter-index C) 0 0 (counter-queue C)))
      ((and (< minutes (counter-tt C)) (>= minutes (counter-et C))) (make-counter (counter-index C) (- (counter-tt C) minutes) 0 (counter-queue C)))
      ((and (>= minutes (counter-tt C)) (< minutes (counter-et C))) (make-counter (counter-index C) 0 (- (counter-et C) minutes) (counter-queue C)))
      (else (make-counter (counter-index C)  (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-queue C)))
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
(define (serve requests fast-counters slow-counters)
  (newServe requests fast-counters slow-counters '()))



(define (newServe requests fast-counters slow-counters list-out)


  (define (add-people name n-items counters)
    (if
     (and (<= n-items 5) (<= (car (min-tt counters)) (length fast-counters)))
     (newServe(cdr requests) (update (lambda(x) ((add-to-counter name n-items) x)) fast-counters (car (min-tt fast-counters))) slow-counters list-out)
     (newServe(cdr requests) fast-counters (update (lambda(x) ((add-to-counter name n-items) x)) slow-counters (car (min-tt slow-counters))) list-out)))

  (define (count_delays index minutes)
    (if
     (<= index (length fast-counters)) (newServe (cdr requests) (update (lambda (x) ((tt+ minutes) x))
                                                                     (update (lambda (x) ((et+ minutes) x)) fast-counters  index)  index) slow-counters list-out)
     (newServe(cdr requests) fast-counters (update (lambda (x) ((tt+ minutes) x))
                                                 (update (lambda (x) ((et+ minutes) x)) slow-counters  index)  index)list-out)))

  (define (sumi L)
    (if (null? L)
        0
        (+ (counter-tt (car L)) (sumi (cdr L)))))
  
  (define (ensure-time average slow-counters)
    (if
     (> (/ (sumi (append fast-counters slow-counters)) (length (append fast-counters slow-counters))) average)
     (ensure-time average (append slow-counters (list (empty-counter (+ (length (append fast-counters slow-counters)) 1)))))
     (newServe(cdr requests) fast-counters slow-counters list-out)
    ))
  
  (define (pass-time-through-queue minutes C)
    (cond
       ((queue-empty? (counter-queue C)) ((pass-time-through-counter minutes) C))
       ((<= (counter-et C) minutes) (pass-time-through-queue (- minutes (counter-et C)) (remove-first-from-counter C)))
       ((equal? minutes 0) C)
       (else ((pass-time-through-counter minutes) C))))
     


  (define (get-list-out counters minutes list-out)
    (if (equal? minutes 0)
        list-out
        (get-list-out (crossing-counters 1 counters) (- minutes 1) (foldl (lambda (x list-out) (if (and (not (queue-empty? (counter-queue x))) (equal? (counter-et x) 1))
                                                                                                   (append list-out (list (cons (counter-index x) (car (top (counter-queue x))))))
                                                                                                   list-out)) list-out counters))))
  
  (define (crossing-counters minutes counters)
    (map (lambda (x) (pass-time-through-queue minutes x)) counters))

  (define (pass-time minutes)
    (newServe
            (cdr requests)
            (crossing-counters minutes fast-counters)
            (crossing-counters minutes slow-counters)
            (get-list-out (append fast-counters slow-counters) minutes list-out)))
  
   (if (null? requests)
      (cons list-out (append fast-counters slow-counters))
      (match (car requests)
        [(list 'delay index minutes) (count_delays index minutes)]
        [(list 'ensure average) (ensure-time average slow-counters)]
        [(list name n-items) (add-people name n-items (append fast-counters slow-counters)) ]
        [minutes (pass-time minutes) ]
        
       
      )))
