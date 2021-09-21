#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index tt et state queue) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 #t empty-queue))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))

(define (update f counters index)
  (cond
   ((null? counters) counters)
   ((equal? null index) counters)
   ((> index  0)  (map (lambda (x) (if (equal? (counter-index x) index) (f x) x)) counters))
   (else counters)))

(define tt+
  (lambda (minutes)
    (lambda (C)  
      (make-counter (counter-index C) (+ (counter-tt C) minutes) (counter-et C) (counter-state C) (counter-queue C)))))


(define et+
  (lambda (minutes)
    (lambda (C)  
      (make-counter (counter-index C) (counter-tt C) (+ (counter-et C) minutes) (counter-state C)  (counter-queue C)))))

(define change-state
  (lambda (C)
    (make-counter (counter-index C) (counter-tt C) (counter-et C) #f (counter-queue C))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if
       (and(equal? (queue-size-l (counter-queue C)) 0) (equal? (queue-size-r (counter-queue C)) 0))
       (make-counter (counter-index C) (+ (counter-tt C) items) items  (counter-state C) (enqueue (cons name items) (counter-queue C)))
       (make-counter (counter-index C) (+ (counter-tt C) items) (counter-et C) (counter-state C) (enqueue (cons name items) (counter-queue C))))))



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


(define (remove-first C)
  (make-counter (counter-index C)
                (MySum (cdr (append (stream->list(queue-left (counter-queue C))) (queue-right (counter-queue C)))))
                (cond
                  ((equal? (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 1) 0)
                  ((equal? (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) (cdr (first(queue-right (counter-queue C)))))
                  ((> (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C)))  (cdr (second (stream->list(queue-left (counter-queue C)))))))
                (counter-state C)
                (dequeue (counter-queue C))))


(define (pass-time-through-counter minutes)
  (λ (C)
    (cond
      ((and (>= minutes (counter-tt C)) (>= minutes (counter-et C))) (make-counter (counter-index C) 0 0 (counter-state C) (counter-queue C)))
      ((and (< minutes (counter-tt C)) (>= minutes (counter-et C))) (make-counter (counter-index C) (- (counter-tt C) minutes) 0 (counter-state C) (counter-queue C)))
      ((and (>= minutes (counter-tt C)) (< minutes (counter-et C))) (make-counter (counter-index C) 0 (- (counter-et C) minutes) (counter-state C) (counter-queue C)))
      (else (make-counter (counter-index C)  (- (counter-tt C) minutes) (- (counter-et C) minutes) (counter-state C) (counter-queue C)))
      )))
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (serve requests fast-counters slow-counters)
  (newServe requests fast-counters slow-counters '()))


(define (newServe requests fast-counters slow-counters list-out)

     (define (add-people name n-items counters)
       (if
        (and (<= n-items 5) (<= (car (min-tt counters)) (length fast-counters)))
        (newServe(cdr requests) (update (lambda(x) ((add-to-counter name n-items) x)) fast-counters  (car (min-tt fast-counters))) slow-counters list-out)
        (newServe(cdr requests) fast-counters (update (lambda(x) ((add-to-counter name n-items) x)) slow-counters  (car (min-tt slow-counters))) list-out)))

;(filter (lambda (x) (not(queue-empty? (counter-queue x)))) fast-counters)
      ;(filter (lambda(x) (equal? (counter-state x) #t)) fast-counters)
  (define (pass-time-through-queue minutes C)
    (cond
       ((queue-empty? (counter-queue C)) ((pass-time-through-counter minutes) C))
       ((<= (counter-et C) minutes) (pass-time-through-queue (- minutes (counter-et C)) (remove-first C )))
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

  (define (count_delays index minutes)
    (if
     (<= index (length fast-counters)) (newServe (cdr requests) (update (lambda (x) ((tt+ minutes) x))
                                                                     (update (lambda (x) ((et+ minutes) x)) fast-counters  index)  index) slow-counters list-out)
     (newServe(cdr requests) fast-counters (update (lambda (x) ((tt+ minutes) x))
                                                 (update (lambda (x) ((et+ minutes) x)) slow-counters  index)  index)list-out)))

  (define (count-open-counters counters acc)
    (foldl (lambda (x acc)  (if (equal? (counter-state x) #t) (+ acc 1) acc)) acc counters))
  
  (define (sumi L)
    (if (null? L)
        0
        (+ (counter-tt (car L)) (sumi (cdr L)))))

  
  (define (get-open-counters counters)
    (filter (lambda(x) (equal? (counter-state x) #t)) counters))
  
  (define (ensure-time average slow-counters)
    (if
     (> (/ (sumi (get-open-counters(append fast-counters slow-counters))) (count-open-counters (append fast-counters slow-counters) 0)) average)
     (ensure-time average (append slow-counters (list (empty-counter (+ (length (append fast-counters slow-counters)) 1)))))
     (newServe(cdr requests) fast-counters slow-counters list-out)
    ))
  

  (define (closeNew index)
    (if (<= index (length fast-counters))
        (newServe (cdr requests) (update (lambda(x) (change-state x)) fast-counters index) slow-counters list-out)
        (newServe (cdr requests) fast-counters (update (lambda(x) (change-state x)) slow-counters index) list-out)
       ) 
    )

;(map (lambda (x) (if (equal? index (counter-index x))
 ;                                                        (make-counter (counter-index x) (counter-tt x) (counter-et x) #f (counter-queue x))
  ;                                                        x)) fast-counters )
  
     (if (null? requests)
      (cons list-out (append (map (lambda (x) (cons (counter-index x) (counter-queue x))) (filter (lambda (x) (not(queue-empty? (counter-queue x)))) fast-counters))
                             (map (lambda (x) (cons (counter-index x) (counter-queue x))) (filter (lambda (x) (not(queue-empty? (counter-queue x)))) slow-counters))))
      
      (match (car requests)
        [(list 'delay index minutes) (cons 0'())]
        [(list 'ensure average) (cons 0 '())]
        [(list 'close index)  (closeNew index)]
        [(list name n-items)  (add-people name n-items (get-open-counters (append fast-counters slow-counters)))]
        [minutes (pass-time minutes) ]
        
        
      ))
  )
;(count_delays index minutes)
(serve '((ana 10) 2 (leo 5) (maia 4) (dan 5) 1 (daria 5) 2 (anca 4) (close 2) 2 (clara 7) 1 (alin 13) 3 (sonia 5) 1 (adi 6) 2 (florin 7) 2 (horia 4) 1 (anda 5) (stela 7) (close 4) 2 (ema 14) 5 (alma 10))
                     (list C1 C2)
                     (list C3 C4))