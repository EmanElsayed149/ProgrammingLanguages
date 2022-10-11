
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;;;;;;;;;;;;;;;1;;;;;;;;;;;;;;;;;;;;;
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride ))))


;;;;;;;;;;;;;;;; 2;;;;;;;;;;;;;;;;;;;;;;;;
(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix))  xs))

;;;;;;;;;;;;;;;;;;;;;;3 ;;;;;;;;;;;;;;;;
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number") ]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([f (lambda (counter rem ys)
                          (if (= counter rem)
                              (car ys)
                              (f (+ counter 1) rem (cdr ys))))])
              (f 0 (remainder n (length xs)) xs))]
        ))
  

;;;;;;;;;;;;;;;;;;;;;;; 4 ;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-for-n-steps stream n)
  (letrec ([checker (lambda (counter stream)
                   (if (> counter n ) null (cons (car (stream)) (checker (+ counter 1) (cdr (stream))))))])
    (checker 1 stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;n5;;;;;;;;;;;;;;;;;;;;;;;
(define funny-number-stream 
                             (letrec ([f (lambda (x)
                                         (if (= (remainder x 5) 0)
                                             (cons (- 0 x) (lambda() (f (+ x 1))))
                                             (cons x (lambda() (f (+ x 1))))))])
                               (lambda() (f 1))))


;;;;;;;;;;;;;;;;;; 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dan-then-dog
  (letrec ([f (lambda (s)
                (if (string=? s "dan.jpg")
                    (cons s (lambda () (f "dog.jpg")))
                    (cons s (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))




;;;;;;;;;;;;;;;;;;;;;;;;7;;;;;;;;;;;;;;;;;;;;;
(define (stream-add-zero stream)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda () (f (cdr (s)))) ))])
    (lambda() (f stream))))



;;;;;;;;;;;;;;;;;;;;8 ;;;;;;;;;;;;;;;;;;;;;;;;
(define (cycle-lists xs ys)
  (letrec ([f-final (lambda (x1 y1) (cons (cons (car x1) (car y1)) (lambda () (f (cdr x1) (cdr y1)))))] 
           [f (lambda (x y)
                (cond[(and (null? x) (null? y)) (f-final xs ys)]
                     [(null? x) (f-final xs y)]
                     [(null? y) (f-final x ys)]
                     [#t (f-final x y)]))])
    (lambda () (f xs ys))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; 9 ;;;;;;;;;;;;;;;;;;
(define (vector-assoc value v )
  (letrec ([f (lambda (steps) (cond [(= steps (vector-length v)) #f]
                                        [(pair? (vector-ref v steps)) (let* ([pair (vector-ref v steps) ]
                                                                            [key (car pair)]
                                                                            [val (cdr pair)])
                                                                         (if(equal? key value) pair (f (+ steps 1))))]
                                        [#t (f (+ steps 1))]))])
    (f 0)))

;;;;;;;;;;;;;;;;;;;;; 10 ;;;;;;;;;;;;;;;;;;;;;;;;
(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)]
             [cache-pos 0]
             [find (lambda (v)
                     (let ([value-from-cache (vector-assoc v cache )])
                       (if value-from-cache
                           value-from-cache
                           (let ([value-from-xs (assoc v xs)])
                             (if value-from-xs
                                 (begin (vector-set! cache cache-pos value-from-xs)
                                        (if (= (+ cache-pos 1) n)  (set! cache-pos 0) (set! cache-pos (+ 1 cache-pos)))
                                        value-from-xs)
                                 value-from-xs)))))])
      find))