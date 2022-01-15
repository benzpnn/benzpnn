;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |find possible x solution for cubic congruence|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; this program is assuming x0 <= x <= xn
;;!!! please remember to check if your solutions are within bounds
(define (cubic-cong a b c d e x0 xn) ;*
  ; (ax^3 + bx^2 + cx + d) (mod e) = 0
  ; x0 and xn are your lower bound and higher bound
  (local
    [(define (listrange x0 xn)
       (cond
         [(equal? x0 xn)
          (list x0)]
         [else (cons x0 (listrange (add1 x0) xn))]))
     (define lon (listrange x0 xn))
     (define (f x)
       (+ (* a (expt x 3))(* b (expt x 2))(* c x) d)) ;*
     (define (modulolist lon)
       (cond
          [(empty? lon)empty]
          [(zero? (modulo (f (first lon)) e)) ;*
           (cons (first lon)(modulolist (rest lon)))]
          [else (modulolist (rest lon))]))]
    (modulolist lon)))

(check-expect (cubic-cong 513 262148 4100 0 8 0 8)(list 0 2 4 6 8))

;;if want to calculate higher power equation，make arrangement at where "*" are.

;;*************** quadratic-congruence-calculator***********
(define (quadratic-cong a b c d x0 xn)
  ; (ax^2 + bx + c)(mod d) = 0
  ; x0 and xn are your lower bound and higher bound
  (local
    [(define (listrange x0 xn)
       (cond
         [(equal? x0 xn)
          (list x0)]
         [else (cons x0 (listrange (add1 x0) xn))]))
     (define lon (listrange x0 xn))
     (define (f x)
       (+ (* a (expt x 2))(* b (expt x 1)) c))
     (define (modulolist lon)
       (cond
          [(empty? lon)empty]
          [(zero? (modulo (f (first lon)) d))
           (cons (first lon)(modulolist (rest lon)))]
          [else (modulolist (rest lon))]))]
    (modulolist lon)))

(check-expect (quadratic-cong 65 514 65 8 0 8)(list 3 7))

(define (mono-cong a b c x0 xn)
  ; (ax^2 + bx + c)(mod d) = 0
  ; x0 and xn are your lower bound and higher bound
  (local
    [(define (listrange x0 xn)
       (cond
         [(equal? x0 xn)
          (list x0)]
         [else (cons x0 (listrange (add1 x0) xn))]))
     (define lon (listrange x0 xn))
     (define (f x)
       (+ (* a x) b))
     (define (modulolist lon)
       (cond
          [(empty? lon)empty]
          [(zero? (modulo (f (first lon)) c))
           (cons (first lon)(modulolist (rest lon)))]
          [else (modulolist (rest lon))]))]
    (modulolist lon)))
