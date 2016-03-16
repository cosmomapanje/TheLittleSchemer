(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define firsts
  (lambda (list)
  (cond ((null? list) (quote ()))
	(else (cons (car (car list)) (firsts (cdr list)))))))

(define insertR
  (lambda (new old l)
    (cond ((null? l) (quote ()))
	  (else 
	   (cond ((eq? (car l) old) (cons old (cons new (cdr l))))
		 (else (cons (car l) (insertR new old (cdr l)))))))))

(define insertL
  (lambda (new old l)
    (cond ((null? l) (quote ()))
	  (else
	   (cond ((eq? (car l) old) (cons new (cons old (cdr l))))
		 (else (cons (car l) (insertL new old (cdr l)))))))))

(define subst
  (lambda (new old l)
    (if (null? l) '()
	  (cons (subst-in-s-exp new old (car l))
		      (subst new old (cdr l))))))

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
	(if (eqv? sexp old) new sexp)
	(subst new old sexp))))

(define add1
  (lambda (m)
    (+ m 1)))

(define sub1
  (lambda (m)
    (- m 1)))

(define tup+
  (lambda (t1 t2)
    (cond ((null? t1) t2)
	  ((null? t2) t1)
	  (else 
	   (cons (+ (car t1) (car t2))
		 (tup+ 
		  (cdr t1) (cdr t2)))))))
;;; P72 ">"
(define >?
  (lambda (n m)
    (cond ((zero? n) #f)
	  ((zero? m) #t)
	  (else (>? (sub1 n) (sub1 m))))))

;;; P73 "<"
(define <?
  (lambda (n m)
    (cond ((zero? m) #f)
	  ((zero? n) #t)
	  (else (<? (sub1 n) (sub1 m))))))

;;; P74 "="
(define =?
  (lambda (n m)
    (cond ((zero? m) (zero? n))
	  ((zero? n) #f)
	  (else (=? (sub1 n) (sub1 m))))))

;; rewrite "=" using "<" and ">"
(define ==?
  (lambda (n m)
    (cond ((or (>? n m) (<? n m)) #f)
	  (else #t))))

;;; P74 "^"
(define ^
  (lambda (n m)
    (cond 
	  ((zero? m) 1)
	  (else (* n (^ n (sub1 m)))))))

;;; P75 "/"
(define div
  (lambda (n m)
    (cond ((<? n m) 0)
	  (else (add1 (div (- n m) m))))))

;;; P76 "length"
(define length
  (lambda (lst)
    (cond ((null? lst) 0)
	  (else (add1 (length (cdr lst)))))))

;;; P76 "pick"
(define pick
  (lambda (n lst)
    (cond ((null? lst) (quote ()))
	  ((zero? (sub1 n)) (car lst))
	  (else (pick (sub1 n) (cdr lst))))))

;;; P76 "rempick"
(define rempick
  (lambda (n lst)
    (cond ((null? lst) (quote ()))
	  ((zero? (sub1 n)) (cdr lst))
	  (else (cons (car lst) (rempick (sub1 n) (cdr lst)))))))
	   
;;; P77 "number?"

;;; P77 "no-nums"
(define no-nums
  (lambda (l)
    (cond ((null? l) (quote ()))
	  (else (cond ((number? (car l))
		       (no-nums (cdr l)))
		      (else (cons (car l)
				  (no-nums (cdr l)))))))))

;;; P78 "all-numsers)
(define all-numbers
  (lambda (l)
    (cond ((null? l) (quote ()))
	  (else (cond ((number? (car l))
		       (cons (car l) (all-numsers (cdr l))))
		      (else (all-numbers (cdr l))))))))

;;; P78 "eqan?"
(define eqan?
  (lambda (n1 n2)
    (cond ((and (number? n1) (number? n2)) (= n1 n2))
	  ((or (number? n1) (number? n2)) #f)
	  (else (eq? n1 n2)))))

;;; P78 "occur"
(define occur
  (lambda (n lst)
    (cond ((null? lst) 0)
	  (else (cond ((eqan? n (car lst)) (+ 1 (occur n (cdr lst))))
		      (else (+ (occur n (cdr lst)))))))))
