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

;;; P79 "one?"
(define one?
  (lambda (n)
    (cond ((zero? n) #f)
	  (else (zero? (sub1 n))))))

;;; P79 rewrite "rempick-v2"
(define rempick-v2
  (lambda (n lst)
    (cond ((one? n) (cdr lst))
	  (else (cons (car lst) (rempick (sub1 n) (cdr lst)))))))


;; Chapter 5

;;;P81 "rember*"
(define rember*
  (lambda (a lst)
    (cond ((null? lst) (quote ()))
	  (else (cond ((atom? (car lst)) 
		       (cond ((eq? (car lst) a) 
			      (rember* a (cdr lst)))
			     (else (cons (car lst)
					 (rember* a (cdr lst))))))
		      (else (cons (rember* a (car lst))
				  (rember* a (cdr lst)))))))))

;;; P82 "insertR*"
(define insertR*
  (lambda (new old lst)
    (cond ((null? lst) (quote ()))
	  (else (cond ((atom? (car lst))
		       (cond ((eq? old (car lst))
			      (cons old (cons new
					      (insertR* new old (cdr lst)))))
			     (else (cons (car lst) 
					 (insertR* new old (cdr lst))))))
		      (else (cons (insertR* new old (car lst))
				  (insertR* new old (cdr lst)))))))))
		       
		       
;;; P84 "occur*"
(define occur*
  (lambda (a lst)
    (cond ((null? lst) 0)
	  ((atom? (car lst))
	   (cond 
	    ((eq? a (car lst))
	     (add1 (occur* a (cdr lst))))
	    (else (occur* a (cdr lst)))))
	  (else (+ (occur* a (car lst)) 
		   (occur* a (cdr lst)))))))

;;; P85 "subst*
(define subst*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
	  ((atom? (car l))
	   (cond 
	    ((eq? old (car l))
	     (cons new (subst* new old (cdr l))))
	    (else (cons (car l) (subst* new old (cdr l))))))
	  (else (cons (subst* new old (car l)) 
		      (subst* new old (cdr l)))))))
	   
;;; P86 "insertL*"
(define insertL*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
	  ((atom? (car l)) 
	   (cond ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
		 (else (cons (car l) (insertL* new old (cdr l))))))
	  (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
	 
;;; P87 "member*"
(define member*
  (lambda (a lst)
    (cond ((null? lst) #f)
	  ((atom? (car lst))
	   (or (eq? a (car lst)) (member* a (cdr lst))))
	  (else (or (member* a (car lst)) (member* a (cdr lst)))))))

;;; P87 "leftmost"
(define leftmost
  (lambda (lst)
    (cond 
     ((atom? (car lst)) (car lst))
     (else (leftmost (car lst))))))

;;; P91 "eqlist?"
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((and (null? l1) (atom? (car l2))) #f)
	  ((null? l1) #f)
	  ((and (atom? (car l1)) (null? l2)) #f)
	  ((and (atom? (car l1)) (atom? (car l2)))
	   (and (eqan? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2))))
	  ((atom? (car l1)) #f)
	  ((null? l2) #f)
	  ((atom? (car l2)) #f)
	  (else (and (eqlist? (car l1) (car l2))
		     (eqlist? (cdr l1) (cdr l2)))))))

;;; P92 "eqlist2?"
(define eqlist2?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  ((and (atom? (car l1)) (atom? (car l2)))
	   (and (eqan? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2))))
	  ((or (atom? (car l1)) (atom? (car l2)))
	   #f)
	  (else (and (eqlist2? (car l1) (car l2))
		     (eqlist2? (cdr l1) (cdr l2)))))))

;;; P92/93 "equal?"
(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	   (eqan? s1 s2))
	  ((or (atom? s1) (atom? s2)) #f)
	  (else (eqlist2? s1 s2)))))

;;; P93 "eqlist3?" using equal?
(define eqlist3?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  (else (and (equal? (car l1) (car l2))
		     (equal? (cdr l1) (cdr l2)))))))
		
;;; P94 "simply-rember"

;;; P100 "numbered?"
(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
	  ((eq? (car (cdr aexp)) (quote +))
	   (and (numbered? (car aexp)) 
		(numbered? (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote -))
	   (and (numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote *))
	   (and (numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote /))
	   (and (numbered? (car aexp))
		(numbered? (car (cdr (cdr aexp)))))))))

;;; P101 "simply-numbered?"
(define simply-numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
	  (else (and (numbered? (car aexp))
	       (numbered? (car (cdr (cdr aexp)))))))))

;;; P102 "value"
(define value
  (lambda (aexp)
    (cond ((atom? aexp) (cond ((number? aexp) aexp)))
	  ((eq? (car (cdr aexp)) (quote +))
	   (+ (value (car aexp))
	      (value (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote -))
	   (- (value (car aexp))
	      (value (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote *))
	   (* (value (car aexp))
	      (value (car (cdr (cdr aexp))))))
	  ((eq? (car (cdr aexp)) (quote /))
	   (/ (value (car aexp))
	      (value (car (cdr (cdr aexp)))))))))
