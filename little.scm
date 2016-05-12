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
;;; P104 "new-value"
;;; first try new arithmetic expression like this (+ 1 3)
(define new-value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (car nexp) (quote +))
	   (+ (new-value (cdr nexp))
	      (new-value (cdr (cdr nexp)))))
	  ((eq? (car nexp) (quote -))
	   (- (new-value (cdr nexp))
	      (new-value (cdr (cdr nexp)))))
	  ((eq? (car nexp) (quote *))
	   (* (new-value (cdr nexp))
	      (new-value (cdr (cdr nexp)))))
	  ((eq? (car nexp) (quote /))
	   (/ (new-value (cdr nexp) (quote /))
	      (new-value (cdr (cdr nexp))))))))
		
;;; P105 "first-exp"
(define first-exp
  (lambda (aexp)
    (car (cdr aexp))))

;;; P106 "second-exp"
(define second-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;;; P106 "operator"
(define operator
  (lambda (aexp)
    (car aexp)))

;;; P106 "new-value2"
(define new-value2
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (operator nexp) (quote +))
	   (+ (new-value2 (first-exp nexp)) (new-value2 (second-exp nexp))))
	  ((eq? (operator nexp) (quote -))
	   (- (new-value2 (first-exp nexp)) (new-value2 (second-exp nexp))))
	  ((eq? (operator nexp) (quote *))
	   (* (new-value2 (first-exp nexp)) (new-value2 (second-exp nexp))))
	  ((eq? (operator nexp) (quote /))
	   (/ (new-value2 (first-exp nexp)) (new-value2 (second-exp nexp)))))))

;;; P108 "sero?"
(define sero?
  (lambda (n)
    (null? n)))

;;; P108 "edd1"
(define edd1
  (lambda (n)
      (cons (quote ()) n)))

;;; P108 "zub1"
(define zub1
  (lambda (n)
    (cdr n)))

;;; P111 "set?"
(define set?
  (lambda (lst)
    (cond ((null? lst) #t)
	  ((member* (car lst) (cdr lst)) #f)
	  (else (set? (cdr lst))))))

;;; P112 "makeset"
(define makeset
  (lambda (lst)
    (cond ((null? lst) (quote ()))
	  ((member* (car lst) (cdr lst)) (makeset (cdr lst)))
	  (else (cons (car lst) (makeset (cdr lst)))))))

;;; P112 "makeset2"
;;; using multirember
(define makeset2
  (lambda (lst)
    (cond ((null? lst) (quote ()))
	  (else (cons (car lst) (makeset2 (rember* (car lst) (cdr lst))))))))

;;; P114 "subset?"
(define subset?
  (lambda (l1 l2)
    (cond ((null? l1) #t)
	  ((member* (car l1) l2) (subset? (cdr l1) l2))
	  (else #f))))

;;; P115 "eqset?"
(define eqset?
  (lambda (l1 l2)
    (and (subset? l1 l2)
	 (subset? l2 l1))))

;;; P115 "intersect?"
(define intersect?
  (lambda (l1 l2)
    (cond ((null? l1) #f)
	  ((member* (car l1) l2) #t)
	  (else (intersect? (cdr l1) l2)))))

;;; P115 "intersect2?"
(define intersect2?
  (lambda (l1 l2)
    (cond ((null? l1) #f)
	  (else (or (member* (car l1) l2) (intersect2? (cdr l1) l2))))))

;;; P116 "intersect"
(define intersect
  (lambda (l1 l2)
    (cond ((null? l1) (quote ()))
	  ((member* (car l1) l2)
	   (cons (car l1) (intersect (cdr l1) l2)))
	  (else (intersect (cdr l1) l2)))))

;;; P116 "union"
(define union
  (lambda (l1 l2)
    (cond ((null? l1) l2)
	  ((member* (car l1) l2)
	   (union (cdr l1) l2))
	  (else (cons (car l1) (union (cdr l1) l2))))))

;;; P117 "xxx"
(define xxx
  (lambda (l1 l2)
    (cond ((null? l1) (quote ()))
	  ((member* (car l1) l2)
	   (xxx (cdr l1) l2))
	  (else (cons (car l1) (xxx (cdr l1) l2))))))

;;; P117 "intersectall"
(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
	  (else (intersectall (cons (intersect (car l-set) (car (cdr l-set))) 
			      (cdr (cdr l-set))))))))

;;; P118 "a-pair?"
(define a-pair?
  (lambda (l)
    (cond ((atom? l) #f)
	  ((null? l) #f)
	  ((null? (cdr l)) #f)
	  ((null? (cdr (cdr l))) #t)
	  (else #f))))

;;; P119 "first"
(define first
  (lambda (p)
    (car p)))

;;; P119 "second"
(define second
  (lambda (p)
    (car (cdr p))))

;;; P119 "build"
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

;;; P120 "fun?"
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;;; P121 "revrel"
(define revrel
  (lambda (rel)
    (cond ((null? rel) (quote ()))
	  (else (cons (cons (car (cdr (car rel)))
			    (cons (car (car rel)) (quote ())))
		      (revrel (cdr rel)))))))

;;; P121 "revpair"
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;;; P121 "revrel2"
(define revrel2
  (lambda (rel)
    (cond ((null? rel) (quote ()))
	  (else (cons (revpair (car rel))
		      (revrel2 (cdr rel)))))))

;;; P122 "fullfun?"
(define fullfun?
  (lambda (rel)
    (and (fun? rel)
	 (fun? (revrel2 rel)))))

;;; P126 "rember-f"
(define rember-f
  (lambda (func a list)
    (cond ((null? list) (quote ()))
	  (else (cond ((func (car list) a) (cdr list))
		      (else (cons (car list) (rember-f func a (cdr list)))))))))

;;; P126 "rember-f-sv"
(define rember-f-sv
  (lambda (func a list)
    (cond ((null? list) (quote ()))
	  ((func (car list) a) (cdr list))
	  (else (cons (car list) (rember-f-sv func a (cdr list)))))))

;;; P127 "eq?-c"
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;;; P128 "rember-f-new"
(define rember-f-new
  (lambda (func)
    (lambda (a list)
      (cond ((null? list) (quote ()))
	    ((func (car list) a) (cdr list))
	    (else (cons (car list) ((rember-f-new func) a (cdr list))))))))

;;; P130 "insertL-f"    
(define insertL-f
  (lambda (func)
    (lambda (new old list)
      (cond ((null? list) (quote ()))
	    ((func (car list) old) (cons new (cons old (cdr list))))
	    (else (cons (car list) ((insertL-f func) new old (cdr list))))))))

;;; P130 "insertR-f"
(define insertR-f
  (lambda (func)
    (lambda (new old list)
      (cond ((null? list) (quote ()))
	    ((func (car list) old) (cons old (cons new (cdr list))))
	    (else (cons (car list) ((insertR-f func) new old (cdr list))))))))
;;; P131 "seqL"
(define seqL
  (lambda (new old list)
    (cons new (cons old list))))

;;; P131 "seqR"
(define seqR
  (lambda (new old list)
    (cons old (cons new list))))
    
;;; P132
(define insert-g
  (lambda (seq)
    (lambda (new old list)
      (cond ((null? list) (quote ()))
	    ((eq? (car list) old) (seq new old (cdr list)))
	    (else (cons (car list) ((insert-g seq) new old (cdr list))))))))
;;; P132
(define insertR-fn
  (insert-g seqR))

;;; P132
(define insertL-fn
  (insert-g seqL))

;;; P133
(define seqS
  (lambda (new old l)
	(cons new l)))

;;; P133 subst_fn
(define subst_fn
  (insert-g seqS))

;;; P133 yyy
(define yyy
  (lambda (a l)
	((insert-g seqrem) #f a l)))

;;; P133 seqrem
(define seqrem
  (lambda (new old l)
	l))

;;; P135 multirember-f
(define multirember-f
  (lambda (test-func)
	(lambda (a lat)
	  (cond ((null? lat) (quote ()))
			((test-func a (car lat))
			 ((multirember-f test-func) a (cdr lat)))
			(else (cons (car lat)
						((multirember-f test-func) a
						 (cdr lat))))))))

;;; P136 eq?-tuna
(define eq?-tuna
  (eq?-c (quote tuna)))

;;; P137 multiremberT
(define multiremberT
  (lambda (test-func lat)
	(cond ((null? lat) (quote ()))
		  ((test-func (car lat))
		   (multiremberT test-func (cdr lat)))
		  (else (cons (car lat)
					  (multiremberT test-func (cdr lat)))))))

;;; P137 multirember-co
(define multirember-co
  (lambda (a lat col)
	(cond ((null? lat)
		   (col (quote ()) (quote ())))
		  ((eq? (car lat) a)
		   (multirember-co a (cdr lat)
						   (lambda (newlat seen)
							 (col newlat
								  (cons (car lat) seen)))))
		  (else
		   (multirember-co a (cdr lat)
						   (lambda (newlat seen)
							 (col (cons (car lat) newlat)
								  seen)))))))

;;; P138 a-friend
(define a-friend
  (lambda (x y)
	(null? y)))

; (multirember-co a lat col)

; a is tuna
; lat is ()
; col is a-friend
; => #t 

; a is tuna
; lat is (tuna)
; col is a-friend
; =>

; a is tuna
; lat is (strawberries tuna and swordfish)
; col is a-friend

;;; P140 last-friend
(define last-friend
  (lambda (x y)
	(length x)))

;;; P141 multiinsertLR
(define multiinsertLR
  (lambda (new oldL oldR lat)
	(cond ((null? lat) (quote ()))
		  ((eq? (car lat) oldL)
		   (cons new
				 (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
		  ((eq? (car lat) oldR)
		   (cons oldR
				 (cons new (multiinsertLR new oldL oldR (cdr lat)))))
		  (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

;;; P142 multiinsertLR-co
(define multiinsertLR-co
  (lambda (new oldL oldR lat col)
	(cond ((null? lat)
		   (col ((quote ()) 0 0)))
		  ((eq? oldL (car lat))
		   (multiinsertLR-co new oldL oldR
							 (cdr lat)
							 (lambda (newlat L R)
							   ())))
		  ((eq? oldR (car lat))
		   (multiinsertLR-co new oldL oldR
							 (cdr lat)
							 (lambda (newlat L R)
							   ())))
		  (else
		   (multiinsertLR-co new oldL oldR
							 (cdr lat)
							 (lambda (newlat L R)
							   ()))))))
