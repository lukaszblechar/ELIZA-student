;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(car '(druciany rekin is a nickname))
(cdr '(druciany rekin is a nick))
(cons 'druciany '(rekin is a nick))
(list 'druciany 'rekin 'is 'a 'nickname)
(list 'druciany '(rekin is a nickname))

(defun double (n)
  (* n 2))

(defun insert-second (item lis)
  (cons (car lis) (cons item (cdr lis))))

(atom 5)
(atom t)
(atom nil)
(listp t)
(listp nil)


(defun double-guard (n)
	(cond	((numberp n) (* n 2))
			(t 'non-number)))
(double-guard pig)

(defun food-expert ()
  (let (input)		; local variable input (LP, 18)
    (loop
     (terpri)		; new line (LP, 19 )
     (princ "Type the food name to be tasted: ") ; printing literal strings (LP, 19 )
     (setq input (read)); assigning a value to a variable (LP, 6)
     (cond ((equal input 'bye) (return 'goodbye))
	   ((equal input 'pizza) (princ 'good))
	 ((equal input 'beer) (princ 'delicious))
	 ((equal input 'chips) (princ 'fine))
	 (t (princ "no idea"))))))






(defun list-sum (lis)
  (cond ((null lis) 0)
	   (t (+ (car lis) (list-sum (cdr lis))))))

 (list-sum '(1 2 3))
 (trace list-sum)   
(list-sum '(1 2 3))

(defun double-list (lis)
  (cond ((null lis) nil)
	(t (cons (* (car lis) 2) (double-list (cdr lis))))))



(defun list-sum (lis)
  (cond ((null lis) 0)
	(t (+ (car lis) (list-sum (cdr lis))))))


(defun match1 (p s)
  (equal p s))

(defun match2 (p s)
  (cond ((atom p) (atom s))
	((atom s) nil)
	((match2 (car p)(car s))(match2 (cdr p)(cdr s)))
	(t nil)))

(defun match3 (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)
	((equal (car p) (car s)) (match3 (cdr p) (cdr s)))
	((equal (car p) '?) (match3 (cdr p) (cdr s)))
	(t nil)))

(defun match4 (p s)
  (cond ((null p) (null s))
	((equal (car p)(car s)) (match4 (cdr p)(cdr s)))
	((and (listp (car p))
	      (equal (length (car p)) 2)
	      (equal (caar p) '?)
	      (match4 (cdr p) (cdr s)))
	 (set (cadar p) (car s)) t)
	(t nil)))


(set (cadar p) (car s))
(set (car '(x y z)) '(a b c))

(defun match4 (p s)
  (cond ((null p)(null s))
	((equal (car p)(car s))(match4(cdr p)(cdr s)))
	((and (listp (car p))
	      (equal (length (car p)) 2)
	      (equal (caar p) '?)
	      (match4 (cdr p)(cdr s)))
	 (set (cadar p)(car s)) t)

	(t nil)))

(defun match5 (p s)
  (cond  ((null p) (null s))
	 ((or (atom p)(atom s)) nil)
	 ((equal (car p)(car s)) (match5 (cdr p) (cdr s)))
	 
  ((and (listp (car p))
	(equal (length (car p)) 2)
	(equal (caar p) '?)
	(match5 (cdr p)(cdr s)))
   (set (cadar p) (car s)) t)

  ((and (listp (car p))
	(equal (length (car p)) 2)
	(apply (caar p) (list (car s)))
	(match5 (cdr p)(cdr s)))
   (set (cadar p)(car s))t)
  (t nil)))

;;; if there is no matching there is no variable assignments because assigning is action and cannot be done without condition part 


(apply 'car (list '(a b c)))

(apply (caar '((atom x) b)) (list (car '(p b))))

(apply (caar 'p) (list (car s)))



(defun match (p s)
  (cond ((null p) (null s))
	((or (atom p) (atom s)) nil)

	((equal (car p) (car s))
	 (match (cdr p) (cdr s)))
	((atom (car p)) nil)

	((and (equal (caar p) '?)
	      (match (cdr p) (cdr s)))
	 (set (cadar p) (car s)) t)

	((equal (caar p) '*)
	 (cond ((match (cdr p) (cdr s))
		(set (cadar p) (list (car s))) t)
	       ((match (cdr p) s)
		(set (cadar p) nil) t)
	       ((match p (cdr s))
		(set (cadar p) (cons (car s) (eval (cadar p)))) t)))

	((and (apply (caar p) (list (car s)))
	      (match (cdr p) (cdr s)))
	 (set (cadar p) (car s)) t)

	(t nil)))
		

(mapcar '1+ '(1 2 3 4))



(defun printlist (lis)
  (mapcar #'(lambda (x) (princ x) (princ " ")) lis))

(defun putprop (atom value property)
  (setf (get atom property) value))

(putprop 'mary '(bill ann jane) 'children)
(putprop 'mary 29 'age)
(putprop 'mary '(john kate) 'parents)
