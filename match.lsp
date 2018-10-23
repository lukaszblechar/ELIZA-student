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
