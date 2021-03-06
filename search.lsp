(defun putprop (atom value property)
  (setf (get atom property) value))

(putprop 'brest '(rennes) 'adjcnt)
(putprop 'rennes '(caen paris brest nantes) 'adjcnt)
(putprop 'caen '(calais paris rennes) 'adjcnt)
(putprop 'calais '(nancy paris caen) 'adjcnt)
(putprop 'nancy '(strasbourg dijon paris calais) 'adjcnt)
(putprop 'strasbourg '(dijon nancy) 'adjcnt)
(putprop 'dijon '(strasbourg lyon  paris nancy) 'adjcnt)
(putprop 'lyon '(grenoble avignon limoges dijon) 'adjcnt)
(putprop 'grenoble '(avignon lyon) 'adjcnt)
(putprop 'avignon '(grenoble marseille montpellier lyon) 'adjcnt)
(putprop 'marseille '(nice avignon) 'adjcnt)
(putprop 'nice '(marseille) 'adjcnt)
(putprop 'montpellier '(avignon toulouse) 'adjcnt)
(putprop 'toulouse '(montpellier bordeaux limoges) 'adjcnt)
(putprop 'bordeaux '(limoges toulouse nantes) 'adjcnt)
(putprop 'limoges '(lyon toulouse bordeaux nantes paris) 'adjcnt)
(putprop 'nantes '(limoges bordeaux rennes) 'adjcnt)
(putprop 'paris '(calais nancy dijon limoges rennes caen) 'adjcnt)

;;; Depth-First Search

(defun depth-first-search (start-node goal-node)
  (let ((open (list start-node)) (closed nil) n l)
    (putprop start-node nil 'pointer)
    (loop
     (cond ((null open) (return 'failure)))
     (setq n (car open))
     (print n)
     (princ "Open: ")
     (princ open)
     (setq open (cdr open))
     (setq closed (cons n closed))
     (cond ((equal n goal-node) (return (extract-path n))))
     (setq l (successors n))
     (setq l (set-difference l closed))
     (setq open (append l (set-difference open l)))
     (mapcar #'(lambda (x) (putprop x n 'pointer)) l))))

(defun extract-path (n)
  (cond ((null n) nil)
	(t (append (extract-path (get n 'pointer)) (list n)))))

(defun successors (n)
  (get n 'adjcnt))
