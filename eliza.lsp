;;; The Eliza program simulating a Rogeria psychiatrist
;;; Matching function to be loaded from separate file

(load "match.lsp")

;;; Top level function to be called to start the program

(defun eliza ()
  (setq wword-count 0)
  (setq punt-count 0)
  (format t "WELCOME TO MY SOFA!~%")
  (format t "PLEASE ENCLOSE YOUR INPUT IN PARANTHESES:~%")
  (loop (setq s (you-me-map (read)))
	(cond ((match '(bye) s) (return 'goodbye))
	      ((match '(do me think (* x)) s)
	       (format t "I think you should answer that yourself.~%"))
	       ((member 'you s)
	       (printl s))
	      ((match '(you are (* x)) s)
	       (printl (append '(please tell me) (list (wword)) '(you are) x)))
	      ((match '(you have (* x)) s)
	       (printl (append '(how long have you had) x '(?))))
	      ((match '(you feel (* x)) s)
	       (format t "I sometimes feel the same way.~%"))
	      ((match '(because (* x)) s)
	       (format t "is that really the reason?~%"))
	      ((match nil s) (format t "Say something!~%"))
	      ((match '(yes (* x)) s)
	       (printl (append '(how can you be so sure) x)))
	      ((match '(me are (* x)) s)
	       (printl (append '(oh yes I am) x)))
	      ((match '((verbp v) (* x)) s)
	       (printl (append '(why do you want me to) (list v) x)))
	      ((match '((wpred w) (* x)) s)
	       (printl (append '(you tell me) (list w))))
	      ((match '((dpred w) me (* x)) s)
	       (printl (append '(perhaps I) (list w) x)))
	      	      ((member 'dream s)
	       (format t "for dream analysis see Freud~%"))
	      ((member 'love s)
	       (format t "all is fair in love and war~%"))
	      ((member 'sex s)
	       (format t "you know nothing about sex!!!~%"))
	      ((member 'no s)
	       (format t "don't be so negative!~%"))
	      ((member 'maybe s)
	       (format t "be more decisive!~%"))
	     	      (t (punt)))))

;;;; nicely prints a list without paranthess

(defun printl (list)
  (mapcar #'(lambda (x) (format t "~a " x)) list)
  (terpri))
       
;;;; Punts returs one of 6 default responses
;(defun punt ()
 ; (setq punt-count (1+ punt-count))
  ;(cond ((= punt-count 6) (setq punt-count 0)))
  ;(printl (nth punt-count
;	       '((please go on)
;		(tell me more)
;		(i see)
;		(what does that indicate)
;		(but wh be concerned about it)
;		(just tell me how you feel)))))

(defun you-me (w)
  (cond ((eq w 'i) 'you)
       ((eq w 'me) 'you)
       ((eq w 'you) 'me)
       ((eq w 'my) 'your)
       ((eq w 'your) 'my)
       ((eq w 'yours) 'mine)
       ((eq w 'mine) 'yours)
       ((eq w 'am) 'are)
       (t w)))
(defun you-me-map (list)
  (mapcar #'you-me list))


;(defun printl (list)
;  (mapcar #'print-elem-and-space list)
 ; (terpri))

;(defun print-elem-and-space (x)
 ; (princ x) (princ " "))
(defun printl (list)
  (mapcar #'(lambda (x) (format t "~a " x)) list)
  (terpri))

;;; file:///usr/share/doc/gcl/gcl-si/Numbers.html#index-RANDOM

(defun punt ()
    (printl (nth (random 5)
	       '((please go on)
		(tell me more)
		(i see)
		(what does that indicate)
		(but why be concerned about it)
		(just tell me how you feel)))))

(defun wword ()
   (nth (random 3) '(when why where how))))

(defun verbp (w)
  (member w '(go have be try eat tak help make get jump write type fill put turn compute think drink blink crash crunch add)))

(defun wpred (w)
  (member w '(why where when what)))

(defun dpred (w)
  (member w '(do can should would)))
