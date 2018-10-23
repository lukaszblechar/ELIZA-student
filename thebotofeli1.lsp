;;; The Eliza program simulating a Rogeria psychiatrist
;;; Matching function to be loaded from separate file

(load "match.lsp")

;;; Top level function to be called to start the program
					; co chcę zrobić?
					; 1: punt jako cytaty z biblii
					; 2: formuła spowiedzi zachowana w konwersacji
					; 3: pozmieniać już istniejące produkcje na bardziej spowiedziowe
					; 4: punt/old-s (zrobione)
					; 5: licznik penance
					; 6: wyświetlenie wszystkich pokut na koniec



(defun thebotofeli ()
  (setq wword-count 0)
  (setq punt-count 0)
  (setq sin-counter 0)
  (setq sinner 0)
  (format t "Welcome in the bot-space of God!~%")
  (format t "PLEASE MAKE THE SIGH OF THE CROSS AND REMEMBER TO ENCLOSE YOUR INPUT IN PARANTHESES:~%")
  (loop (setq s (you-me-map (read)))
	(cond
	 
	 ((match '(amen) s)
	  (sins)
	  (printl (append '(BASED ON YOUR BEHAVIOR YOU NEED TO) sinner))
	  (return "but God has forgiven your sins. Go in peace."))

	 ((match '(Forgive me Father) s)
	  (format t "Father believes in you and I do too.~%"))

	 ((match '(do me think (* x)) s)
	  (format t "I think you should answer that yourself.~%")
	  ((member 'you s)
	   (printl s)
	   (setq old-s s)))

	 ((match '(you are (* x)) s)
	  (printl (append '(please tell me) (list (wword)) '(you are) x))
	  (setq old-s s))

	 ((match '(you have (* x)) s)
	  (printl (append '(how long have you had) x '(?)))
	  (setq old-s s))

	 ((match '(you feel (* x)) s)
	  (format t "I sometimes feel the same way.~%")
	  (setq old-s s))

	 ((match '(because (* x)) s)
	  (format t "is that really the reason?~%")
	  (setq old-s s))

	 ((match nil s)
	  (format t "Even a fool, when he keeps silent, is considered wise; When he closes his lips, he is considered prudent.~%")
	  (setq old-s s))

	 ((match '(yes (* x)) s)
	  (printl (append '(how can you be so sure) x))
	  (setq old-s s))

	 ((match '(me are (* x)) s)
	  (printl (append '(I am not) x))
	  (setq old-s s))

	 ((match '((verbp v) (* x)) s)
	  (printl (append '(why do you want to) (list v) x))
	  (setq old-s s))

	 ((match '((wpred w) (* x)) s)
	  (printl (append '(tell me why) (list w)))
	  (setq old-s s))

	 ((match '((dpred w) me (* x)) s)
	  (printl (append '(perhaps I) (list w) x)))

	 ((member 'dream s)
	  (format t "And Joseph dreamed a dream, and he told it his brethren: and they hated him yet the more.~%")
	  (setq old-s s))

	 ((member 'love s)
	  (format t "Love is patient, love is kind. It does not envy, it does not boast, it is not proud. It does not dishonor others, it is not self-seeking, it is not easily angered, it keeps no record of wrongs. Love does not delight in evil but rejoices with the truth. It always protects, always trusts, always hopes, always perseveres.~%")
	  (setq old-s s))

	 ((member 'bible s)
	  (format t "bible is always right~%")
	  (setq old-s s))

	 ((member 'happy s)
	  (format t "that's dangerous!~%")
	  (setq old-s s))

	 ((member 'sex s)
	  (format t "eternal damnation!!!~%")
	  (setq old-s s))

	 ((member 'no s)
	  (format t "don't be so negative!~%")
	  (setq old-s s))

	 ((member 'maybe s)
	  (format t "be more decisive!~%")
	  (setq old-s s))

	 ((member 'lust s)
	  (format t "that's awful~%")
	  (setq old-s s)
	  (setq sin-counter (1+ sin-counter))
	  (setq sinner()))

	 ((member 'gluttony s)
	  (format t "eat less~%")
	  (setq old-s s)
	  (setq sin-counter (+ 2 sin-counter))
	  (setq sinner()))

	 ((member 'greed s)
	  (format t "you don't need all that stuff~%")
	  (setq old-s s)
	  (setq sin-counter (+ 3 sin-counter))
	  (setq sinner()))

	 ((member 'sloth s)
	  (format t "don't be so passive~%")
	  (setq old-s s)
	  (setq sin-counter (+ 4 sin-counter))
	  (setq sinner ()))

	 ((member 'wrath s)
	  (format t "chill out, youngster~%")
	  (setq old-s s)
	  (setq sin-counter (+ 5 sin-counter))
	  (setq sinner ()))

	 ((member 'envy s)
	  (format t "you are perfect the way you are~%")
	  (setq old-s s)
	  (setq sin-counter (+ 6 sin-counter))
	  (setq sinner ()))

	 ((member 'pride s)
	  (format t "you aren't that much better yourself~%")
	  (setq old-s s)
	  (setq sin-counter (+ 7 sin-counter))
	  (setq sinner ()))
	 
	 
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

;;; file:///usr/share/doc/gcl/gcl-si/Numbers.html#index-RANDO
(defun putin ()
    (printl (nth (random 7)
	       '((please go on)
		(tell me more)
		(i see)
		(what does that indicate)
		(but why be concerned about it)
		(just tell me how you feel)))))
		

(defun old-punt ()
 (cond ((boundp 'old-s) (printl (append '("PREVIOUSLY YOU SPOKE ABOUT") old-s '("PLEASE SAY SOMETHING MORE ABOUT IT"))))
(t (putin))))

(defun punt ()
  (eval (nth (random 2)
       '((putin)
       (old-punt)))))
  
(defun wword ()
   (nth (random 3) '(when why where how)))

(defun verbp (w)
  (member w '(go have be try eat tak help make get jump write type fill put turn compute think drink blink crash crunch add)))

(defun wpred (w)
  (member w '(why where when what)))

(defun dpred (w)
  (member w '(do can should would)))

;(defun sins ()
 ; (cond
  ;  ((equal (>= sin-counter 10) (setq sinner '(pray the whole rosary and be gone)))
   ; ((and (>= sin-counter 7) (<= sin-counter 9))
    ;(setq sinner '(meditate on one of the Rosaries Mysteries)))
  ; ((and (>= sin-counter 1) (<= sin-counter 3))
;    (setq sinner '(one hail mary)))
;   ((equal (sin-counter 0)
;	   (setq sinner '(you are sinless go away))))
;   ((and (>= sin-counter 4) (<= sin-counter 6))
;    (setq sinner '(two hail maries)))
;  
 ;  )))


    

(defun sins ()
  (cond
	((equal sin-counter 0)
		(setq sinner '(keep going)))
	((and (>= sin-counter 1) (<= sin-counter 3))
		(setq sinner '(one hail mary)))
	((and (>= sin-counter 4) (<= sin-counter 6))
		(setq sinner '(two hail maries)))
	((and (>= sin-counter 7) (<= sin-counter 9))
		(setq sinner '(meditate on one of the Rosaries Mysteries)))
	(t
		(setq sinner '(pray the whole rosary and be gone)))))
