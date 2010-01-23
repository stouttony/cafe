(defpackage :st.tools
	(:use :cl)
	(:export :defglobal
					 :plist/alist))

(in-package :st.tools)


(defmacro defglobal (name value)
	"Creates function that returns value.
	I make for as replacement of `defvar` cause dynamically variables are
	thread-local"
	(let ((val (gensym (symbol-name name))))
		`(let ((,val ,value))
			 (defun ,name ()
				 ,val))))

(defun plist/alist (plist)
	(labels ((get-one (dst src)
						 (if src
								 (get-one (cons (cons (car src) (cadr src))
																dst)
													(cddr src))
								 dst)))
		(nreverse (get-one nil plist))))
