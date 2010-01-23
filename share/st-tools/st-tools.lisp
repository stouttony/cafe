(defpackage :st.tools
	(:use :cl)
	(:export :defglobal))

(in-package :st.tools)


(defmacro defglobal (name value)
	"Creates function that returns value.
	I make for as replacement of `defvar` cause dynamically variables are
	thread-local"
	(let ((val (gensym (symbol-name name))))
		`(let ((,val ,value))
			 (defun ,name ()
				 ,val))))
