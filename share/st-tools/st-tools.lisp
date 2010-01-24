(defpackage :st.tools
	(:use :cl :cl-ppcre)
	(:export :defglobal
					 :plist/alist
					 :db/lisp
					 :db/symbol
					 :keyword/db))

(in-package :st.tools)



(defun db/lisp (string)
	(string-upcase (regex-replace-all "_" string "-")))

(defun db/symbol (name &optional package)
	(intern (db/lisp name)
					(or package
							(sb-int:sane-package))))

(defun keyword/db (kw)
	(subseq	(symbol-name kw) 1))


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
