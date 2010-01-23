(defpackage :st.orm
	(:use :postmodern :cl :cl-ppcre))


(in-package :st.orm)

(defun db/lisp (string)
	(string-upcase (regex-replace-all "_" string "-")))

(defun db/symbol (name &optional package)
	(intern (db/lisp name)
					(or package
							(sb-int:sane-package))))

(defun schema/package-name (schema &optional (prefix "st.db."))
	(concatenate 'string
							 (string-upcase prefix)
							 (db/lisp schema)))

(defmacro defschema (name &optional (prefix "st.db."))
	`(defpackge ,(intern (schema/package-name name prefix) "KEYWORD")
			 (:use :postmodern :cl)))


(defclass db-object ()
	((key-values :initform (make-hash-table))))

(defmacro deftable-class (table-name schema w
