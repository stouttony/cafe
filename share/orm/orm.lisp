(in-package :st.orm)

(defun db/lisp (string)
	(string-upcase (regex-replace-all "_" string "-")))

(defun db/symbol (name &optional package)
	(intern (db/lisp name)
					(or package
							(sb-int:sane-package))))

(defun keyword/db (kw)
	(subseq	(symbol-name kw) 1))

(defclass db-object ()
	((key-values :initform (make-hash-table)
							 :reader key-values)))

(defmethod where-statement ((obj db-object))
	(format nil "where ~{~a = ~a~#[~:; AND ~]~}"
					(loop for keyfield being the hash-keys in (key-values obj)
						 using (hash-value value) append
							 (list (keyword/db keyfield) value))))

(defmacro deftable (name field-defs)
	`(tagbody
			(defclass ,(db/symbol name) (db-object)
				,(loop for field-def in field-defs
						collect
							(list
							 (db/symbol (getf field-def :name))
							 :initarg
							 (db/symbol (getf field-def :name) "KEYWORD")
							 :initform
							(if (or (getf field-def :is-nullable)
											(getf field-def :has-default))
									:null
									`(error ,(format nil "~a cannot be NULL in database. Set associated field" (getf field-def :name))))
							:type
							(if (or (getf field-def :is-nullable)
											(getf field-def :has-default))
									`(or keyword ,(getf field-def :type))
									(getf field-def :type))
							:reader (db/symbol (getf field-def :name)))))
			(defmethod initialize-instance :after ((object ,(db/symbol name)) &rest values)
				,@(cons
					 (let ((fd-has-default (remove-if-not (lambda (x) (getf x :has-default)) field-defs)))
						 `(let ((returned
										 (query (:insert-into ',(db/symbol name) :set
																					,@(loop for fd in field-defs append
																								 `(',(db/symbol (getf fd :name))
																										 ,(if (and (not (getf fd :is-nullable))
																															 (getf fd :has-default))
																													`(if (eq (,(db/symbol (getf fd :name)) object) :null)
																															 :default
																															 (,(db/symbol (getf fd :name)) object))
																													`(,(db/symbol (getf fd :name)) object))))
																					,@(when fd-has-default
																									`(:returning
																										,@(loop for fd in fd-has-default collect `',(db/symbol (getf fd :name)))))))))
								,@(loop for i from 0
										 for fd in fd-has-default
											 collect
											 `(setf (slot-value object ',(db/symbol (getf fd :name))) (nth ,i (car returned))))))
					 (let ((keys (mapcar (lambda (fd)
																 (getf fd :name))
															 (remove nil field-defs :key (lambda (x) (getf x :is-key))))))
						 (mapcar (lambda (key)
											 `(setf (gethash ,(db/symbol key "KEYWORD") (key-values object))
															(,(db/symbol key) object)))
										 keys))))
			,@(mapcar (lambda (field-def)
									`(defmethod (setf ,(db/symbol (getf field-def :name)))
											 (value (object ,(db/symbol name)))
										 (query (:update ',(db/symbol name) :set ',(db/symbol (getf field-def :name)) value
																		 :where
																		 (:and
																			,@(loop for fd in field-defs
																						 if (getf fd :is-key) collect
																						 `(:= ',(db/symbol (getf fd :name))
																									(gethash ,(db/symbol (getf fd :name) "KEYWORD") (key-values object)))))))
										 (setf (slot-value object ',(db/symbol (getf field-def :name))) value)))
								(remove nil field-defs :test-not #'eql :key (lambda (x) (getf x :is-key))))))
																		

		

																
(defmacro defschema (name)
	`(tagbody
			,@(mapcar (lambda (table-definition)
									`(deftable ,(car table-definition) ,(cdr table-definition)))
								(schema name))))

	

