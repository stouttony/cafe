(in-package :st.orm)

(defclass db-object ()
	((key-values :initform (make-hash-table)
							 :reader key-values)))

(defmethod where-statement ((obj db-object))
	(format nil "where ~{~a = ~a~#[~:; AND ~]~}"
					(loop for keyfield being the hash-keys in (key-values obj)
						 using (hash-value value) append
							 (list (keyword/db keyfield) value))))

(defgeneric select (name additional-clauses))

(defgeneric delete-orm (object))

(defgeneric delete-query (object))

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
			(defmethod initialize-instance :after ((object ,(db/symbol name)) &key (need-insert t) &allow-other-keys)
				,@(cons
					 (let ((fd-has-default (remove-if-not (lambda (x) (getf x :has-default)) field-defs)))
						 `(when need-insert
								(let ((returned
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
												 `(setf (slot-value object ',(db/symbol (getf fd :name))) (nth ,i (car returned)))))))
					 (let ((keys (mapcar (lambda (fd)
																 (getf fd :name))
															 (remove nil field-defs :key (lambda (x) (getf x :is-key))))))
						 (mapcar (lambda (key)
											 `(setf (gethash ,(db/symbol key "KEYWORD") (key-values object))
															(,(db/symbol key) object)))
										 keys))))
			(defmacro ,(db/symbol (concatenate 'string "select-" name)) (s-sql-condition)
				`(let ((result
								(query (:select ,,@(mapcar (lambda (fd) `'',(db/symbol (getf fd :name))) field-defs)
																:from
																',',(db/symbol name) ,@(when s-sql-condition `(:where ,s-sql-condition))))))
					 (mapcar
						(lambda (row)
							(make-instance ',',(db/symbol name)
														 ,,@(loop for i from 0
																	 for fd in field-defs
																	 append
																		 `(',(db/symbol (getf fd :name) "KEYWORD") '(nth ,i row)))
														 :need-insert nil))
						result)))
			(defmethod st.orm::delete-orm ((object ,(db/symbol name)))
				(query (:delete-from ',(db/symbol name)
														 :where
														 (:and
															,@(loop for fd in field-defs
																	 if (getf fd :is-key) collect
																		 `(:= ',(db/symbol (getf fd :name))
																					(gethash ,(db/symbol (getf fd :name) "KEYWORD") (key-values object))))))))
			(defmethod st.orm::delete-query ((object ,(db/symbol name)))
				(sql
				 (:delete-from ',(db/symbol name)
											 :where
											 (:and
												,@(loop for fd in field-defs
														 if (getf fd :is-key) collect
															 `(:= ',(db/symbol (getf fd :name))
																		(gethash ,(db/symbol (getf fd :name) "KEYWORD") (key-values object))))))))
			(defmethod st.orm::select-orm ((type (eql ,(db/symbol name "KEYWORD"))) additional-clauses)
				(let ((result
							 (query (format nil "~a~@[ where ~a~]"
															,(format nil "select ~{~a~#[~:;, ~]~} from ~a"
																			 (mapcar (lambda (x) (getf x :name)) field-defs)
																			 name)
															additional-clauses))))
					(mapcar
					 (lambda (row)
						 (make-instance ',(db/symbol name)
														,@(loop for i from 0
																 for fd in field-defs
																 append
																	 `(,(db/symbol (getf fd :name) "KEYWORD") (nth ,i row)))
														:need-insert nil))
					 result)))
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

	

