(in-package :st.orm)


(defun schema-list ()
	"Returns list of schemas presented in DB"
	(mapcar #'car
					(query "select distinct table_schema from information_schema.tables")))

(defparameter +types-plist+
	(list '("integer". fixnum)
				'("character varying" . string)))

(defun schema (schema-name)
	(let ((tables (make-hash-table :test #'equal)))
		(loop for col-definition in
				 (query (:select (:as (:\|\| 'c.table_schema "." 'c.table-name) 'table-name)
												 'c.column-name
												 'c.data-type
												 (:as (:not (:is-null 'k.column_name)) 'is-key)
												 (:as (:not (:is-null 'c.column_default)) 'has-default)
												 (:as (:= 'c.is-nullable "YES") 'is-nullable)
												 :from (:as 'information-schema.columns 'c)
												 :left-join (:as 'information-schema.key-column-usage 'k)
												 :on (:and (:= 'c.table-schema 'k.table-schema)
																	 (:= 'c.table-name  'k.table-name)
															(:= 'c.column-name 'k.column_name))
												 :where (:= 'c.table_schema schema-name)))
			 do
				 (setf (gethash (first col-definition) tables)
							 (cons
								(list :name (second col-definition)
											:type (cdr (assoc (third col-definition)
																				+types-plist+
																				:test #'equal))
											:is-key (fourth col-definition)
											:has-default (fifth col-definition)
											:is-nullable (sixth col-definition))
								(gethash (first col-definition) tables))))
		(loop for table-name being the hash-keys in tables
				 using (hash-value field-defs)
				 collect (cons table-name field-defs))))

