(in-package :st.db-webface)


(defmacro base-html ((&key title) &body body)
	`(with-html-output-to-string (out)
		 (:html
			(:head
			 (:title (or ,title "Stout Tony database web interface")))
			(:body
			 ,@body))))

(defmacro schema-row (schema-name)
	`(:a :href (genurl 'table-list :schema-name ,schema-name)  ,schema-name))

(defmacro main-page ()
	`(define-route ,(intern "MAIN-PAGE") ("")
		 (base-html (:title "Main page")
			 ,@(loop for schema-name in (schema-list) append `((schema-row schema-name) (:br))))))

(defmacro table-list ()
	`(deifne-route ,(intern "TABLE-LIST") (":(schema-name).html")
								 (base-html (:title (fmt "Schema: ~a" schema-name)))))



