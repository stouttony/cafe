(defsystem "st.orm"
	:description "Stout Tonny ORM system"
	:version "0.0.1-dev"
	:author "Pavel G. Koukoushkin <k.pavel.g@gmail.com>"
	:license "BSD"
	:depends-on ("postmodern" "cl-ppcre")
	:components
	((:file "package")
	 (:file "schema-extractor" :depends-on ("package"))
	 (:file "orm" :depends-on ("schema-extractor" "package"))))
	