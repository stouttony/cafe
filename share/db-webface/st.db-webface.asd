(defsystem "st.db-webface"
	:description "Stout Tony webface for PostgresQL DB"
	:version "0.0.1-dev"
	:author "Pavel G. Koukoushkin <k.pavel.g@gmail.com>"
	:license "BSD"
	:depends-on ("postmodern" "st.orm" "restas")
	:components
	((:file "package.lisp"
	 (:file "db-webface.lisp" :depends-on ("package")))))
	