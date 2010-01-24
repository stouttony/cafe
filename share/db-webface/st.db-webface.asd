(defsystem "st.db-webface"
	:description "Stout Tony webface for PostgresQL DB"
	:version "0.0.1-dev"
	:author "Pavel G. Koukoushkin <k.pavel.g@gmail.com>"
	:license "BSD"
	:depends-on ("postmodern" "st.orm" "restas" "cl-who")
	:components
	((:file "package")
	 (:file "db-webface" :depends-on ("package"))))
	