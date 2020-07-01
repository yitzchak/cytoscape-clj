(asdf:defsystem #:cytoscape-clj
  :description "A cytoscape widgetfor Common Lisp Jupyter."
  :version "0.2.0"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:common-lisp-jupyter
     :jsown)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "version")
         (:file "context-menu")
         (:file "graph-layout")
         (:file "cytoscape")))))
