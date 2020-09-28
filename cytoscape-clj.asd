(asdf:defsystem #:cytoscape-clj
  :description "A cytoscape widget for Common Lisp Jupyter."
  :version "0.2.18"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:common-lisp-jupyter)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "version")
         (:file "context-menu")
         (:file "graph-layout")
         (:file "cytoscape")))))
