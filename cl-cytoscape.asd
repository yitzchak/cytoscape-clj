(asdf:defsystem #:cl-cytoscape
  :description "A ipycytoscape for Common Lisp."
  :version "0.1"
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
         (:file "cytoscape")))))
