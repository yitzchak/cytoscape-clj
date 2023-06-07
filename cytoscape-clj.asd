(asdf:defsystem #:cytoscape-clj
  :description "A cytoscape widget for Common Lisp Jupyter."
  :version "0.2.18"
  :author "Tarn W. Burton"
  :license "MIT"
  :defsystem-depends-on (#:jupyter-lab-extension)
  :depends-on (#:common-lisp-jupyter)
  :components ((:jupyter-lab-extension cytoscape-clj
                :pathname "prebuilt/")
               (:module src
                :serial t
                :components ((:file "packages")
                             (:file "version")
                             (:file "common-slots")
                             (:file "context-menu")
                             (:file "graph-layout")
                             (:file "cytoscape")))))
