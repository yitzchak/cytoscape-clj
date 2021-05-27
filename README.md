# cytoscape-clj

A [common-lisp-jupyter][] widget for [cytoscape.js][].

## Installation

The Jupyter Lab frontend code should be installed using the following command:

```
jupyter-labextension install cytoscape-clj
```

Once the extension has been installed then widget can be loaded in a notebook
using Quicklisp.

```lisp
(ql:quickload :cytoscape-clj)
```

## Usage

For sample notebooks please see the examples directory. The main widgets are
`cytoscape-widget` and `element`. For example to create and display a graph
use the following.

```lisp
(make-intance 'cytoscape:cytoscape-widget
              :graph-layouts (list (make-instance 'cytoscape:cose-layout))
              :graph-style "node { label: data(id); }"
              :elements (list (make-instance 'cytoscape:element
                                             :group "nodes"
                                             :data `(:object-alist
                                                     ("id" . "a")))
                              (make-instance 'cytoscape:element
                                             :group "nodes"
                                             :data `(:object-alist
                                                     ("id" . "b")))
                              (make-instance 'cytoscape:element
                                             :group "nodes"
                                             :data `(:object-alist
                                                     ("id" . "c")))
                              (make-instance 'cytoscape:element
                                             :group "edges"
                                             :data `(:object-alist
                                                     ("source" . "a")
                                                     ("target" . "b")))
                              (make-instance 'cytoscape:element
                                             :group "edges"
                                             :data `(:object-alist
                                                     ("source" . "a")
                                                     ("target" . "c")))
                              (make-instance 'cytoscape:element
                                             :group "edges"
                                             :data `(:object-alist
                                                     ("source" . "b")
                                                     ("target" . "c")))))
```

[common-lisp-jupyter]: https://yitzchak.github.io/common-lisp-jupyter/ 
[cytoscape.js]: https://js.cytoscape.org/
