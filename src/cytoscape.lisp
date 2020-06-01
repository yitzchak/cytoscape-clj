(in-package :cytoscape)

(defparameter +module-name+ "jupyter-cytoscape")
(defparameter +module-version+ "^0.2.2")


(defclass part (jupyter-widgets:widget)
  ((group
     :accessor group
     :initarg :group
     :initform ""
     :trait :unicode)
   (removed
     :accessor removed
     :initarg :removed
     :initform nil
     :trait :bool)
   (selected
     :accessor selected
     :initarg :selected
     :initform nil
     :trait :bool)
   (selectable
     :accessor selectable
     :initarg :selectable
     :initform nil
     :trait :bool)
   (locked
     :accessor locked
     :initarg :locked
     :initform nil
     :trait :bool)
   (grabbed
     :accessor grabbed
     :initarg :grabbed
     :initform nil
     :trait :bool)
   (grabbable
     :accessor grabbable
     :initarg :grabbable
     :initform nil
     :trait :bool)
   (classes
     :accessor classes
     :initarg :classes
     :initform ""
     :trait :unicode)
   (data
     :accessor data
     :initarg :data
     :initform nil
     :trait :dict)
    (%position
     :accessor %position
     :initarg :%position
     :initform nil
     :trait :dict))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-module +module-name+
    :%view-module-version +module-version+))



(defclass edge (part)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "EdgeModel"))


(defclass node (part)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "NodeModel"
    :%view-name "NodeView"))


(defclass graph (jupyter-widgets:widget)
  ((nodes
     :accessor nodes
     :initarg :nodes
     :initform nil
     :trait :widget-list)
   (edges
     :accessor edges
     :initarg :edges
     :initform nil
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "GraphModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defun add-graph-from-json (instance json)
  (setf (nodes instance)
        (mapcar (lambda (j)
                  (make-instance 'node :data (cdr (jsown:val j "data"))))
                (jsown:val json "nodes"))

        (edges instance)
        (mapcar (lambda (j)
                  (make-instance 'edge :data (cdr (jsown:val j "data"))))
                (jsown:val json "edges")))
  (values))


(defclass cytoscape-widget (jupyter-widgets:dom-widget)
  ((auto-unselectify
     :accessor auto-unselectify
     :initarg :auto-unselectify
     :initform t
     :trait :bool)
   (box-selection-enabled
     :accessor box-selection-enabled
     :initarg :box-selection-enabled
     :initform nil
     :trait :bool)
   (cytoscape-layout
     :accessor cytoscape-layout
     :initarg :cytoscape-layout
     :initform (list (cons "name" "cola"))
     :trait :dict)
   (cytoscape-style
     :accessor cytoscape-style
     :initarg :cytoscape-style
     :initform (list (jsown:new-js
                       ("selector" "node")
                       ("css" (jsown:new-js
                                ("background-color" "#11479e"))))
                     (jsown:new-js
                       ("selector" "node:parent")
                       ("css" (jsown:new-js
                                ("background-opacity" 0.333))))
                     (jsown:new-js
                       ("selector" "edge")
                       ("style" (jsown:new-js
                                  ("width" 4)
                                  ("line-color" "#9dbaea")))))
     :trait :json)
   (zoom
     :accessor zoom
     :initarg :zoom
     :initform 2.0
     :trait :float)
   (rendered-position
     :accessor rendered-position
     :initarg :rendered-position
     :initform (jsown:new-js
                 ("renderedPosition" (jsown:new-js
                                       ("x" 100)
                                       ("y" 100))))
     :trait :json)
   (tooltip-source
     :accessor tooltip-source
     :initarg :tooltip-source
     :initform "tooltip"
     :trait :unicode)
   (graph
     :accessor graph
     :initarg :graph
     :initform (make-instance 'graph)
     :trait :widget))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "CytoscapeModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "CytoscapeView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


