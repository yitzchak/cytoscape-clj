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
   (position
     :accessor %position
     :initarg :position
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


(defgeneric add-graph (instance value))

(defmethod add-graph ((instance graph) (value list))
  (setf (nodes instance)
        (mapcar (lambda (j)
                  (make-instance 'node :data (cdr (jsown:val j "data"))))
                (jsown:val value "nodes"))

        (edges instance)
        (mapcar (lambda (j)
                  (make-instance 'edge :data (cdr (jsown:val j "data"))))
                (jsown:val value "edges")))
  (values))


(defclass cytoscape-widget (jupyter-widgets:dom-widget)
  ((min-zoom
     :accessor min-zoom
     :initarg :min-zoom
     :initform 1d-50
     :trait :float)
   (max-zoom
     :accessor max-zoom
     :initarg :max-zoom
     :initform 1d50
     :trait :float)
   (zooming-enabled
     :accessor zooming-enabled
     :initarg :zooming-enabled
     :initform t
     :trait :bool)
   (panning-enabled
     :accessor panning-enabled
     :initarg :panning-enabled
     :initform t
     :trait :bool)
   (user-panning-enabled
     :accessor user-panning-enabled
     :initarg :panning-enabled
     :initform t
     :trait :bool)
   (box-selection-enabled
     :accessor box-selection-enabled
     :initarg :box-selection-enabled
     :initform nil
     :trait :bool)
   (selection-type
     :accessor selection-type
     :initarg :selection-type
     :initform "single"
     :trait :unicode)
   (touch-tap-threshold
     :accessor touch-tap-threshold
     :initarg :touch-tap-threshold
     :initform 8
     :trait :int)
   (desktop-tap-threshold
     :accessor desktop-tap-threshold
     :initarg :desktop-tap-threshold
     :initform 4
     :trait :int)
   (autolock
     :accessor autolock
     :initarg :autolock
     :initform nil
     :trait :bool)
   (auto-ungrabify
     :accessor auto-ungrabify
     :initarg :auto-ungrabify
     :initform nil
     :trait :bool)
   (auto-unselectify
     :accessor auto-unselectify
     :initarg :auto-unselectify
     :initform t
     :trait :bool)
   (headless
     :accessor headless
     :initarg :headless
     :initform nil
     :trait :bool)
   (style-enabled
     :accessor style-enabled
     :initarg :style-enabled
     :initform t
     :trait :bool)
   (hide-edges-on-viewport
     :accessor hide-edges-on-viewport
     :initarg :hide-edges-on-viewport
     :initform nil
     :trait :bool)
   (texture-on-viewport
     :accessor texture-on-viewport
     :initarg :texture-on-viewport
     :initform nil
     :trait :bool)
   (motion-blur
     :accessor motion-blur
     :initarg :motion-blur
     :initform nil
     :trait :bool)
   (motion-blur-opacity
     :accessor motion-blur-opacity
     :initarg :motion-blur-opacity
     :initform 0.2
     :trait :float)
   (wheel-sensitivity
     :accessor wheel-sensitivity
     :initarg :wheel-sensitivity
     :initform 1.0
     :trait :float)
   (cytoscape-layout
     :accessor cytoscape-layout
     :initarg :cytoscape-layout
     :initform (list (cons "name" "cola"))
     :trait :dict)
   (cytoscape-style
     :accessor cytoscape-style
     :initarg :cytoscape-style
     :initform nil
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


(defmethod add-graph ((instance cytoscape-widget) value)
  (add-graph (graph instance) value))

