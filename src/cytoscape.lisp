(in-package :cytoscape)

(defclass element (jupyter-widgets:widget)
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
     :initform t
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
     :initform t
     :trait :bool)
   (classes
     :accessor classes
     :initarg :classes
     :initform nil
     :trait :list)
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
    :%model-name "ElementModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "ElementView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget element)


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
     :initform t
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
     :initform nil
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
   (graph-layout
     :accessor graph-layout
     :initarg :graph-layout
     :initform '(("name" . "cola"))
     :trait :dict)
   (graph-style
     :accessor graph-style
     :initarg :graph-style
     :initform nil
     :trait :json)
   (zoom
     :accessor zoom
     :initarg :zoom
     :initform 2.0
     :trait :float)
   (tooltip-source
     :accessor tooltip-source
     :initarg :tooltip-source
     :initform "tooltip"
     :trait :unicode)
   (elements
     :accessor elements
     :initarg :elements
     :initform nil
     :trait :widget-list)
   (context-menus
     :accessor context-menus
     :initarg :context-menus
     :initform nil
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "CytoscapeModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "CytoscapeView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget cytoscape-widget)


(defgeneric add-graph (instance value))

(defmethod add-graph (instance (value list))
  (dolist (group '("nodes" "edges"))
    (setf (elements instance)
          (append (elements instance)
                  (mapcar (lambda (j)
                            (make-instance 'element :group group :data (cdr (jsown:val j "data"))))
                          (jsown:val value group))))))

