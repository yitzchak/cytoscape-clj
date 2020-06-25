(in-package :cytoscape)


(defclass graph-layout (jupyter-widgets:widget)
  ((selector
     :accessor selector
     :initarg :selector
     :initform "*"
     :documentation "Elements that the layout applies to."
     :trait :unicode))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Graph layout algorithm.")
  (:default-initargs
    :%model-name "GraphLayoutModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "GraphLayoutView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget graph-layout)


(defclass bounding-box-slot ()
  ((bounding-box
     :accessor bounding-box
     :initarg :bounding-box
     :initform nil
     :documentation "constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }"
     :trait :dict))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass null-layout (graph-layout)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Null graph layout algorithm.")
  (:default-initargs
    :%model-name "NullLayoutModel"
    :%view-name "NullLayoutView"))

(jupyter-widgets:register-widget null-layout)


(defclass random-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Random graph layout algorithm.")
  (:default-initargs
    :%model-name "RandomLayoutModel"
    :%view-name "RandomLayoutView"))

(jupyter-widgets:register-widget random-layout)


(defclass preset-layout (graph-layout)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Preset graph layout algorithm.")
  (:default-initargs
    :%model-name "PresetLayoutModel"
    :%view-name "PresetLayoutView"))

(jupyter-widgets:register-widget preset-layout)


(defclass grid-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Grid graph layout algorithm.")
  (:default-initargs
    :%model-name "GridLayoutModel"
    :%view-name "GridLayoutView"))

(jupyter-widgets:register-widget grid-layout)


(defclass circle-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Circle graph layout algorithm.")
  (:default-initargs
    :%model-name "CircleLayoutModel"
    :%view-name "CircleLayoutView"))

(jupyter-widgets:register-widget circle-layout)


(defclass concentric-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Concentric graph layout algorithm.")
  (:default-initargs
    :%model-name "ConcentricLayoutModel"
    :%view-name "ConcentricLayoutView"))

(jupyter-widgets:register-widget concentric-layout)


(defclass breadth-first-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Breadth first graph layout algorithm.")
  (:default-initargs
    :%model-name "BreadthFirstLayoutModel"
    :%view-name "BreadthFirstLayoutView"))

(jupyter-widgets:register-widget breadth-first-layout)


(defclass cose-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Cose graph layout algorithm.")
  (:default-initargs
    :%model-name "CoseLayoutModel"
    :%view-name "CoseLayoutView"))

(jupyter-widgets:register-widget cose-layout)


(defclass cola-layout (graph-layout bounding-box-slot)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Cola graph layout algorithm.")
  (:default-initargs
    :%model-name "ColaLayoutModel"
    :%view-name "ColaLayoutView"))

(jupyter-widgets:register-widget cola-layout)

