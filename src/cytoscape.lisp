(in-package :cytoscape)

(defclass element (jupyter-widgets:widget)
  ((group
     :accessor group
     :initarg :group
     :initform "nodes"
     :documentation ""
     :trait :unicode)
   (removed
     :accessor removed
     :initarg :removed
     :initform nil
     :documentation ""
     :trait :bool)
   (selected
     :accessor selected
     :initarg :selected
     :initform nil
     :documentation ""
     :trait :bool)
   (selectable
     :accessor selectable
     :initarg :selectable
     :initform t
     :documentation ""
     :trait :bool)
   (locked
     :accessor locked
     :initarg :locked
     :initform nil
     :documentation ""
     :trait :bool)
   (grabbed
     :accessor grabbed
     :initarg :grabbed
     :initform nil
     :documentation ""
     :trait :bool)
   (grabbable
     :accessor grabbable
     :initarg :grabbable
     :initform t
     :documentation ""
     :trait :bool)
   (classes
     :accessor classes
     :initarg :classes
     :initform nil
     :documentation ""
     :trait :list)
   (data
     :accessor data
     :initarg :data
     :initform nil
     :documentation ""
     :trait :dict)
   (position
     :accessor %position
     :initarg :position
     :initform nil
     :documentation ""
     :trait :dict))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "ElementModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "ElementView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget element)


(defclass cytoscape-style (jupyter-widgets:style)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "CytoscapeStyleModel"
    :%model-module +module-name+
    :%model-module-version +module-version+)
  (:documentation "Cytoscape style widget"))

(jupyter-widgets:register-widget cytoscape-style)


(defclass cytoscape-widget (jupyter-widgets:styled-widget)
  ((min-zoom
     :accessor min-zoom
     :initarg :min-zoom
     :initform 1d-50
     :documentation "A minimum bound on the zoom level of the graph. The viewport
       can not be scaled smaller than this zoom level."
     :trait :float)
   (max-zoom
     :accessor max-zoom
     :initarg :max-zoom
     :initform 1d50
     :documentation "A maximum bound on the zoom level of the graph. The viewport
       can not be scaled larger than this zoom level."
     :trait :float)
   (zooming-enabled
     :accessor zooming-enabled
     :initarg :zooming-enabled
     :initform t
     :documentation "Whether zooming the graph is enabled, both by user events
       and programmatically."
     :trait :bool)
   (user-zooming-enabled
     :accessor user-zooming-enabled
     :initarg :user-zooming-enabled
     :initform t
     :documentation " Whether user events (e.g. mouse wheel, pinch-to-zoom) are
       allowed to zoom the graph. Programmatic changes to zoom are unaffected
       by this option."
     :trait :bool)
   (panning-enabled
     :accessor panning-enabled
     :initarg :panning-enabled
     :initform t
     :documentation "Whether panning the graph is enabled, both by user events
       sand programmatically."
     :trait :bool)
   (user-panning-enabled
     :accessor user-panning-enabled
     :initarg :panning-enabled
     :initform t
     :documentation "Whether user events (e.g. dragging the graph background) are
       allowed to pan the graph. Programmatic changes to pan are unaffected by
       this option."
     :trait :bool)
   (box-selection-enabled
     :accessor box-selection-enabled
     :initarg :box-selection-enabled
     :initform t
     :documentation "Whether box selection (i.e. drag a box overlay around, and
       release it to select) is enabled. If enabled while panning is also enabled,
       the user must use a modifier key (shift, alt, control, or command) to use
       box selection."
     :trait :bool)
   (selection-type
     :accessor selection-type
     :initarg :selection-type
     :initform "single"
     :documentation "A string indicating the selection behaviour from user input.
       For 'additive', a new selection made by the user adds to the set of
       currently selected elements. For 'single', a new selection made by the user
       becomes the entire set of currently selected elements (i.e. the previous
       elements are unselected)."
     :trait :unicode)
   (touch-tap-threshold
     :accessor touch-tap-threshold
     :initarg :touch-tap-threshold
     :initform 8
     :documentation "A non-negative integer that indicates the maximum allowable
       distance that a user may move during a tap gesture"
     :trait :int)
   (desktop-tap-threshold
     :accessor desktop-tap-threshold
     :initarg :desktop-tap-threshold
     :initform 4
     :documentation "A non-negative integer that indicates the maximum allowable
       distance that a user may move during a tap gesture"
     :trait :int)
   (autolock
     :accessor autolock
     :initarg :autolock
     :initform nil
     :documentation "Whether nodes should be locked (not draggable at all) by default
       (if true, overrides individual node state)."
     :trait :bool)
   (auto-ungrabify
     :accessor auto-ungrabify
     :initarg :auto-ungrabify
     :initform nil
     :documentation "Whether nodes should be ungrabified (not grabbable by user) by
       default (if true, overrides individual node state)."
     :trait :bool)
   (auto-unselectify
     :accessor auto-unselectify
     :initarg :auto-unselectify
     :initform nil
     :documentation "Whether nodes should be unselectified (immutable selection state) by
       default (if true, overrides individual element state)."
     :trait :bool)
   (headless
     :accessor headless
     :initarg :headless
     :initform nil
     :documentation "A convenience option that initialises the instance to run headlessly."
     :trait :bool)
   (style-enabled
     :accessor style-enabled
     :initarg :style-enabled
     :initform t
     :documentation "A boolean that indicates whether styling should be used.
       For headless (i.e. outside the browser) environments, display is not
       necessary and so neither is styling necessary — thereby speeding up your code."
     :trait :bool)
   (hide-edges-on-viewport
     :accessor hide-edges-on-viewport
     :initarg :hide-edges-on-viewport
     :initform nil
     :documentation "A rendering hint that when set to t makes the renderer
       not render edges while the viewport is being manipulated. This makes panning,
       zooming, dragging, et cetera more responsive for large graphs."
     :trait :bool)
   (texture-on-viewport
     :accessor texture-on-viewport
     :initarg :texture-on-viewport
     :initform nil
     :documentation "A rendering hint that when set to true makes the
       renderer use a texture during panning and zooming instead of
       drawing the elements, making large graphs more responsive."
     :trait :bool)
   (motion-blur
     :accessor motion-blur
     :initarg :motion-blur
     :initform nil
     :documentation "A rendering hint that when set to t makes the renderer
       use a motion blur effect to make the transition between frames seem smoother. "
     :trait :bool)
   (motion-blur-opacity
     :accessor motion-blur-opacity
     :initarg :motion-blur-opacity
     :initform 0.2
     :documentation "When motionBlur: t, this value controls the opacity of
       motion blur frames."
     :trait :float)
   (wheel-sensitivity
     :accessor wheel-sensitivity
     :initarg :wheel-sensitivity
     :initform 1.0
     :documentation "Changes the scroll wheel sensitivity when zooming.
       This is a multiplicative modifier. So, a value between 0 and 1
       reduces the sensitivity (zooms slower), and a value greater than
       1 increases the sensitivity (zooms faster)."
     :trait :float)
   (graph-layouts
     :accessor graph-layouts
     :initarg :graph-layouts
     :initform nil
     :documentation "Layout algorithms"
     :trait :widget-list)
   (graph-style
     :accessor graph-style
     :initarg :graph-style
     :initform nil
     :documentation "The stylesheet for the graph"
     :trait :json)
   (pan
     :accessor pan
     :initarg :pan
     :initform (list (cons :x 0) (cons :y 0))
     :documentation "Pan location of the graph."
     :trait :dict)
   (zoom
     :accessor zoom
     :initarg :zoom
     :initform 1.0
     :documentation "Zoom level of the graph."
     :trait :float)
   (tooltip-source
     :accessor tooltip-source
     :initarg :tooltip-source
     :initform "tooltip"
     :documentation "Field name in data of the tooltips."
     :trait :unicode)
   (elements
     :accessor elements
     :initarg :elements
     :initform nil
     :documentation "The list of graph elements"
     :trait :widget-list)
   (context-menus
     :accessor context-menus
     :initarg :context-menus
     :initform nil
     :documentation "The context menus for the graph"
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "CytoscapeModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "CytoscapeView"
    :%view-module +module-name+
    :%view-module-version +module-version+
    :style (make-instance 'cytoscape-style)))

(jupyter-widgets:register-widget cytoscape-widget)

(defun reset (instance)
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "reset"))))


(defun center (instance)
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "center"))))


(defun fit-elements (instance &key elements (padding 0))
  (declare (ignore elements))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "fit")
                                 ("padding" padding))))


(defun toggle-fullscreen (instance)
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "toggle_fullscreen"))))

(defun layout (instance)
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "layout"))))


(defgeneric add-graph (instance value &rest args)
  (:documentation "Add a graph to a cytoscape widget based on value type."))

(defmethod add-graph (instance (value list) &rest args)
  (declare (ignore args))
  (dolist (group '("nodes" "edges"))
    (setf (elements instance)
          (append (elements instance)
                  (mapcar (lambda (j)
                            (make-instance 'element :group group :data (cdr (jsown:val j "data"))))
                          (jsown:val value group))))))

; (defmethod add-graph (instance (value array))
;   (unless (and (= 2 (array-rank value))
;                (= (array-dimension value 0) (array-dimension value 1)))
;     (error "Adjacency matrix must be a square matrix."))
;   (prog (elements (source 0) target)
;     (push (make-instance 'cytoscape:element :group "nodes"
;                          :data (list (cons "id" (write-to-string source))))
;           elements)
;     (setq target source)
;     (push (make-instance 'cytoscape:element :group "edges"
;                          :data (list (cons "source" (write-to-string source))

;           elements)

      
