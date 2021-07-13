(in-package :cytoscape)


(defclass bounding-box-slot ()
  ((bounding-box
     :accessor bounding-box
     :initarg :bounding-box
     :initform :null
     :documentation "constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }"
     :trait :alist))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass common-slots ()
  ((animate
     :accessor animate
     :initarg :animate
     :initform t
     :documentation "Whether to animate changes to the layout"
     :trait :bool)
   (fit
     :accessor fit
     :initarg :fit
     :initform t
     :documentation "Whether to fit the viewport to the graph"
     :trait :bool)
   (padding
     :accessor padding
     :initarg :padding
     :initform 30
     :documentation "Padding to leave between graph and viewport"
     :trait :int))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass animation-slots ()
  ((animation-duration
     :accessor animation-duration
     :initarg :animation-duration
     :initform 250
     :documentation "Duration of animation in ms, if enabled"
     :trait :int)
   (animation-easing
     :accessor animation-easing
     :initarg :animation-easing
     :initform :null
     :documentation "Easing of animation, if enabled"
     :trait :unicode))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass avoid-overlap-slot ()
  ((avoid-overlap
     :accessor avoid-overlap
     :initarg :avoid-overlap
     :initform t
     :documentation "Prevents node overlap, may overflow boundingBox if not enough space"
     :trait :bool))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass node-dimensions-include-labels-slot ()
  ((node-dimensions-include-labels
     :accessor node-dimensions-include-labels
     :initarg :node-dimensions-include-labels
     :initform nil
     :documentation "Includes the label when calculating node bounding boxes for the layout algorithm."
     :trait :bool))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass spacing-factor-slot ()
  ((spacing-factor
     :accessor spacing-factor
     :initarg :spacing-factor
     :initform :null
     :documentation "Applies a multiplicative factor (>0) to expand or compress the overall area that the nodes take up"
     :trait :float))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass radial-slots ()
  ((clockwise
     :accessor clockwise
     :initarg :clockwise
     :initform t
     :documentation "Whether the layout should go clockwise (true) or counterclockwise/anticlockwise (false)"
     :trait :bool)
   (start-angle
     :accessor start-angle
     :initarg :start-angle
     :initform (* 3/2 pi)
     :documentation "Where nodes start in radians"
     :trait :float)
   (sweep
     :accessor sweep
     :initarg :sweep
     :initform :null
     :documentation "How many radians should be between the first and last node (defaults to full circle)"
     :trait :float))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass refresh-slot ()
  ((refresh
     :accessor refresh
     :initarg :refresh
     :initform 1
     :documentation "Number of ticks per frame; higher is faster but more jerky"
     :trait :int))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass randomize-slot ()
  ((randomize
     :accessor randomize
     :initarg :randomize
     :initform nil
     :documentation "Use random node positions at beginning of layout"
     :trait :bool))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass nesting-factor-slot ()
  ((nesting-factor
     :accessor nesting-factor
     :initarg :nesting-factor
     :initform 1.2d0
     :documentation "Nesting factor (multiplier) to compute ideal edge length for nested edges"
     :trait :float))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass num-iter-slot ()
  ((num-iter
     :accessor num-iter
     :initarg :num-iter
     :initform 1000
     :documentation "Maximum number of iterations to perform"
     :trait :int))
  (:metaclass jupyter/widgets:trait-metaclass))


(defclass gravity-slot ()
  ((gravity
     :accessor gravity
     :initarg :gravity
     :initform 1
     :documentation "Gravity force (constant)"
     :trait :float))
  (:metaclass jupyter/widgets:trait-metaclass))

