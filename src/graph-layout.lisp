(in-package :cytoscape)


(defclass graph-layout (jupyter-widgets:widget)
  ((selector
     :accessor selector
     :initarg :selector
     :initform "*"
     :documentation "Elements that the layout applies to."
     :trait :unicode)
   (on-layout-stop
     :initarg :on-layout-stop
     :initform nil
     :accessor %on-layout-stop))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Graph layout algorithm.")
  (:default-initargs
    :%model-name "GraphLayoutModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "GraphLayoutView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defun on-layout-stop (widget handler)
  (push handler (%on-layout-stop widget)))

(defmethod jupyter-widgets:on-custom-message ((w graph-layout) content buffers)
  (declare (ignore buffers))
  (if (equal (gethash "event" content) "layout_stop")
    (dolist (handler (%on-layout-stop w))
            ()
      (funcall handler w))
    (call-next-method)))


(defclass bounding-box-slot ()
  ((bounding-box
     :accessor bounding-box
     :initarg :bounding-box
     :initform :null
     :documentation "constrain layout bounds; { x1, y1, x2, y2 } or { x1, y1, w, h }"
     :trait :alist))
  (:metaclass jupyter-widgets:trait-metaclass))


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
  (:metaclass jupyter-widgets:trait-metaclass))


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
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass avoid-overlap-slot ()
  ((avoid-overlap
     :accessor avoid-overlap
     :initarg :avoid-overlap
     :initform t
     :documentation "Prevents node overlap, may overflow boundingBox if not enough space"
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass node-dimensions-include-labels-slot ()
  ((node-dimensions-include-labels
     :accessor node-dimensions-include-labels
     :initarg :node-dimensions-include-labels
     :initform nil
     :documentation "Includes the label when calculating node bounding boxes for the layout algorithm."
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass spacing-factor-slot ()
  ((spacing-factor
     :accessor spacing-factor
     :initarg :spacing-factor
     :initform :null
     :documentation "Applies a multiplicative factor (>0) to expand or compress the overall area that the nodes take up"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass))


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
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass refresh-slot ()
  ((refresh
     :accessor refresh
     :initarg :refresh
     :initform 1
     :documentation "Number of ticks per frame; higher is faster but more jerky"
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass randomize-slot ()
  ((randomize
     :accessor randomize
     :initarg :randomize
     :initform nil
     :documentation "Use random node positions at beginning of layout"
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass nesting-factor-slot ()
  ((nesting-factor
     :accessor nesting-factor
     :initarg :nesting-factor
     :initform 1.2d0
     :documentation "Nesting factor (multiplier) to compute ideal edge length for nested edges"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass num-iter-slot ()
  ((num-iter
     :accessor num-iter
     :initarg :num-iter
     :initform 1000
     :documentation "Maximum number of iterations to perform"
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass gravity-slot ()
  ((gravity
     :accessor gravity
     :initarg :gravity
     :initform 1
     :documentation "Gravity force (constant)"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass))


(defclass null-layout (graph-layout)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Null graph layout algorithm.")
  (:default-initargs
    :%model-name "NullLayoutModel"
    :%view-name "NullLayoutView"))


(defclass random-layout (graph-layout bounding-box-slot common-slots animation-slots)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Random graph layout algorithm.")
  (:default-initargs
    :%model-name "RandomLayoutModel"
    :%view-name "RandomLayoutView"))


(defclass preset-layout (graph-layout common-slots animation-slots)
  ((pan
     :accessor pan
     :initarg :pan
     :initform :null
     :documentation "The pan level to set (prob want fit = false if set)"
     :trait :json)
   (zoom
     :accessor zoom
     :initarg :zoom
     :initform :null
     :documentation "The zoom level to set (prob want fit = false if set)"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Preset graph layout algorithm.")
  (:default-initargs
    :%model-name "PresetLayoutModel"
    :%view-name "PresetLayoutView"))


(defclass grid-layout (graph-layout bounding-box-slot common-slots animation-slots
                       avoid-overlap-slot node-dimensions-include-labels-slot spacing-factor-slot)
  ((avoid-overlap-padding
     :accessor avoid-overlap-padding
     :initarg :avoid-overlap-padding
     :initform 10
     :documentation "Extra spacing around nodes when avoidOverlap: true"
     :trait :int)
   (cols
     :accessor cols
     :initarg :cols
     :initform :null
     :documentation "Force num of cols in the grid"
     :trait :int)
   (condense
     :accessor condense
     :initarg :condense
     :initform nil
     :documentation "Uses all available space on false, uses minimal space on true"
     :trait :bool)
   (rows
     :accessor rows
     :initarg :rows
     :initform :null
     :documentation "Force num of rows in the grid"
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Grid graph layout algorithm.")
  (:default-initargs
    :%model-name "GridLayoutModel"
    :%view-name "GridLayoutView"))


(defclass circle-layout (graph-layout bounding-box-slot common-slots animation-slots
                         avoid-overlap-slot node-dimensions-include-labels-slot spacing-factor-slot
                         radial-slots)
  ((radius
     :accessor radius
     :initarg :radius
     :initform :null
     :documentation "The radius of the circle"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Circle graph layout algorithm.")
  (:default-initargs
    :%model-name "CircleLayoutModel"
    :%view-name "CircleLayoutView"))


(defclass concentric-layout (graph-layout bounding-box-slot common-slots animation-slots
                             avoid-overlap-slot node-dimensions-include-labels-slot
                             spacing-factor-slot radial-slots)
  ((equidistant
     :accessor equidistant
     :initarg :equidistant
     :initform nil
     :documentation "Whether levels have an equal radial distance betwen them, may cause bounding box overflow"
     :trait :bool)
   (min-node-spacing
     :accessor min-node-spacing
     :initarg :min-node-spacing
     :initform 10
     :documentation "Min spacing between outside of nodes (used for radius adjustment)"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Concentric graph layout algorithm.")
  (:default-initargs
    :%model-name "ConcentricLayoutModel"
    :%view-name "ConcentricLayoutView"))


(defclass breadth-first-layout (graph-layout bounding-box-slot common-slots animation-slots
                                avoid-overlap-slot node-dimensions-include-labels-slot
                                spacing-factor-slot)
  ((circle
     :accessor circle
     :initarg :circle
     :initform nil
     :documentation "Put depths in concentric circles if true, put depths top down if false"
     :trait :bool)
   (directed
     :accessor directed
     :initarg :directed
     :initform nil
     :documentation "Whether the tree is directed downwards (or edges can point in any direction if false)"
     :trait :bool)
   (grid
     :accessor grid
     :initarg :grid
     :initform nil
     :documentation "Whether to create an even grid into which the DAG is placed (circle:false only)"
     :trait :bool)
   (maximal
     :accessor maximal
     :initarg :maximal
     :initform nil
     :documentation "Whether to shift nodes down their natural BFS depths in order to avoid upwards edges (DAGS only)"
     :trait :bool)
   (roots
     :accessor roots
     :initarg :roots
     :initform :null
     :documentation "The roots of the trees"
     :trait :list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Breadth first graph layout algorithm.")
  (:default-initargs
    :%model-name "BreadthFirstLayoutModel"
    :%view-name "BreadthFirstLayoutView"))


(defclass cose-layout (graph-layout bounding-box-slot common-slots animation-slots
                       node-dimensions-include-labels-slot refresh-slot randomize-slot
                       nesting-factor-slot num-iter-slot gravity-slot)
  ((component-spacing
     :accessor component-spacing
     :initarg :component-spacing
     :initform 40
     :documentation "Extra spacing between components in non-compound graphs"
     :trait :int)
   (cooling-factor
     :accessor cooling-factor
     :initarg :cooling-factor
     :initform 0.99d0
     :documentation "Cooling factor (how the temperature is reduced between consecutive iterations"
     :trait :float)
   (edge-elasticity
     :accessor edge-elasticity
     :initarg :edge-elasticity
     :initform 32
     :documentation "Divisor to compute edge forces. Can also be set per edge via data."
     :trait :int)
   (ideal-edge-length
     :accessor ideal-edge-length
     :initarg :ideal-edge-length
     :initform 32
     :documentation "Ideal edge (non nested) length. Can also be set per edge via data."
     :trait :int)
   (initial-temp
     :accessor initial-temp
     :initarg :initial-temp
     :initform 1000d0
     :documentation "Initial temperature (maximum node displacement)"
     :trait :float)
   (min-temp
     :accessor min-temp
     :initarg :min-temp
     :initform 1.0d0
     :documentation "Lower temperature threshold (below this point the layout will end)"
     :trait :float)
   (node-overlap
     :accessor node-overlap
     :initarg :node-overlap
     :initform 4
     :documentation "Node repulsion (overlapping) multiplier"
     :trait :int)
   (node-repulsion
     :accessor node-repulsion
     :initarg :node-repulsion
     :initform 2048
     :documentation "Node repulsion (non overlapping) multiplier. Can also be set per node in data."
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Cose graph layout algorithm.")
  (:default-initargs
    :%model-name "CoseLayoutModel"
    :%view-name "CoseLayoutView"
    :refresh 10))


(defclass cola-layout (graph-layout bounding-box-slot common-slots avoid-overlap-slot
                       node-dimensions-include-labels-slot refresh-slot randomize-slot)
  ((alignment
     :accessor alignment
     :initarg :alignment
     :initform :null
     :documentation "Relative alignment constraints on nodes. Can also be set per node via data."
     :trait :json)
   (all-const-iter
     :accessor all-const-iter
     :initarg :all-const-iter
     :initform :null
     :documentation "Initial layout iterations with all constraints including non-overlap"
     :trait :int)
   (convergence-threshold
     :accessor convergence-threshold
     :initarg :convergence-threshold
     :initform 0.01d0
     :documentation "When the alpha value (system energy) falls below this value, the layout stops"
     :trait :float)
   (edge-jaccard-length
     :accessor edge-jaccard-length
     :initarg :edge-jaccard-length
     :initform :null
     :documentation "Jaccard edge length in simulation. Can also be specified per edge via data."
     :trait :int)
   (edge-length
     :accessor edge-length
     :initarg :edge-length
     :initform :null
     :documentation "Sets edge length directly in simulation. Can also be specified per edge via data."
     :trait :int)
   (edge-sym-diff-length
     :accessor edge-sym-diff-length
     :initarg :edge-sym-diff-length
     :initform :null
     :documentation "Symmetric diff edge length in simulation. Can also be specified per edge via data."
     :trait :int)
   (flow
     :accessor flow
     :initarg :flow
     :initform :null
     :documentation "use DAG/tree flow layout if specified, e.g. { axis: 'y', minSeparation: 30 }"
     :trait :alist)
   (gap-inequalities
     :accessor gap-inequalities
     :initarg :gap-inequalities
     :initform :null
     :documentation "list of inequality constraints for the gap between the nodes, e.g. [{\"axis\":\"y\", \"left\":node1, \"right\":node2, \"gap\":25}]"
     :trait :alist)
   (handle-disconnected
     :accessor handle-disconnected
     :initarg :handle-disconnected
     :initform nil
     :documentation "if true, avoids disconnected components from overlapping"
     :trait :bool)
   (max-simulation-time
     :accessor max-simulation-time
     :initarg :max-simulation-time
     :initform 4000
     :documentation "max length in ms to run the layout"
     :trait :int)
   (unconst-iter
     :accessor unconst-iter
     :initarg :unconst-iter
     :initform :null
     :documentation "unconstrained initial layout iterations"
     :trait :int)
   (ungrabify-while-simulating
     :accessor ungrabify-while-simulating
     :initarg :ungrabify-while-simulating
     :initform nil
     :documentation "So you can't drag nodes during layout"
     :trait :bool)
   (user-const-iter
     :accessor user-const-iter
     :initarg :user-const-iter
     :initform :null
     :documentation "initial layout iterations with user-specified constraints"
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Cola graph layout algorithm.")
  (:default-initargs
    :%model-name "ColaLayoutModel"
    :%view-name "ColaLayoutView"))


(defclass dagre-layout (graph-layout bounding-box-slot common-slots animation-slots
                        node-dimensions-include-labels-slot spacing-factor-slot)
  ((edge-sep
     :accessor edge-sep
     :initarg :edge-sep
     :initform :null
     :documentation "the separation between adjacent edges in the same rank"
     :trait :int)
   (edge-weight
     :accessor edge-weight
     :initarg :edge-weight
     :initform 1
     :documentation "Higher weight edges are generally made shorter and straighter than lower weight edges. Can also be set per edge via data."
     :trait :int)
   (min-length
     :accessor min-length
     :initarg :min-length
     :initform 1
     :documentation "Number of ranks to keep between the source and target of the edge. Can also be set per edge via data."
     :trait :int)
   (node-sep
     :accessor node-sep
     :initarg :node-sep
     :initform :null
     :documentation "the separation between adjacent nodes in the same rank"
     :trait :int)
   (rank-dir
     :accessor rank-dir
     :initarg :rank-dir
     :initform :null
     :documentation "'TB' for top to bottom flow, 'LR' for left to right,"
     :trait :unicode)
   (ranker
     :accessor ranker
     :initarg :ranker
     :initform :null
     :documentation "Type of algorithm to assign a rank to each node in the input graph. Possible values: 'network-simplex', 'tight-tree' or 'longest-path'"
     :trait :unicode)
   (rank-sep
     :accessor rank-sep
     :initarg :rank-sep
     :initform :null
     :documentation "the separation between each rank in the layout"
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Dagre graph layout algorithm.")
  (:default-initargs
    :%model-name "DagreLayoutModel"
    :%view-name "DagreLayoutView"))


(defclass fcose-layout (graph-layout animation-slots common-slots gravity-slot nesting-factor-slot
                        node-dimensions-include-labels-slot num-iter-slot randomize-slot)
  ((quality
     :accessor quality
     :initarg :quality
     :initform "default"
     :documentation "Layout quality"
     :trait :unicode)
   (uniform-node-dimensions
     :accessor uniform-node-dimensions
     :initarg :uniform-node-dimensions
     :initform nil
     :documentation "Whether or not simple nodes (non-compound nodes) are of uniform dimensions"
     :trait :bool)
   (pack-components
     :accessor pack-components
     :initarg :pack-components
     :initform t
     :documentation "Whether to pack disconnected components - valid only if randomize: true"
     :trait :bool)
   (sampling-type
     :accessor sampling-type
     :initarg :sampling-type
     :initform t
     :documentation "False for random, true for greedy sampling"
     :trait :bool)
   (sample-size
     :accessor sample-size
     :initarg :sample-size
     :initform 25
     :documentation "Sample size to construct distance matrix"
     :trait :int)
   (node-separation
     :accessor node-separation
     :initarg :node-separation
     :initform 75
     :documentation "Separation amount between nodes"
     :trait :int)
   (pi-tol
     :accessor pi-tol
     :initarg :pi-tol
     :initform 0.0000001
     :documentation "Power iteration tolerance"
     :trait :int)
   (node-repulsion
     :accessor node-repulsion
     :initarg :node-repulsion
     :initform 4500
     :documentation "Node repulsion (non overlapping) multiplier"
     :trait :int)
   (ideal-edge-length
     :accessor ideal-edge-length
     :initarg :ideal-edge-length
     :initform 50
     :documentation "Ideal edge (non nested) length"
     :trait :int)
   (edge-elasticity
     :accessor edge-elasticity
     :initarg :edge-elasticity
     :initform 0.45
     :documentation "Divisor to compute edge forces"
     :trait :float)
   (tile
     :accessor tile
     :initarg :tile
     :initform t
     :documentation "For enabling tiling"
     :trait :bool)
   (tile-padding-vertical
     :accessor tile-padding-vertical
     :initarg :tile-padding-vertical
     :initform 10
     :documentation "Represents the amount of the vertical space to put between the zero degree members during the tiling operation(can also be a function)"
     :trait :int)
   (tile-padding-horizontal
     :accessor tile-padding-horizontal
     :initarg :tile-padding-horizontal
     :initform 10
     :documentation "Represents the amount of the horizontal space to put between the zero degree members during the tiling operation(can also be a function)"
     :trait :int)
   (gravity-range-compound
     :accessor gravity-range-compound
     :initarg :gravity-range-compound
     :initform 1.5d0
     :documentation "Gravity range (constant) for compounds"
     :trait :float)
   (gravity-range
     :accessor gravity-range
     :initarg :gravity-range
     :initform 3.8d0
     :documentation "Gravity range (constant)"
     :trait :float)
   (gravity-compound
     :accessor gravity-compound
     :initarg :gravity-compound
     :initform 1.0d0
     :documentation "Gravity force (constant) for compounds"
     :trait :float)
   (initial-energy-on-incremental
     :accessor initial-energy-on-incremental
     :initarg :initial-energy-on-incremental
     :initform 0.3d0
     :documentation "Initial cooling factor for incremental layout"
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "fCoSE graph layout algorithm.")
  (:default-initargs
    :%model-name "FCoSE_LayoutModel"
    :%view-name "FCoSE_LayoutView"
    :nesting-factor 0.1d0
    :num-iter 2500
    :gravity 0.25d0
    :randomize t))

