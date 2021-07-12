(in-package :cytoscape)


(jupyter-widgets:defwidget graph-layout (jupyter-widgets:widget)
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


(jupyter-widgets:defwidget null-layout (graph-layout)
  ()
  (:documentation "Null graph layout algorithm.")
  (:default-initargs
    :%model-name "NullLayoutModel"
    :%view-name "NullLayoutView"))


(jupyter-widgets:defwidget random-layout (graph-layout bounding-box-slot common-slots
                                          animation-slots)
  ()
  (:documentation "Random graph layout algorithm.")
  (:default-initargs
    :%model-name "RandomLayoutModel"
    :%view-name "RandomLayoutView"))


(jupyter-widgets:defwidget preset-layout (graph-layout common-slots animation-slots)
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
  (:documentation "Preset graph layout algorithm.")
  (:default-initargs
    :%model-name "PresetLayoutModel"
    :%view-name "PresetLayoutView"))


(jupyter-widgets:defwidget grid-layout (graph-layout bounding-box-slot common-slots animation-slots
                                        avoid-overlap-slot node-dimensions-include-labels-slot
                                        spacing-factor-slot)
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
  (:documentation "Grid graph layout algorithm.")
  (:default-initargs
    :%model-name "GridLayoutModel"
    :%view-name "GridLayoutView"))


(jupyter-widgets:defwidget circle-layout (graph-layout bounding-box-slot common-slots
                                          animation-slots avoid-overlap-slot
                                          node-dimensions-include-labels-slot spacing-factor-slot
                                          radial-slots)
  ((radius
     :accessor radius
     :initarg :radius
     :initform :null
     :documentation "The radius of the circle"
     :trait :float))
  (:documentation "Circle graph layout algorithm.")
  (:default-initargs
    :%model-name "CircleLayoutModel"
    :%view-name "CircleLayoutView"))


(jupyter-widgets:defwidget concentric-layout (graph-layout bounding-box-slot common-slots
                                              animation-slots avoid-overlap-slot
                                              node-dimensions-include-labels-slot
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
  (:documentation "Concentric graph layout algorithm.")
  (:default-initargs
    :%model-name "ConcentricLayoutModel"
    :%view-name "ConcentricLayoutView"))


(jupyter-widgets:defwidget breadth-first-layout (graph-layout bounding-box-slot common-slots
                                                 animation-slots avoid-overlap-slot
                                                 node-dimensions-include-labels-slot
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
  (:documentation "Breadth first graph layout algorithm.")
  (:default-initargs
    :%model-name "BreadthFirstLayoutModel"
    :%view-name "BreadthFirstLayoutView"))


(jupyter-widgets:defwidget cose-layout (graph-layout bounding-box-slot common-slots animation-slots
                                        node-dimensions-include-labels-slot refresh-slot
                                        randomize-slot nesting-factor-slot num-iter-slot
                                        gravity-slot)
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
  (:documentation "Cose graph layout algorithm.")
  (:default-initargs
    :%model-name "CoseLayoutModel"
    :%view-name "CoseLayoutView"
    :refresh 10))


(jupyter-widgets:defwidget cola-layout (graph-layout bounding-box-slot common-slots
                                        avoid-overlap-slot node-dimensions-include-labels-slot
                                        refresh-slot randomize-slot)
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
  (:documentation "Cola graph layout algorithm.")
  (:default-initargs
    :%model-name "ColaLayoutModel"
    :%view-name "ColaLayoutView"))


(jupyter-widgets:defwidget dagre-layout (graph-layout bounding-box-slot common-slots animation-slots
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
  (:documentation "Dagre graph layout algorithm.")
  (:default-initargs
    :%model-name "DagreLayoutModel"
    :%view-name "DagreLayoutView"))


(jupyter-widgets:defwidget fcose-layout (graph-layout animation-slots common-slots gravity-slot
                                         nesting-factor-slot node-dimensions-include-labels-slot
                                         num-iter-slot randomize-slot)
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
  (:documentation "fCoSE graph layout algorithm.")
  (:default-initargs
    :%model-name "FCoSE_LayoutModel"
    :%view-name "FCoSE_LayoutView"
    :nesting-factor 0.1d0
    :num-iter 2500
    :gravity 0.25d0
    :randomize t))


(jupyter-widgets:defwidget klay-layout (graph-layout common-slots animation-slots
                                        node-dimensions-include-labels-slot)
  ((add-unnecessary-bendpoints
     :accessor add-unnecessary-bendpoints
     :initarg :add-unnecessary-bendpoints
     :initform nil
     :documentation ""
     :trait :boolean)
   (aspect-ratio
     :accessor aspect-ratio
     :initarg :aspect-ratio
     :initform 1.6
     :documentation ""
     :trait :float)
   (border-spacing
     :accessor border-spacing
     :initarg :border-spacing
     :initform 20
     :documentation ""
     :trait :integer)
   (compact-components
     :accessor compact-components
     :initarg :compact-components
     :initform nil
     :documentation ""
     :trait :boolean)
   (crossing-minimization
     :accessor crossing-minimization
     :initarg :crossing-minimization
     :initform "LAYER_SWEEP"
     :documentation ""
     :trait :string)
   (cycle-breaking
     :accessor cycle-breakingcycle-breaking
     :initarg :cycle-breaking
     :initform "GREEDY"
     :documentation ""
     :trait :string)
   (direction
     :accessor direction
     :initarg :direction
     :initform 20
     :documentation "UNDEFINED"
     :trait :string)
   (edge-routing
     :accessor edge-routing
     :initarg :edge-routing
     :initform "ORTHOGONAL"
     :documentation ""
     :trait :string)
   (edge-spacing-factor
     :accessor edge-spacing-factor
     :initarg :edge-spacing-factor
     :initform 0.5
     :documentation ""
     :trait :float)
   (feedback-edges
     :accessor feedback-edges
     :initarg :feedback-edges
     :initform nil
     :documentation ""
     :trait :boolean)
   (fixed-alignment
     :accessor fixed-alignment
     :initarg :fixed-alignment
     :initform "NONE"
     :documentation ""
     :trait :string)
   (in-layer-spacing-factor
     :accessor in-layer-spacing-factor
     :initarg :in-layer-spacing-factor
     :initform 1.0
     :documentation ""
     :trait :float)
   (layout-hierarchy
     :accessor layout-hierarchy
     :initarg :layout-hierarchy
     :initform nil
     :documentation ""
     :trait :boolean)
   (linear-segments-deflection-dampening
     :accessor linear-segments-deflection-dampening
     :initarg :linear-segments-deflection-dampening
     :initform 0.3
     :documentation ""
     :trait :float)
   (merge-edges
     :accessor merge-edges
     :initarg :merge-edges
     :initform nil
     :documentation ""
     :trait :boolean)
   (merge-hierarchy-crossing-edges
     :accessor merge-hierarchy-crossing-edges
     :initarg :merge-hierarchy-crossing-edges
     :initform t
     :documentation ""
     :trait :boolean)
   (node-layering
     :accessor node-layering
     :initarg :node-layering
     :initform "NETWORK_SIMPLEX"
     :documentation ""
     :trait :string)
   (node-placement
     :accessor node-placement
     :initarg :node-placement
     :initform "BRANDES_KOEPF"
     :documentation ""
     :trait :string)
   (randomization-seed
     :accessor randomization-seed
     :initarg :randomization-seed
     :initform 1
     :documentation ""
     :trait :integer)
   (route-self-loop-inside
     :accessor route-self-loop-inside
     :initarg :route-self-loop-inside
     :initform nil
     :documentation ""
     :trait :boolean)
   (separate-connected-components
     :accessor separate-connected-components
     :initarg :separate-connected-components
     :initform t
     :documentation ""
     :trait :boolean)
   (spacing
     :accessor spacing
     :initarg :spacing
     :initform 20
     :documentation ""
     :trait :integer)
   (thoroughness
     :accessor thoroughness
     :initarg :thoroughness
     :initform 7
     :documentation ""
     :trait :integer))
  (:documentation "Klay graph layout algorithm.")
  (:default-initargs
    :%model-name "KlayLayoutModel"
    :%view-name "KlayLayoutView"))

