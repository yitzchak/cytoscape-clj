(in-package :cytoscape)

(defclass menu-command (jupyter-widgets:widget)
  ((fill-color
     :accessor fill-color
     :initarg :fill-color
     :initform nil
     :documentation "Custom background color for item."
     :trait :json)
   (content
     :accessor content
     :initarg :content
     :initform ""
     :documentation "HTML/text content to be displayed in the menu."
     :trait :unicode)
   (content-style
     :accessor content-style
     :initarg :content-style
     :initform nil
     :documentation "css key:value pairs to set the command's css in js if you want."
     :trait :json)
   (enabled
     :accessor enabled
     :initarg :enabled
     :initform t
     :documentation "Whether the command is selectable."
     :trait :bool)
   (on-select
     :initarg :on-select
     :initform nil
     :documentation "Selection handlers"
     :accessor on-select))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Menu commands and selection handlers for context menus.")
  (:default-initargs
    :%model-name "MenuCommandModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "MenuCommandView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defun on-menu-command-select (widget handler)
  "Add a new selection handler for a menu command."
  (push handler (on-select widget)))

(defmethod jupyter-widgets:on-custom-message ((w menu-command) content buffers)
  (declare (ignore buffers))
  (if (equal (gethash "event" content) "select")
    (dolist (handler (on-select w))
            ()
      (funcall handler w (gethash "id" content)))
    (call-next-method)))


(defclass context-menu (jupyter-widgets:widget)
  ((menu-radius
     :accessor menu-radius
     :initarg :menu-radius
     :initform 100
     :documentation "the radius of the circular menu in pixels"
     :trait :integer)
   (selector
     :accessor selector
     :initarg :selector
     :initform "node"
     :documentation "Elements matching this Cytoscape.js selector will trigger this menu"
     :trait :unicode)
   (commands
     :accessor commands
     :initarg :commands
     :initform nil
     :documentation "an array of commands to list in the menu or a function that returns the array"
     :trait :widget-list)
   (fill-color
     :accessor fill-color
     :initarg :fill-color
     :initform "rgba(0, 0, 0, 0.75)"
     :documentation "the background colour of the menu"
     :trait :unicode)
   (active-fill-color
     :accessor active-fill-color
     :initarg :active-fill-color
     :initform "rgba(1, 105, 217, 0.75)"
     :documentation "the colour used to indicate the selected command"
     :trait :unicode)
   (active-padding
     :accessor active-padding
     :initarg :active-padding
     :initform 20
     :documentation "additional size in pixels for the active command"
     :trait :int)
   (indicator-size
     :accessor indicator-size
     :initarg :indicator-size
     :initform 24
     :documentation "the size in pixels of the pointer to the active command"
     :trait :int)
   (separator-width
     :accessor separator-width
     :initarg :separator-width
     :initform 3
     :documentation " the empty spacing in pixels between successive commands"
     :trait :int)
   (spotlight-padding
     :accessor spotlight-padding
     :initarg :spotlight-padding
     :initform 4
     :documentation "extra spacing in pixels between the element and the spotlight"
     :trait :int)
   (min-spotlight-radius
     :accessor min-spotlight-radius
     :initarg :min-spotlight-radius
     :initform 24
     :documentation "the minimum radius in pixels of the spotlight"
     :trait :int)
   (max-spotlight-radius
     :accessor max-spotlight-radius
     :initarg :max-spotlight-radius
     :initform 38
     :documentation "the maximum radius in pixels of the spotlight"
     :trait :int)
   (open-menu-events
     :accessor open-menu-events
     :initarg :open-menu-events
     :initform "cxttapstart taphold"
     :documentation "space-separated cytoscape events that will open the menu; only `cxttapstart` and/or `taphold` work here"
     :trait :unicode)
   (item-color
     :accessor item-color
     :initarg :item-color
     :initform "white"
     :documentation "the colour of text in the command's content"
     :trait :unicode)
   (item-text-shadow-color
     :accessor item-text-shadow-color
     :initarg :item-text-shadow-color
     :initform "transparent"
     :documentation "the text shadow colour of the command's content"
     :trait :unicode)
   (z-index
     :accessor z-index
     :initarg :z-index
     :initform 9999
     :documentation "the z-index of the ui div"
     :trait :int)
   (at-mouse
     :accessor at-mouse
     :initarg :at-mouse
     :initform nil
     :documentation "draw menu at mouse position"
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "Context menu for cytoscape.")
  (:default-initargs
    :%model-name "ContextMenuModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "ContextMenuView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

