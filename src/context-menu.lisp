(in-package :cytoscape)

(defclass menu-command (jupyter-widgets:widget)
  ((fill-color
     :accessor fill-color
     :initarg :fill-color
     :initform nil
     :trait :json)
   (content
     :accessor content
     :initarg :content
     :initform ""
     :trait :unicode)
   (content-style
     :accessor content-style
     :initarg :content-style
     :initform nil
     :trait :dict)
   (enabled
     :accessor enabled
     :initarg :enabled
     :initform t
     :trait :bool)
   (on-select
     :initarg :on-select
     :initform nil
     :accessor on-select))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "MenuCommandModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "MenuCommandView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget menu-command)

(defun on-menu-command-select (widget handler)
  (push handler (on-select widget)))

(defmethod jupyter-widgets:on-custom-message ((w menu-command) content buffers)
  (declare (ignore buffers))
  (if (equal (jupyter:json-getf content "event") "select")
    (dolist (handler (on-select w))
            ()
      (funcall handler w (jupyter:json-getf content "id")))
    (call-next-method)))


(defclass context-menu (jupyter-widgets:widget)
  ((menu-radius
     :accessor menu-radius
     :initarg :menu-radius
     :initform 100
     :trait :integer)
   (selector
     :accessor selector
     :initarg :selector
     :initform "node"
     :trait :unicode)
   (commands
     :accessor commands
     :initarg :commands
     :initform nil
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :%model-name "ContextMenuModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "ContextMenuView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget context-menu)

