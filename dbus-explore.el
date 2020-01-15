;;; dbus-explore.el --- D-Bus tree-explorer  -*- lexical-binding: t; -*-

;;; Header:

;; unlicensed

;; Author: timor (timor.dd@googlemail.com)
;; Version: 1.2
;; URL: https://github.com/timor/dbus-explore

;;; Commentary:
;; This is a D-Bus explorer for emacs.
;; As usual, add the file to load path and do a
;; =(require 'dbus-explore)=

;; To start, run the command =dbus-explore= and choose either =:session= or
;; =:system= as a bus to explore.

;; Click the Nodes in the explorer Buffer to browse the objects and their interfaces.
;;; Implementation:

;; * Interactive Emacs DBus Explorer

;; D-Bus objects are represented in a tree fashion, using emacs' tree
;; widget in a separate buffer, with one top-level object for each known
;; D-Bus object.
(require 'tree-widget)
(require 'dbus)
(require 'cl-lib)

;; ** Variables
;; All DBus objects implement some standard interfaces, hide them per default.
(defvar dbus-explore-hide-standard-interfaces t)
(defconst dbus-explore-standard-interfaces
  '( "org.freedesktop.DBus.Properties"
     "org.freedesktop.DBus.Introspectable"
     "org.freedesktop.DBus.Peer"
     "org.freedesktop.DBus.ObjectManager"
     ))
;; ** Tree Widget Creation
;; The tree items are created on button-click time by providing an
;; expander function to an item.  The general pattern here is to create
;; the expanders when the next item is created.  This expander-creation is
;; recursive, and creating lexical closures allows us to carry the
;; information that is needed for the next child-node in the closed-over
;; arguments for the next handler-creation.

;; The top-level nodes are services, and expand into nodes.
(defun make-dbus-explore-service-expander (bus service)
  "Create an expander that will create an overview over the services nodes."
  (lambda (widget)
    (let ((path "/"))
      (loop for node in (dbus-introspect-get-node-names bus service "/")
	    for new-path = (concat path node)
	    collect
	    (widget-convert 'tree-widget :tag node :expander (make-dbus-explore-node-expander bus service new-path))))))

;; All other nodes are checked for sub-nodes and interfaces.
(defun make-dbus-explore-node-expander (bus service path)
  "Create an expander that will create the next level of nodes of a node."
  (lambda (widget)
    (let ((child-nodes
	   (loop for node in (dbus-introspect-get-node-names bus service path)
		 for new-path = (concat path "/" node)
		 collect
		 (widget-convert 'tree-widget :tag node :expander (make-dbus-explore-node-expander bus service new-path))))
	  (interfaces
	   (loop for iface in (dbus-introspect-get-interface-names bus service path)
		 unless (and dbus-explore-hide-standard-interfaces
			   (member iface dbus-explore-standard-interfaces))
		 collect
		 (widget-convert 'tree-widget :tag (concat "I: " iface) :expander (dbus-explore-make-interface-expander bus service path iface)))))
      (append child-nodes interfaces))))

;; *** Interfaces
;; Interfaces can have multiple kinds of information: Signals, Properties
;; and Methods.

;; For signals and methods, emacs returns a parsed xml-expression.

;; The args part is decomposed into a list of ~((name type))~ pairs
(defun dbus-explore-method-call-args (args)
  "Create a list of arg specs to call a method from the ARGS argument structure."
  (loop for arg in args
        for arg-list = (second arg)
        for type = (alist-get 'type arg-list)
        for name = (alist-get 'name arg-list)
        unless (string-equal "out" (alist-get 'direction arg-list))
        collect (list name type)))

;; This is formatted
;; into a signature as follows:
(defun dbus-explore-format-signal/method-node (name args)
  (destructuring-bind (out-args in-args)
      (loop for arg in args
	    for arg-list = (second arg)
	    for out = (string-equal "out" (alist-get 'direction arg-list))
	    for type = (alist-get 'type arg-list)
	    for name = (alist-get 'name arg-list)
	    for formatted = (format "%s %s" type name)
	    if out
	    collect formatted into out-args else
	    collect formatted into in-args
	    finally return (list out-args in-args))
    (format "%s(%s)%s" name (string-join in-args ", ")
	    (if out-args (format " = (%s)" (string-join out-args ", "))
	      ""))))

;; This method is the click handler for the "Call" buttons in the Method
;; items.  It interactively queries for the method arguments and calls the
;; actual DBus Method
(defun dbus-explore-query-call-method (bus service path interface method args)
  (let ((params
         (loop for (name type) in args
               collect (read-minibuffer (format "Value for argument '%s', type '%s': " name type)))))
    (kill-new (format "%S" `(dbus-call-method ,bus ,service ,path ,interface ,method ,@params)))
    (message "Result('%s'): '%s'" method
             (apply 'dbus-call-method bus service path interface method params))))

(defun dbus-explore-query-send-signal (bus service path interface signal args)
  (let ((params
         (loop for (name type) in args
               collect (read-minibuffer (format "Value for argument '%s', type '%s': " name type)))))
    (apply 'dbus-send-signal bus service path interface signal params)))

;; The tree is built by providing expander closures.  These are the expanders
;; for the Interface Nodes, which list the properties, methods and signals.
(defun dbus-explore-make-interface-expander (bus service path interface)
  (lambda (widget)
    (let ((properties
           (loop
            for (property . value) in (dbus-get-all-properties bus service path interface)
            collect
            (dbus-explore-make-property-item bus service path interface property value)))
          (signals
           (loop
            for signal in (dbus-introspect-get-signal-names bus service path interface)
            collect
            (let* ((definition (dbus-introspect-get-signal bus service path interface signal))
                   ;; get rid of strings in the xml element, only return the args nodes
                   (args-struct (cl-remove-if-not 'consp (cl-subseq definition 2))))
              (widget-convert 'push-button :format "%t %[Send%]\n" :tag (concat "S: " (dbus-explore-format-signal/method-node signal args-struct))
                              :value (list bus service path interface signal (dbus-explore-method-call-args args-struct))
                              :notify (lambda (button &rest ignore)
                                        (apply 'dbus-explore-query-send-signal (widget-get button :value)))))))
          ;; This is a bit unfortunate duplicate code.  Could be eliminated when working from the all-objects path, bypassing the abstractions.
          (methods
           (loop
            for method in (dbus-introspect-get-method-names bus service path interface)
            collect
            (let* ((definition (dbus-introspect-get-method bus service path interface method))
                   (args-struct (cl-remove-if-not 'consp (cl-subseq definition 2))))
              (widget-convert 'push-button :format "%t %[Call%]\n" :tag (concat "M: " (dbus-explore-format-signal/method-node method args-struct))
                              :value (list bus service path interface method (dbus-explore-method-call-args args-struct))
                              :notify (lambda (button &rest ignore)
                                        (apply 'dbus-explore-query-call-method (widget-get button :value) ))
                              )))))
      (append properties methods signals))))

;; **** Properties
;; Properties can be somewhat complex to display due to D-Bus' flexible
;; type system.  Currently, we infer only arrays of simple stuff and
;; dictionaries, by checking for conses and nested conses, respectively

(defun dbus-explore-make-property-item (bus service path interface property value)
  "Helper that gets called during interface expansion."
  (let* ((type (if (consp value)
		  (if (consp (car value))
		      :dict
		    :array)
		 :simple))
	 (tag (case type
		(:simple (format "P: %s: %s" property value))
		(:array (format "P: %s(%s)" property (length value)))
		(:dict (format "P: %s{%s}" property (length value)))))
	 (expander (case type
		     (:simple nil)
		     (:array (dbus-explore-make-array-expander bus service path interface property value))
		     (:dict (dbus-explore-make-dict-expander bus service path interface property value)))))
    (if (eq type :simple)
	(widget-convert 'item :tag tag)
      (widget-convert 'tree-widget :tag tag :expander expander))))

(defun dbus-explore-make-array-expander (bus service path interface property value)
  "Expander that gets called when an array should be expanded"
  (lambda (widget)
    (loop for elt in value
	  for i from 0
	  collect
	  (widget-convert 'item :tag (format "%s: %s" i elt)))))

(defun dbus-explore-make-dict-expander (bus service path interface property value)
  "Expander that gets called when a dict should be expanded"
  (lambda (widget)
    (loop for (key val) in value
	  collect
	  (widget-convert 'item :tag (format "%s: %s" key (if (listp val)
								     (car val)
								   val))))))

;; ** Initialization

;; To begin processing, simply create a (collapsed) tree-widget for each
;; discovered service on the bus.

(defun dbus-explore-create-top-widgets (bus)
    (loop for name in (dbus-list-known-names bus) do
 	(widget-create 'tree-widget :tag name :expander (make-dbus-explore-service-expander bus name))))

;; This is actually the main user entry point.  The argument =bus= is either
;; =:session= or =:system=, and will use the corresponding bus.
;;;###autoload
(defun dbus-explore (bus)
  (interactive (list
                (intern (completing-read "Choose bus: " '(:session :system)))))
  (with-current-buffer (generate-new-buffer (format "*D-Bus explorer%s*" bus))
    (dbus-explore-create-top-widgets bus)
    (widget-setup)
    (widget-minor-mode 1)
    (switch-to-buffer (current-buffer))))

;;; Footer:
(provide 'dbus-explore)
;; dbus-explore.el ends here
