;;; dbus-explore.el --- D-Bus tree-explorer  -*- lexical-binding: t; -*-

;;; About:
;; This is a D-Bus explorer for emacs.

;;; Usage:
;; =M-: (dbus-explore :system)=
;; or
;; =M-: (dbus-explore :session)=

;; Click the Nodes in the explorer Buffer to browse the objects and their interfaces.
;;; Implementation:
;; D-Bus objects are represented in a tree fashion, using emacs' tree
;; widget in a separate buffer, with one top-level object for each known
;; D-Bus object.
;; #+BEGIN_SRC emacs-lisp
(require 'tree-widget)

;; #+END_SRC
;; ** Variables
;; All DBus objects implement some standard interfaces, hide them per default.
;; #+BEGIN_SRC emacs-lisp
(defvar dbus-explore-hide-standard-interfaces t)
(defconst dbus-explore-standard-interfaces
  '( "org.freedesktop.DBus.Properties"
     "org.freedesktop.DBus.Introspectable"
     "org.freedesktop.DBus.Peer"
     ))
;; #+END_SRC
;; ** Tree Widget Creation
;; The tree items are created on button-click time by providing an
;; expander function to an item.  The general pattern here is to create
;; the expanders when the next item is created.  This expander-creation is
;; recursive, and creating lexical closures allows us to carry the
;; information that is needed for the next child-node in the closed-over
;; arguments for the next handler-creation.

;; The top-level nodes are services, and expand into nodes.
;; #+BEGIN_SRC emacs-lisp
(defun make-dbus-explore-service-expander (bus service)
  "Create an expander that will create an overview over the services nodes."
  (lambda (widget)
    (let ((path "/"))
      (loop for node in (dbus-introspect-get-node-names bus service "/")
	    for new-path = (concat path node)
	    collect
	    (widget-convert 'tree-widget :tag node :expander (make-dbus-explore-node-expander bus service new-path))))))
;; #+END_SRC

;; All other nodes are checked for sub-nodes and interfaces.
;; #+BEGIN_SRC emacs-lisp
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
;; #+END_SRC

;; Interfaces (TODO)
;; #+BEGIN_SRC emacs-lisp
(defun dbus-explore-make-interface-expander (bus service path interface)
  (lambda (widget)
    nil))
;; #+END_SRC

;; ** initialization

;; To begin processing, simply create a (collapsed) tree-widget for each
;; discovered service on the bus.

;; #+BEGIN_SRC emacs-lisp
(defun dbus-explore-create-top-widgets (bus)
;;   (loop for name in (dbus-list-known-names bus) do
;; 	(widget-create 'tree-widget :tag name :expander (make-dbus-explore-service-expander bus name))))
;; #+END_SRC

;; TODO: make this interactive and fix running doc.
;; #+BEGIN_SRC emacs-lisp
;; (defun dbus-explore (bus)
;;   (with-current-buffer (generate-new-buffer (format "*D-Bus explorer%s*" bus))
;;     (dbus-explore-create-top-widgets bus)
;;     (switch-to-buffer (current-buffer))))

;; #+END_SRC
