;;; dbus-explore.el --- D-Bus tree-explorer  -*- lexical-binding: t; -*-

;;; Widget Tree
;; #+BEGIN_SRC emacs-lisp

(defun make-dbus-explore-service-expander (bus service)
  "Create an expander that will create an overview over the services nodes."
  (lambda (widget)
    (let ((path "/"))
      (loop for node in (dbus-introspect-get-node-names bus service "/")
	    for new-path = (concat path node)
	    collect
	    (widget-convert 'tree-widget :tag node :expander (make-dbus-explore-node-expander bus service new-path))))))

(defun make-dbus-explore-node-expander (bus service path)
  "Create an expander that will create the next level of nodes of a node."
  (lambda (widget)
    (loop for node in (dbus-introspect-get-node-names bus service path)
	  for new-path = (concat path "/" node)
	  collect
	  (widget-convert 'tree-widget :tag node :expander (make-dbus-explore-node-expander bus service new-path)))))

(defun dbus-explore-create-top-widgets (bus)
  (loop for name in (dbus-list-known-names bus) do
	(widget-create 'tree-widget :tag name :expander (make-dbus-explore-service-expander bus name))))

(defun dbus-explore (bus)
  (with-current-buffer (generate-new-buffer (format "*D-Bus explorer%s*" bus))
    (dbus-explore-create-top-widgets bus)
    (switch-to-buffer (current-buffer))))

;; #+END_SRC
