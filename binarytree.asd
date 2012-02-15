(defpackage :binarytree.defsystem
  (:use :common-lisp :asdf))

(in-package :binarytree.defsystem)

(defsystem #:binarytree
  :description "Proof of concept implementation of a binary tree in CL"
  :version "0.0.0"
  :serial T
  :pathname "src/"
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "binarytree")))
