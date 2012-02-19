(defpackage :binarytree
  (:use :common-lisp :alexandria)
  (:export #:make-tree
           #:insert
           #:extract-all
           #:delete-key
           #:minimum
           #:maximum
           #:search
           #:predecessor
           #:successor))