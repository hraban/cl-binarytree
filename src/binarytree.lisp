(in-package :binarytree)

(defclass node ()
  ((key
    :initarg :key
    :accessor key)
   (left
    :initform NIL
    :type (or null node)
    :accessor left)
   (right
    :initform NIL
    :type (or null node)
    :accessor right)
   (parent
    :initform NIL
    :type (or null node)
    :accessor parent)
   (tree
    :initarg :tree
    :type tree
    :accessor tree)
   (value
    :initarg :value
    :accessor value)))

(defclass tree ()
  ((root
    :initarg :root
    :initform NIL
    :type (or null node)
    :accessor root)))

(defmethod (setf left) :after ((new node) (node node))
  (when new
    (setf (parent new) node)))

(defmethod (setf right) :after ((new node) (node node))
  (when new
    (setf (parent new) node)))

(defmethod (setf root) :after ((root node) tree)
  (declare (ignore tree))
  (when root
    (setf (parent root) NIL)))

(defun make-node (key &rest argv)
  (apply #'make-instance 'node :key key argv))

(defun make-tree (&rest argv)
  (apply #'make-instance 'tree argv))

(defun left-rotate (x &aux (tree x))
  "Rotate two nodes left without violating binary tree constraints:

    x                y
   / \              / \
  a   y    -->     x   c
     / \          / \
    b   c        a   b
"
  (declare (type tree tree)
           (type node x))
  (let ((y (right x))
        (top (parent x)))
    (unless top
      (setf (root tree) y))
    (setf (right x) (left y))
    (setf (left y) x)
    (setf (parent y) top))
  tree)

(defun right-rotate (y &aux (tree (tree y)))
  "Opposite of left-rotate"
  (declare (type tree tree)
           (type node y))
  (let ((x (left y))
        (top (parent y)))
    (unless top
      (setf (root tree) x))
    (setf (left y) (right x))
    (setf (right x) y)
    (setf (parent x) top))
  tree)

(defun initialize-tree (node tree)
  "Given an empty tree, initialize it with this node as its root."
  (assert (null (root tree)))
  (setf (root tree) node)
  tree)

(defun insert-node (new tree &optional (current (root tree)) setter)
  "Insert a new node in the tree. Current indicates the recursion subtree step."
  (declare (type node new)
           (type tree tree))
  ;; TODO: Generalize comparison
  (if current
      (multiple-value-bind (get-next set-next)
          (if (<= (key new) (key current))
              (values #'left #'(setf left))
              (values #'right #'(setf right)))
        (insert-node new
                     tree
                     (funcall get-next current)
                     (rcurry set-next current)))
      (if setter
          (funcall setter new)
          (initialize-tree new tree))))

(defun insert (key tree)
  (declare (type tree tree))
  (insert-node (make-node key :tree tree) tree))

(defgeneric extract-all (tree &optional opt)
  (:documentation "List of all elements in a binary search tree, ordered."))

(defmethod extract-all ((tree tree) &optional _)
  (declare (ignore _))
  (extract-all (root tree)))

(defmethod extract-all ((tree null) &optional tail)
  tail)

(defmethod extract-all ((node node) &optional tail)
  ;; Inner recursion step can not be transformed to tail recursion but maximum
  ;; stack depth is O(tree-height). Use parent relation for O(1).
  (with-slots (left right) node
    (extract-all left
                 (cons (key node)
                       (extract-all right
                                    tail)))))

(defgeneric minimum (tree))

(defmethod minimum ((tree tree))
  (minimum (root tree)))

(defmethod minimum ((tree null))
  NIL)

(defmethod minimum ((tree node))
  (with-slots (left) tree
    (if left
        (minimum left)
        (key tree))))

(defgeneric maximum (tree))

(defmethod maximum ((tree tree))
  (maximum (root tree)))

(defmethod maximum ((tree null))
  NIL)

(defmethod maximum ((tree node))
  (with-slots (right) tree
    (if right
        (maximum right)
        (key tree))))

