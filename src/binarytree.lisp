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
    :accessor root)
   (compare
    :initarg :compare
    :initform (lambda (x y) (cond ((< x y) -1)
                                  ((> x y) 1)
                                  (t 0)))
    :type function)))

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

(defun compare-keys (tree x y)
  (declare (type tree tree))
  (funcall (slot-value tree 'compare) x y))

(defun compare-nodes (x y)
  (declare (type node x y))
  (assert (eq (tree x) (tree y)))
  (compare-keys (tree x) (key x) (key y)))

(defun compare-keys-cont (tree x y lt gt &optional (eq gt))
  "Compare given keys and call given functions on appropriate comparison evaluation."
  (declare (type tree tree)
           (type function lt gt eq))
  (funcall
   (ecase (the (member -1 0 1) (compare-keys tree x y))
     (-1 lt)
     (0 eq)
     (1 gt))))

(defun compare-nodes-cont (x y lt gt &optional (eq gt))
  "Compare nodes and call continuations appropriately"
  (declare (type node x y))
  (assert (eq (tree x) (tree y)))
  (compare-keys-cont (tree x) (key x) (key y) lt gt eq))

(defun search-node (key tree &optional (subtree (root tree)))
  (declare (type tree tree)
           (type (or null node) subtree))
  (if subtree
    (compare-keys-cont tree
                       key
                       (key subtree)
                       (lambda () (search-node key tree (left subtree)))
                       (lambda () (search-node key tree (right subtree)))
                       (constantly subtree))))

(defun insert-node-subtree (new subtree setter)
  "Insert a new node in the subtree."
  (declare (type node new)
           (type function setter)
           (type (or null node) subtree))
  (assert (or (null subtree) (eq (tree new) (tree subtree))))
  (if subtree
      (let ((cont (curry #'insert-node-subtree new)))
        (compare-nodes-cont new
                            subtree
                            (curry cont
                                   (left subtree)
                                   (rcurry #'(setf left) subtree))
                            (curry cont
                                   (right subtree)
                                   (rcurry #'(setf right) subtree))
                            (compose (lambda ()
                                       (error "Duplicate key: reinserting ~S"
                                              (key new))))))
      (funcall setter new)))

(defun insert-node (new tree)
  (declare (type tree tree)
           (type node new))
  (insert-node-subtree new (root tree) (rcurry #'initialize-tree tree)))

(defun insert (key tree &optional value)
  (declare (type tree tree))
  (insert-node (make-node key :tree tree :value value) tree))

(defgeneric extract-all (tree &optional opt)
  (:documentation "List of all elements in a binary search tree as an ordered
association list."))

(defmethod extract-all ((tree tree) &optional _)
  (declare (ignore _))
  (extract-all (root tree)))

(defmethod extract-all ((tree null) &optional tail)
  tail)

(defmethod extract-all ((node node) &optional tail)
  ;; Inner recursion step can not be transformed to tail recursion but maximum
  ;; stack depth is O(tree-height). Use parent relation for O(1).
  (declare (type list tail))
  (with-slots (left right) node
    (the list (extract-all left
                           (cons (cons (key node) (value node))
                                 (the list (extract-all right
                                                        tail)))))))

(defgeneric tree-minimum (tree))

(defmethod tree-minimum ((tree tree))
  (tree-minimum (root tree)))

(defmethod tree-minimum ((tree null))
  NIL)

(defmethod tree-minimum ((tree node))
  (with-slots (left) tree
    (if left
        (tree-minimum left)
        tree)))

(defun minimum (x)
  (let ((node (tree-minimum x)))
    (when node
      (cons (key node) (value node)))))

(defgeneric tree-maximum (tree))

(defmethod tree-maximum ((tree tree))
  (tree-maximum (root tree)))

(defmethod tree-maximum ((tree null))
  NIL)

(defmethod tree-maximum ((tree node))
  (with-slots (right) tree
    (if right
        (tree-maximum right)
        tree)))

(defun maximum (x)
  (let ((node (tree-maximum x)))
    (when node
      (cons (key node) (value node)))))

(defun transplant (x y)
  "Replace the subtree x by subtree y"
  (declare (type node x)
           (type (or null node) y))
  (let ((parent (parent x)))
    (if parent
        (if (eq x (left parent))
            (setf (left parent) y)
            (setf (right parent) y))
        (setf (root (tree x)) y))))

(defun delete-node (node)
  (let ((parent (parent node))
        (next (tree-minimum (right node))))
    ;; Find the node that should take the place of the deleted node
    (let ((new
           (if next
               (progn
                 ;; The minimum can have no left (and thus smaller) child
                 (assert (null (left next)))
                 (transplant next (right next))
                 (setf (left next) (left node))
                 (setf (right next) (right node))
                 next)
               (progn
                 ;; There is no minimum right element -> there is no right child
                 (assert (null (right node)))
                 (left node)))))
      ;; Set the parent to point to that node
      (if parent
          (if (eq node (left parent))
              (setf (left parent) new)
              (setf (right parent) new))
          (setf (root (tree node)) new)))))

(defgeneric delete-key (key tree))

(defmethod delete-key (key (tree tree))
  (let ((node (search-node key tree)))
    (if node
        (delete-node node)
        (error "Key not found: ~S" key)))
  tree)

(defgeneric height (tree))

(defmethod height ((tree tree))
  (height (root tree)))

(defmethod height ((tree null))
  (declare (ignore tree))
  0)

(defmethod height ((tree node))
  (1+ (max (height (left tree)) (height (right tree)))))

(defgeneric size (tree))

(defmethod size ((tree tree))
  (size (root tree)))

(defmethod size ((tree null))
  (declare (ignore tree))
  0)

(defmethod size ((tree node))
  (with-slots (left right) tree
    (+ 1 (size left) (size right))))

(defgeneric orderedp (tree))

(defmethod orderedp ((tree tree))
  (orderedp (root tree)))

(defmethod orderedp ((tree null))
  T)

(defmethod orderedp ((tree node))
  (with-slots (left right) tree
    (and (or (null left)
             (compare-nodes-cont left
                                 tree
                                 (curry #'orderedp left)
                                 (constantly NIL)))
         (or (null right)
             (compare-nodes-cont tree
                                 right
                                 (curry #'orderedp right)
                                 (constantly NIL))))))

(defun node->kvpair (node)
  (declare (type node node))
  (with-slots (key value) node
    (cons key value)))

(defun extract-node (key tree &key not-found)
  (declare (type tree tree))
  (let ((node (search-node key tree)))
    (if node
        node
        (ecase not-found
          ((:error) (error "Key not found: ~S" key))
          ((NIL) NIL)))))

(defun extract-value (&rest argv)
  (value (apply #'extract-node argv)))

(defun extract (&rest argv)
  (node->kvpair (apply #'extract-node argv)))
