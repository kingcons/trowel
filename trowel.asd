(defsystem #:trowel
  :name "trowel"
  :description "A Code Excavation Toolkit"
  :version "0.0.1"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:cl-6502 :romreader :graph)
  :serial t
  :components ((:file "packages")
               (:file "jumps")
               (:file "search")
               (:file "decompiler"))
  :in-order-to ((test-op (load-op trowel-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :trowel-tests)
                             (intern "TROWEL-TESTS" :trowel-tests))))

(defsystem #:trowel-tests
  :depends-on (:trowel :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "tests")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :trowel))))
  (values nil))

(defpackage #:trowel-conf (:export #:app-path))
(defvar trowel-conf::*basedir*
  (make-pathname :defaults *load-truename* :name nil :type nil))
(defun trowel-conf:app-path (path &rest args)
  (merge-pathnames (apply 'format nil path args) trowel-conf::*basedir*))
