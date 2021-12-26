(define-foreign-library cap
  (:unix "libcap.so"))

(use-foreign-library cap)

(define-foreign-type cap-return-code-type ()
  ()
  (:actual-type :int)
  (:simple-parser cap-return-code))

(defmethod translate-from-foreign (value (type cap-return-code-type))
  (if (zerop value) :ok (error "libcap error")))

(defcenum cap-flag
  (:effective 0)
  (:permitted 1)
  (:inheritable 2))

(defcenum cap-flag-value
  (:clear 0)
  (:set 1))

(defcfun "cap_get_proc" :pointer)
(defcfun "cap_set_flag" cap-return-code
  (state :pointer)
  (flag cap-flag)
  (count :int)
  (capabilities (:pointer :int))
  (value cap-flag-value))

(defcfun "cap_set_proc" cap-return-code
  (state :pointer))

(defcfun "cap_free" cap-return-code
  (object :pointer))

(defcfun "cap_to_text" :pointer
  (state :pointer)
  (length-ptr :pointer))

(defmacro with-process-capabilities (var &body body)
  `(let ((,var (cap-get-proc)))
     (unwind-protect (progn ,@body) (cap-free ,var))))

(defun describe-capabilities (cap-state)
  (let* ((text-ptr (cap-to-text cap-state (null-pointer)))
         (text (foreign-string-to-lisp text-ptr)))
    (prog1 text (cap-free text-ptr))))

(defun request-raw-i/o-capability ()
  (with-process-capabilities cap-state
    (progn
      (with-foreign-array (caps #(17) '(:array :int 1))
        (cap-set-flag cap-state :effective 1 caps :set))
      (cap-set-proc cap-state))))

(defun describe-process-capabilities ()
  (with-process-capabilities cap-state
    (describe-capabilities cap-state)))
