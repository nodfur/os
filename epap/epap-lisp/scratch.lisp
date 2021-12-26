(in-package :epap)

(defun byte-vector->bit-array (array width height)
  (let ((bit-array (make-array (list height width) :element-type 'bit)))
    (prog1 bit-array
      (loop
        for y from 0 below height
        do (loop
             for x from 0 below width
             do (let* ((pixel-index (+ (* y width) x))
                       (byte (aref array (truncate pixel-index 8))))
                  (setf (bit bit-array y x)
                        (if (logbitp (mod pixel-index 8) byte) 1 0))))))))

(test 'byte-vector->bit-array
  (assert-equalp
   (byte-vector->bit-array #(1) 1 8)
   (make-array '(8 1)
               :element-type 'bit
               :initial-contents '((1) (0) (0) (0) (0) (0) (0) (0)))))

(defun build-zig-function (name code)
  ;; For some weird reason, Zig when called from the Lisp process like this
  ;; seems to generate object files without the exported functions.
  (let* ((source-path
           (format nil "zig-lisp/~A.zig" name))
         (build-command
           (format nil "zig build-lib -dynamic ~A" source-path))
         (library-filename
           (format nil "lib~A.so" name))
         (library-path
           (format nil "zig-lisp/~A" library-filename)))
    (with-open-file (source-file source-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-sequence code source-file)
      (uiop:run-program build-command
                        :output :interactive
                        :error-output :interactive)
      (uiop:run-program (format nil "mv ~A zig-lisp/" library-filename))
      (cffi::register-foreign-library name `((:unix ,library-path)))
      (load-foreign-library name))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (= 1 (aref array i j)))))

(defun elisp-fun-xbm (width height text)
  ;; very slow
  (let* ((canvas
           (test-draw-line text :width width :height height))
         (bits (2d-array-to-list canvas))
         (elisp `(slime-media-insert-image
                  (create-image
                   (vconcat (mapcar (lambda (x)
                                      (apply #'bool-vector x))
                                    (quote ,bits)))
                   'xbm t :width ,width :height ,height)
                  " ")))
    (if *dry-run* elisp (swank:eval-in-emacs elisp))))

(defun elisp-fun-png (width height text)
  (canvas-to-png (test-draw-line text :width width :height height)))
