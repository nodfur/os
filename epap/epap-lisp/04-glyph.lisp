;;
;; Copyright (C) 2021  Restless Hypermedia, Inc.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :epap)

(defvar *current-font*)

(defparameter *font-table*
  '(:cozette "fonts/cozette.bdf"
    :dm-mono "fonts/DMMono-Regular.ttf"
    :concrete-roman "fonts/computer-modern/cmunorm.otf"
    :fantasque "fonts/FantasqueSansMono-Regular.otf"
    :fantasque-bold "fonts/FantasqueSansMono-Bold.otf"
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FreeType bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "FT_Error_String" :string (error-code :int32))
(defcfun "FT_Init_FreeType" :int32 (library :pointer))
(defcfun "FT_Done_FreeType" :int32 (library :pointer))

(defcstruct glyph-metrics
  (:width :int64)
  (:height :int64)
  (:horizontal-bearing-x :int64)
  (:horizontal-bearing-y :int64)
  (:horizontal-advance :int64)
  (:vertical-bearing-x :int64)
  (:vertical-bearing-y :int64)
  (:vertical-advance :int64))

(defcenum (glyph-format :uint32)
  :none
  (:composite #x636f6d70)
  (:bitmap #x62697473)
  (:outline #x6f75746c))

(defcenum (pixel-mode :uint8)
  :none :mono :gray :gray2 :gray4 :lcd :lcd-vv :bgra)

(defcstruct freetype-bitmap
  (:rows :uint32)
  (:width :uint32)
  (:pitch :int32)
  (:buffer :pointer)
  (:num-grays :uint16)
  (:pixel-mode pixel-mode)
  (:palette-mode :uint8)
  (:palette :pointer))

(defcstruct freetype-outline
  (:n-contours :int16)
  (:n-points :int16)
  (:points :pointer)
  (:tags :pointer)
  (:contours :pointer)
  (:flags :int32))

(defcstruct freetype-glyph-slot
  (:library :pointer)
  (:face :pointer )
  (:next (:pointer (:struct freetype-glyph-slot)))
  (:glyph-index :uint32)
  (:generic :pointer)
  (:generic-finalizer :pointer)
  (:metrics (:struct glyph-metrics))
  (:linear-horizontal-advance :int64)
  (:linear-vertical-advance :int64)
  (:advance-x :int64)
  (:advance-y :int64)
  (:format glyph-format)
  (:bitmap (:struct freetype-bitmap))
  (:bitmap-left :int32)
  (:bitmap-top :int32)
  (:outline (:struct freetype-outline))
  (:num-subglyphs :uint32)
  (:subglyphs :pointer)
  (:control-data :pointer)
  (:control-length :int64)
  (:lsb-delta :int64)
  (:rsb-delta :int64)
  (:other :pointer)
  (:internal :pointer))

(defcstruct (freetype-face)
  (:num-faces :int64)
  (:face-index :int64)
  (:face-flags :int64)
  (:style-flags :int64)
  (:num-glyphs :int64)
  (:family-name :string)
  (:style-name :string)
  (:num-fixed-sizes :int32)
  (:available-sizes :pointer)
  (:num-charmaps :int32)
  (:charmaps :pointer)
  (:generic :pointer)
  (:generic-finalizer :pointer)

  ;; these are only relevant for scalable outlines
  (:bbox (:array :int64 4))
  (:units-per-em :int16)
  (:ascender :int16)
  (:descender :int16)
  (:height :int16)
  (:max-advance-width :int16)
  (:max-advance-height :int16)
  (:underline-position :int16)
  (:underline-thickness :int16)

  (:glyph (:pointer (:struct freetype-glyph-slot)))
  (:size :pointer)
  (:charmap :pointer))

(defcfun "FT_New_Face" :int32
  (library :pointer)
  (path :string)
  (face-index :long)
  (face-ptr (:pointer (:struct freetype-face))))

(defcfun "FT_Set_Pixel_Sizes" :int32
  (face :pointer)
  (width :uint32)
  (height :uint32))

(defcfun "FT_Load_Glyph" :int32
  (face :pointer)
  (glyph-index :uint32)
  (load-flags :int32))

(defcenum ft-render-mode
  :normal :light :mono :lcd :lcd-v :sdf)

(defun ft-load-target (mode)
  (ash (foreign-enum-value 'ft-render-mode mode) 16))

(defun load-flag-number (flag)
  (ecase flag
    (:render (ash 1 2))
    (:target-mono (ft-load-target :mono))
    (:force-autohint (ash 1 5))))

(defun make-load-flags (flags)
  (apply #'logior (mapcar #'load-flag-number flags)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Harfbuzz bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "hb_ft_font_create_referenced" :pointer
  (freetype-font :pointer))

(defcfun "hb_ft_font_set_funcs" :void
  (font :pointer))

(defcfun "hb_buffer_create" :pointer)
(defcfun "hb_buffer_destroy" :void (buffer :pointer))

(defcenum hb-direction
  (:invalid 0)
  (:left-to-right 4)
  :right-to-left
  :top-to-bottom
  :bottom-to-top)

(defcfun "hb_buffer_set_direction" :void
  (buffer :pointer)
  (direction hb-direction))

(defcfun "hb_script_from_string" :uint32
  (string :string)
  (len :int))

(defcfun "hb_buffer_set_script" :void
  (buffer :pointer)
  (script :uint32))

(defcfun "hb_buffer_set_language" :void
  (buffer :pointer)
  (language :pointer))

(defcfun "hb_language_from_string" :pointer
  (string :string)
  (len :int))

(defcfun "hb_buffer_add_utf8" :void
  (buffer :pointer)
  (text :string)
  (length :int32)
  (offset :uint32)
  (count :int32))

(defcfun "hb_shape" :void
  (font :pointer)
  (buffer :pointer)
  (features :pointer)
  (num-features :uint32))

(defcstruct (glyph-info :size 20)
  (:codepoint :uint32)
  (:cluster :uint32 :offset 8))

(defcstruct (glyph-position :size 20)
  (:x-advance :int32)
  (:y-advance :int32)
  (:x-offset :int32)
  (:y-offset :int32))

(defcstruct glyph-extents
  (:x-bearing :int32)
  (:y-bearing :int32)
  (:width :int32)
  (:height :int32))

(defcfun "hb_buffer_get_glyph_infos"
    (:pointer (:struct glyph-info))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_buffer_has_positions" :boolean
  (buffer :pointer))

(defcfun "hb_buffer_get_glyph_positions"
    (:pointer (:struct glyph-position))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_font_get_glyph_extents"
    :bool
  (font :pointer)
  (glyph :uint32)
  (extents (:pointer (:struct glyph-extents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition freetype-error (error)
  ((code :initarg :code :reader freetype-error-code)))

(defun check-freetype-result (result)
  (unless (zerop result)
    (error 'freetype-error :code result)))

(defun initialize-freetype ()
  (format t "; Initializing FreeType~%")
  (let ((freetype (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-init-freetype freetype))
    (mem-ref freetype :pointer)))

(defstruct font
  height
  freetype-ptr
  harfbuzz-ptr)

(defvar *freetype*
  (initialize-freetype))

;; (setf *freetype* (initialize-freetype))

(defun ensure-freetype ()
  (unless *freetype*
    (setf *freetype* (initialize-freetype))))

(defun load-freetype-font (path height)
  (ensure-freetype)
  (printv:printv
   *freetype* path height)
  (let ((face-ptr (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-new-face *freetype* path 0 face-ptr))
    (let ((face (mem-ref face-ptr :pointer)))
      (prog1 face
        (check-freetype-result
         (ft-set-pixel-sizes face 0 height))))))

(defun load-font (path height)
  (format t "; loading font ~A at ~Apx~%" path height)
  (let* ((freetype-ptr (load-freetype-font path height))
         (harfbuzz-ptr (hb-ft-font-create-referenced freetype-ptr)))
    (hb-ft-font-set-funcs harfbuzz-ptr)
    (make-font
     :height height
     :freetype-ptr freetype-ptr
     :harfbuzz-ptr harfbuzz-ptr)))

(defun find-font (name height)
  (let ((path (getf *font-table* name)))
    (if path (load-font path height)
        (error "unknown font name ~A" name))))

(defmacro with-font (name height &body body)
  `(let ((*current-font* (find-font ,name ,height)))
     ,@body))

(defun change-font (name height)
  (setf *current-font* (find-font name height)))

(defmacro with-hb-buffer (var &body body)
  `(let ((,var (hb-buffer-create)))
     (unwind-protect (progn ,@body)
       (hb-buffer-destroy ,var))))

(defun read-foreign-array (&key pointer element-type count)
  (foreign-array-to-lisp pointer `(:array ,element-type ,count)))

(defun shape-text (text &key
                          (language "en")
                          (direction :left-to-right)
                          (script "Latn"))
  (with-hb-buffer buffer
    (hb-buffer-set-direction buffer direction)
    (hb-buffer-set-script buffer (hb-script-from-string script -1))
    (hb-buffer-set-language buffer (hb-language-from-string language -1))
    (hb-buffer-add-utf8 buffer text -1 0 -1)
    (hb-shape (font-harfbuzz-ptr *current-font*) buffer (null-pointer) 0)
    (with-foreign-object (glyph-count :uint32)
      (let* ((glyph-infos
               (read-foreign-array
                :pointer (hb-buffer-get-glyph-infos buffer glyph-count)
                :element-type '(:struct glyph-info)
                :count (mem-ref glyph-count :uint32)))
             (glyph-positions
               (read-foreign-array
                :pointer (hb-buffer-get-glyph-positions buffer glyph-count)
                :element-type '(:struct glyph-position)
                :count (mem-ref glyph-count :uint32))))
        (list glyph-infos glyph-positions)))))

(test 'shape-text
  (assert-equalp
   (with-font :cozette 13
     (shape-text "xyz"))
   (list #((:cluster 0 :codepoint 121)
           (:cluster 1 :codepoint 122)
           (:cluster 2 :codepoint 123))
         #((:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
           (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
           (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)))))

(defun read-glyph-slot (font)
  (mem-ref
   (getf
    (mem-ref (font-freetype-ptr font) '(:struct freetype-face))
    :glyph)
   '(:struct freetype-glyph-slot)))

(defun read-glyph-bitmap (font)
  (getf (read-glyph-slot font) :bitmap))

(defun load-glyph (font glyph-id)
  (check-freetype-result
   (ft-load-glyph (font-freetype-ptr font) glyph-id
                  (make-load-flags '(:render :target-mono :force-autohint))))
  (with-foreign-object (extents '(:struct glyph-extents))
    (unless (hb-font-get-glyph-extents (font-harfbuzz-ptr font)
                                       glyph-id extents)
      (error "harfbuzz: failed to get glyph extents for glyph ~A" glyph-id))
    (mem-ref extents '(:struct glyph-extents))))
