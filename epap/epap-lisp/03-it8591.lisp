;;
;; Driver for the IT8591 e-paper display controller via GPIO/SPI from
;; the Raspberry Pi 400.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module parameters and interesting variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vcom* -1.73
  "This is a unit-specific voltage value.")

(defvar *display-width*
  "The discovered pixel width of the e-paper screen.")

(defvar *display-height*
  "The discovered pixel height of the e-paper screen.")

(defvar *framebuffer-address*
  ;; I don't really know how this works.
  ;; Can we use multiple framebuffers?
  ;; Can they be composited together somehow?
  "The discovered address of the default framebuffer.")

(defvar *local-framebuffer* nil
  "Our Lisp bit array that we render into the framebuffer.")

(defun clear-local-framebuffer ()
  (dotimes (y *display-height* nil)
    (dotimes (x *display-width* nil)
      (setf (sbit *local-framebuffer* y x) 0))))

(defun clear-local-area (x y w h)
  (dotimes (i h nil)
    (dotimes (j w nil)
      (setf (sbit *local-framebuffer* (+ y i) (+ x j)) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level protocol stuff for the IT8591 controller.
;;
;; We introduce GPIO pins, commands, packets, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype pin ()
  '(member :rst :cs :busy))

(deftype packet-type ()
  '(member :read :write :command))

(deftype command-id ()
  '(member
    :run :standby :sleep :read-register :write-register
    :vcom :dev-info :load-img-area-start :load-img-end
    :display-area :display-area-buf))

(deftype register ()
  '(member
    :I80CPCR
    :LISAR+0
    :LISAR+2
    :LUTAFSR
    :UP1SR+2
    :BGVR))

(defun register-number (register)
  (ecase register
    (:I80CPCR #x4)
    (:LISAR+0 (+ #x200 #x8))
    (:LISAR+2 (+ #x200 #x8 2))
    (:LUTAFSR (+ #x1000 #x224))
    (:UP1SR+2 (+ #x1000 #x138 2))
    (:BGVR (+ #x1000 #x250))))

(defun pin-number (pin)
  (ecase pin
    (:rst 17)
    (:cs 8)
    (:busy 24)))

(defun packet-type-number (packet-type)
  (ecase packet-type
    (:read #x1000)
    (:write #x0000)
    (:command #x6000)))

(defun command-number (command-id)
  (ecase command-id
    (:run #x1)
    (:standby #x2)
    (:sleep #x3)
    (:read-register #x10)
    (:write-register #x11)
    (:vcom #x39)
    (:dev-info #x302)
    (:load-img-area-start #x21)
    (:load-img-end #x22)
    (:display-area #x34)
    (:display-area-buf #x37)))

(defun-with-dry-run initialize-bcm2835 ()
  (unless (bcm2835-init)
    (error "bcm2835_init failed"))
  (unless (bcm2835-spi-begin)
    (error "bcm2835_spi_begin failed"))

  (bcm2835-spi-setBitOrder :most-significant-bit-first)
  (bcm2835-spi-setDataMode :mode-0)
  (bcm2835-spi-setClockDivider :divider-32))

(defun-with-dry-run close-bcm2835 ()
  (gpio-write :cs 0)
  (gpio-write :rst 0)
  (bcm2835-spi-end)
  (unless (bcm2835-close)
    (error "bcm2835_close failed")))

(defun gpio-mode (pin mode)
  (no-dry-run)
  (bcm2835-gpio-fsel (pin-number pin) mode))

(defun gpio-read (pin)
  (no-dry-run)
  (bcm2835-gpio-lev (pin-number pin)))

(defun gpio-wait ()
  (sb-ext:with-timeout 5
    (loop until (= 1 (gpio-read :busy)))))

(defun cs-low ()
  (gpio-write :cs 0))

(defun cs-high ()
  (gpio-write :cs 1))

(defun-with-dry-run initialize-gpio ()
  (gpio-mode :rst :output)
  (gpio-mode :cs :output)
  (gpio-mode :busy :input)
  (cs-high))

(defun start-packet (packet-type)
  (gpio-wait)
  (cs-low)
  (spi-write-word (packet-type-number packet-type))
  (gpio-wait))

(defun-with-dry-run write-command (cmd)
  (start-packet :command)
  (spi-write-word (command-number cmd))
  (cs-high))

(defun spi-write-word (x)
  (no-dry-run)
  (spi-write-byte (ldb (byte 8 8) x))
  (spi-write-byte (ldb (byte 8 0) x)))

(defun spi-read-word ()
  (let* ((high (spi-read-byte))
         (low (spi-read-byte)))
    (dpb high (byte 8 8) low)))

(defun spi-read-address ()
  (let* ((low (spi-read-word))
         (high (spi-read-word)))
    (dpb high (byte 16 16) low)))

(defun start-reading ()
  (start-packet :read)
  (spi-read-word) ;; read a dummy word
  (gpio-wait))

(defun request-word ()
  (start-reading)
  (prog1 (spi-read-word)
    (cs-high)))

(defun-with-dry-run write-word-packet (word)
  (start-packet :write)
  (spi-write-word word)
  (cs-high))

(defun-with-dry-run write-repetitive-multiword-packet (n value)
  (start-packet :write)
  (dotimes (i n)
    (spi-write-word value))
  (cs-high))

(defun-with-dry-run write-multiword-packet (words)
  (start-packet :write)
  (dolist (word words)
    (spi-write-word word))
  (cs-high))

(defun-with-dry-run write-word-packets (words)
  (dolist (word words)
    (write-word-packet word)))

(defun-with-dry-run write-register (register value)
  (write-command :write-register)
  (write-word-packet (register-number register))
  (write-word-packet value))

(defun read-register (register)
  (write-command :read-register)
  (write-word-packet (register-number register))
  (request-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Having established the communication primitives for the IT8591 protocol,
;; we can define composite operations that read and write multiple words.
;;
;; Now we start to talk about pixel formats, panel widths, framebuffers,
;; partial updates, and so on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bpp-pixel-format (bits)
  (ecase bits
    (2 0)
    (3 1)
    (4 2)
    (8 3)))

(defstruct system-info
  width height address firmware-version lut-version)

(defun get-system-info ()
  (if *dry-run*
      (make-system-info
       :width 1872
       :height 1404
       :address #xdeadbeef
       :firmware-version "firmware"
       :lut-version "lookup-table")
      (progn
        (write-command :dev-info)
        (start-reading)
        (prog1
            (make-system-info
             :width (spi-read-word)
             :height (spi-read-word)
             :address (spi-read-address)
             :firmware-version
             (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii)
             :lut-version
             (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii))
          (cs-high)))))

(defun-with-dry-run reset-display ()
  (gpio-write :rst 1)
  (delay-milliseconds 200)
  (gpio-write :rst 0)
  (delay-milliseconds 10)
  (gpio-write :rst 1)
  (delay-milliseconds 200))

(defun-with-dry-run set-vcom (vcom)
  (write-command :vcom)
  (write-word-packet 1)
  (write-word-packet (round (* 1000 (abs vcom)))))

(defun initialize-display ()
  (reset-display)
  (write-command :run)
  (prog1 (get-system-info)
    (write-register :I80CPCR 1)
    (set-vcom
     (or *vcom* (restart-case (error "VCOM value required")
                  (use-1.73 ()
                    :report "Use the -1.73 value."
                    -1.73)
                  (provide-vcom-value (vcom)
                    :report "Provide a value."
                    :interactive (lambda ()
                                   (format t "VCOM value: ")
                                   (multiple-value-list (eval (read))))
                    vcom))))))

(defun-with-dry-run enter-sleep-mode ()
  (write-command :sleep))

(defun start-display ()
  (initialize-bcm2835)
  (initialize-gpio)
  (let ((info (initialize-display)))
    (prog1 info
      (setf *display-width* (system-info-width info)
            *display-height* (system-info-height info)
            *local-framebuffer* (or *local-framebuffer*
                                    (make-array
                                     (list *display-height* *display-width*)
                                     :element-type 'bit))
            *framebuffer-address* (system-info-address info)))))

(defun stop-display ()
  (enter-sleep-mode)
  (close-bcm2835))

(defmacro with-display-running (&body body)
  `(progn
     (start-display)
     (unwind-protect (progn ,@body)
       (stop-display))))

(defmacro optimistic-timeout (seconds check)
  `(unless (loop repeat 10000 never ,check)
     (sb-ext:with-timeout 5
       (loop until ,check))))

(defun-with-dry-run wait-for-display ()
  (optimistic-timeout 5 (zerop (read-register :lutafsr))))

(defparameter *image-endianness* 0)     ;; little
(defparameter *image-rotation* 0)       ;; normal
(defparameter *image-bpp* 3)            ;; bitmap

(defun set-framebuffer-address (address)
  (write-register :lisar+2 (ldb (byte 16 16) address))
  (write-register :lisar+0 (ldb (byte 16 0) address)))

(defun read-word-from-bitmap (bitmap row offset)
  (unless (zerop (mod offset 16))
    (error "bitmap offset ~A not word aligned" offset))
  (loop for i from 0 below 16
        sum (ash (bit bitmap row (+ offset i)) i)))

;; (test 'read-word-from-bitmap
;;   (let ((canvas (make-array '(32 32) :element-type 'bit)))
;;     (setf (bit canvas 0 0) 1
;;           (bit canvas 0 1) 1
;;           (bit canvas 1 30) 1)
;;     (assert-equalp
;;      (read-word-from-bitmap canvas 0 0)
;;      #b1100000000000000)
;;     (assert-equalp
;;      (read-word-from-bitmap canvas 0 16)
;;      0)
;;     (assert-equalp
;;      (read-word-from-bitmap canvas 1 16)
;;      #b0000000000000010)))

(defparameter *pixel-rounding* 32)

(defun round-down-to-word (i)
  (* *pixel-rounding* (truncate i *pixel-rounding*)))

(defun round-up-to-word (i)
  (* *pixel-rounding*
     (truncate (+ i *pixel-rounding* -1)
               *pixel-rounding*)))

(defun read-area-words (x y w h)
  (loop
    for i from y below (+ y h)
    collecting
    (loop
      for j from x by 16 below (+ x w)
      collecting (lognot
                  (read-word-from-bitmap *local-framebuffer* i j)))))

(defparameter *use-packed-writes* t)

(defmacro with-packed-writes (use-packed-writes &body body)
  `(let ((*use-packed-writes* ,use-packed-writes))
     ,@body))

(defun-with-dry-run start-loading-image-area (x y w h)
  (wait-for-display)
  (set-framebuffer-address *framebuffer-address*)
  (let* ((format (logior (ash *image-endianness* 8)
                         (ash *image-bpp* 4)
                         (ash *image-rotation* 0)))
         (args (list format x y w h)))
    (write-command :load-img-area-start)
    (write-word-packets args)))

(defun-with-dry-run copy-area-to-framebuffer (x y w h)
  (let* ((x% (round-down-to-word x))
         (y% (round-down-to-word y))
         (xe (- x x%))
         (ye (- y y%))
         (w% (min *display-width* (round-up-to-word (+ w xe))))
         (h% (min *display-height* (round-up-to-word (+ h ye)))))
    (let ((*image-bpp* 3))
      (start-loading-image-area (/ x% 8) y% (/ w% 8) h%))
    (let ((words (apply #'append (read-area-words x% y% w% h%))))
      (if *use-packed-writes*
          (write-multiword-packet words)
          (write-word-packets words))))
  (write-command :load-img-end))

(defun write-register-bit (register &key index bit)
  (let ((value (read-register register)))
    (write-register register (dpb bit (byte 1 index) value))))

(defparameter *a2-mode* 6)
(defparameter *initialize-mode* 0)

(defun-with-dry-run display-area (&key address rectangle mode)
  (destructuring-bind (&key x y w h) rectangle
    (let* ((x% (round-down-to-word x))
           (y% (round-down-to-word y))
           (xe (- x x%))
           (ye (- y y%))
           (w% (min *display-width* (round-up-to-word (+ w xe))))
           (h% (min *display-height* (round-up-to-word (+ h ye)))))
      (ecase mode
        (:fast-monochrome
         (progn
           (write-register-bit :up1sr+2 :index 2 :bit 1)
           (write-register :bgvr #xf0)
           (write-command :display-area-buf)
           (write-word-packets
            (list x% y% w% h% *a2-mode*
                  (ldb (byte 16 0) address)
                  (ldb (byte 16 16) address)))
           (wait-for-display)
           (write-register-bit :up1sr+2 :index 2 :bit 0)))
        (:initialize
         (progn
           (write-command :display-area)
           (write-word-packets
            (list x% y% w% h% *initialize-mode*))))))))

(defun full-screen-rectangle ()
  (list :x 0 :y 0 :w *display-width* :h *display-height*))

(defun initialize-blank-display ()
  (let* ((*image-bpp* 2)
         (pixel-count (* *display-width* *display-height*))
         (bit-count (* pixel-count 4))
         (word-count (/ bit-count 16)))
    (start-loading-image-area 0 0 *display-width* *display-height*)
    (write-repetitive-multiword-packet word-count #xffff)
    (write-command :load-img-end)
    (clear-local-framebuffer)
    (display-area
     :address *framebuffer-address*
     :rectangle (full-screen-rectangle)
     :mode :initialize)))
