;;
;; Bindings to Mike McCauley's BCM2385 library for Raspberry Pi
;; GPIO/SPI, which is a submodule in vendor/bcm2385-1.70 and compiled
;; with `make` into a shared library which this file loads.
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

(defcenum (spi-bit-order :uint8)
  :least-significant-bit-first
  :most-significant-bit-first)

(defcenum (spi-data-mode :uint8)
  :mode-0 :mode-1 :mode-2 :mode-3)

(defcenum (spi-clock-divider :uint16)
  (:divider-32 32)
  (:divider-16 16))

(defcenum (gpio-function-select :uint8)
  :input :output)

(defcfun "bcm2835_init" :boolean)
(defcfun "bcm2835_close" :boolean)
(defcfun "bcm2835_spi_begin" :boolean)
(defcfun "bcm2835_spi_end" :boolean)

(defcfun "bcm2835_spi_setBitOrder" :void
  (order spi-bit-order))
(defcfun "bcm2835_spi_setDataMode" :void
  (mode spi-data-mode))
(defcfun "bcm2835_spi_setClockDivider" :void
  (divider spi-clock-divider))

(defcfun "bcm2835_gpio_write" :void
  (pin :uint8)
  (bit :uint8))

(defcfun "bcm2835_gpio_lev" :uint8
  (pin :uint8))

(defcfun "bcm2835_gpio_fsel" :void
  (pin :uint8)
  (mode gpio-function-select))

(defcfun "bcm2835_delay" :void
  (milliseconds :uint32))

(defcfun "bcm2835_delayMicroseconds" :void
  (microseconds :uint64))

(defcfun "bcm2835_spi_transfer" :uint8
  (value :uint8))

(defun gpio-write (pin bit)
  (no-dry-run)
  (bcm2835-gpio-write (pin-number pin) bit))

(defun delay-milliseconds (ms)
  (if *dry-run*
      (sleep (/ ms 1000))
      (bcm2835-delay ms)))

(defun delay-microseconds (us)
  (bcm2835-delaymicroseconds us))

(defun spi-write-byte (x)
  (no-dry-run)
  (bcm2835-spi-transfer x))

(defun spi-read-byte ()
  (no-dry-run)
  (bcm2835-spi-transfer 0))

(defun spi-read-bytes (count)
  ;; slightly inefficient
  (remove-if #'zerop
             (make-array
              (list count)
              :element-type '(unsigned-byte 8)
              :initial-contents (loop repeat count
                                      collect (spi-read-byte)))))
