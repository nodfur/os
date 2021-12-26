;;
;; Load foreign symbols from FreeType, Harfbuzz, etc.
;;
;; They're all built into one shared library file using Zig.
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

;; (define-foreign-library freetype2
;;   (:unix (:or "libfreetype.so" "libfreetype.dylib")))

;; (define-foreign-library harfbuzz
;;   (:unix (:or "libharfbuzz.so" "libharfbuzz.dylib")))

;; (use-foreign-library freetype2)
;; (use-foreign-library harfbuzz)

(define-foreign-library epap-zig
  (:unix (:or "./epap-zig/zig-out/lib/libepapi.so"
              "./epap-zig/zig-out/lib/libepapi-text.dylib")))

(use-foreign-library epap-zig)
