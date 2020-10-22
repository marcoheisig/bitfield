(cl:in-package #:common-lisp-user)

(defpackage #:bitfield
  (:use #:common-lisp)
  (:export
   #:bitfield
   #:bitfield-slot-name
   #:bitfield-slot-start
   #:bitfield-slot-end
   #:bitfield-slot-size
   #:bitfield-slot-reader
   #:bitfield-slot-initform
   #:bitfield-slot-pack
   #:bitfield-slot-unpack
   #:parse-atomic-bitfield-slot-specifier
   #:parse-compound-bitfield-slot-specifier
   #:bitfield-slot
   #:bitfield-boolean-slot
   #:bitfield-integer-slot
   #:bitfield-member-slot
   #:define-bitfield))
