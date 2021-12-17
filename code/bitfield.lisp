(in-package #:bitfield)

(deftype bitfield ()
  "A bitfield is a non-negative integer that efficiently encodes
information about some booleans, enumerations, or small integers."
  'unsigned-byte)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bitfield Slots

(defgeneric bitfield-slot-name (bitfield-slot)
  (:documentation
   "Returns a symbol that is the name of the bitfield slot."))

(defgeneric bitfield-slot-start (bitfield-slot)
  (:documentation
   "Returns the position of the first bit of this slot in the bitfield."))

(defgeneric bitfield-slot-end (bitfield-slot)
  (:documentation
   "Returns the position right after the last bit of this slot in the bitfield."))

(defgeneric bitfield-slot-size (bitfield-slot)
  (:documentation
   "Returns an unsigned byte that is the number of distinct states of the slot."))

(defgeneric bitfield-slot-initform (bitfield-slot)
  (:documentation
   "Returns a form that produces the initial value for that slot."))

(defgeneric bitfield-slot-pack (bitfield-slot value-form)
  (:documentation
   "Takes a form that produces a value and turns it into a form that produces
a non-negative integer representing that value."))

(defgeneric bitfield-slot-unpack (bitfield-slot value-form)
  (:documentation
   "Take a form that produces a value that is encoded as a non-negative
integer (as produced by BITFIELD-SLOT-PACK), and turn it into a form that
produces the decoded value."))

(defgeneric parse-atomic-bitfield-slot-specifier
    (specifier &key initform)
  (:documentation
   "Parses an atomic bitfield slot specifier, i.e., a bitfield slot
specifier that is not a list.  Returns three values:

1. A designator for a bitfield slot class.

2. The size of the bitfield slot.

3. A list of additional arguments that will be supplied to MAKE-INSTANCE
when creating the bitfield slot instance."))

(defgeneric parse-compound-bitfield-slot-specifier
    (specifier arguments &key initform)
  (:documentation
   "Parses a compount bitfield slot specifier, i.e., a bitfield slot
specifier that is a list.  The SPECIFIER is the CAR of that list and the
ARGUMENTS are the CDR of that list.  Returns three values:

1. A designator for a bitfield slot class.

2. The size of the bitfield slot.

3. A list of additional arguments that will be supplied to MAKE-INSTANCE
when creating the bitfield slot instance."))

(defclass bitfield-slot ()
  ((%name :initarg :name :reader bitfield-slot-name)
   (%initform :initarg :initform :reader bitfield-slot-initform)
   (%start :initarg :start :reader bitfield-slot-start)
   (%end :initarg :end :reader bitfield-slot-end)
   (%size :initarg :size :reader bitfield-slot-size)))

;;; Boolean Slots

(defclass bitfield-boolean-slot (bitfield-slot)
  ())

(defmethod bitfield-slot-pack ((slot bitfield-boolean-slot) value-form)
  `(if ,value-form 1 0))

(defmethod bitfield-slot-unpack ((slot bitfield-boolean-slot) value-form)
  `(ecase ,value-form (0 nil) (1 t)))

(defmethod parse-atomic-bitfield-slot-specifier
    ((specifier (eql 'boolean)) &key (initform 'nil))
  (values 'bitfield-boolean-slot
          2
          `(:initform ,initform)))

;;; Integer Slots

(defclass bitfield-integer-slot (bitfield-slot)
  ((%offset
    :type integer
    :initarg :offset
    :reader bitfield-integer-slot-offset)))

(defmethod bitfield-slot-pack ((slot bitfield-integer-slot) value-form)
  (let ((offset (bitfield-integer-slot-offset slot))
        (size (bitfield-slot-size slot)))
    `(the (integer 0 (,size))
          (- (the (integer ,offset (,(+ offset size))) ,value-form)
             ,offset))))

(defmethod bitfield-slot-unpack ((slot bitfield-integer-slot) value-form)
  (let ((offset (bitfield-integer-slot-offset slot))
        (size (bitfield-slot-size slot)))
    `(the (integer ,offset (,(+ offset size)))
          (+ ,value-form ,offset))))

(defmethod parse-atomic-bitfield-slot-specifier
    ((specifier (eql 'bit)) &key (initform '0))
  (values 'bitfield-unsigned-byte-slot
          2
          `(:offset 0 :initform ,initform)))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'unsigned-byte)) arguments &key (initform '0))
  (destructuring-bind (bits) arguments
    (check-type bits unsigned-byte)
    (values 'bitfield-integer-slot
            (expt 2 bits)
            `(:offset 0 :initform ,initform))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'signed-byte)) arguments &key (initform '0))
  (destructuring-bind (bits) arguments
    (check-type bits unsigned-byte)
    (values 'bitfield-integer-slot
            (expt 2 bits)
            `(:offset ,(- (expt 2 (1- bits))) :initform ,initform))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'integer)) bounds &key (initform nil initform-supplied-p))
  (flet ((fail ()
           (error "Invalid integer bitfield slot specifier: ~S"
                  `(integer ,@bounds))))
    (unless (typep bounds '(cons t (cons t null)))
      (fail))
    (destructuring-bind (lo hi) bounds
      (let* ((start (typecase lo
                      (integer lo)
                      ((cons integer null)
                       (1+ (first lo)))
                      (otherwise (fail))))
             (end (typecase hi
                    (integer (1+ hi))
                    ((cons integer null)
                     (first hi))
                    (otherwise (fail))))
             (size (- end start)))
        (unless (plusp size)
          (fail))
        (values 'bitfield-integer-slot
                size
                `(:offset ,start :initform ,(if initform-supplied-p initform start)))))))

;;; Member Slots

(defclass bitfield-member-slot (bitfield-slot)
  ((%objects
    :type list
    :initarg :objects
    :reader bitfield-member-slot-objects)))

(defmethod bitfield-slot-pack ((slot bitfield-member-slot) value-form)
  `(ecase ,value-form
     ,@(loop for key in (bitfield-member-slot-objects slot)
             for value from 0
             collect `((,key) ,value))))

(defmethod bitfield-slot-unpack ((slot bitfield-member-slot) value-form)
  `(ecase ,value-form
     ,@(loop for key from 0
             for value in (bitfield-member-slot-objects slot)
             collect `((,key) ',value))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'member)) objects &key (initform `',(first objects)))
  (values 'bitfield-member-slot
          (length objects)
          `(:initform ,initform :objects ,objects)))

;;; Parsing

;;; The position right after the last slot that has been parsed so far.
(defvar *bitfield-position*)

(defun parse-bitfield-slot (slot)
  (destructuring-bind (slot-name slot-specifier &rest rest) slot
    (check-type slot-name symbol)
    (multiple-value-bind (slot-class size args)
        (if (consp slot-specifier)
            (apply #'parse-compound-bitfield-slot-specifier
                   (car slot-specifier)
                   (cdr slot-specifier)
                   rest)
            (apply #'parse-atomic-bitfield-slot-specifier
                   slot-specifier
                   rest))
      (apply #'make-instance slot-class
             :name slot-name
             :size size
             :start *bitfield-position*
             :end (incf *bitfield-position* (integer-length (1- size)))
             args))))

(defmacro define-bitfield (name &body slots)
  "Defines an encoding of enumerable properties like booleans,
integers or finite sets as a single non-negative integer.

For a supplied bitfield name NAME, and for some slot definitions of the
form (SLOT-NAME TYPE &KEY INITFORM &ALLOW-OTHER-KEYS), this macro defines
the following functions:

1. A constructor named MAKE-{NAME}, that takes one keyword argument per
   SLOT-NAME, similar to the default constructor generated by DEFSTRUCT.
   It returns a bitfield whose entries have the values indicated by the
   keyword arguments, or the supplied initform.

2. A clone operation named CLONE-{NAME}, that takes an existing bitfield
   and one keyword argument per SLOT-NAME.  It returns a copy of the
   existing bitfield, but where each supplied keyword argument supersedes
   the value of the corresponding slot.

3. A reader function named {NAME}-{SLOT-NAME} for each slot.

In addition to these functions, NAME is defined as a suitable subtype of
UNSIGNED-BYTE.

This macro supports boolean, integer, and member slots.  It is also
possible to add new kinds of slots by defining new subclasses of
BITFIELD-SLOT and the corresponding methods on BITFIELD-SLOT-PACK,
BITFIELD-SLOT-UNPACK and PARSE-ATOMIC-BITFIELD-SLOT-SPECIFIER or
PARSE-COMPOUND-BITFIELD-SLOT-SPECIFIER.

 Example:

 (define-bitfield examplebits
   (a boolean)
   (b (signed-byte 2))
   (c (unsigned-byte 3) :initform 1)
   (d (integer -100 100))
   (e (member foo bar baz)))

 (defun examplebits-values (examplebits)
   (list
    (examplebits-a examplebits)
    (examplebits-b examplebits)
    (examplebits-c examplebits)
    (examplebits-d examplebits)
    (examplebits-e examplebits)))

 (defparameter *default* (make-examplebits))

 (examplebits-values *default*)
 ;; => (nil 0 1 -100 foo)

 (defparameter *explicit* (make-examplebits :a t :b -1 :c 7 :d 42 :e 'baz))

 (examplebits-values *explicit*)
 ;; => (t -1 7 42 baz)

 (defparameter *clone* (clone-examplebits *explicit* :a nil :b -1 :c 2 :d -12 :e 'bar))

 (examplebits-values *clone*)
 ;; => (nil -1 2 -12 bar)
"
  (let* ((*bitfield-position* 0)
         (package (symbol-package name))
         (constructor
           (intern (concatenate 'string "MAKE-" (symbol-name name)) package))
         (cloner
           (intern (concatenate 'string "CLONE-" (symbol-name name)) package))
         (reader-prefix
           (concatenate 'string ))
         (slots
           (mapcar #'parse-bitfield-slot slots))
         (reader-names
           (loop for slot in slots
                 collect
                 (intern (concatenate 'string (symbol-name name) "-" reader-prefix
                                      (symbol-name (bitfield-slot-name slot)))
                         package))))
    `(progn
       (deftype ,name () '(unsigned-byte ,*bitfield-position*))
       ;; Define all slot readers.
       ,@(loop for slot in slots
               for reader-name in reader-names
               for start = (bitfield-slot-start slot)
               for end = (bitfield-slot-end slot)
               collect
               `(declaim (inline ,reader-name))
               collect
               `(defun ,reader-name (,name)
                  (declare (,name ,name))
                  ,(bitfield-slot-unpack
                    slot
                    `(ldb (byte ,(- end start) ,start) ,name))))
       ;; Define the cloner.
       (declaim (inline ,cloner))
       (defun ,cloner
           (,name &key ,@(loop for slot in slots
                               for reader-name in reader-names
                               collect
                               `(,(bitfield-slot-name slot)
                                 (,reader-name ,name))))
         (declare (,name ,name))
         (logior
          ,@(loop for slot in slots
                  collect
                  `(ash ,(bitfield-slot-pack slot (bitfield-slot-name slot))
                        ,(bitfield-slot-start slot)))))
       ;; Define the constructor.
       (declaim (inline ,constructor))
       (defun ,constructor
           (&key ,@(loop for slot in slots
                         collect
                         `(,(bitfield-slot-name slot)
                           ,(bitfield-slot-initform slot))))
         (logior
          ,@(loop for slot in slots
                  collect
                  `(ash ,(bitfield-slot-pack slot (bitfield-slot-name slot))
                        ,(bitfield-slot-start slot)))))
       ',name)))
