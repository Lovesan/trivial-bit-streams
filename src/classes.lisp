;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010, Dmitry Ignatiev <lovesan.ru@gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:trivial-bit-streams)

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype bit-counter () '(integer 0 7))
(deftype ub4 () '(unsigned-byte 4))
(deftype ub8 () '(unsigned-byte 8))
(deftype simple-ub8-vector () '(simple-array ub8 (*)))

(defvar *default-bit-output-stream-buffer-size* 4096
  "Default buffer size for output bit stream, in octets.")
(defvar *default-bit-input-stream-buffer-size* 4096
  "Default buffer size for input bit stream, in octets.")
(defvar *bit-stream-bit-io* t
"In case when this variable is bound to NIL, WRITE-BYTE/READ-BYTE and
WRITE-SEQUENCE/READ-SEQUENCE will behave like
WRITE-OCTET/READ-OCTET and WRITE-OCTET-VECTOR/READ-OCTET-VECTOR
while operating on bit streams. Otherwise, they will treat supplied
parameter as a single bit or a sequence of bits correspondingly.")

(define-condition bit-stream-error (stream-error)
  ()
  (:documentation "Represents a bit stream i/o error"))

(define-condition bit-stream-closed-error (bit-stream-error)
  ()
  (:report (lambda (condition stream)
             (declare (type stream stream))
             (format stream "Bit stream ~s is closed."
                     (stream-error-stream condition))
             condition))
  (:documentation
"Represents a condition when someone is trying to perform an i/o operation
on closed bit stream."))

(define-condition bit-stream-end-of-file (bit-stream-error end-of-file)
  ()
  (:documentation
    "Represents a condition when no more bits are availiable to read from bit input stream"))

(define-condition bit-stream-buffer-overflow (bit-stream-error)
  ((index :initarg :index
          :accessor bit-stream-buffer-overflow-index)
   (buffer-size :initarg :buffer-size
                :accessor bit-stream-buffer-overflow-buffer-size))
  (:report
    "Bit input stream callback returned an index that is greater than the size of the buffer")
  (:documentation
"Represents a condition when callback of a bit input stream returned value that
is greater than the size of the internal buffer"))

(defstruct (bit-stream-core (:constructor nil))
  (buffer nil :type simple-ub8-vector)  
  (end 0 :type non-negative-fixnum)
  (ibyte 0 :type non-negative-fixnum)
  (ibit  0 :type bit-counter)
  (byte-counter 0 :type unsigned-byte))

(declaim (inline make-bit-output-stream-core))
(defstruct (bit-output-stream-core
             (:include bit-stream-core)
             (:constructor
               make-bit-output-stream-core
               (buffer-size &aux (buffer (make-array buffer-size :element-type 'ub8))
                                 (end buffer-size)))))

(declaim (inline make-bit-input-stream-core))
(defstruct (bit-input-stream-core
            (:include bit-stream-core)
            (:constructor
              make-bit-input-stream-core
              (buffer-size &aux (buffer (make-array buffer-size :element-type 'ub8))))))

(defclass bit-stream (fundamental-binary-stream)
  ((core :type bit-stream-core)
   (callback :type function
             :initarg :callback
             :accessor bit-stream-callback)
   (buffer-size :initarg :buffer-size
                :type positive-fixnum))
  (:documentation "A mixin, that describes the state of a bit stream. Not to be instantiated."))

(defclass bit-output-stream (bit-stream
                             trivial-gray-stream-mixin
                             fundamental-binary-output-stream)
  ()
  (:default-initargs
    :buffer-size *default-bit-input-stream-buffer-size*
    :callback (lambda (&rest args)
                (declare (ignore args))
                0))
  (:documentation "Represents the state of the bit output stream."))

(defclass bit-input-stream (bit-stream
                            trivial-gray-stream-mixin
                            fundamental-binary-input-stream)
  ()
  (:default-initargs
    :buffer-size *default-bit-output-stream-buffer-size*
    :callback (lambda (&rest args)
                (declare (ignore args))
                (values)))
  (:documentation "Represents the state of the bit input stream."))
