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

(declaim (optimize (speed 3)))

(defun bit-stream-byte-counter (bit-stream)
  (declare (type bit-stream bit-stream))
  "Retrieves total number of bytes processed by bit stream"
  (bit-stream-core-byte-counter (slot-value bit-stream 'core)))

(defmethod stream-element-type ((stream bit-stream))
  'bit)

(defmethod shared-initialize :after
  ((stream bit-input-stream) slot-names &key buffer-size callback)
  (declare (ignore slot-names))
  (check-type buffer-size positive-fixnum)
  (check-type callback function)
  (setf (slot-value stream 'core)
        (make-bit-input-stream-core buffer-size))
  stream)

(defmethod shared-initialize :after
  ((stream bit-output-stream) slot-names &key buffer-size callback)
  (declare (ignore slot-names))
  (check-type buffer-size positive-fixnum)
  (check-type callback function)
  (setf (slot-value stream 'core)
        (make-bit-output-stream-core buffer-size))
  stream)

(declaim (inline inc-bit-counter))
(defun inc-bit-counter (bit-stream-core &optional (n 1))
  (declare (type bit-stream-core bit-stream-core)
           (type (integer 0 8) n))
  (with-accessors ((ibit bit-stream-core-ibit)
                   (ibyte bit-stream-core-ibyte))
      bit-stream-core
    (multiple-value-bind
        (div mod) (floor (the ub4 (+ ibit n)) 8)
      (setf ibit mod ibyte (+ ibyte div))))
  (values))

(defun ensure-output (bit-stream-core callback)
  (declare (type bit-output-stream-core bit-stream-core)
           (type function callback))
  (with-accessors ((end bit-stream-core-end)
                   (ibyte bit-stream-core-ibyte)
                   (ibit bit-stream-core-ibit)
                   (buffer bit-stream-core-buffer))
      bit-stream-core
    (when (>= ibyte end)
      (funcall callback buffer end)
      (setf ibyte 0 ibit 0)))
  (values))

(defun write-bit (bit bit-stream)
  (declare (type bit-output-stream bit-stream)
           (type bit bit))
  "Writes a single bit to the bit output stream."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-output-stream-core bit-stream-core))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer))      
        bit-stream-core
      (setf (aref buffer ibyte)
            (dpb bit
                 (byte 1 ibit)
                 (ldb (byte ibit 0) (aref buffer ibyte))))    
      (when (zerop ibit)
        (incf byte-counter))
      (inc-bit-counter bit-stream-core)
      (ensure-output bit-stream-core (bit-stream-callback bit-stream)))
    bit))

(defun write-bits (bits bits-to-write bit-stream)
  (declare (type bit-output-stream bit-stream)
           (type unsigned-byte bits)
           (type non-negative-fixnum bits-to-write))
"Writes a sequence of bits to the bit output stream
starting from the least significant bit of the supplied integer."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core))
        (callback (bit-stream-callback bit-stream)))
    (declare (type bit-output-stream-core bit-stream-core))
    (with-accessors ((bit-stream-core-buffer bit-stream-core-buffer)
                     (ibyte bit-stream-core-ibyte)
                     (ibit bit-stream-core-ibit)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer))
        bit-stream-core
      (let ((buffer bit-stream-core-buffer)
            (rv bits)
            (current-octet (ldb (byte ibit 0) (aref buffer ibyte))))
        (declare (type ub8 current-octet)
                 (type unsigned-byte rv))
        (dotimes (i (ceiling (+ ibit bits-to-write) 8) rv)
          (let ((bits-to-add (min bits-to-write (- 8 ibit))))
            (declare (type (integer 0 8) bits-to-add))
            (setf (aref buffer ibyte) (dpb (ldb (byte bits-to-add 0) bits)
                                           (byte bits-to-add ibit)
                                           current-octet)                
                  current-octet 0
                  bits-to-write (- bits-to-write bits-to-add)
                  bits (ash bits (- bits-to-add)))
            (when (zerop ibit) (incf byte-counter))
            (inc-bit-counter bit-stream-core bits-to-add)
            (ensure-output bit-stream-core callback)))))))

(defun write-octet (octet bit-stream)
  (declare (type bit-output-stream bit-stream)
           (type ub8 octet))
  "Writes a single non-packed octet to the bit output stream."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core))
        (callback (bit-stream-callback bit-stream)))
    (declare (type bit-output-stream-core bit-stream-core)
             (type function callback))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte)
                     (buffer bit-stream-core-buffer)
                     (byte-counter bit-stream-core-byte-counter))
        bit-stream-core
      (unless (zerop ibit)
        (inc-bit-counter bit-stream-core (- 8 ibit))
        (ensure-output bit-stream-core callback))
      (setf (aref buffer ibyte) octet)
      (incf ibyte)
      (incf byte-counter)
      (ensure-output bit-stream-core callback))
    octet))

(defun write-octet-vector (vector bit-stream &key (start 0) end)
  (declare (type bit-output-stream bit-stream)
           (type simple-ub8-vector vector)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
  "Writes a vector of non-packed octets to the bit output stream."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (unless end (setf end (length vector)))
  (let ((bit-stream-core (slot-value bit-stream 'core))
        (callback (bit-stream-callback bit-stream)))
    (declare (type bit-output-stream-core bit-stream-core)
             (type function callback)
             (type non-negative-fixnum end))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer)
                     (buffer-end bit-stream-core-end))
        bit-stream-core
      (unless (zerop ibit)
        (inc-bit-counter bit-stream-core (- 8 ibit)))
      (ensure-output bit-stream-core callback)
      (loop (when (>= start end) (return vector))
            (let ((bytes-to-add (min (- end start)
                                     (- buffer-end ibyte))))
              (declare (type non-negative-fixnum bytes-to-add))
              (replace buffer vector
                       :start1 ibyte :end1 buffer-end
                       :start2 start :end2 end)
              (incf start bytes-to-add)
              (incf ibyte bytes-to-add)
              (incf byte-counter bytes-to-add)
              (ensure-output bit-stream-core callback))))))

(defun flush-bit-output-stream (bit-stream)
  (declare (type bit-output-stream bit-stream))
  "Invokes bit output stream's callback and resets stream's internal bit counters."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-output-stream-core bit-stream-core))
    (with-accessors ((buffer bit-stream-core-buffer)
                     (end bit-stream-core-end)
                     (ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte))
        bit-stream-core
      (funcall (the function (bit-stream-callback bit-stream))
               buffer
               (if (zerop ibit)
                 ibyte
                 (the non-negative-fixnum (1+ ibyte))))
      (setf ibyte 0 ibit 0)
      nil)))

(defmethod stream-force-output ((stream bit-output-stream))
  (flush-bit-output-stream stream))

(defmethod stream-finish-output ((stream bit-output-stream))
  (flush-bit-output-stream stream))

(defmethod stream-clear-output ((stream bit-output-stream))
  (unless (open-stream-p stream)
    (error 'bit-stream-closed-error :stream stream))
  (let ((bit-stream-core (slot-value stream 'core)))
    (declare (type bit-output-stream-core bit-stream-core))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte))
        bit-stream-core
      (setf ibit 0 ibyte 0)
      nil)))

(defmethod stream-write-byte ((stream bit-output-stream) integer)
  (if *bit-stream-bit-io*
    (locally
      (declare (type bit integer))
      (write-bit integer stream))
    (locally
      (declare (type ub8 integer))
      (write-octet integer stream))))

(defmethod stream-write-sequence ((stream bit-output-stream) sequence start end &key)
  (declare (type sequence sequence)
           (type non-negative-fixnum start end))
  (if *bit-stream-bit-io*
    (let ((bits 0)
          (length (max 0 (- end start))))
      (declare (type unsigned-byte bits)
               (type non-negative-fixnum length))      
      (dotimes (i length)
        (setf (ldb (byte 1 i) bits) (elt sequence i)))
      (write-bits bits length stream)
      sequence)
    (if (typep sequence 'simple-ub8-vector)
      (write-octet-vector sequence stream :start start :end end)
      (loop :for i fixnum :from start :below end
        :do (write-octet (elt sequence i) stream)
        :finally (return sequence)))))


(defun ensure-input (bit-stream-core bit-stream)
  (declare (type bit-input-stream-core bit-stream-core)
           (type bit-input-stream bit-stream))
  (with-accessors ((ibit bit-stream-core-ibit)
                   (buffer bit-stream-core-buffer)
                   (ibyte bit-stream-core-ibyte)
                   (end   bit-stream-core-end))
      bit-stream-core
    (if (< ibyte end)
      T
      (let ((bytes-available
              (funcall (the function (bit-stream-callback bit-stream))
                       buffer)))
        (check-type bytes-available non-negative-fixnum)
        (tagbody
          check-value
          (restart-case
            (when (> (the non-negative-fixnum bytes-available)
                     (length buffer))
              (error 'bit-stream-buffer-overflow
                     :stream bit-stream
                     :index bytes-available
                     :buffer-size (length buffer)))
            (new-value (new-value)
              (check-type new-value non-negative-fixnum)                 
              (setf bytes-available new-value)
              (go check-value))
            (truncate-to-buffer-size ()
              (setf bytes-available (length buffer)))))
        (setf ibit 0 ibyte 0 end bytes-available)
        (not (zerop (the non-negative-fixnum bytes-available)))))))
  
(defun read-bit (bit-stream &optional (eof-error-p T) eof-value)
  (declare (type bit-input-stream bit-stream))
  "Retrieves the next bit from bit input stream."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (unless (ensure-input bit-stream-core bit-stream)
      (if eof-error-p
        (error 'bit-stream-end-of-file :stream bit-stream)
        (return-from read-bit eof-value)))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer))
        bit-stream-core
      (when (zerop ibit) (incf byte-counter))
      (prog1 (ldb (byte 1 ibit) (aref buffer ibyte))
       (inc-bit-counter bit-stream-core)))))

(defun read-bits (bits-to-read bit-stream)
  (declare (type bit-input-stream bit-stream)
           (type non-negative-fixnum bits-to-read))
"Retrieves the next BITS-TO-READ bits from bit input stream.
Returns an integer which is filled with bits from the stream starting
from the least significant bit, and the total count of bits readen,
which may be less than the requested count."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (with-accessors ((ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer))
        bit-stream-core
      (let ((result 0)
            (result-size 0))
        (declare (type unsigned-byte result)
                 (type non-negative-fixnum result-size))
        (dotimes (i (ceiling (+ bits-to-read ibit) 8))
          (unless (ensure-input bit-stream-core bit-stream)
            (return-from read-bits (values result result-size)))
          (let ((bits-to-add (min bits-to-read (- 8 ibit))))
            (declare (type (integer 0 8) bits-to-add))
            (setf result (logior
                           result
                           (ash (ldb (byte bits-to-add ibit)
                                     (aref buffer ibyte))
                                result-size))
                  bits-to-read (- bits-to-read bits-to-add)
                  result-size (+ result-size bits-to-add))
            (when (zerop ibit) (incf byte-counter))
            (inc-bit-counter bit-stream-core bits-to-add)))
        (values result result-size)))))

(defun read-octet (bit-stream &optional (eof-error-p T) eof-value)
  (declare (type bit-input-stream bit-stream))
  "Retrieves the next non-packed octet from the bit input stream."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (with-accessors ((ibyte bit-stream-core-ibyte)
                     (ibit bit-stream-core-ibit)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer))
        bit-stream-core
      (unless (zerop ibit)
        (inc-bit-counter bit-stream-core (- 8 ibit)))
      (unless (ensure-input bit-stream-core bit-stream)
        (if eof-error-p
          (error 'bit-stream-end-of-file :stream bit-stream)
          (return-from read-octet eof-value)))
      (prog1 (aref buffer ibyte)
       (incf ibyte)
       (incf byte-counter)))))

(defun read-octet-vector (vector bit-stream &key (start 0) end)
  (declare (type bit-input-stream bit-stream)
           (type simple-ub8-vector vector)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
"Destructively modifies VECTOR by reading non-packed octets
from bit input stream. Returns the total number of octets readen."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (unless end (setf end (length vector)))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (with-accessors ((ibyte bit-stream-core-ibyte)
                     (ibit bit-stream-core-ibit)
                     (byte-counter bit-stream-core-byte-counter)
                     (buffer bit-stream-core-buffer)
                     (buffer-end bit-stream-core-end))
        bit-stream-core
      (let ((bytes-available 0))
        (declare (type non-negative-fixnum bytes-available))
        (unless (zerop ibit)
          (inc-bit-counter bit-stream-core (- 8 ibit)))
        (loop (when (or (>= start end)
                        (not (ensure-input bit-stream-core bit-stream)))
                (return bytes-available))
              (let ((bytes-to-add (min (- end start)
                                       (- buffer-end ibyte))))
                (declare (type non-negative-fixnum bytes-to-add))
                (replace vector buffer
                         :start1 start :end1 end
                         :start2 ibyte :end2 buffer-end)
                (incf bytes-available bytes-to-add)
                (incf start bytes-to-add)
                (incf ibyte bytes-to-add)
                (incf byte-counter bytes-to-add)))))))

(defun flush-bit-input-stream (bit-stream)
  (declare (type bit-input-stream bit-stream))
"Resets stream's internal bit counters, effectively
invalidating current contents of the stream's buffer."
  (unless (open-stream-p bit-stream)
    (error 'bit-stream-closed-error :stream bit-stream))
  (let ((bit-stream-core (slot-value bit-stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (with-accessors ((end bit-stream-core-end)
                     (ibit bit-stream-core-ibit)
                     (ibyte bit-stream-core-ibyte))
        bit-stream-core
      (setf end 0 ibit 0 ibyte 0)
      nil)))

(defmethod stream-clear-input ((stream bit-input-stream))
  (flush-bit-input-stream stream))

(defmethod stream-listen ((stream bit-input-stream))
  (unless (open-stream-p stream)
    (error 'bit-stream-closed-error :stream stream))
  (let ((bit-stream-core (slot-value stream 'core)))
    (declare (type bit-input-stream-core bit-stream-core))
    (with-accessors ((ibyte bit-stream-core-ibyte)
                     (end bit-stream-core-end))
        bit-stream-core
      (< ibyte end))))

(defmethod stream-read-byte ((stream bit-input-stream))
  (if *bit-stream-bit-io*
    (read-bit stream nil :eof)
    (read-octet stream nil :eof)))

(defmethod stream-read-sequence ((stream bit-input-stream) sequence start end &key)
  (declare (type sequence sequence)
           (type non-negative-fixnum start end))
  (if *bit-stream-bit-io*
    (multiple-value-bind
        (result size) (read-bits (max 0 (- end start)) stream)      
      (loop :for count :below size
        :for i :from start :below end
        :do (setf (elt sequence i) (ldb (byte 1 i) result))
        :finally (return count)))
    (if (typep sequence 'simple-ub8-vector)
      (read-octet-vector sequence stream :start start :end end)
      (loop :for i fixnum :from start :below end        
        :for count fixnum :from 0
        :for b = (read-octet stream nil nil)
        :while b :do (setf (elt sequence i) b)
        :finally (return count)))))

(defun make-stream-output-callback (stream)
  (declare (type stream stream))
  (lambda (buffer end)
    (write-sequence buffer stream :end end)))

(defun make-stream-input-callback (stream)
  (declare (type stream stream))
  (lambda (buffer)
    (read-sequence buffer stream)))

(defun make-eof-input-callback ()
  (lambda (&rest args)
    (declare (ignore args))
    0))

(defun make-blackhole-output-callback ()
  (lambda (&rest args)
    (declare (ignore args))
    (values)))

(defmacro with-bit-input-stream
    ((var &key (callback
                 '(make-eof-input-callback))
               (buffer-size
                 '*default-bit-input-stream-buffer-size*))
     &body body)
  (check-type var symbol)
  `(let ((,var (make-instance 'bit-input-stream
                 :callback ,callback
                 :buffer-size ,buffer-size)))
     (unwind-protect
         (progn ,@body)
       (close ,var))))

(defmacro with-bit-output-stream
    ((var &key (callback
                 '(make-blackhole-output-callback))
               (buffer-size
                 '*default-bit-output-stream-buffer-size*))
     &body body)
  (check-type var symbol)
  `(let ((,var (make-instance 'bit-output-stream
                 :callback ,callback
                 :buffer-size ,buffer-size)))
     (unwind-protect
         (multiple-value-prog1
          (progn ,@body)
          (flush-bit-output-stream ,var))
       (close ,var))))
