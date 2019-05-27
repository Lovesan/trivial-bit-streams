;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2010-2019, Dmitry Ignatiev <lovesan.ru@gmail.com>

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

(in-package #:trivial-bit-streams-tests)

(def-suite tbs :description "trivial-bit-streams test suite")

(defun run-tests ()
  (explain! (run 'tbs)))

(defmacro with-trivial-bits-to-sequence ((stream) &rest body)
  (let ((flexi-stream (gensym)))
    `(flexi-streams:with-output-to-sequence (,flexi-stream)
       (with-bit-output-stream (,stream :callback (make-stream-output-callback ,flexi-stream))
         ,@body))))

(defmacro with-sequence-to-trivial-bits ((stream seq) &rest body)
  (let ((flexi-stream (gensym)))
    `(flexi-streams:with-input-from-sequence (,flexi-stream ,seq)
       (with-bit-input-stream (,stream :callback (make-stream-input-callback ,flexi-stream))
         ,@body))))

(in-suite tbs)
(test read-bit
  (let ((sequence
         (with-trivial-bits-to-sequence (stream)
           (write-bit 1 stream)
           (write-bit 1 stream)
           (write-bit 1 stream)
           (write-bit 1 stream)
           (write-bit 0 stream)
           (write-bit 1 stream))))
    (with-sequence-to-trivial-bits (stream sequence)
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 0))
      (is (= (read-bit stream) 1)))))

(test read-bits
  (let ((sequence (with-trivial-bits-to-sequence (stream)
           (write-bits 0 2 stream)
           (write-bits 3 2 stream)
           (write-bits 5 3 stream)
           (write-bits 12 4 stream))))
    (with-sequence-to-trivial-bits (stream sequence)
      (is (= (read-bits 2 stream) 0))
      (is (= (read-bits 2 stream) 3))
      (is (= (read-bits 3 stream) 5))
      (is (= (read-bits 4 stream) 12)))))

(test buffer-cross
  (let ((*default-bit-output-stream-buffer-size* 1)
        (*default-bit-input-stream-buffer-size* 1)
        (sequence (with-trivial-bits-to-sequence (stream)
                    (write-bits 15 4 stream)
                    (write-bits 129 8 stream)
                    (write-bits 9 4 stream))))
    (with-sequence-to-trivial-bits (stream sequence)
      (is (= (read-bits 4 stream) 15))
      (is (= (read-bits 8 stream) 129))
      (is (= (read-bits 4 stream) 9)))))

(test read-octet
  (let ((sequence (with-trivial-bits-to-sequence (stream)
                    (write-octet 240 stream)
                    (write-octet 15 stream))))
    (with-sequence-to-trivial-bits (stream sequence)
      (is (= (read-octet stream) 240))
      (is (= (read-octet stream) 15)))))

(test mixed
  (let ((sequence (with-trivial-bits-to-sequence (stream)
                    (write-bit 1 stream)
                    (write-bit 1 stream)
                    (write-bit 1 stream)
                    (write-bit 0 stream)
                    (write-bits 5 3 stream)
                    (pad-to-byte-alignment 1 stream)
                    (write-octet 240 stream))))
    (with-sequence-to-trivial-bits (stream sequence)
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 1))
      (is (= (read-bit stream) 0))
      (is (= (read-bits 3 stream) 5))
      (read-to-byte-alignment stream)
      (is (= (read-octet stream) 240)))))
