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

(in-package #:cl-user)

(defpackage #:trivial-bit-streams
  (:use #:cl #:trivial-gray-streams)
  (:nicknames #:tbs #:trivial-bs)
  (:export
    #:*default-bit-output-stream-buffer-size*
    #:*default-bit-input-stream-buffer-size*
    #:*bit-stream-bit-io*
    #:bit-stream-error
    #:bit-stream-closed-error
    #:bit-stream-end-of-file
    #:bit-stream-buffer-overflow
    #:bit-stream-buffer-overflow-index
    #:bit-stream-buffer-overflow-buffer-size
    #:bit-stream
    #:bit-output-stream
    #:bit-input-stream
    #:bit-stream-byte-counter
    #:write-bit
    #:write-bits
    #:write-octet
    #:write-octet-vector
    #:pad-to-byte-alignment
    #:flush-bit-output-stream
    #:read-bit
    #:read-bits
    #:read-octet
    #:read-octet-vector
    #:read-to-byte-alignment
    #:flush-bit-input-stream
    #:make-stream-output-callback
    #:make-stream-input-callback
    #:make-eof-input-callback
    #:make-blackhole-output-callback
    #:with-bit-input-stream
    #:with-bit-output-stream))
