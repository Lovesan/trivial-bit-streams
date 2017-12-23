# Trivial-Bit-Streams

Trivial-bit-streams is a library which implements flexible
buffered bit streams.

Input into INPUT-BIT-STREAM's buffer and output from OUTPUT-BIT-STREAM's one
is managed through callbacks, so you can easily wrap bit streams around any
desirable data type.

The library's single dependency is trivial-gray-streams library.

## Usage

The bit streams wraps a `(simple-array (unsigned-byte 8) (,buffer-size))' buffer and reads bits from it.
The underlying array is populated by the passed callback. The callback should also return the number
of bytes read into the buffer. When the buffer is empty the callback is again called to refill the
buffer.

NOTE: Bits are read from LSB to MSB. So (byte 254) reads: 0, 1, 1, 1, 1, 1, 1, 1.

#### Ex: Wrapping a stream.

Typical use is wrapping a `stream' with :element-type '(unsigned-byte 8).

```
(with-open-file (stream #P"/path/to/file" :element-type '(unsigned-byte 8))
  (let ((bit-stream (make-instance 'bit-input-stream
                                   :callback (lambda (buffer)
                                               (read-sequence buffer stream))
                                   :buffer-size 4096))) ;; buffer-size defaults to 4096.
    (read-byte s)
    (read-byte s)))
```

A convenience function `make-stream-input-callback' is also defined.

```
(with-open-file (stream #P"/path/to/file" :element-type '(unsigned-byte 8))
  (let ((bit-stream (make-instance 'bit-input-stream
                                   :callback (make-stream-input-callback stream))))
    (read-byte s)
    (read-byte s)))
```

#### Ex: Infinite bit-stream.

As the buffer if filled when all elements have been read this will result in an
infinite stream of bits.

```
(let ((s (make-instance 'bit-input-stream
                        :callback (lambda (buffer)
                                    (dotimes (i (length buffer))
                                      (setf (aref buffer i) 254))
                                    (length buffer))
                        :buffer-size 4096)))
  (read-byte s) ;;-> 0
  (read-byte s)) ;;-> 1
```


#### Macros.

There is also wrapper macro `with-bit-input-stream'.
It is also possible to write to a sequence. Any type will work.

```
(let ((octet-stream (make-some-octet-stream-with-elements-254)) ;; Imagined function.
      (bit-seq (make-array 16 :element-type 'bit)))
  (with-bit-input-stream (bit-stream :callback (make-stream-input-callback stream))
    (read-sequence bit-seq bit-stream)))

;; -> #*0111111101111111
```

#### Writing.

Writing bits is done in much the same way.

```
(let ((octet-stream (flexi-streams:make-in-memory-output-stream)))
  (with-bit-output-stream (s :callback (make-stream-output-callback octet-stream))
         (write-byte 0 s)
         (write-byte 1 s)
         (flush-bit-output-stream s)
         (flexi-streams:get-output-stream-sequence octet-stream)))

;;-> #(2)
```

The library implements trivial-gray-streams so the typical stream interface is implemented.


## Installation

(ql:quickload :trivial-bit-streams)

## Author

* Dmitry Ignatiev (lovesan.ru@gmail.com)

* README by Christopher Eames (Chream) <chream@gmx.com>

## Copyright

Copyright (c) 2017 Dmitry Ignatiev (lovesan.ru@gmail.com)

## License

Licensed under the MIT License.
