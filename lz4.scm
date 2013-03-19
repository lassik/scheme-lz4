(module
 lz4
 (compress
  compress-hc
  uncompress)

(import chicken scheme foreign)
(use srfi-4)

(foreign-declare "#include \"lz4.h\"")
(foreign-declare "#include \"lz4hc.h\"")

(define (lz4-condition loc msg . args)
  (make-composite-condition
   (make-property-condition 'exn 'arguments args 'location loc 'message msg)
   (make-property-condition 'ffi)
   (make-property-condition 'lz4)))

(define foreign-compress
  (foreign-lambda int "LZ4_compress" s8vector s8vector integer))

(define foreign-uncompress
  (foreign-lambda int "LZ4_uncompress" s8vector s8vector integer))

(define foreign-compress-hc
  (foreign-lambda int "LZ4_compressHC" s8vector s8vector integer))

(define (compress in-buf)
  (unless (s8vector? in-buf)
    (signal (lz4-condition 'compress "Argument to compress must be a srfi-4 s8vector, got" in-buf)))
  (let* ((len (s8vector-length in-buf))
         (out-buf (make-s8vector (add1 len) 0)) ;; plus 1 for the header byte
         (bytes-written (foreign-compress in-buf out-buf len)))
    (unless (< 0 bytes-written) (signal (lz4-condition 'compress "error returned from compress procedure")))
    (subs8vector out-buf 0  bytes-written)))

(define (compress-hc in-buf)
  (unless (s8vector? in-buf)
    (signal (lz4-condition 'compress-hc "Argument to compress must be a srfi-4 s8vector, got" in-buf)))
  (let* ((len (s8vector-length in-buf))
         (out-buf (make-s8vector (add1 len) 0))
         (bytes-written (foreign-compress-hc in-buf out-buf len)))
    (unless (< 0 bytes-written) (signal (lz4-condition 'compress-hc "error returned from compress procedure")))
    (subs8vector out-buf 0  bytes-written)))

(define (uncompress in-buf)
  (unless (s8vector? in-buf)
    (signal (lz4-condition 'uncompress "Argument to uncompress must be a srfi-4 s8vector, got" in-buf)))
  (let* ((len (sub1 (s8vector-length in-buf)))
         (out-buf (make-s8vector len 0))
         (bytes-written (foreign-uncompress in-buf out-buf len)))
    (unless (< 0 bytes-written) (signal (lz4-condition 'uncompress "error returned from compress procedure")))
    (subs8vector out-buf 0 len))))
