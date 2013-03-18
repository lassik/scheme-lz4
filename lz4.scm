(module
 lz4
 (compress
  compress-hc
  uncompress)

(import chicken scheme foreign)
(use srfi-4)

(foreign-declare "#include \"lz4.h\"")
(foreign-declare "#include \"lz4hc.h\"")

(define foreign-compress
  (foreign-lambda int "LZ4_compress" s8vector s8vector integer))

(define foreign-uncompress
  (foreign-lambda int "LZ4_uncompress" s8vector s8vector integer))

(define foreign-compress-hc
  (foreign-lambda int "LZ4_compressHC" s8vector s8vector integer))

(define (compress in-buf)
  (assert (s8vector? in-buf) "Argument to compress must be a srfi-4 s8vector, got " in-buf)
  (let* ((len (s8vector-length in-buf))
         (out-buf (make-s8vector (add1 len) 0)) ;; plus 1 for the header byte
         (bytes-written (foreign-compress in-buf out-buf len)))
    (unless (< 0 bytes-written) (error "error returned from compress procedure"))
    (subs8vector out-buf 0  bytes-written)))

(define (compress-hc in-buf)
  (assert (s8vector? in-buf) "Argument to compress must be a srfi-4 s8vector, got " in-buf)
  (let* ((len (s8vector-length in-buf))
         (out-buf (make-s8vector (add1 len) 0))
         (bytes-written (foreign-compress-hc in-buf out-buf len)))
    (unless (< 0 bytes-written) (error "error returned from compress procedure"))
    (subs8vector out-buf 0  bytes-written)))

(define (uncompress in-buf)
  (assert (s8vector? in-buf) "Argument to uncompress must be a srfi-4 s8vector, got " in-buf)
  (let* ((len (sub1 (s8vector-length in-buf)))
         (out-buf (make-s8vector len 0))
         (bytes-written (foreign-uncompress in-buf out-buf len)))
    (unless (< 0 bytes-written) (error "error returned from uncompress procedure"))
    (subs8vector out-buf 0 len))))
