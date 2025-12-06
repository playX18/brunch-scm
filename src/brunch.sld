(define-library (brunch)
  (import (brunch bench) 
          (scheme base)
          (scheme file))
  (export benches bench bench-spacer)
    
(begin 
  (define-syntax benches 
    (syntax-rules () 
      [(_ (bname filename) bench ...)
        (define (bname)
          (define port (current-error-port))
          (write-string "\x1b;[1;38;5;199mStarting:\x1b;[0m Running benchmark(s). Stand by!\n\n" port)
          (flush-output-port port)
          (define bs (make-benches))
          (begin 
            (write-string "\x1b;[1;34M•\x1b;[0m" port)
            (flush-output-port port)
            
            (set! bs (benches-add bs bench))) ...
          (write-string "\n\n" port)
          (benches-finish bs)
          (if (file-exists? filename)
            (delete-file filename))
          (call-with-output-file filename
            (lambda (file)
              (benches-finish-csv bs file))))]
      [(_ bname bench ...)
        (define (bname)
          (define port (current-error-port))
          (write-string "\x1b;[1;38;5;199mStarting:\x1b;[0m Running benchmark(s). Stand by!\n\n" port)
          (flush-output-port port)
          (define bs (make-benches))
          (begin 
            (write-string "\x1b;[1;34M•\x1b;[0m" port)
            (flush-output-port port)
            
            (set! bs (benches-add bs bench))) ...
          (write-string "\n\n" port)
          (benches-finish bs))]))

  
  (define-syntax bench-aux 
    (syntax-rules (samples timeout begin)
      [(_ (name s t b) (samples v) rest ...)
        (bench-aux (name v t b) rest ...)]
      [(_ (name s t b) (timeout v) rest ...)
        (bench-aux (name s v b) rest ...)]
      [(_ (name s t b) (begin . body) rest ...)
        (bench-aux (name s t body) rest ...)]
      [(_ (name s t b))
        (bench-run (%make-bench name s t #f) (lambda () . b))]))
  (define-syntax bench
    (syntax-rules ()
      [(_ name . rest)
        (bench-aux (name (default-samples) (default-timeout) #f) . rest)]))
))