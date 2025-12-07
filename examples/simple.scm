(import (scheme base))
(import (brunch))
(import (scheme process-context))

(define cmd (command-line))
(define dest (if (> (length cmd) 1)
                 (list-ref cmd 1)
                 "simple.csv"))

(define (fib-rec len)
  (define (fib-idx n)
    (cond 
      ((= n 0) 0)
      ((< n 3) 1)
      (else (+ (fib-idx (- n 1)) (fib-idx (- n 2))))))
  (define res (make-vector len))
  
  (let loop ((i 0))
    (if (< i len)
        (begin
          (vector-set! res i (fib-idx i))
          (loop (+ i 1)))
        res)))

(define (fib-loop len)
  (define out (make-vector len))
  
  (cond 
    ((= len 1) (vector-set! out 0 0))
    (else 
      (vector-set! out 0 0)
      (vector-set! out 1 1)

      (let loop ((n 2))
        (if (< n len)
            (begin
              (vector-set! out n (+ (vector-ref out (- n 1)) (vector-ref out (- n 2))))
              (loop (+ n 1)))
            out)))))

(define (fac-loop n)
  (let loop ((i n) (acc 1))
    (if (= i 0)
        acc
        (loop (- i 1) (* acc i)))))

(define (fac-rec n)
  (if (= n 0)
      1
      (* n (fac-rec (- n 1)))))


(benches (simple dest)
  (bench "fib-loop (30)"
    (samples 10)
    (begin (fib-loop 30)))
  (bench "fib-rec (30)"
    (samples 10)
    (begin (fib-rec 30)))
  bench-spacer
  (bench "fac-loop (3000)"
    (samples 10)
    (begin (fac-loop 3000)))
  (bench "fac-rec (3000)"
    (samples 10)
    (begin (fac-rec 3000)))
  bench-spacer)

(simple)