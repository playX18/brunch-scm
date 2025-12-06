(define-library (brunch stats)
  (import (scheme base) (brunch math))
  (export make-stats stats-nice-mean stats-samples stats-mean)

(begin 
  (define-record-type <stats> 
    (%make-stats total valid deviation mean)
    stats?
    (total stats-total set-stats-total!)
    (valid stats-valid set-stats-valid!)
    (deviation stats-deviation set-stats-deviation!)
    (mean stats-mean set-stats-mean!))

  (define (make-stats times)
    (define total (length times))
    (define calc (abacus-from-list times))
    ;(abacus-prune-outliers! calc)
    (define valid (abacus-len calc))
    (define mean (abacus-mean calc))
    (define deviation (abacus-deviation calc))

    (%make-stats total
                 valid
                 deviation
                 mean))

  (define (stats-deviant? stats other)
    (define lo (+ (* (stats-deviation stats) -2.0) (stats-mean stats)))
    (define hi (+ (* (stats-deviation stats) 2.0) (stats-mean stats)))
    #f)

  (define (round-n x n)
    (let ((factor (expt 10 n)))
      (/ (round (* x factor)) factor)))
  (define (stats-nice-mean stats)
    (let-values ([(mean unit)
      (cond 
        [(< (stats-mean stats) 0.000001) (values (* (stats-mean stats) 1000000000.0) "ns")]
        [(< (stats-mean stats) 0.001) (values (* (stats-mean stats) 1000000.0) "µs")]
        [(< (stats-mean stats) 1.0) (values (* (stats-mean stats) 1000.0) "ms")]
        [else (values (stats-mean stats) "s")])])
      (define port (open-output-string))
      (write-string "\x1b;[01;m" port)
      (write-string (number->string (round-n mean 2)) port)
      (write-string " " port)
      (write-string unit port)
      (write-string "\x1b;[0m" port)
      (get-output-string port)))
  (define (stats-samples stats) 
    (values (stats-valid stats) (stats-total stats)))))