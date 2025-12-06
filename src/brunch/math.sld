(define-library (brunch math)
    (import (scheme base)
            (srfi 1)
            (scheme r5rs))
    (export abacus-from-list
            abacus-deviation
            abacus-max
            abacus-mean
            abacus-min
            abacus-quantile
            ideal-quantile
            abacus-prune-outliers!
            abacus-len)
    
(begin
  (define-record-type <abacus>
    (%make-abacus set len unique total)
    abacus?
    (set abacus-set set-abacus-set!)
    (len abacus-len set-abacus-len!)
    (unique abacus-unique set-abacus-unique!)
    (total abacus-total set-abacus-total!))

  (define (abacus-empty? abacus)
    (= (abacus-len abacus) 0))

  (define (abacus-flen abacus)
    (exact->inexact (abacus-len abacus)))

  ;; Standard deviation
  (define (abacus-deviation abacus)
    (cond 
      [(or (abacus-empty? abacus)
           (= (abacus-unique abacus) 1))
        0.0]
      [else 
        (define mean (abacus-mean abacus))
        (define squares (fold
                          (lambda (x acc)
                            (+ acc (expt (- x mean) 2)))
                          0.0
                          (abacus-set abacus))) 
        (sqrt (/ squares (abacus-flen abacus)))]))

  (define (abacus-max abacus)
    (cond 
      [(abacus-empty? abacus) 0.0]
      [else (list-ref (abacus-set abacus)
                      (- (abacus-len abacus) 1))]))

  (define (abacus-mean abacus)
    (cond 
      [(abacus-empty? abacus) 0.0]
      [(= (abacus-unique abacus) 1)
        (list-ref (abacus-set abacus) 0)]
      [else (/ (abacus-total abacus)
               (abacus-flen abacus))]))  
  (define (abacus-min abacus)
    (cond 
      [(abacus-empty? abacus) 0.0]
      [else (list-ref (abacus-set abacus) 0)]))
    
  (define (count-unique set)
    (let loop ([count 0] [last +inf.0] [lst set])
      (cond 
        [(null? lst)
          count]
        [else
          (define current (car lst))
  
          (if (not (= current last))
            (loop (+ count 1) current (cdr lst))
            (loop count last (cdr lst)))])))
  (define (abs-diff lhs rhs)
    (if (> lhs rhs)
      (- lhs rhs)
      (- rhs lhs)))
  (define (quantile-diff below above ref-below ref-above)
    (define below-diff (exact->inexact (abs-diff below ref-below)))
    (define above-diff (exact->inexact (abs-diff above ref-above)))

    (/ (+ below-diff above-diff) 2))

  (define (midpoint a b)
    (+ (/ a 2.0)
       (/ b 2.0)))

  ;; # Idealized Quantile.
	;;
	;; Return the quantile at the corresponding percentage. Unlike `abacus-quantile`,
	;; the result may not actually be present in the set. (Sparse entries are
	;; smoothed out to provide an "idealized" representation of where the cut
	;; would fall if the data were better.)
	;;
	;; This was inspired by the [`quantogram`](https://crates.io/crates/quantogram) crate's `fussy_quantile`
	;; calculations, but wound up much simpler because we have only a singular
	;; use case to worry about.
  (define (ideal-quantile abacus phi) 
    (cond 
      [(abacus-empty? abacus) 0.0]
      [(<= phi 0.0) (abacus-min abacus)]
      [(>= phi 1.0) (abacus-max abacus)]
      [(or (= (abacus-len abacus) 1)
           (= (abacus-unique abacus) 1))
        (car (abacus-set abacus))]
      [else 
        (define epsilon (/ 1.0 (+ 2.0 (abacus-flen abacus))))
        (define quantile (abacus-quantile abacus phi))
     
        (cond 
          [(or (= quantile 0.0)
               (<= phi (* 1.5 epsilon))
               (>= phi (+ (* epsilon 1.5) 1.0)))
            quantile]
          [else 
            (define lo (abacus-quantile abacus (- phi epsilon)))
            (define hi (abacus-quantile abacus (+ phi epsilon)))
            (define lo-diff (- quantile lo))
            (define hi-diff (- hi quantile))

            (cond 
              [(>= lo-diff (* hi-diff 2.0))
                (midpoint lo quantile)]
              [(>= hi-diff (* lo-diff 2.0))
                (midpoint hi quantile)]
              [else 0.0])])]))
  
  	;; # Quantile.
    ;;
    ;; Return the quantile at the corresponding percentage. Values are clamped
    ;; to the set's minimum and maximum, but will always correspond to a value
    ;; that is actually in the set.
    (define (abacus-quantile abacus phi)
      (cond 
        [(abacus-empty? abacus) 0.0]
        [(<= phi 0.0) (abacus-min abacus)]
        [(>= phi 1.0) (abacus-max abacus)]
        [(or (= (abacus-len abacus) 1)
             (= (abacus-unique abacus) 1))
          (car (abacus-set abacus))]
        [else 
          (define target (inexact->exact (round (* phi (abacus-flen abacus)))))
       
          (cond 
            [(zero? target)
              (abacus-min abacus)]
            [(>= target (- (abacus-len abacus) 1))
              (abacus-max abacus)]
            [else 
              ;; The number of entries below and above our starting point.
              ;; Since we mathed this guess, this serves as the "ideal"
              ;; reference distribution.
              (define target-below target)
              (define target-above-x (- (abacus-len abacus) target))
              (define target-above (if (< target-above-x 0)
                                      0
                                      target-above-x))
              (define out* (list-ref (abacus-set abacus) target))

              (define (try-step-up last out diff)
                (let loop ([last (list-ref (abacus-set abacus) target)]
                               [out out]
                               [diff diff])
                    
                      (cond 
                        [(step-up abacus last)
                          => (lambda (other)
                            (define diff2 (quantile-diff (count-below abacus other)
                                                        (count-above abacus other)
                                                        target-below
                                                        target-above))
                            (if (< diff2 diff)
                              (loop other diff2 other)
                              out))]
                        [else out])))

              (let loop ([out out*]
                         [diff (quantile-diff (count-below abacus out*)
                                              (count-above abacus out*)
                                              target-below
                                              target-above)]
                         [last (list-ref (abacus-set abacus) target)])
                (cond 
                  [(step-down abacus last)
                    => (lambda (other)
                      (define diff2 (quantile-diff (count-below abacus other)
                                                  (count-above abacus other)
                                                  target-below
                                                  target-above))
                      (if (< diff2 diff)
                        (loop other diff2 other)
                        (try-step-up last out diff)))]
                  [else 
                    (try-step-up last out diff)]))])]))

  ;;  Return the largest entry in the set with a value lower than the target,
  ;; if any.
  (define (step-down abacus num)
    (let loop ([lst (abacus-set abacus)])
      (cond 
        [(null? lst) #f]
        [(< (car lst) num)
          (if (null? (cdr lst))
            (car lst)
            (let () (define next (cadr lst))
            (if (>= next num)
              (car lst)
              (loop (cdr lst)))))]
        [else #f])))
  
  (define (step-up abacus num)
    (let loop ([lst (abacus-set abacus)])
      (cond 
        [(null? lst) #f]
        [(> (car lst) num)
          (car lst)]
        [else 
          (if (null? (cdr lst))
            #f
            (loop (cdr lst)))])))

  (define (count-below abacus num)
    (let loop ([count 0] [lst (abacus-set abacus)])
      (cond 
        [(null? lst) count]
        [(< (car lst) num)
          (loop (+ count 1) (cdr lst))]
        [else count])))
  
  (define (count-above abacus num)
    (let loop ([count 0] [lst (abacus-set abacus)]  )
      (cond 
        [(null? lst) count]
        [(> (car lst) num)
          (loop (+ count 1) (cdr lst))]
        [else count])))
	;; # Prune Outliers.
	;;
	;; This calculates an IQR using the 5th and 95th quantiles (fuzzily), and
	;; removes entries below the lower boundary or above the upper one, using
	;; a multiplier of `1.5`.
  (define (abacus-prune-outliers! abacus)
    (cond 
      [(and (< 1 (abacus-unique abacus))
            (< 0.0 (abacus-deviation abacus)))
        (define q1 (ideal-quantile abacus 0.05))
        (define q3 (ideal-quantile abacus 0.95))
        (define iqr (- q3 q1))

        (define lo (+ (* iqr -1.5) q1))
        (define hi (+ (* iqr 1.5) q3))
        (define new-set (filter 
                          (lambda (x)
                            (and (>= x lo)
                                 (<= x hi)))
                          (abacus-set abacus)))
        (define len (length new-set))
        (unless (= len (length (abacus-set abacus)))
          (set-abacus-set! abacus new-set)
          (set-abacus-len! abacus (inexact->exact len))
          (set-abacus-unique! abacus (count-unique new-set)))]
    [else #f]))
  (define (list-sort lst)
    (if (null? lst)
      '()
      (let () (define pivot (car lst))
      (define less (filter (lambda (x) (< x pivot)) (cdr lst)))
      (define greater-eq (filter (lambda (x) (>= x pivot)) (cdr lst)))
      (append (list-sort less)
              (list pivot)
              (list-sort greater-eq)))))
  ;; Construct an abacus from a list of numbers.
  (define (abacus-from-list lst)
    (define set (list-sort lst))
    (define len (length set))
    (define unique (count-unique set))
    (define total (fold + 0.0 set))
    (%make-abacus set
                   len
                   unique
                   total))

 
))