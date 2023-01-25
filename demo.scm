(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))


(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 2))
           (fib (- n 1)))))

(define (fibiter n)
    (iter 0 1 n))

(define (iter a b n)
    (if (= n 0)
        a
        (if (= n 1)
            b
            (iter b (+ a b) (- n 1)))))
