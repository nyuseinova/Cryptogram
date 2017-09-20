#lang racket
(define (index element lst)
  (define (index-helper element lst idx)
    (cond
      [(null? lst) (display "Given element is not found in the list.\n")]
      [(equal? element (car lst)) idx]
      [else (index-helper element (cdr lst) (+ idx 1))]))
  (index-helper element lst 0))

(define (all-unique-letters words-lst)
  (let ([l (remove-duplicates (append-map string->list words-lst))])
    (cond
      [(> (length l) 10) (error "Too many letters => no solution.")]
      [else (append (map (λ (_) #\?) (range (- 10 (length l)))) l)])))

(define (word->number letters-lst word)
  (and (not (zero? (index (string-ref word 0) letters-lst)))
       (car 
         (foldr
            (λ (char cons-elements)
              (cons (+ (car cons-elements) (* (index char letters-lst) (cdr cons-elements)))
                    (* (cdr cons-elements) 10)))
            (cons 0 1)
            (string->list word)))))

(define (subtract letters-lst words-lst)
  (let ([words-values (map (λ (word) (word->number letters-lst word)) words-lst)])
    (and (andmap number? words-values)
         (abs
           (let solution ([lst words-values])
             (if (null? (cdr lst))
                 (- (car lst))
                 (+ (car lst) (solution (cdr lst)))))))))

(define (random-swap letters-lst)
  (define (swap lst idx1 idx2)
   (define (swap-helper res count)
     (cond
       [(= count (length lst)) res]
       [(= count idx1) (swap-helper (append res (list (list-ref lst idx2))) (+ count 1))]
       [(= count idx2) (swap-helper (append res (list (list-ref lst idx1))) (+ count 1))]
       [else (swap-helper (append res (list (list-ref lst count))) (+ count 1))]))
   (swap-helper '() 0))
  (let* ([i (random (length letters-lst))]
         [j (random (length letters-lst))])
    (swap letters-lst i j)))

(define (output letters-lst words-lst)
  (printf "~n")
  (if (null? (cdr (cdr words-lst)))
      (begin
        (printf "~a" (car words-lst))
        (printf " ---> ")
        (printf "~a" (word->number letters-lst (car words-lst)))
        (printf "~n = ~n")
        (printf "~a" (car (cdr words-lst)))
        (printf " ---> ")
        (printf "~a" (word->number letters-lst (car (cdr words-lst)))))
      (begin
        (printf "~a" (car words-lst))
        (printf " ---> ")
        (printf "~a" (word->number letters-lst (car words-lst)))
        (printf "~n +")
        (output letters-lst (cdr words-lst)))))

(define (cryptogram words-lst)
  (cond
    [(or
      (null? words-lst)
      (= (length words-lst) 1)
      (= (length words-lst) 2)) (and
                                  (display "Not enough words.Try again entering at least 3 words.\n")
                                  (cryptogram (input)))]
    [(not (andmap string? words-lst)) (and
                                        (display "All words must be entered in quotation marks.Try again.\n")
                                        (cryptogram (input)))]
    [(not (andmap
            (λ(elem)
              (<= (string-length elem) (string-length (last words-lst))))
            (take words-lst (- (length words-lst) 1))))
     (display "Unsolvable case.\n")]
    [else
      (define (cryptogram-helper solution best-solution)
        (let* ([new-solution (random-swap solution)]
               [new-best-solution (subtract new-solution words-lst)])
          (cond
            [(and new-best-solution (= 0 new-best-solution))
             (and
               (printf "~n")
               (output new-solution words-lst))]
            [(or
               (not best-solution)
               (and new-best-solution (< new-best-solution best-solution))
               (zero? (random 100)))
             (cryptogram-helper new-solution new-best-solution)]
            [else
             (cryptogram-helper solution best-solution)])))
      (cryptogram-helper (all-unique-letters words-lst) (subtract (all-unique-letters words-lst) words-lst))]))

(define (input)
  (display "word: ")
  (define in (read))
  (if (eq? in 'solve)
       '()
       (begin
         (newline)
         (cons in (input)))))

(display "Enter word in quotation marks.\n")
(display "To display the solution - enter: solve.\n")
(cryptogram (input))
