;MUGOYA DIHFAHSIH
;Student no. 2100702353
;Reg no. 2021/HD05/2353U
#lang racket
(require net/url)
(require data-science-master)
(require plot)
(require math)
(require csv-reading math math/matrix plot racket/hash)

;The following are the abstractions and their procedures identified from Data Science Master project

; a) linear-model
(define (linear-model xs y)
  (let ([X (list*->matrix
	    (map (λ (x y) (flatten (list x y)))
		 (build-list (length xs) (const 1)) xs))]
	[Y (->col-matrix y)])
    ;; We solve for A, a col-matrix containing [intercept slope]
    ;; A = ((X^TX)^-1)X^TY
    ;; Where X^T means transpose of X, and ^-1 means inverse
    (matrix->list (matrix*
		   (matrix-inverse (matrix* (matrix-transpose X) X))
		   (matrix* (matrix-transpose X) Y)))))

; b) list->sentiment
(define (list->sentiment lst #:lexicon [lexicon 'nrc])
  (define (pack-sentiment lst lexicon)
    (apply append (list '("word" "sentiment" "freq"))
	   (map (λ (x) 
		  (let ([result (token->sentiment (first x) #:lexicon lexicon)])
		    (map (λ (y) (append y (list (second x)))) result)))
		lst)))
  (let ([sentiment (pack-sentiment lst lexicon)])
    (if (> (length sentiment) 1)
	sentiment
	'())))

; c) read-csv
(define (read-csv file-path
		  #:->number? [->number? #f]
		  #:header? [header? #t])
  (let ((csv-reader (make-csv-reader-maker
		     '((comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (if header?
		  (cons (car tmp) (map (lambda (x) (map string->number x)) (cdr tmp)))
		  (map (lambda (x) (map string->number x)) tmp))
	      ;; Else, leave everything as strings
	      tmp))))))

; d)qq-plot*
(define (qq-plot* lst #:scale? [scale? #t])
  (plot (qq-plot lst #:scale? scale?)
	#:x-label "Theoretical Normal Quantiles"
	#:y-label "Sample Quantiles"))
; data abstractions: qq-plot*, qq-plot

; e) hist
(define (hist lst)
  (discrete-histogram (sorted-counts lst)))

; data abstractions: hist, discrete-histogram, sorted-counts