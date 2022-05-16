
#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)


(require net/url 
  net/uri-codec
  web-server/stuffers/hmac-sha1
  net/base64
  rackunit)


;############################################################
;#     ANALYSING THE DATA THAT WAS PICKED FROM TWITTER      #
;############################################################

;Procedure replaces the word "false" from the raw twitter file
(define (remove-false str)
  (regexp-replace* #px"false" str " "))

;Procedure replaces the word "true" from the raw twitter file
(define (remove-true str)
  (regexp-replace* #px"true" str " "))

;Procedure replaces the word "sensitive" from the raw twitter file
(define (remove-sensitive str)
  (regexp-replace* #px"sensitive" str " "))

;Procedure replaces the word "favorited" from the raw twitter file
(define (remove-favorited str)
  (regexp-replace* #px"favorited" str " "))

;Procedure replaces the word "favorite" from the raw twitter file
(define (remove-favorite str)
  (regexp-replace* #px"favorite" str " "))

;Reads all characters from in and returns them as a string.Reading from a file in this case we are reading from tweeterdata_uganda_combined.json
(define input_tweets (port->string (open-input-file "data.json")))


;changing all words to lower case, then removing urls,then remove punctuation makes
;the normalizing the space between words
;;; the cleaned data is save as cleanedinutdata and the input is rawiputdata
(define cleanedinutdata (string-normalize-spaces
                         (remove-punctuation
                          (remove-false
                           (remove-true
                            (remove-sensitive
                             (remove-favorite
                              (remove-favorited
                               (remove-urls
                                (string-downcase input_tweets))))))))))


;;This proceedure outputs quoted strings in the list. The order of the strings is not maintained from input to output
;;Further cleaning is done by removing stop words using the defaults lexicon (SMART)
;;INPUT: cleanedinutdata
;;OUTPUT: cleanedinutdata1
;(define cleanedinutdata2 (remove-stopwords (string-split cleanedinutdata)))

;;Returns a list of pairs. Each pair consists of a unique word/token from str with its frequency.
;;the number of times a word occurs in the tweets selected
;;INPUT:cleanedinutdata
;;OUTPUT:cleanedinutdata1
(define cleanedinutdata1 (document->tokens cleanedinutdata #:sort? #t))


;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.
;;INPUT:cleanedinutdata1
;;OUTPUT:sentiment
(define sentiment (list->sentiment cleanedinutdata1 #:lexicon 'nrc))



;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
;;INPUT:sentiment
;;OUTPUT:data1
(define data1 (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))

(newline)

;Sample list output of the aggregate data
;->'(("positive" 105944)("trust" 71888)("disgust" 10463)("fear" 11507)("negative" 24929)("sadness" 11003)
;                     ("anticipation" 24320)("surprise" 12054)("joy" 7452)("anger" 1820)) 
data1

(newline)
(newline)
;;;We can visualize this result as a barplot (discrete-histogram)
(let ([counts data1])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "SeaGreen"
	    #:line-color "SeaGreen"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

;;;Using the bing lexicon to determine the ratio of
;;; positive-to-negative words
;;INPUT:sentiment1
;;OUTPUT:cleanedinutdata1
(newline)
(newline)
(display "SENTIMENTAL POLARITY USING BING LEXICON")
(newline)
(newline)
(define sentiment1 (list->sentiment cleanedinutdata1 #:lexicon 'bing))
(define data2 (aggregate sum ($ sentiment1 'sentiment) ($ sentiment1 'freq)))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 data2
	 #:y-min 0
	 ;#:y-max 2000
	 #:invert? #t
	 #:color "Blue"
	 #:line-color "Blue")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))


;;; We can also look at which words are contributing the most to our
;;; positive and negative sentiment scores. We'll look at the top 20
;;; influential (i.e., most frequent) positive and negative words
(define negative-tokens
  (take (cdr (subset sentiment1 'sentiment "negative")) 20))
(define positive-tokens
  (take (cdr (subset sentiment1 'sentiment "positive")) 20))

;;; Some clever reshaping for plotting purposes
(define n (map (λ (x) (list (first x) (- 0 (third x))))
	       negative-tokens))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))

;;; Plot the results
(display "TOP WORDS CONTRIBUTING TO EACH POLARITY")
(newline)
(newline)
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot (list
	 (tick-grid)
	 (discrete-histogram n #:y-min -500
			     #:y-max 0
			     #:color "Red"
			     #:line-color "Red"
			     #:label "Negative Sentiment") 
	 (discrete-histogram p #:y-min 0
			     #:y-max 500
			     #:x-min 20
			     #:color "SeaGreen"
			     #:line-color "SeaGreen"
			     #:label "Positive Sentiment"))
	#:x-label "Word"
	#:y-label "Contribution to sentiment"))


(define (qq-plot data2 #:scale? [scale? #t])
  (let* ([n (length data2)]
	 [lst-mean (mean data1)]
	 [lst-stddev (stddev data1)]
	 [probs (map (λ (x) (/ x (+ 2 n))) (range 1 (add1 n)))]
	 [normal-quantiles
	  (map (λ (x) (inv-cdf (normal-dist lst-mean lst-stddev) x)) probs)]
	 ;; Scale the data?
	 [xs (if scale? (scale normal-quantiles) normal-quantiles)]
	 [ys (if scale? (scale data1) data1)])
    (points (map vector (sort-samples < xs)
		 (sort-samples < ys)))))

;;; Same as qq-plot, but automatically passed the renderer to `plot`
;;; for quick convenience
(define (qq-plot* data1 #:scale? [scale? #t])
  (plot (qq-plot data1 #:scale? scale?)
	#:x-label "Theoretical Normal Quantiles"
	#:y-label "Sample Quantiles")
	(display("nothing was plotted"))
	)
(data1)