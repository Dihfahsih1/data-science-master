;MUGOYA DIHFAHSIH
;Student no. 2100702353
;Reg no. 2021/HD05/2353U


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

;Reads all characters from in and returns them as a string.Reading from a file in this case we are reading from data.json
(define input_tweets (port->string (open-input-file "data.json")))


;changing all words to lower case, then removing urls,then remove punctuation makes
;the normalizing the space between words the cleaned data is save as cleanedinutdata and the input is rawiputdata
(define cleaned_input_data (string-normalize-spaces
                         (remove-punctuation
                          (remove-false
                           (remove-true
                            (remove-sensitive
                             (remove-favorite
                              (remove-favorited
                               (remove-urls
                                (string-downcase input_tweets))))))))))

;;This proceedure outputs quoted strings in the list. The order of the strings is not maintained from input to output
;;Further cleaning is done by removing stop words using the defaults lexicon 

;;Returns a list of pairs. Each pair consists of a unique word/token from str with its frequency.
;;the number of times a word occurs in the tweets selected
(define cleaned_input_data1 (document->tokens cleaned_input_data #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.
(define sentiment (list->sentiment cleaned_input_data1 #:lexicon 'nrc))


;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(define sentimize_twitter_data (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))
(newline)
sentimize_twitter_data

(newline)
(newline)
(display "SENTIMENTAL DISCRETE-HISTOGRAM")
(newline)
(newline)
;;;We can visualize this result as a barplot (discrete-histogram)
(let ([counts sentimize_twitter_data])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "SeaGreen"
	    #:line-color "SeaGreen"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

;;;Using the bing lexicon to determine the ratio ofpositive-to-negative words
(newline)
(newline)
(display "SENTIMENTAL POLARITY USING BING LEXICON")
(newline)
(newline)
(define sentiment1 (list->sentiment cleaned_input_data1 #:lexicon 'bing))
(define better_histogram (aggregate sum ($ sentiment1 'sentiment) ($ sentiment1 'freq)))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 better_histogram
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

