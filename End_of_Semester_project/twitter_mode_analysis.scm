#lang racket

;Personal Details
;David Kabiito
;2018/HD05/1954U
;1800737705

(require data-science-master)
(require plot)
(require math)

(require net/url 
  net/uri-codec
  web-server/stuffers/hmac-sha1
  net/base64
  rackunit)

;###################################################
;# OAuth 1.0a for API authentication               #
;###################################################


;oauth-single-user class

(define oauth-single-user%
  (class object%
    (super-new)
    
    ;; mandatory fields, must be
    ;; set by user
    (init-field consumer-key)
    (init-field consumer-secret)
    (init-field access-token)
    (init-field access-token-secret)
    
    
    ;;;;oauth-single-user constants 
    (define time-stamp (number->string (current-seconds)))
    (define oauth-version "1.0")
    (define signature-method "HMAC-SHA1")
    (define nonce (string-append (number->string (current-seconds))
                 (number->string (random
                        (current-seconds) (make-pseudo-random-generator)))))
    
    (define (get-key-as-string key_and_value)
      (symbol->string (car key_and_value)))
    
    
    ;;; url helper functions
    (define (get-base-url request-url)
      (let ([old-url (string->url request-url)])
        (url->string (make-url (url-scheme old-url) #f 
                         (url-host old-url) #f #t 
                         (url-path old-url) empty #f))))
    
    (define (add-params-to-url base-url params)
      (let ([old-url (string->url base-url)])
        (url->string (make-url (url-scheme old-url) #f
                         (url-host old-url) #f #t
                         (url-path old-url) 
                         params
                         #f))))
                         
      
    ;creates param string to be used in signature 
    (define (create-param-string list_of_keys param_string) 
      (cond 
        [(empty? list_of_keys) param_string]
        [(equal? param_string "")
         (create-param-string (rest list_of_keys)
                  (string-append (symbol->string(car (first list_of_keys)))
                        "="
                       (cdr (first list_of_keys))))]
        [else 
         (create-param-string (rest list_of_keys) 
                            (string-append param_string "&" 
             (symbol->string (car (first list_of_keys)))
          "="
          (cdr (first list_of_keys))))]))  
  
    ;create list of oauth params
    (define/private (create-oauth-keys-and-values) 
      (list (cons 'oauth_consumer_key consumer-key)
            (cons 'oauth_signature_method signature-method)
            (cons 'oauth_version oauth-version)
            (cons 'oauth_timestamp time-stamp)
            (cons 'oauth_nonce nonce)
            (cons 'oauth_token access-token)))
    
 (define (generate-base-string http-method base-url params)
  ;signature base string is 
   ;http-method || "&" || base-url || "&" || param-string
   ;all values are percent encoded
   ;http-method must be in all caps, e.g. POST
   ;base-url of request, e.g. https://api.twitter.com/1/statuses/update.json
   ;param-string is in x=y format where
   ;x is the key of the param and y is the value of the param
   ;each key and value are followed by a & if another key/value pair exists
  (string-append (string-upcase http-method) "&" 
   (uri-unreserved-encode base-url) "&" 
   (uri-unreserved-encode 
    (create-param-string 
     (sort (percent-encode-keys-and-values 
            (append (create-oauth-keys-and-values) params)) 
       string<? #:key get-key-as-string)
   ""))))  
    
 (define/private (generate-signing-key consumer-secret token-secret)
  ;;signature_key = consumer_secret || "&" || token_secret
  (string-append (uri-unreserved-encode consumer-secret)
   "&"
   (uri-unreserved-encode token-secret)))
    
  (define/private (generate-signature http-method base-url params)
    ;signature = base64(hmac-sha1(signature_base_string, signature_key))
    (uri-unreserved-encode 
    (bytes->string/utf-8
     ;base64-encode returns #"xxxx\r\n"
     ;use regexp-replace to take out \r and \n
     (regexp-replace #rx#"[\r\n]+$" (base64-encode (HMAC-SHA1
      ;create signing key
      (string->bytes/utf-8 
       (generate-signing-key consumer-secret access-token-secret))
      ;create signature base string 
      (string->bytes/utf-8 
       (generate-base-string http-method  base-url
                             params))))""))) )
    
    (define/private (generate-auth-header http-method base-url params)
      (string-append "Authorization: OAuth " 
                 "oauth_consumer_key=\"" consumer-key "\","
                 "oauth_nonce=\"" nonce "\"," 
                 "oauth_signature=\"" (generate-signature http-method base-url params) "\","
                 "oauth_signature_method=\"" signature-method "\","
                 "oauth_timestamp=\"" time-stamp "\","
                 "oauth_token=\"" access-token "\","
                 "oauth_version=\"" oauth-version "\""))
    
     ;percent encode all keys and values of parameter list
     (define (percent-encode-keys-and-values list-of-params)
       (map (lambda (param)
              (cons 
                (string->symbol(uri-unreserved-encode 
                               (symbol->string (car param))))
                (uri-unreserved-encode (cdr param))))
              list-of-params))
    
      (define/public (get-request base-url [params empty])
         (regexp-match
            #px".*"
            (get-pure-port
             (if (empty? params)
                 (string->url base-url)
                 (string->url (add-params-to-url base-url params)))
             (list
              (generate-auth-header "get" base-url params)))))
  
      (define/public (post-request base-url post-data)
        (regexp-match
            #px".*"
            (post-pure-port
             (if (empty? post-data)
                 (string->url base-url)
                 (string->url (add-params-to-url base-url post-data))) 
             ;post data
             (string->bytes/utf-8 (create-param-string 
                                   (sort 
                                    (percent-encode-keys-and-values post-data) 
                                    string<? #:key get-key-as-string) ""))
             (list
              (generate-auth-header "post" base-url post-data)))))))

;;;;;Sample twitter OAuth 1.0a for API authentication
(define twitter-oauth (new oauth-single-user%  
     [consumer-key "n4Bk8XrSSjEhTZPCASOQDJvWi"]
     [consumer-secret "niTJzSwcOzNrKYrWhcqYffWrBpdMqMbydmnXspt5bYnmmov80A"]
     [access-token "1055029678334009344-8iBju1rGfLj7vfrruDGs42PZ404o6C"]
     [access-token-secret "kpHNSWgpYZ8oYJPAoxIknwHw0P9ZmRy2FeiCrebK4q0Tr"]))


;(send twitter-oauth get-request 
;  "https://api.twitter.com/1.1/friends/ids.json" )

;Functionality for getting the country to analysis
;(display "PLEASE ENTER A PLACE_COUNTRY CODE THAT YOU WOULD WISH TO ANALYSIS\n")
;(display "Sample Country codes UG for Uganda, TZ for Tanzania. in the format place_country:UG \n")
;(define place_country (read-line))

;(send twitter-oauth get-request 
;  "https://api.twitter.com/1.1/search/tweets.json" 
;  (list (cons 'q country)))
;  (list (cons 'q country) (cons 'count "10") (cons 'until "2018-10-01") (cons 'geocode "0.316667,32.583330,1km")))
;  (list (cons 'q country) (cons 'count 100) (cons 'since_id 967824267948773377) (cons 'geocode "0.316667,32.583330,1km")))
;  (list (cons 'q country) (cons 'since_id "967824267948773377") (cons 'geocode "0.316667,32.583330,1km")))

(define tweeterdata (send twitter-oauth get-request 
  "https://api.twitter.com/1.1/tweets/search/30day/prod.json" 
  (list (cons 'query "place_country:UG")(cons 'maxResults "100") (cons 'fromDate "201810311200") (cons 'toDate "201810312359"))))

;writing the Twitter data to a file
(define out (open-output-file "tweeterdata_uganda_combined.json"))
(write tweeterdata out)
(close-output-port out)

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
(define input_tweets (port->string (open-input-file "tweeterdata_uganda_combined.json")))


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
