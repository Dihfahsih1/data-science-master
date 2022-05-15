#lan racket
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
     [consumer-key "7WIHdgxmnaYaupMv3i4rdo5Lp"]
     [consumer-secret "XTj927kTI3zYean9rkpsjt5jckwEm01qrxUOH6Cp7wt4pG3iLc"]
     [access-token "1401150439668727814-IKQXVgKg8zbfRrmuQALvPYeH39VQkm"]
     [access-token-secret "cEuf9Oskc03PPHcUOkYYWZSPNRmZH7ASB8tsTqyArwebp"]))

(define tweeterdata (send twitter-oauth get-request 
  "https://api.twitter.com/2/tweets/search/tweets.json" 
  (list (cons 'query "place_country:UG")
  (cons 'maxResults "100")
   (cons 'fromDate "2021-01-31-12:00") 
   (cons 'toDate "202112312359"))))

;writing the Twitter data to a file
(define out (open-output-file "data.json"))
(write tweeterdata out)
(close-output-port out)