#lang racket/base

(require rackunit)
(require racket/port)

(require "packet.rkt")

(check-equal? (unescape-stomp-header "abc\\ndef") "abc\ndef")
(check-equal? (unescape-stomp-header "abc\\cdef") "abc:def")
(check-equal? (unescape-stomp-header "abc\\\\def") "abc\\def")
(check-equal? (unescape-stomp-header "abc\rdef") "abc\rdef")
(check-exn exn:fail:syntax? (lambda () (unescape-stomp-header "abc\\rdef")))

(check-equal? (escape-stomp-header "abc\ndef") "abc\\ndef")
(check-equal? (escape-stomp-header "abc:def") "abc\\cdef")
(check-equal? (escape-stomp-header "abc\\def") "abc\\\\def")
(check-equal? (escape-stomp-header "abc\rdef") "abc\rdef")

(check-equal? (call-with-input-string
	       (string-append "COMMAND\n"
			      "header1:value1\n"
			      "header two:value two\n"
			      "a:b\r\\n\\cc:d\n"
			      "a:another\n"
			      "\n"
			      "some body\0")
	       read-stomp-frame)
	      (stomp-frame '|COMMAND|
			   '((header1 "value1")
			     (|header two| "value two")
			     (a "b\r\n:c:d")
			      (a "another"))
			   #"some body"))

(check-equal? (call-with-input-string
	       (string-append "COMMAND\n"
			      "header1:value1\n"
			      "header two:value two\n"
			      "a:b\r\\n\\cc:d\n"
			      "a:another\n"
			      "\n"
			      "some body\0")
	       (lambda (p) (read-stomp-frame p #:unescape? #f)))
	      (stomp-frame '|COMMAND|
			   '((header1 "value1")
			     (|header two| "value two")
			     (a "b\r\\n\\cc:d")
			      (a "another"))
			   #"some body"))

(check-equal? (call-with-input-string
	       (string-append "COMMAND\n"
			      "header1:value1\n"
			      "header two:value two\n"
			      "a:b\r\\n\\cc\n"
			      "a:another\n"
			      "content-length:9\n"
			      "\n"
			      "some body\0")
	       read-stomp-frame)
	      (stomp-frame '|COMMAND|
			   '((header1 "value1")
			     (|header two| "value two")
			     (a "b\r\n:c")
			     (a "another")
			     (content-length "9"))
			   #"some body"))

(check-equal? (call-with-output-string
	       (lambda (p)
		 (write-stomp-frame (stomp-frame '|COMMAND|
						 '((header1 "value1")
						   (|header two| "value two")
						   (a "b\r\n:c")
						   (a "another"))
						 #"some body")
				    p)))
	      (string-append "COMMAND\n"
			     "content-length:9\n"
			     "header1:value1\n"
			     "header two:value two\n"
			     "a:b\r\\n\\cc\n"
			     "a:another\n"
			     "\n"
			     "some body\0"))

(check-equal? (call-with-output-string
	       (lambda (p)
		 (write-stomp-frame (stomp-frame '|COMMAND|
						 '((header1 "value1")
						   (|header two| "value two")
						   (a "b\r\n:c")
						   (a "another"))
						 #"some body")
				    p)))
	      (string-append "COMMAND\n"
			     "content-length:9\n"
			     "header1:value1\n"
			     "header two:value two\n"
			     "a:b\r\\n\\cc\n"
			     "a:another\n"
			     "\n"
			     "some body\0"))

(check-equal? (call-with-output-string
	       (lambda (p)
		 (write-stomp-frame (stomp-frame '|COMMAND|
						 '((a "b:c"))
						 #f)
				    p
				    #:escape? #f)))
	      (string-append "COMMAND\n"
			     "a:b:c\n"
			     "\n"
			     "\0"))

(check-exn #rx"as it contains a newline"
	   (lambda ()
	     (call-with-output-string
	      (lambda (p)
		(write-stomp-frame (stomp-frame '|COMMAND|
						'((a "b\nc"))
						#f)
				   p
				   #:escape? #f)))))

(check-exn #rx"as it contains a colon"
	   (lambda ()
	     (call-with-output-string
	      (lambda (p)
		(write-stomp-frame (stomp-frame '|COMMAND|
						'((a:b "c"))
						#f)
				   p
				   #:escape? #f)))))

(check-equal? (call-with-output-string
	       (lambda (p)
		 (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #f) p)))
	      (string-append "COMMAND\n"
			     "a:b\n"
			     "\n"
			     "\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #f) p
					      #:use-content-length 'default)))
	      (string-append "COMMAND\n" "a:b\n" "\n" "\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #f) p
					      #:use-content-length 'always)))
	      (string-append "COMMAND\n" "content-length:0\n" "a:b\n" "\n" "\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #f) p
					      #:use-content-length 'never)))
	      (string-append "COMMAND\n" "a:b\n" "\n" "\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #"hi") p
					      #:use-content-length 'default)))
	      (string-append "COMMAND\n" "content-length:2\n" "a:b\n" "\n" "hi\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #"hi") p
					      #:use-content-length 'always)))
	      (string-append "COMMAND\n" "content-length:2\n" "a:b\n" "\n" "hi\0"))

(check-equal? (call-with-output-string
	       (lambda (p) (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #"hi") p
					      #:use-content-length 'never)))
	      (string-append "COMMAND\n" "a:b\n" "\n" "hi\0"))
