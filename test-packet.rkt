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
			      "a:b\r\\n\\cc\n"
			      "a:another\n"
			      "\n"
			      "some body\0")
	       read-stomp-frame)
	      (stomp-frame '|COMMAND|
			   '((header1 "value1")
			     (|header two| "value two")
			     (a "b\r\n:c")
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
		 (write-stomp-frame (stomp-frame '|COMMAND| '((a "b")) #f) p)))
	      (string-append "COMMAND\n"
			     "a:b\n"
			     "\n"
			     "\0"))
