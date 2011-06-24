#lang racket/base

;; STOMP 1.1

;; LF                  = <US-ASCII new line (line feed) (octet 10)>
;; OCTET               = <any 8-bit sequence of data>
;; NULL                = <octet 0>
;;
;; frame-stream        = 1*frame
;;
;; frame               = command LF
;;                       *( header LF )
;;                       LF
;;                       *OCTET
;;                       NULL
;;                       *( LF )
;;
;; command             = client-command | server-command
;;
;; client-command      = "SEND"
;;                       | "SUBSCRIBE"
;;                       | "UNSUBSCRIBE"
;;                       | "BEGIN"
;;                       | "COMMIT"
;;                       | "ABORT"
;;                       | "ACK"
;;                       | "NACK"
;;                       | "DISCONNECT"
;;                       | "CONNECT"
;;                       | "STOMP"
;;
;; server-command      = "CONNECTED"
;;                       | "MESSAGE"
;;                       | "RECEIPT"
;;                       | "ERROR"
;;
;; header              = header-name ":" header-value
;; header-name         = 1*<any OCTET except LF or ":">
;; header-value        = *<any OCTET except LF or ":">

(require racket/match)
(require srfi/2)

(provide (struct-out stomp-frame)
	 stomp-frame-header
	 read-stomp-frame
	 write-stomp-frame
	 escape-stomp-header
	 unescape-stomp-header)

(struct stomp-frame (command headers body) #:transparent)

(define (stomp-frame-header frame header-name [default-value #f])
  (cond
   ((assq header-name (stomp-frame-headers frame)) => cadr)
   (else default-value)))

(define (read-stomp-frame port)
  (let ((command (read-stomp-command port)))
    (if (eof-object? command)
	command
	(let ((headers (read-stomp-headers port '())))
	  (if (eof-object? headers)
	      headers
	      (let ((body (read-stomp-body headers port)))
		(if (eof-object? body)
		    body
		    (stomp-frame command headers body))))))))

(define (read-stomp-line port)
  (let ((line (read-bytes-line port 'linefeed)))
    (if (eof-object? line)
	line
	(bytes->string/utf-8 line))))

(define (stomp-newline port)
  (write-byte 10 port))

(define (write-stomp-line line port)
  (write-bytes (string->bytes/utf-8 line) port)
  (stomp-newline port))

(define (stomp-syntax-error message)
  (raise (exn:fail:syntax message (current-continuation-marks) '())))

(define (unescape-stomp-header str)
  (list->string
   (let loop ((cs (string->list str)))
     (match cs
       [`() '()]
       [`(#\\ #\\ . ,rest) (cons #\\ (loop rest))]
       [`(#\\ #\n . ,rest) (cons #\newline (loop rest))]
       [`(#\\ #\c . ,rest) (cons #\: (loop rest))]
       [`(#\\ ,c . ,rest) (stomp-syntax-error "Illegal header backslash escape")]
       [`(,c . ,rest) (cons c (loop rest))]))))

(define (escape-stomp-header str)
  (list->string
   (let loop ((cs (string->list str)))
     (match cs
       [`() '()]
       [`(#\\ . ,rest) (cons #\\ (cons #\\ (loop rest)))]
       [`(#\newline . ,rest) (cons #\\ (cons #\n (loop rest)))]
       [`(#\: . ,rest) (cons #\\ (cons #\c (loop rest)))]
       [`(,c . ,rest) (cons c (loop rest))]))))

(define (read-stomp-command port)
  (let ((line (read-stomp-line port)))
    (cond
     ((eof-object? line) line)
     ((string=? line "") (read-stomp-command port))
     (else (string->symbol line)))))

(define (read-stomp-headers port acc)
  (let ((line (read-stomp-line port)))
    (cond
     ((eof-object? line) line)
     ((string=? line "") (reverse acc))
     (else (match (regexp-split ":" line)
	     [`(,escaped-key ,escaped-value)
	      (read-stomp-headers port
				  (cons (list (string->symbol (unescape-stomp-header escaped-key))
					      (unescape-stomp-header escaped-value))
					acc))]
	     [_ (stomp-syntax-error "Invalid STOMP header")])))))

(define (read-stomp-body headers port)
  (cond
   ((assq 'content-length headers) =>
    (lambda (entry)
      (let ((len (string->number (cadr entry))))
	(if (not len)
	    (stomp-syntax-error "Bad content-length header")
	    (read-counted-body port len)))))
   (else (read-nul-delimited-body port))))

(define (read-counted-body port len)
  (let ((buffer (read-bytes len port)))
    (cond
     ((eof-object? buffer) buffer)
     ((< (bytes-length buffer) len) (stomp-syntax-error "Body shorter than expected"))
     (else (if (= (read-byte port) 0)
	       buffer
	       (stomp-syntax-error "Missing NUL byte after explicitly-counted body"))))))

(define (read-nul-delimited-body port)
  (let loop ((acc '()))
    (let ((b (read-byte port)))
      (cond
       ((eof-object? b) b)
       ((= b 0) (list->bytes (reverse acc)))
       (else (loop (cons b acc)))))))

(define (write-stomp-frame frame port)
  (let* ((body (or (stomp-frame-body frame) #""))
	 (len (bytes-length body))
	 (user-headers (filter (lambda (header) (not (eq? (car header) 'content-length)))
			       (stomp-frame-headers frame)))
	 (headers (if (positive? len)
		      (cons (list 'content-length (number->string len)) user-headers)
		      user-headers)))
    (write-stomp-line (symbol->string (stomp-frame-command frame)) port)
    (for-each (lambda (header)
		(write-stomp-line
		 (string-append (escape-stomp-header (symbol->string (car header)))
				":"
				(escape-stomp-header (cadr header)))
		 port))
	      headers)
    (stomp-newline port)
    (write-bytes body port)
    (write-byte 0 port)))
