#lang racket/base

(require "packet.rkt")
(require racket/tcp)
(require racket/match)
(require "functional-queue.rkt")

(provide (struct-out stomp-session)
	 (struct-out exn:stomp)
	 stomp-connect
	 stomp-disconnect
	 stomp-disconnect/abrupt
	 stomp-flush

	 stomp-message-id

	 wait-for-receipt
	 call-with-receipt

	 stomp-send-command
	 stomp-next-frame
	 stomp-next-frame/filter
	 stomp-next-message

	 stomp-send
	 stomp-send/flush
	 stomp-subscribe
	 stomp-unsubscribe
	 stomp-ack
	 stomp-ack-message
	 stomp-nack
	 stomp-begin
	 stomp-commit
	 stomp-abort

	 call-with-stomp-transaction)

(struct stomp-session (input
		       output
		       id
		       server-info
		       [buffer #:mutable])
	#:transparent)

(struct exn:stomp exn:fail (frame)
	#:transparent)

(define (session-exn-closer session)
  (lambda (e)
    (stomp-disconnect/abrupt session)
    (raise e)))

(define (stomp-connect hostname
		       [login #f]
		       [passcode #f]
		       [virtual-host hostname]
		       [port-number 61613])
  (let-values (((i o) (tcp-connect hostname port-number)))
    (let ((session0 (stomp-session i o #f #f (make-queue))))
      (with-handlers ([exn? (session-exn-closer session0)])
	(stomp-send-command session0 '|CONNECT|
			    `((accept-version "1.1")
			      (host ,virtual-host)
			      ,@(if login `((login ,login)) '())
			      ,@(if passcode `((passcode ,passcode)) '())))
	(match (stomp-next-frame session0)
	  [(and connected-frame
		(stomp-frame '|CONNECTED|
			     `(,_ ... (version "1.1") ,_ ...)
			     _))
	   (stomp-session i
			  o
			  (stomp-frame-header connected-frame 'session)
			  (stomp-frame-header connected-frame 'server)
			  (make-queue))]
	  [v (error 'stomp-connect "Could not CONNECT to STOMP server: ~v" v)])))))

(define (stomp-disconnect session)
  (call-with-receipt session
   (lambda (receipt)
     (stomp-send-command session '|DISCONNECT| `((receipt ,receipt)))))
  (stomp-disconnect/abrupt session))

(define (stomp-disconnect/abrupt session)
  (close-input-port (stomp-session-input session))
  (close-output-port (stomp-session-output session)))

(define (stomp-flush session)
  (flush-output (stomp-session-output session)))

(define (stomp-message-id frame)
  (or (stomp-frame-header frame 'message-id)
      (raise (exn:stomp "Message frame missing message-id header"
			(current-continuation-marks)
			frame))))

(define (wait-for-receipt session receipt)
  (let ((f (stomp-next-frame/filter session
	    (lambda (frame)
	      (and (eq? (stomp-frame-command frame) '|RECEIPT|)
		   (equal? receipt (stomp-frame-header frame 'receipt-id)))))))
    (if (eof-object? f)
	(raise (exn:fail:read:eof
		(format "Disconnected before receipt ~s was received" receipt)
		(current-continuation-marks)
		(list)))
	#t)))

(define (make-counter prefix initial-value)
  (let ((value initial-value))
    (lambda ()
      (let ((v value))
	(set! value (+ value 1))
	(string-append prefix (number->string v))))))

(define call-with-receipt
  (let ((counter (make-counter "R" 14641)))
    (lambda (session proc)
      (let* ((receipt (counter))
	     (result (proc receipt)))
	(wait-for-receipt session receipt)
	result))))

(define (stomp-send-command session command [headers '()] [body #f])
  (write-stomp-frame (stomp-frame command headers body) (stomp-session-output session)))

(define (stomp-next-frame session [block? #t])
  (stomp-next-frame/filter session (lambda (frame) #t) block?))

(define (stomp-next-frame/filter session predicate [block? #t])
  (let-values (((frame remainder) (queue-extract (stomp-session-buffer session) predicate)))
    (cond
     (frame
      (set-stomp-session-buffer! session remainder)
      frame)
     ((not block?) #f)
     (else (block-on-predicate session predicate)))))

(define (block-on-predicate session predicate)
  (stomp-flush session)
  (let loop ()
    (let ((frame (read-stomp-frame (stomp-session-input session))))
      (cond
       ((eof-object? frame) frame)
       ((predicate frame) frame)
       ((eq? (stomp-frame-command frame) '|ERROR|)
	(raise (exn:stomp "Received ERROR" (current-continuation-marks) frame)))
       (else
	(set-stomp-session-buffer! session
				   (enqueue (stomp-session-buffer session) frame))
	(loop))))))

(define (stomp-next-message session subscription-id [block? #t])
  (stomp-next-frame/filter session
			   (lambda (frame)
			     (and (eq? (stomp-frame-command frame) '|MESSAGE|)
				  (equal? (stomp-frame-header frame 'subscription)
					  subscription-id)))
			   block?))

(define (stomp-send session destination body [headers '()])
  (stomp-send-command session '|SEND| `((destination ,destination) ,@headers) body))

(define (stomp-send/flush session destination body [headers '()])
  (stomp-send session destination body headers)
  (stomp-flush session))

(define (stomp-subscribe session destination subscription-id [ack-mode 'auto] [headers '()])
  (stomp-send-command session '|SUBSCRIBE|
		      `((destination ,destination)
			(id ,subscription-id)
			(ack ,(case ack-mode
				((auto) "auto")
				((client) "client")
				((client-individual) "client-individual")
				(else (raise (exn:fail:contract
					      (format "Invalid subscribe ack-mode ~v" ack-mode)
					      (current-continuation-marks))))))
			,@headers)))

(define (stomp-unsubscribe session subscription-id [headers '()])
  (stomp-send-command session '|UNSUBSCRIBE| `((id ,subscription-id) ,@headers)))

(define (stomp-ack session subscription-id message-id [headers '()])
  (stomp-send-command session '|ACK|
		      `((subscription ,subscription-id)
			(message-id ,message-id)
			,@headers)))

(define (stomp-ack-message session message [headers '()])
  (stomp-ack session
	     (or (stomp-frame-header message 'subscription)
		 (raise (exn:stomp "Message frame missing subscription header"
				   (current-continuation-marks)
				   message)))
	     (stomp-message-id message)
	     headers))

(define (stomp-nack session subscription-id message-id [headers '()])
  (stomp-send-command session '|NACK|
		      `((subscription ,subscription-id)
			(message-id ,message-id)
			,@headers)))

(define (stomp-begin session transaction [headers '()])
  (stomp-send-command session '|BEGIN|
		      `((transaction ,transaction)
			,@headers)))

(define (stomp-commit session transaction [headers '()])
  (stomp-send-command session '|COMMIT|
		      `((transaction ,transaction)
			,@headers)))

(define (stomp-abort session transaction [headers '()])
  (stomp-send-command session '|ABORT|
		      `((transaction ,transaction)
			,@headers)))

(define (rollback-on-error session transaction)
  (lambda (exn)
    (with-handlers ([exn? (lambda (ignored-inner-exn)
			    ;; Give up on the rollback
			    (stomp-disconnect/abrupt session))])
      (stomp-abort session transaction))
    (raise exn)))

(define call-with-stomp-transaction
  (let ((counter (make-counter "Tx" 1039)))
    (lambda (session proc)
      (let ((transaction (counter)))
	(stomp-begin session transaction)
	(let ((result (with-handlers ([exn? (rollback-on-error session transaction)])
			(proc transaction))))
	  (call-with-receipt session
	   (lambda (receipt)
	     (stomp-commit session transaction)))
	  result)))))
