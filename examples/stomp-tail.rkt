#lang racket

(require "../main.rkt") ;; or (planet tonyg/stomp)

(define s (stomp-connect "dev.rabbitmq.com" "guest" "guest" "/"))

(stomp-subscribe s "/exchange/amq.rabbitmq.log/#" "s1")

(define (w fmt . args)
  (stomp-send s "/exchange/amq.rabbitmq.log/stomp-tail"
	      (string->bytes/utf-8 (apply format fmt args))
	      `((content-type "application/octet-stream"))))

(w "Here I am! My session is ~v" (stomp-session-id s))
(let loop ()
  (let ((m (with-handlers ([exn:break?
			    (lambda (e)
			      (w "Goodbye! from ~v" (stomp-session-id s))
			      (stomp-unsubscribe s "s1")
			      (stomp-disconnect s)
			      (pretty-print 'clean-disconnect-complete)
			      (raise e))])
	     (stomp-next-message s "s1"))))
    (pretty-print m)
    (loop)))
