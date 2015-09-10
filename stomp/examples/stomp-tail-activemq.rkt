#lang racket

(require "../main.rkt") ;; or (planet tonyg/stomp)

(define s (stomp-connect "localhost"))

(stomp-subscribe s "/topic/ActiveMQ.Advisory.Connection" "s1")

(with-handlers ([exn:break?
		 (lambda (e)
		   (stomp-unsubscribe s "s1")
		   (stomp-disconnect s)
		   (pretty-print 'clean-disconnect-complete)
		   (raise e))])
  (let loop ()
    (let ((m (stomp-next-message s "s1")))
      (when (not (eof-object? m))
	(pretty-print m)
	(loop))))
  (pretty-print 'broker-went-away))
