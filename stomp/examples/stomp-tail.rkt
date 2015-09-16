#lang racket

(require "../main.rkt") ;; or just (require stomp)

(with-handlers ([exn:stomp?
                 (lambda (e)
                   (pretty-print e))])
  (define s (stomp-connect "dev.rabbitmq.com"
                           #:login "guest"
                           #:passcode "guest"
                           #:virtual-host "/"))

  (stomp-subscribe s "/exchange/amq.rabbitmq.log/#" "s1")
  (stomp-subscribe s "/exchange/amq.direct/stomp-tail" "s2")

  (define (w fmt . args)
    (stomp-send s "/exchange/amq.direct/stomp-tail"
                (string->bytes/utf-8 (apply format fmt args))
                #:headers `((content-type "application/octet-stream"))))

  (w "Here I am! My session is ~v" (stomp-session-id s))
  (with-handlers ([exn:break?
                   (lambda (e)
                     (w "Goodbye! from ~v" (stomp-session-id s))
                     (stomp-unsubscribe s "s1")
                     (stomp-unsubscribe s "s2")
                     (stomp-disconnect s)
                     (pretty-print 'clean-disconnect-complete)
                     (raise e))])
    (let loop ()
      (let ((m (stomp-next-message s (set "s1" "s2"))))
        (when (not (eof-object? m))
          (pretty-print m)
          (loop))))
    (pretty-print 'broker-went-away)))
