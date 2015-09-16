#lang racket

(module+ main
  (require "../main.rkt") ;; or just (require stomp)

  ;; The program takes one mandatory positional argument: the name of a
  ;; person to greet.
  (define name-to-greet
    (command-line
     #:program "rpc-client.rkt"
     #:args (name-to-greet)
     name-to-greet))

  ;; We wrap the code in a handler for exn:stomp, in order to get
  ;; insight into the details of any error messages returned by the
  ;; server.
  (with-handlers ([exn:stomp?
                   (lambda (e)
                     (pretty-print e))])
    (define s (stomp-connect "dev.rabbitmq.com"
                             #:login "guest"
                             #:passcode "guest"
                             #:virtual-host "/"))

    ;; We send the request to the name associated with the RPC service
    ;; from rpc-server.rkt. The body of the message is just the name of
    ;; the person to greet from the command line.
    ;;
    ;; We make sure to include a "reply-to" header containing the name
    ;; of a temporary queue. When the server replies, it will cause a
    ;; MESSAGE frame to be delivered to us labelled with the name of the
    ;; temporary queue as its *subscription id*. For details of the way
    ;; RabbitMQ uses temporary queues, please see their documentation at
    ;; https://www.rabbitmq.com/stomp.html
    (stomp-send s "/queue/rpc-server.rkt" (string->bytes/utf-8 name-to-greet)
                #:headers '((reply-to "/temp-queue/my-replies")))

    ;; This line waits for the reply MESSAGE frame to appear on the
    ;; correct temporary queue subscription. Note that the temp-queue
    ;; labels used only have to be unique on a per-connection basis --
    ;; they don't have to be globally unique.
    (define reply (stomp-next-message s "/temp-queue/my-replies"))

    ;; We simply print the MESSAGE we got from the server...
    (pretty-print reply)

    ;; ... and disconnect.
    (stomp-disconnect s)))

