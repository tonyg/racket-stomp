#lang racket

(module+ main
  (require "../main.rkt") ;; or just (require stomp)

  (define start-time (current-inexact-milliseconds))

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

    ;; We listen for requests on the RabbitMQ queue named
    ;; "rpc-server.rkt". Note that we specified the "/amq/queue/..."
    ;; variation rather than the "/queue/..." variation. This primarily
    ;; impacts questions such as what happens to requests coming from
    ;; clients when no server is attached to the broker. With
    ;; /amq/queue, requests survive across server outages; with /queue,
    ;; they are dropped on the floor if no server is running at the
    ;; time. We choose the more robust behaviour here.
    ;;
    ;; Note well we use the `'client-individual` ack-mode here. This
    ;; means that each request we receive needs to be acknowledged; if
    ;; it isn't acknowledged, and we disconnect, it will be resent next
    ;; time we (or another service instance) connects.
    ;;
    ;; To see this in action, try sending a request with the word
    ;; "crash" in the message body sometime a few seconds after
    ;; successfully starting the server.
    ;;
    ;; Another interesting thing to try is running more than one
    ;; instance of this program. When you issue requests, they will be
    ;; dealt out reasonably evenly to all attached service instances.
    (stomp-subscribe s "/amq/queue/rpc-server.rkt" "s1" #:ack-mode 'client-individual)

    (printf "Ready for requests.\n")
    (let loop ()
      ;; Read the next request on our subscription "s1".
      (match (stomp-next-message s "s1")

        ;; EOF objects mean we were disconnected.
        [(? eof-object?) (void)]

        ;; Otherwise, it will be a stomp-frame. We extract the headers
        ;; and the body. The body will be a `bytes?`; the headers will
        ;; contain information we will need to use to route the reply to
        ;; the requesting party.
        [(and request-message (stomp-frame _ headers body))
         (match (assq 'reply-to headers)
           [#f
            (printf "Missing reply-to header in request.\n")]
           [(list _ reply-to)
            (match body

              ;; If we are asked to greet "crash", we simulate a buggy server.
              [#"crash"
                (cond
                  [(< (- (current-inexact-milliseconds) start-time) 1000)
                   (printf "We were asked to crash just as we were getting started. Ignore it\n")
                   (printf "since it's likely this is a crash request still queued from a\n")
                   (printf "previous incarnation of the server.\n")
                   (stomp-send s reply-to #"Nice try. Not crashing today!")
                   (stomp-ack-message s request-message)]
                  [else
                   (printf "We were asked to crash! This is a simulation of a buggy server.\n")
                   (error 'rpc-server.rkt "Exiting due to simulated bug.")])]

              ;; Otherwise, we actually handle the request properly,
              ;; building ans sending a reply, and acknowledging the
              ;; request so it's properly removed from the queue.
              [_
               (printf "Greeting ~v.\n" body)
               ;; This sends the reply:
               (stomp-send s reply-to (string->bytes/utf-8 (format "Hello, ~a!" body)))
               ;; This acknowledges the request:
               (stomp-ack-message s request-message)])])
         (loop)]))))
