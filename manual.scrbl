#lang scribble/manual

@(require scribble/racket
	  (for-label racket
		     "functional-queue.rkt"
		     "main.rkt"))

@title{STOMP}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@local-table-of-contents[]

If you find that this library lacks some feature you need, or you have
a suggestion for improving it, please don't hesitate to
@link["mailto:tonygarnockjones@gmail.com"]{get in touch with me}!

@section{Introduction}

This library implements the
@link["http://stomp.github.com/index.html"]{STOMP} protocol, both the
codec for STOMP frames and the session protocol used by clients.

The
@link["http://stomp.github.com/stomp-specification-1.1.html"]{STOMP
specification} includes a lot of important information that's needed
to make sense of the definitions below, including such things as

@itemize[
  @item{what a "destination" is}
  @item{how to correctly acknowledge messages received from the server}
  @item{how to arrange for acknowledgements-of-receipt from the server}
  @item{how to manage transactions}
]

@subsection{Destinations}

When using @link["http://www.rabbitmq.com/"]{RabbitMQ} with its
@link["http://www.rabbitmq.com/stomp.html"]{STOMP plugin} as your
STOMP server, the available destinations are

@itemize[

  @item{@racket[(string-append "/queue/" _queue-name)] --- When used
  with @racket[stomp-send], delivers a message directly to the named
  queue. When used with @racket[stomp-subscribe], consumes from the
  named queue (at which point it's interesting to think about which
  @racket[ack-mode] you want to use).}

  @item{@racket[(string-append "/exchange/" _exchange-name "/" _routing-key)]
  --- When used with @racket[stomp-send], delivers a
  message via the named exchange, using the given
  @racket[_routing-key]. When used with @racket[stomp-subscribe], creates
  an anonymous queue, binds it to the named exchange using
  @racket[_routing-key] as a binding pattern, and starts consuming from
  the anonymous queue.}

]

@link["http://activemq.apache.org/"]{ActiveMQ} and other STOMP message
brokers have different destination name schemata and routing
behaviours.

@section{Examples}

In the examples below, I'll make use of the
@link["http://www.rabbitmq.com/examples.html#demo-server"]{RabbitMQ
demonstration broker service}.

@subsection{Sending a message}

@racketblock[
	     (require (planet tonyg/stomp))
	     (define s (stomp-connect "dev.rabbitmq.com" "guest" "guest" "/"))
	     (stomp-send s "/exchange/amq.rabbitmq.log/info"
			 (string->bytes/utf-8 "Hello world, from Racket!"))
	     (stomp-disconnect s)
]

@subsection{Sending a message, with a receipt}

@racketblock[
	     (require (planet tonyg/stomp))
	     (define s (stomp-connect "dev.rabbitmq.com" "guest" "guest" "/"))
	     (call-with-receipt s
	      (lambda (receipt)
		(stomp-send s "/exchange/amq.rabbitmq.log/info"
			    (string->bytes/utf-8 "Hello world, from Racket!")
			    `((receipt ,receipt)))))
	     (code:comment @#,t{At this point, we know the server has received our})
	     (code:comment @#,t{SEND, because we have received its RECEIPT frame. See})
	     (code:comment @#,t{the STOMP specification for details of what exact})
	     (code:comment @#,t{implications this has for whether the server has})
	     (code:comment @#,t{processed the SEND or not.})
	     (stomp-disconnect s)
]

@subsection{Subscribing to an exchange}

This example uses RabbitMQ's AMQP "wildcards" to subscribe to all
messages travelling through the "amq.rabbitmq.log" exchange.

@racketblock[
	   (require (planet tonyg/stomp))
	   (define s (stomp-connect "dev.rabbitmq.com" "guest" "guest" "/"))
	   (stomp-subscribe s "/exchange/amq.rabbitmq.log/#" "my-subscription")
	   (let loop ()
	     (let ((m (stomp-next-message s "my-subscription")))
	       (pretty-print m)
	       (loop)))
]

@section{API}

@defstruct*[(exn:stomp exn:fail) ([frame stomp-frame?])
	    #:transparent]{

Represents a STOMP error. The @racket[frame] is either an ERROR frame
received from the server, or a frame used locally that is problematic
in some way. }

@defstruct*[stomp-frame ([command string?]
			 [headers (listof (list symbol? string?))]
			 [body (or bytes? #f)])
			#:transparent]{

Represents a STOMP frame, with its three parts:

@itemize[

  @item{@racket[command] --- The STOMP command part of the frame. For
  frames received from the server, this will usually be
  @racket["MESSAGE"]. Frames sent to the server are usually constructed
  using the procedures described below (in particular
  @racket[stomp-send]).}

  @item{@racket[headers] --- The STOMP headers sent or received with
  the frame. See the
  @link["http://stomp.github.com/stomp-specification-1.1.html"]{STOMP
  specification} for details of the meaning of various headers.}

  @item{@racket[body] --- The body sent or received with the
  frame. This is entirely application-specific: STOMP makes no
  restrictions on the length or format of the body part of a frame. Note
  that it is a byte vector, however: make sure to use
  @racket[string->bytes/utf-8] and @racket[bytes->string/utf-8] as
  appropriate.}

] }

@defproc[(stomp-frame-header [frame stomp-frame?]
			     [header symbol?]
			     [default-value any? #f])
	 any?]{

Convenience function for extracting a single header from a STOMP
frame. If a header named @racket[header] is present, its (string)
value is returned; otherwise, @racket[default-value] is returned.
}

@defproc[(stomp-connect [hostname string?]
			[login (or string? #f) #f]
			[passcode (or string? #f) #f]
			[virtual-host string? hostname]
			[port-number (and/c exact-nonnegative-integer?
					    (integer-in 0 65535)) 61613])
	 stomp-session?]{

Opens a STOMP connection and session to the given @racket[hostname] at
TCP port @racket[port-number]. If @racket[login] and/or
@racket[passcode] are supplied, they are sent during connection
negotiation. (Note: they are sent in the clear!) If
@racket[virtual-host] is not specified, the value of @racket[hostname]
is used. Note that @link["http://www.rabbitmq.com/"]{RabbitMQ}'s
default virtual host is @racket["/"], so to make a connection to the
demo STOMP server hosted at dev.rabbitmq.com, you would use:

@racketinput[(stomp-connect "dev.rabbitmq.com" "guest" "guest" "/")]

}

@defproc[(stomp-disconnect [session stomp-session?]) void?]{

Cleanly disconnects a STOMP session, sending a DISCONNECT frame and
waiting for receipt of an acknowledgement before closing the session's
underlying socket. }

@defproc[(stomp-disconnect/abrupt [session stomp-session?]) void?]{

Abruptly disconnects a STOMP session, closing the socket without
sending a DISCONNECT (or any other) frame. }

@defproc[(stomp-flush [session stomp-session?]) void?]{

Outbound frames will frequently be buffered to improve
performance. Automatic buffer flushes happen every time a frame is
read from the connection, so in many cases manual flushing is not
required, but every now and then (especially during development and
testing) it can be important to force all queued frames to be sent
down the socket. This is what @racket[stomp-flush] is for. }

@defproc[(stomp-message-id [frame stomp-frame?]) (or string? #f)]{

Extracts the @racket[message-id] header from the given frame (usually
a MESSAGE frame), if any. Returns @racket[#f] if no such header exists
within the frame. }

@defproc[(wait-for-receipt [session stomp-session?]
			   [receipt string?]) void?]{

Waits for the server to send a RECEIPT frame with a
@racket[receipt-id] header matching the given @racket[receipt]
value. Returns an end-of-file object if the connection was closed
before such a receipt was received. }

@defproc[(call-with-receipt [session stomp-session?]
			    [proc (-> string? any?)]) any?]{

Creates a fresh receipt string (of the form @racket["R1234"]) and
passes it to @racket[proc], which should use it in a @racket[receipt]
header of a frame sent to the server. After @racket[proc] returns,
this function calls @racket[wait-for-receipt] to wait for the server's
acknowledgement. The result of @racket[proc] is finally returned as
the result of the whole call.

The following example demonstrates the use of a receipt request with a
SEND operation:

@racketinput[(call-with-receipt session
	      (lambda (receipt)
		(stomp-send session
			    "/queue/a"
			    (string->bytes/utf-8 "some message body")
			    `((receipt ,receipt)))))]

}

@defproc[(stomp-send-command [session stomp-session?]
			     [command string?]
			     [headers (listof (list symbol? string?)) '()]
			     [body (or bytes? #f) #f])
	 void?]{

Sends a STOMP frame with the given @racket[command], @racket[headers]
and optional @racket[body] to the server. See
@racket[call-with-receipt] for information on how to wait for an
acknowledgement-of-receipt from the server.

Note that this is a low-level way of sending commands to the server:
better to use @racket[stomp-send], @racket[stomp-subscribe],
@racket[stomp-ack-message] etc. }

@defproc[(stomp-next-frame [session stomp-session?]
			   [block? boolean? #t])
	 (or stomp-frame? eof-object? #f)]{

Retrieves the next frame from the server. If no frames are in the
session's buffer of waiting frames already received, returns
@racket[#f] if @racket[block?] is @racket[#f] and waits for a frame to
arrive otherwise. If the connection closes before a frame arrives,
returns an end-of-file object. Otherwise, returns a
@racket[stomp-frame] structure. }

@defproc[(stomp-next-frame/filter [session stomp-session?]
				  [predicate (-> stomp-frame? boolean?)]
				  [block? boolean? #t])
	 (or stomp-frame? eof-object? #f)]{

As @racket[stomp-next-frame], except returns the first frame that
matches @racket[predicate], if any. If no match is found in the buffer
and @racket[block?] is @racket[#f], returns @racket[#f] ; otherwise,
waits for a match to arrive, buffering non-matching frames as it goes.
Never reorders frames in the buffer. }

@defproc[(stomp-next-message [session stomp-session?]
			     [subscription-id string?]
			     [block? boolean? #t])
	 (or stomp-frame? eof-object? #f)]{

Uses @racket[stomp-next-frame/filter] to retrieve the next available
MESSAGE frame that has a @racket[subscription] header matching
@racket[subscription-id]. The @racket[block?] argument acts as for
@racket[stomp-next-frame/filter]. Returns the first matching MESSAGE
frame, end-of-file if the connection closed, or #f if @racket[block?]
was @racket[#f] and no matching MESSAGE was available in the session's
buffer. }

@defproc[(stomp-send [session stomp-session?]
		     [destination string?]
		     [body (or bytes? #f)]
		     [headers (listof (list symbol? string?))'()])
	 void?]{

Sends a SEND frame to the server with the given @racket[destination],
other @racket[headers], and optional @racket[body]. See
@racket[call-with-receipt] for information on getting acknowledgements
back from the server.

This is the procedure you will want to use to actually publish
messages to the STOMP server. }

@defproc[(stomp-send/flush [session stomp-session?]
			   [destination string?]
			   [body (or bytes? #f)]
			   [headers (listof (list symbol? string?))'()])
	 void?]{

Just like @racket[stomp-send], but calls @racket[stomp-flush] as it
returns. }

@defproc[(stomp-subscribe [session stomp-session?]
			  [destination string?]
			  [subscription-id string?]
			  [ack-mode (or 'auto 'client 'client-individual)]
			  [headers (listof (list symbol? string?)) '()])
	 void?]{

Sends a SUBSCRIBE frame to the server. The @racket[destination] is the
name of the message source to subscribe to (called "destination"
because that's how message senders think of it, presumably). The
@racket[subscription-id] is a session-unique string identifying this
subscription. The @racket[subscription-id] will be sent by the server
in MESSAGE frames that result from this SUBSCRIBE operation. The
@racket[ack-mode] parameter must be one of the following:

@itemize[
  @item{@racket['auto] --- The server will not expect any ACK frames
  in response to MESSAGEs it sends.}

  @item{@racket['client] --- The server will expect ACK frames, and
  will interpret an acknowledgement of message ID @racket[m] to mean
  that message @italic{and all preceding messages}.}

  @item{@racket['client-individual] --- The server will expect ACK
  frames, but will interpret each such frame as acknowledging only the
  message ID mentioned within it.}
]

Proceeds without waiting for a reply. To wait for a reply, supply a @racket[receipt]
header; see @racket[call-with-receipt].

}

@defproc[(stomp-ack [session stomp-session?]
		    [subscription-id string?]
		    [message-id string?]
		    [headers (listof (list symbol? string?)) '()])
	 void?]{

Sends an ACK frame in response to some previous MESSAGE received from
the server. The @racket[subscription-id] and @racket[message-id]
should match the @racket[subscription] and @racket[message-id] headers
from the MESSAGE being responded to, respectively.

Use this procedure or @racket[stomp-ack-message] to acknowledge
messages received via a call to @racket[stomp-subscribe] where
@racket[ack-mode] was either @racket['client] or
@racket['client-individual]. }

@defproc[(stomp-ack-message [session stomp-session?]
			    [message stomp-frame?]
			    [headers (listof (list symbol? string?)) '()])
	 void?]{

Convenience function that extracts the @racket[subscription] and
@racket[message-id] headers from the @racket[message] and delegates to
@racket[stomp-ack]. }

@defproc[(stomp-nack [session stomp-session?]
		     [subscription-id string?]
		     [message-id string?]
		     [headers (listof (list symbol? string?)) '()])
	 void?]{

Sends a NACK frame to the server. See the
@link["http://stomp.github.com/stomp-specification-1.1.html#NACK"]{STOMP
specification of NACK} for more information. Use with caution! }

@defproc[(stomp-begin [session stomp-session?]
		      [transaction string?]
		      [headers (listof (list symbol? string?)) '()])
	 void?]{}
@defproc[(stomp-commit [session stomp-session?]
		       [transaction string?]
		       [headers (listof (list symbol? string?)) '()])
	 void?]{}
@defproc[(stomp-abort [session stomp-session?]
		      [transaction string?]
		      [headers (listof (list symbol? string?)) '()])
	 void?]{}

Start, commit, or abort a transaction, respectively. Transaction names
are managed by the client. See
@link["http://stomp.github.com/stomp-specification-1.1.html#BEGIN"]{the
STOMP specification for BEGIN, COMMIT and ABORT} for more information
on how transaction names are used.

The procedure @racket[call-with-stomp-transaction] abstracts away from
some of the detail of managing transactions for you.

@defproc[(call-with-stomp-transaction [session stomp-session?]
				      [proc (-> string? any)])
	 any?]{

Creates a fresh transaction name (of the form @racket["Tx1234"]),
sends a BEGIN frame, calls @racket[proc] with the transaction name as
its argument, and sends a COMMIT frame. If @racket[proc] terminates
normally, the result of @racket[proc] becomes the result of the whole
call. If it terminates with an exception, an ABORT frame is sent to
the server before the exception propagates out of the call to
@racket[call-with-stomp-transaction]. }

@defstruct*[stomp-session ([input input-port?]
			   [output output-port?]
			   [id (or string? #f)]
			   [server-info (or string? #f)]
			   [buffer queue?])
			  #:transparent]{

Represents a STOMP client session. The @racket[input] and
@racket[output] represent the socket connection to the server. The
@racket[id] is the session ID, as decided by the server. The
@racket[server-info] is ad-hoc server information, if any was sent
during connection setup. The @racket[buffer] is a queue of received
frames that have not yet been processed. }
