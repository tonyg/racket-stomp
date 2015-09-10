#lang setup/infotab
(define name "STOMP")
(define blurb
  (list
   `(p "A "(a ((href "http://stomp.github.com/index.html")) "STOMP")" codec and client")))
(define release-notes
  (list
   `(ul (li "Initial release"))))
(define categories '(net))
(define can-be-loaded-with 'all)
(define homepage "https://github.com/tonyg/racket-stomp")
(define primary-file "main.rkt")
(define repositories '("4.x"))
(define scribblings '(("stomp.scrbl" ())))
