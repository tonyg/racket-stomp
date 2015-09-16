#lang racket/base

(module+ test
  (require rackunit)
  (require "functional-queue.rkt")

  (define-syntax check-multiple-equal?
    (syntax-rules ()
      ((_ actual expected)
       (check-equal? (call-with-values (lambda () actual) list)
                     (call-with-values (lambda () expected) list)))))

  (check-multiple-equal? (dequeue (enqueue (make-queue) 1))
                         (values 1 (make-queue)))

  (check-multiple-equal? (queue->list (enqueue (enqueue (make-queue) 1) 2))
                         (list 1 2))

  (check-multiple-equal? (queue->list (enqueue-all (make-queue) '(1 2)))
                         (list 1 2))

  (check-multiple-equal? (queue->list (list->queue '(1 2)))
                         (list 1 2))

  (check-multiple-equal? (queue->list (list->queue '()))
                         (list))

  (check-multiple-equal? (queue-empty? (list->queue '(1 2)))
                         #f)

  (check-multiple-equal? (queue-empty? (list->queue '()))
                         #t)

  (check-multiple-equal? (queue-empty? (enqueue (make-queue) 1))
                         #f)

  (check-multiple-equal? (queue-empty? (make-queue))
                         #t)

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract (list->queue '(5 1 2 3 4)) even?)))
                           (values v (queue->list remainder)))
                         (values 2 '(5 1 3 4)))

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract (list->queue '(5 1 3 7 9)) even?)))
                           (values v (queue->list remainder)))
                         (values #f '(5 1 3 7 9)))

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract (list->queue '(5 1 3 7 9)) (lambda (v) #t))))
                           (values v (queue->list remainder)))
                         (values 5 '(1 3 7 9)))

  (check-equal? (queue->list (queue-append (list->queue '(1 2)) (make-queue)))
                '(1 2))

  (check-equal? (queue->list (queue-append (list->queue '(1 2)) (list->queue '(3 4))))
                '(1 2 3 4))

  (check-equal? (queue->list (queue-append (make-queue) (list->queue '(3 4))))
                '(3 4))

  (define split-queue (enqueue-all (list->queue '(1 2 3 4 5)) '(6 7 8 9 10)))

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract split-queue (lambda (v) (= v 333)))))
                           (values v (queue->list remainder)))
                         (values #f '(1 2 3 4 5 6 7 8 9 10)))

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract split-queue (lambda (v) (= v 3)))))
                           (values v (queue->list remainder)))
                         (values 3 '(1 2 4 5 6 7 8 9 10)))

  (check-multiple-equal? (let-values (((v remainder)
                                       (queue-extract split-queue (lambda (v) (= v 8)))))
                           (values v (queue->list remainder)))
                         (values 8 '(1 2 3 4 5 6 7 9 10)))
  )
