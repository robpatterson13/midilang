#lang racket

(require "notes.rkt" (for-syntax "preds.rkt" "structs.rkt" syntax/parse))

(define-syntax together
  (lambda (stx)
    (syntax-parse stx
      [(_ notes/events ...+)
       (define/syntax-parse (compiled ...)
         (compile-notes/events (syntax->list #'(notes/events ...))))
       #'(list compiled ...)])))

(begin-for-syntax
  (define (compile-notes/events notes/events)
    (define mapped (map compile-note/event notes/events))
    (for/fold ([result '()])
              ([element mapped])
              (if (list? element)
                  (append result element)
                  (append result (list element)))))

  (define (compile-note/event note/event)
    (define note/event-datum (syntax->datum note/event))
    (cond [(list? note/event-datum)
           (map check-midi-pitch! note/event-datum)]
          [(midi-pitch? note/event-datum)
           (make-mtrk-event 0 (make-note-on-event note/event-datum 127 0))]
          [(midi-event? note/event-datum)
           (make-mtrk-event 0 note/event-datum)]
          [else (raise-syntax-error #f "unsupported type" note/event)])))


#|
map: arity mismatch;
 the expected number of arguments does not match the given number
  expected: at least 2
  given: 0
|#