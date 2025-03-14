#lang racket

(require "notes.rkt" (for-syntax "preds.rkt" "structs.rkt" "notes.rkt" syntax/parse))

(define-syntax together
  (lambda (stx)
    (syntax-parse stx
      [(_ notes/events ...+)
       (define/syntax-parse (expanded-notes/events ...) (local-expand #'(notes/events ...) 'expression #f))
       (define/syntax-parse (compiled ...)
         (compile-notes/events (syntax->list #'(expanded-notes/events ...))))
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
           #`#,(map check-midi-pitch! note/event-datum)]
          [(midi-pitch? note/event-datum)
           #`#,(make-mtrk-event 0 (make-note-on-event note/event-datum 127 0))]
          [(midi-event? note/event-datum)
           (make-mtrk-event 0 note/event-datum)]
          [else (raise-syntax-error #f "unsupported type" note/event)])))

(together (notes 'C 'D 'E))

(sheet-music
 (B4 1/3) (A4 1/4) (G4 1/4) (A4 1/4) || (B4 1/3) (A4 1/4) (G4 1/4) (A4 1/4)] [(B4 1/3) (A4 1/4) (G4 1/4) (A4 1/4)] [(B4 1/3) (A4 1/4) (G4 1/4) (A4 1/4)]))


(music (list (measure (list (14 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4)))))

#||(measure B4(1/4) A4(1/4) G4(1/4) A4(1/4) | B4(1/4) B4(1/4) B4(1/2) |
| A4(1/4) A4(1/4) A4(1/2) | B4(1/4) D5(1/4) D5(1/2) |
| B4(1/4) A4(1/4) G4(1/4) A4(1/4) | B4(1/4) B4(1/4) B4(1/4) B4(1/4) |
| A4(1/4) A4(1/4) B4(1/4) A4(1/4) | G4(1) || |#

#|
map: arity mismatch;
 the expected number of arguments does not match the given number
  expected: at least 2
  given: 0
|#