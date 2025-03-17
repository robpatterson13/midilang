#lang racket

(require (for-syntax syntax/parse) "structs.rkt" "notes.rkt")

#;(music (4 4) 120 ((measure (list (14 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4)))))

#;(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer) tempo:exact-positive-integer (measure ...+))
       ])))

(define ticks-per-quarter-note 96)

;; duration-to-tick : Rational -> Integer
(define (duration-to-tick duration)
  (floor (* duration ticks-per-quarter-note 4)))

;; duration and time-since-start-of-measure are MIDI ticks
(struct played-note [pitch duration velocity channel ticks-since-start-of-measure starts-in-measure? ends-in-measure?] #:transparent)

;; Creates a measure of the given duration from the given notes played
;; create-measure : ExactPositiveInteger PlayedNote ... -> Measure
(define (create-measure expected-duration . notes)
  (let ([table (apply create-measure-table notes)]
        [current-duration 0])
    (apply measure
           (append (list (make-mtrk-event 0 (make-text-event "Start of measure")))
                   (for/fold ([result '()])
                             ([tick (in-range (add1 expected-duration))])
                     (let ([ref (hash-ref table tick '())])
                       (if (null? ref)
                           result
                           (append result
                                   (for/list ([event ref])
                                     (begin0
                                       (make-mtrk-event (- tick current-duration)
                                                        event)
                                       (set! current-duration tick)))))))
                   (list (make-mtrk-event (- expected-duration current-duration)
                                          (make-text-event "End of measure")))))))

;; Creates a table mapping ticks to the events happening on those ticks.
;; create-measure-table : PlayedNote ... -> (HashOf Natural MidiEvent)
(define (create-measure-table . notes)
  (for/foldr ([table (hash)])
    ([note notes])
    (let* ([ticks-since-start (played-note-ticks-since-start-of-measure note)]
           [current-val (hash-ref table ticks-since-start '())]
           [ticks-to-end (+ ticks-since-start (played-note-duration note))]
           [end-val (hash-ref table ticks-to-end '())])
      (hash-set
       (hash-set
        table
        ticks-since-start
        (if (played-note-starts-in-measure? note)
            (cons (played-note->note-on-event note)
                  current-val)
            current-val))
       ticks-to-end
       (if (played-note-ends-in-measure? note)
           (cons (played-note->note-off-event note) end-val)
           end-val)))))

(define (played-note->note-on-event the-note)
  (let ([pitch (played-note-pitch the-note)]
        [velocity (played-note-velocity the-note)]
        [channel (played-note-channel the-note)])
    (make-note-on-event pitch
                        velocity
                        channel)))

(define (played-note->note-off-event the-note)
  (make-note-off-event (played-note-pitch the-note)
                       (played-note-velocity the-note)
                       (played-note-channel the-note)))

(module+ test
  (require rackunit)

  (check-equal? (played-note->note-on-event
                 (played-note 60 96 127 0 0 #t #t))
                (make-note-on-event 60 127 0))
  
  (check-equal? (played-note->note-off-event
                 (played-note 60 96 127 0 0 #t #t))
                (make-note-off-event 60 127 0))

  (check-equal? (create-measure-table (played-note 60 96 127 0 0 #t #t)
                                      (played-note 64 96 127 0 0 #t #t)
                                      (played-note 67 96 127 0 0 #t #t))
                (hash 0 (list (make-note-on-event 60 127 0)
                              (make-note-on-event 64 127 0)
                              (make-note-on-event 67 127 0))
                      96 (list (make-note-off-event 60 127 0)
                               (make-note-off-event 64 127 0)
                               (make-note-off-event 67 127 0))))

  (check-equal? (create-measure 96
                                (played-note 60 96 127 0 0 #t #t)
                                (played-note 64 96 127 0 0 #t #t)
                                (played-note 67 96 127 0 0 #t #t))
                (measure (make-mtrk-event 0 (make-text-event "Start of measure"))
                         (make-mtrk-event 0 (make-note-on-event 60 127 0))
                         (make-mtrk-event 0 (make-note-on-event 64 127 0))
                         (make-mtrk-event 0 (make-note-on-event 67 127 0))
                         (make-mtrk-event 96 (make-note-off-event 60 127 0))
                         (make-mtrk-event 0 (make-note-off-event 64 127 0))
                         (make-mtrk-event 0 (make-note-off-event 67 127 0))
                         (make-mtrk-event 0 (make-text-event "End of measure")))))