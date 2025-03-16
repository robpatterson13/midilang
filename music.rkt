#lang racket

(require (for-syntax syntax/parse) "structs.rkt" "notes.rkt")

#;(music (4 4) 120 ((measure (list (14 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4)))))

#;(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer) tempo:exact-positive-integer (measure ...+))
       ])))

(define ticks-per-quarter-note 96)

;; duration and time-since-start-of-measure are MIDI ticks
(struct played-note [pitch duration velocity channel ticks-since-start-of-measure starts-in-measure? ends-in-measure?] #:transparent)

;; duration-to-tick : Rational -> Integer
(define (duration-to-tick duration)
  (floor (* duration ticks-per-quarter-note 4)))

;; create-measure : ExactPositiveInteger PlayedNote ... -> Measure
(define (create-measure expected-duration . notes)
  (apply measure
         (for/fold ([table (hash)])
                   ([note notes])
           (let ([ticks-since-start (played-note-ticks-since-start-of-measure note)])
             (hash-set table
                     ticks-since-start
                     (append (hash-ref table ticks-since-start '())
                             (played-note->midi-events the-note)))))))

(define (played-note->midi-events the-note)
  (let ([pitch (played-note-pitch the-note)]
        [velocity (played-note-velocity the-note)]
        [channel (played-note-channel the-note)]))
  (append (if (played-note-starts-in-measure? the-note)
              (list (make-note-on-event pitch
                                        velocity
                                        channel))
              '())
          (if (played-note-ends-in-measure? the-note)
              (list (make-note-off-event pitch
                                         velocity
                                         channel))
              '())))