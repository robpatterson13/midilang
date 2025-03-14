#lang racket

(require (for-syntax syntax/parse) "structs.rkt" "notes.rkt")

#;(music (4 4) 120 ((measure (list (14 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4)))))

#;(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer) tempo:exact-positive-integer (measure ...+))
       ])))

(define ticks-per-quarter-note 96)

(struct played-note [pitch duration time-since-last-note spans-measures?] #:transparent)

;; duration-to-tick : Rational -> Integer
(define (duration-to-tick duration)
  (floor (* duration ticks-per-quarter-note 4)))

;; create-measure : ExactPositiveInteger PlayedNote -> Measure
(define (create-measure expected-duration . notes/rests)
  (define pitches (map played-note-pitch notes/rests))
  (define ticks (map duration-to-tick (map played-note-duration notes/rests)))
  (define time-since-last-note (map played-note-time-since-last-note notes/rests))
  (define spans-measures? (map played-note-spans-measures? notes/rests))
  (define current-duration 0)
  )