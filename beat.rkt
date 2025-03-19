#lang racket

(provide beat time-signature?)

;; A TimeSignature is a (time-signature beats note), where beats
;; refers to the number of beats in a measure, and note refers to
;; the type of note (4 = quarter note, 8 = eighth note, etc.) that
;; corresponds to the note assigned to one beat.
(struct time-signature [beats note] #:transparent)
;; Creates a time signature of the given beats in each measure and
;; the type of note (4 = quarter note, 8 = eighth note, etc.) that
;; corresponds to the note assigned to one beat.
;; beat: ExactPositiveInteger ExactPositiveInteger -> TimeSignature
(define (beat beats (note 4))
  (if (and (exact-positive-integer? beats) (exact-positive-integer? note))
      (time-signature beats note)
      (error 'beat "Beat must consist of two positive integers; given ~a and ~a" beats note)))

(module+ test
  (require rackunit)
  
  (check-equal? (beat 4)
                (time-signature 4 4))
  (check-equal? (beat 9 8)
                (time-signature 9 8))
  (check-exn exn:fail?
             (lambda () (beat 3/2 4)))
  (check-exn exn:fail?
             (lambda () (beat 3 3/2))))