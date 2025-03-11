#lang racket

(provide midi-pitch? velocity? channel?)

;; Determines whether the given value is a Natural between 0 and
;; 127 (inclusive).
;; nat<=127?: Any -> Boolean
(define (nat<=127? val)
  (and (natural? val) (<= val 127)))

;; Determines whether the given value is a Natural between 0 and
;; 15 (inclusive).
;; nat<=15?: Any -> Boolean
(define (nat<=15? val)
  (and (natural? val) (<= val 15)))

;; A MidiPitch is a Natural between 0 and 127 (inclusive).
(define midi-pitch? nat<=127?)

;; A Velocity is a Natural between 0 and 127 (inclusive).
(define velocity? nat<=127?)

;; A Channel is a Natural between 0 and 15 (inclusive).
(define channel? nat<=15?)

(module+ test
  (require rackunit)
  
  (check-true (midi-pitch? 0))
  (check-true (midi-pitch? 127))
  (check-false (midi-pitch? -1))
  (check-false (midi-pitch? 128))
  (check-false (midi-pitch? 3/2))

  (check-true (velocity? 0))
  (check-true (velocity? 127))
  (check-false (velocity? -1))
  (check-false (velocity? 128))
  (check-false (velocity? 3/2))

  (check-true (channel? 0))
  (check-true (channel? 15))
  (check-false (channel? -1))
  (check-false (channel? 16))
  (check-false (channel? 3/2)))