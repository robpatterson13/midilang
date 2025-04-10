#lang racket

(provide (all-defined-out))

(define microseconds-per-minute 60000000)

(define ticks-per-quarter-note 96)

(define quarter-notes-per-whole-note 4)

(define ticks-per-whole-note (* ticks-per-quarter-note quarter-notes-per-whole-note))