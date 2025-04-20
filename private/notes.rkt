#lang racket

(require syntax/macro-testing (for-syntax syntax/parse "preds.rkt"))

(provide note)

;; A NoteSymbol is one of the following:
;; - 'C
;; - 'C#
;; - 'Db
;; - 'D
;; ...
;; - 'E
;; - 'F
;; - 'F#
;; - 'Gb
;; ...
;; - 'B

;; Converts the given note to a MidiPitch.
;; Grammar:
;; (note <NoteSymbol>) OR
;; (note <NotesSymbol> <ExactInteger>)
(define-syntax note
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum quote) the-note:id))
       #`#,(make-note (syntax->datum #''the-note) 4)]
      [(_ ((~datum quote) the-note:id) octave:exact-integer)
       #`#,(make-note (syntax->datum #''the-note) (syntax->datum #'octave))])))

(begin-for-syntax
  ;; Generates a MidiPitch used in Midi format to represent the given
  ;; note in the given octave. Values range from 0 (C(-1)) to 127 (G9).
  ;; make-note: NoteSymbol ExactInteger -> MidiPitch
  (define (make-note note-sym octave)
    (define note-pitch
      (case note-sym
        [('C) 0]
        [('C# 'Db) 1]
        [('D) 2]
        [('D# 'Eb) 3]
        [('E) 4]
        [('F) 5]
        [('F# 'Gb) 6]
        [('G) 7]
        [('G# 'Ab) 8]
        [('A) 9]
        [('A# 'Bb) 10]
        [('B) 11]
        [else (raise-syntax-error 'note "Invalid symbol" note-sym)]))
    (define result (+ (* (+ 1 octave) 12) note-pitch))
    (if (midi-pitch? result)
        result
        (raise-syntax-error 'note "Pitch must range between C(-1) and G9"))))

(module+ test
  (require rackunit)
  
  (check-equal? (note 'C -1) 0)
  (check-equal? (note 'C) 60)
  (check-equal? (note 'G 9) 127)
  (check-exn exn:fail?
             (lambda () (convert-compile-time-error (note 'H))))
  (check-exn exn:fail?
             (lambda () (convert-compile-time-error (note 'B -2))))
  (check-exn exn:fail?
             (lambda () (convert-compile-time-error (note 'G# 9)))))


