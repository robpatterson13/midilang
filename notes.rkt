#lang racket

(require syntax/macro-testing (for-syntax syntax/parse "preds.rkt" racket/format))

(provide note notes (for-syntax check-midi-pitch! compile-note))

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


;; Produces the values of the notes represented by the given syntax.
;; Grammar:
;; (notes note-val note-val ...+)
;; where note-val is one of the following:
;; note-val ::= Natural
;;            | NoteSymbol
;;            | (NoteSymbol)
;;            | (NoteSymbol ExactInteger)
(define-syntax notes
  (lambda (stx)
    (syntax-parse stx
      [(_ note1 other-note ...+)
       (define/syntax-parse (compiled ...)
         (compile-notes #'note1 (syntax->list #'(other-note ...))))
       #'(list compiled ...)])))

(begin-for-syntax
  ;; Converts the given syntax objects to MidiPitches.
  ;; compile-notes: Syntax (ListOf Syntax) -> (ListOf MidiPitch)
  (define (compile-notes note1 others)
    (if (null? others)
        (list (compile-note note1))
        (cons (compile-note note1) (compile-notes (car others) (cdr others)))))

  ;; Converts the given syntax object to a MidiPitch.
  ;; compile-note: Syntax -> MidiPitch
  (define (compile-note stx)
    (syntax-parse stx
      [n:nat #`#,(check-midi-pitch! (syntax->datum #'n))]
      [((~datum quote) the-note:id) #'(note 'the-note)]
      [(((~datum quote) the-note:id)) #'(note 'the-note)]
      [(((~datum quote) the-note:id) octave:exact-integer)
       #'(note 'the-note octave)]))

  (define (check-midi-pitch! val)
    (if (midi-pitch? val)
        val
        (raise-syntax-error #f (~a "must provide a valid midi pitch; given " val))))
  )

(module+ test
  (check-equal? (notes 0 1 2)
                (list 0 1 2))
  (check-equal? (notes 'C 'D 'E)
                (list 60 62 64))
  (check-equal? (notes ('C) ('D) ('E))
                (list 60 62 64))
  (check-equal? (notes ('C 4) ('D 4) ('E 4))
                (list 60 62 64))
  (check-exn exn:fail?
             (lambda () (convert-compile-time-error (notes 127 128 127)))))


