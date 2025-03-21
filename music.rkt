#lang racket

(require (for-syntax syntax/parse) "beat.rkt" "structs.rkt" "notes.rkt")
(provide music)

;; A PlayedNote is a (played-note pitch duration velocity channel
;; ticks-since-last-note starts-in-measure? ends-in-measure?), with the
;; corresponding pitch, duration, velocity, and channel of the note, the
;; ticks since the last note played, and whether the note starts and ends
;; in the current measure. duration and ticks-since-last-note are in MIDI
;; ticks.
(struct played-note [pitch duration velocity channel ticks-since-last-note starts-in-measure? ends-in-measure?] #:transparent)

;; played-note->note-on-event: PlayedNote -> NoteOnEvent
(define (played-note->note-on-event the-note)
  (make-note-on-event (played-note-pitch the-note)
                      (played-note-velocity the-note)
                      (played-note-channel the-note)))

;; played-note->note-off-event: PlayedNote -> NoteOffEvent
(define (played-note->note-off-event the-note)
  (make-note-off-event (played-note-pitch the-note)
                       (played-note-velocity the-note)
                       (played-note-channel the-note)))

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
  (define ticks-so-far 0)
  (for/fold ([table (hash)])
            ([note notes])
    (let* ([ticks-since-last-event (played-note-ticks-since-last-note note)]
           [new-tick (+ ticks-so-far ticks-since-last-event)]
           [current-val (hash-ref table new-tick '())]
           [ticks-to-end (+ new-tick (played-note-duration note))]
           [end-val (hash-ref table ticks-to-end '())])
      (begin0
        (hash-set
         (hash-set
          table
          new-tick
          (if (played-note-starts-in-measure? note)
              (append current-val
                      (list  (played-note->note-on-event note)))
              current-val))
         ticks-to-end
         (if (played-note-ends-in-measure? note)
             (append end-val
                     (list (played-note->note-off-event note)))
             end-val))
        (set! ticks-so-far new-tick)))))

;; Converts the given list of PlayedNotes to a measure
;; list->measure: (ListOf PlayedNote) -> Measure
(define (list->measure lst expected-duration)
  (apply create-measure expected-duration lst))

(begin-for-syntax
  ;; Converts the given measures (as represented in music) to a list containing
  ;; lists of PlayedNotes. Those lists represent the measures in the song.
  (define (compile-measures measures)
    (syntax-parse measures
      [(((~datum measure) note/grouped-notes ...) ...)
       (define/syntax-parse (compiled ...)
         (map compile-measure (syntax->list #'((note/grouped-notes ...) ...))))
       #'(list compiled ...)]))

  ;; Converts the given measure (as represented in music) to a list of
  ;; PlayedNotes that represents a measure in the song.
  (define (compile-measure measure)
    (syntax-parse measure
      [(notes/grouped-notes ...)
       (define time-since-last-event 0)
       (define/syntax-parse (compiled-notes/grouped-notes ...)
         (for/fold ([notes #''()])
                   ([note/grouped-notes
                     (syntax->list #'(notes/grouped-notes ...))])
           (define compiled
             (compile-note/grouped-notes
              note/grouped-notes
              time-since-last-event))
           (begin0 #`(append #,notes #,compiled)
                   (set! time-since-last-event
                         (duration-to-tick
                          (syntax->datum
                           (compile-duration note/grouped-notes)))))))
       #'(compiled-notes/grouped-notes ...)]))

  ;; Converts the note or group of notes into a list of PlayedNotes
  ;; representing the notes played at that moment.
  (define (compile-note/grouped-notes note/grouped-notes time-since-last-event)
    (syntax-parse note/grouped-notes
      [((~datum rest) duration:number)
       (compile-note/rest #'(rest duration) time-since-last-event)]
      [(((~datum quote) pitch:id) duration:number)
       (compile-note/rest #'('pitch 4 duration) time-since-last-event)]
      [(((~datum quote) pitch:id) octave:exact-integer duration:number)
       (compile-note/rest #'('pitch octave duration) time-since-last-event)]
      [(midi-pitch:nat duration:number)
       (compile-note/rest #'(midi-pitch duration) time-since-last-event)]
      [(note-stuff:expr ... duration:number)
       (define note-list (syntax->list #'(note-stuff ...)))
       (define/syntax-parse (compiled-notes ...)
         (map compile-note/rest
              note-list
              (build-list (length note-list)
                          (lambda (i)
                            (if (= i 0)
                                time-since-last-event
                                0)))))
       #'(append compiled-notes ...)]))

  ;; Converts the note or rest to a list containing the corresponding
  ;; PlayedNote.
  (define (compile-note/rest note/rest time-since-last-event)
    (syntax-parse note/rest
      [((~datum rest) duration:number)
       (define duration-val (syntax->datum #'duration))
       (if (and (rational? duration-val)
                (> duration-val 0))
           #''()
           (raise-syntax-error #f "must provide a positive rational duration"))]
      [(((~datum quote) pitch:id) octave:exact-integer duration:number)
       (define duration-val (syntax->datum #'duration))
       (if (and (rational? duration-val)
                (> duration-val 0))
           #`(list
              (played-note (note 'pitch octave)
                           #,(duration-to-tick duration-val)
                           127
                           0
                           #,time-since-last-event
                           #t
                           #t)) ; change constants to syntax parameters
           (raise-syntax-error #f "must provide a positive rational duration"))]
      [(midi-pitch:nat duration:number)
       (define duration-val (syntax->datum #'duration))
       (if (and (rational? duration-val)
                (> duration-val 0))
           #`(list
              (played-note midi-pitch
                           #,(duration-to-tick duration-val)
                           127
                           0
                           #,time-since-last-event
                           #t
                           #t)) ; change constants to syntax parameters
           (raise-syntax-error #f "must provide a positive rational duration"))]))

  ;; Extracts the duration of the given note, rest, or group of notes.
  (define (compile-duration note/grouped-notes)
    (syntax-parse note/grouped-notes
      [(contents:expr ... duration:number)
       #'duration]))

  ;; TODO: calculate from tempo
  (define ticks-per-quarter-note 96)

  ;; duration-to-tick : Rational -> Integer
  (define (duration-to-tick duration)
    (floor (* duration ticks-per-quarter-note 4)))) ; 4 quarter notes to a whole note

;; Converts pseudo-musical notation into a format useful for writing MIDI.
;; Grammar:
;; (music (<ExactPositiveInteger> <ExactPositiveInteger>) <ExactPositiveInteger>
;;        (<measure> ...+))
;;
;; <measure> ::= (measure <note/grouped-notes/rest> ...)
;;
;; <note/grouped-notes/rest> ::= <note/rest>
;;                             | (<note/rest> ...)
;;
;; <note/rest> ::= (rest <Rational>)
;;               | (<NoteSymbol> <ExactInteger> <Rational>)
;;               | (<NoteSymbol> <Rational>)
(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer)
          tempo:exact-positive-integer
          (measure:expr ...+))
       (define compiled-measures
         (compile-measures #'(measure ...)))
       #`(song (header 0 (beat beat/measure duration) tempo)
               (list (apply track
                            (map list->measure
                                 #,compiled-measures
                                 (build-list
                                  (length #,compiled-measures)
                                  (lambda (i) 384))))))]))) ; compute value from time signature and tempo

;; EXAMPLE:
(music (4 4) 120 ((measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4)) (measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4)) (measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4))))


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
                         (make-mtrk-event 0 (make-text-event "End of measure"))))

  (check-equal? (create-measure 384
                                (played-note 60 96 127 0 0 #t #t)
                                (played-note 62 96 127 0 96 #t #t)
                                (played-note 64 96 127 0 96 #t #t))
                (measure (make-mtrk-event 0 (make-text-event "Start of measure"))
                         (make-mtrk-event 0 (make-note-on-event 60 127 0))
                         (make-mtrk-event 96 (make-note-off-event 60 127 0))
                         (make-mtrk-event 0 (make-note-on-event 62 127 0))
                         (make-mtrk-event 96 (make-note-off-event 62 127 0))
                         (make-mtrk-event 0 (make-note-on-event 64 127 0))
                         (make-mtrk-event 96 (make-note-off-event 64 127 0))
                         (make-mtrk-event 96 (make-text-event "End of measure"))))

  (check-equal? (create-measure 384
                                (played-note 60 96 127 0 0 #t #t)
                                (played-note 64 96 127 0 192 #t #t))
                (measure (make-mtrk-event 0 (make-text-event "Start of measure"))
                         (make-mtrk-event 0 (make-note-on-event 60 127 0))
                         (make-mtrk-event 96 (make-note-off-event 60 127 0))
                         (make-mtrk-event 96 (make-note-on-event 64 127 0))
                         (make-mtrk-event 96 (make-note-off-event 64 127 0))
                         (make-mtrk-event 96 (make-text-event "End of measure")))))