#lang racket

(require (for-syntax syntax/parse) "beat.rkt" "structs.rkt" "notes.rkt")
(provide music ticks-per-quarter-note)

;; A PlayedNote is a (played-note pitch duration velocity channel
;; ticks-since-last-note starts-in-measure? ends-in-measure?), with the
;; corresponding pitch, duration, velocity, and channel of the note, the
;; ticks since the last note played, and whether the note starts and ends
;; in the current measure. duration and ticks-since-last-note are in MIDI
;; ticks.
(struct played-note [pitch duration velocity channel ticks-since-last-note starts-in-measure? ends-in-measure?] #:transparent)

(struct rest [duration ticks-since-last-note])

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
;; create-measure : ExactPositiveInteger (U PlayedNote Rest) ... -> Measure
(define (create-measure expected-duration . notes)
  (let ([table (apply create-measure-table notes)])
    (apply measure
           (append (list (make-mtrk-event 0 (make-text-event "Start of measure")))
                   (for/fold ([result '()]
                              [current-duration 0]
                              #:result result)
                             ([tick (in-range (add1 expected-duration))])
                     (let ([ref (hash-ref table tick '())])
                       (values
                        (if (null? ref)
                            result
                            (append result
                                    (for/list ([event ref]
                                               [offset
                                                (build-list
                                                 (length ref)
                                                 (lambda (i)
                                                   (if (= i 0)
                                                       (- tick current-duration)
                                                       0)))])
                                      (make-mtrk-event offset
                                                       event))))
                        (if (null? ref)
                            current-duration
                            tick))))
                     (list (make-mtrk-event 0
                                          (make-text-event "End of measure")))))))

;; Creates a table mapping ticks to the events happening on those ticks.
;; create-measure-table : PlayedNote ... -> (HashOf Natural MidiEvent)
(define (create-measure-table . notes)
  (define (ticks-since-last-note note)
    (if (played-note? note)
        (played-note-ticks-since-last-note note)
        (rest-ticks-since-last-note note)))
  (define (duration note)
    (if (played-note? note)
        (played-note-duration note)
        (rest-duration note)))
  (for/fold ([table (hash)]
             [ticks-so-far 0]
             #:result table)
            ([note notes])
    (let* ([ticks-since-last-event (ticks-since-last-note note)]
           [new-tick (+ ticks-so-far ticks-since-last-event)]
           [current-val (hash-ref table new-tick '())]
           [ticks-to-end (+ new-tick (duration note))]
           [end-val (hash-ref table ticks-to-end '())])
      (values
       (if (played-note? note)
           (hash-set* table
                      new-tick
                      (if (played-note-starts-in-measure? note)
                          (append current-val
                                  (list (played-note->note-on-event note)))
                          current-val)
                      ticks-to-end
                      (if (played-note-ends-in-measure? note)
                          (append end-val
                                  (list (played-note->note-off-event note)))
                          end-val))
           table)
       new-tick))))

;; Converts the given list of PlayedNotes to a measure
;; list->measure: (ListOf PlayedNote) -> Measure
(define (list->measure lst expected-duration)
  (apply create-measure expected-duration lst))

(begin-for-syntax
  (define-syntax-class positive-rational
    #:description "positive rational number"
    (pattern val
      #:fail-unless (and (rational? (syntax->datum #'val))
                         (positive? (syntax->datum #'val)))
      "expected a positive rational number"))
  
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
       (define notes-list (syntax->list #'(notes/grouped-notes ...)))
       (define durations (map get-duration notes-list))
       (define midi-durations
         (map duration-to-tick durations))
       (define/syntax-parse (notes ...)
         (for/list ([note notes-list]
                    [time-since-last-event (cons 0 midi-durations)])
           (compile-note/grouped-notes note time-since-last-event)))
       #'(append notes ...)]))

  ;; Converts the note or group of notes into a list of PlayedNotes
  ;; representing the notes played at that moment.
  (define (compile-note/grouped-notes note/grouped-notes time-since-last-event)
    (syntax-parse note/grouped-notes
      [((~datum rest) duration:number)
       #`(list #,(compile-note/rest #'(rest duration) time-since-last-event))]
      [(((~datum quote) pitch:id) duration:number)
       #`(list #,(compile-note/rest #'('pitch 4 duration) time-since-last-event))]
      [(((~datum quote) pitch:id) octave:exact-integer duration:number)
       #`(list #,(compile-note/rest #'('pitch octave duration) time-since-last-event))]
      [(midi-pitch:nat duration:number)
       #`(list #,(compile-note/rest #'(midi-pitch duration) time-since-last-event))]
      [(note-stuff:expr ... duration:number)
       (define note-list (syntax->list #'(note-stuff ...)))
       (define compiled-notes
         (map compile-note/rest
              note-list
              (build-list (length note-list)
                          (lambda (i)
                            (if (= i 0)
                                time-since-last-event
                                0)))))
       #`(apply list #,compiled-notes)]))

  ;; Converts the note or rest to a list containing the corresponding
  ;; PlayedNote.
  (define (compile-note/rest note/rest time-since-last-event)
    (syntax-parse note/rest
      [((~datum rest) duration:positive-rational)
       #`(rest #,(duration-to-tick (syntax->datum #'duration)) #,time-since-last-event)]
      [(((~datum quote) pitch:id) octave:exact-integer duration:positive-rational)
       #`(played-note (note 'pitch octave)
                       #,(duration-to-tick (syntax->datum #'duration))
                       127
                       0
                       #,time-since-last-event
                       #t
                       #t)] ; change constants to syntax parameters
      [(midi-pitch:nat duration:positive-rational)
       #`(played-note midi-pitch
                       #,(duration-to-tick (syntax->datum #'duration))
                       127
                       0
                       #,time-since-last-event
                       #t
                       #t)])) ; change constants to syntax parameters

  ;; Extracts the duration of the given note, rest, or group of notes.
  (define (get-duration note/grouped-notes)
    (syntax->datum
     (syntax-parse note/grouped-notes
       [(contents:expr ... duration:number)
        #'duration])))

  ;; TODO: calculate from tempo
  (define ticks-per-quarter-note 96)

  ;; duration-to-tick : Rational -> Integer
  (define (duration-to-tick duration)
    (floor (* duration ticks-per-quarter-note 4)))) ; 4 quarter notes to a whole note

(define microseconds-per-minute 60000000)

(define ticks-per-quarter-note 96)

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
                            (append (list
                                     (make-mtrk-event 0
                                                      (set-tempo-event
                                                       (floor (/ (* microseconds-per-minute
                                                                    4)
                                                                 duration tempo)))))
                                    (map list->measure
                                         #,compiled-measures
                                         (build-list
                                          (length #,compiled-measures)
                                          (lambda (i) 384)))
                                    (list (make-mtrk-event 0 (end-of-track-event)))))))])))

;; EXAMPLE:
#;(music (4 4) 120 ((measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4)) (measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4)) (measure ('B 4 1/4) ('B 4 1/4) ('B 4 1/4) ('B 4 1/4))))


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