#lang racket

(require (for-syntax syntax/parse "constants.rkt") "beat.rkt" "structs.rkt" "notes.rkt" "constants.rkt")
(provide music)

;; A PlayedNote is a (played-note pitch duration velocity channel
;; ticks-since-last-note starts-in-measure? ends-in-measure?), with the
;; corresponding pitch, duration, velocity, and channel of the note, the
;; ticks since the last note played, and whether the note starts and ends
;; in the current measure. duration and ticks-since-last-note are in MIDI
;; ticks.
(struct played-note [pitch duration velocity channel ticks-since-last-note starts-in-measure? ends-in-measure?] #:transparent)

;; A Rest is a (rest duration ticks-since-last-note), with the given
;; duration of the rest and the ticks since the last note was played.
;; duration and ticks-since-last-note are in in MIDI ticks.
(struct rest [duration ticks-since-last-note] #:transparent)

;; Creates a NoteOnEvent from the given PlayedNote.
;; played-note->note-on-event: PlayedNote -> NoteOnEvent
(define (played-note->note-on-event the-note)
  (make-note-on-event (played-note-pitch the-note)
                      (played-note-velocity the-note)
                      (played-note-channel the-note)))

;; Creates a NoteOffEvent from the given PlayedNote.
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
           (let-values
               ([(body end-time)
                 (for/fold ([result '()]
                            [current-duration 0])
                           ([tick (in-range (add1 expected-duration))])
                   (let ([ref (hash-ref table tick '())])
                     (if (null? ref)
                         (values result current-duration)
                         (values (append result
                                         (build-mtrk-events ref (- tick current-duration)))
                                 tick))))])
             (append (list (make-mtrk-event 0 (make-text-event "Start of measure")))
                     body
                     (list (make-mtrk-event (- expected-duration end-time)
                                            (make-text-event "End of measure"))))))))

;; Creates a list of MTrkEvents for all Events that happen at the same time
;; at the given offset from the last note.
;; build-mtrk-events: (ListOf Event) Natural -> (ListOf MTrkEvent)
(define (build-mtrk-events events offset-from-last-note)
  (for/list ([event events]
             [offset
              (build-list
               (length events)
               (lambda (i)
                 (if (= i 0)
                     offset-from-last-note
                     0)))])
    (make-mtrk-event offset
                     event)))

;; Creates a table mapping ticks to the events happening on those ticks.
;; create-measure-table : PlayedNote ... -> (HashOf Natural MidiEvent)
(define (create-measure-table . notes)
  (for/fold ([table (hash)]
             [ticks-so-far 0]
             #:result table)
            ([note notes])
    (let* ([ticks-since-last-event (ticks-since-last-note note)]
           [new-tick (+ ticks-so-far ticks-since-last-event)])
      (values
       (if (played-note? note)
           (update-table table note ticks-since-last-event new-tick)
           table)
       new-tick))))

;; Gets the number of MIDI ticks since the last note of the given
;; PlayedNote or Rest.
;; ticks-since-last-note: (U PlayedNote Rest) -> Natural
(define (ticks-since-last-note note)
  (if (played-note? note)
      (played-note-ticks-since-last-note note)
      (rest-ticks-since-last-note note)))

;; Gets the duration of the given PlayedNote or Rest.
;; duration: (U PlayedNote Rest) -> Natural
(define (duration note)
  (if (played-note? note)
      (played-note-duration note)
      (rest-duration note)))

;; Updates the given hash of Naturals to ListsOf Event with the given
;; PlayedNote and its number of ticks since the last event and the
;; tick at which it is played.
;; update-table: (HashOf Natural (ListOf Event)) PlayedNote Natural Natural
;;   -> (HashOf Natural (ListOf Event))
(define (update-table table note ticks-since-last-event new-tick)
  (let* ([current-val (hash-ref table new-tick '())]
         [ticks-to-end (+ new-tick (duration note))]
         [end-val (hash-ref table ticks-to-end '())])
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
                   end-val))))

;; Converts the given list of PlayedNotes to a measure
;; list->measure: (ListOf PlayedNote) -> Measure
(define (list->measure lst expected-duration)
  (apply create-measure expected-duration lst))

(begin-for-syntax
  ;; PositiveRational: a positive rational number
  (define-syntax-class positive-rational
    #:description "positive rational number"
    (pattern val
      #:fail-unless (and (rational? (syntax->datum #'val))
                         (positive? (syntax->datum #'val)))
      "expected a positive rational number"))

  ;; SpecialNote: syntax for a note used in a grouped-note setting
  (define-syntax-class special-note
    #:description "note used in a grouped-note setting"
    (pattern ((~datum quote) pitch:id))
    (pattern (((~datum quote) pitch:id)))
    (pattern (((~datum quote) pitch:id) octave:exact-integer))
    (pattern (((~datum quote) pitch:id) octave:exact-integer duration:positive-rational))
    (pattern (midi-pitch:nat))
    (pattern (midi-pitch:nat full-duration:positive-rational)))
  
  ;; Converts the given measures (as represented in music) to a list containing
  ;; lists of PlayedNotes. Those lists represent the measures in the song.
  ;; compile-measures: Syntax PositiveRational -> Syntax
  (define (compile-measures measures measure-length)
    (syntax-parse measures
      [((~and stx ((~datum measure) note/grouped-notes ...)) ...)
       (define notes-list (attribute stx))
       (define/syntax-parse (compiled ...)
         (map compile-measure notes-list (build-list (length notes-list) (lambda (i) measure-length))))
       #'(list compiled ...)]))

  ;; Converts the given measure (as represented in music) to a list of
  ;; PlayedNotes that represents a measure in the song.
  ;; compile-measure: Syntax PositiveRational -> Syntax
  (define (compile-measure measure measure-length)
    (define (check-measure-length! durations measure-length)
      (unless (= (apply + durations)
                  measure-length)
         (raise-syntax-error #f "Measure length does not match time signature" measure)))
    (define (check-full-duration-within-measure! durations full-durations)
      (for/fold [(duration-so-far 0)]
                 [(duration durations)
                  (full-duration full-durations)]
         (if (> (+ duration-so-far full-duration) measure-length)
             (raise-syntax-error #f "Measure contains note ending beyond end of measure" measure)
             (+ duration-so-far duration))))
    (syntax-parse measure
      [((~datum measure) notes/grouped-notes ...)
       (define notes-list (syntax->list #'(notes/grouped-notes ...)))
       (define durations (map get-duration notes-list))
       (check-measure-length! durations measure-length)
       (define full-durations (map get-full-duration notes-list))
       (check-full-duration-within-measure! durations full-durations)
       (define midi-durations
         (map duration-to-tick durations))
       (define/syntax-parse (notes ...)
         (for/list ([note notes-list]
                    [time-since-last-event (cons 0 midi-durations)])
           (compile-note/grouped-notes note time-since-last-event)))
       #'(append notes ...)]))

  ;; Converts the note or group of notes into a list of PlayedNotes
  ;; representing the notes played at that moment.
  ;; compile-note/grouped-notes: Syntax Natural -> Syntax
  (define (compile-note/grouped-notes note/grouped-notes time-since-last-event)
    (syntax-parse note/grouped-notes
      [((~datum rest) duration:positive-rational)
       #`(list #,(compile-note/rest #'(rest duration) time-since-last-event))]
      [(((~datum quote) pitch:id) duration:positive-rational)
       #`(list #,(compile-note/rest #'('pitch 4 duration) time-since-last-event))]
      [(((~datum quote) pitch:id) octave:exact-integer duration:positive-rational)
       #`(list #,(compile-note/rest #'('pitch octave duration) time-since-last-event))]
      [(midi-pitch:nat duration:positive-rational)
       #`(list #,(compile-note/rest #'(midi-pitch duration) time-since-last-event))]
      [(first-note:special-note other-notes:special-note ... duration:positive-rational)
       (define other-note-list (syntax->list #'(other-notes ...)))
       (define/syntax-parse (other-notes-compiled ...)
         (map compile-special-note
              other-note-list
              (build-list (length other-note-list)
                          (lambda (i) 0))
              (build-list (length other-note-list)
                          (lambda (i) #'duration))))
       #`(cons #,(compile-special-note #'first-note time-since-last-event #'duration)
               (list other-notes-compiled ...))]))

  ;; Converts the SpecialNote and duration to the corresponding PlayedNote.
  ;; compile-special-note: SpecialNote Natural PositiveRational -> Syntax
  (define (compile-special-note note time-since-last-event duration)
    (syntax-parse note
      [((~datum quote) pitch:id)
       (compile-note/rest #`('pitch 4 #,duration) time-since-last-event)]
      [(((~datum quote) pitch:id))
       (compile-note/rest #`('pitch 4 #,duration) time-since-last-event)]
      [(((~datum quote) pitch:id) octave:exact-integer)
       (compile-note/rest #`('pitch octave #,duration) time-since-last-event)]
      [(((~datum quote) pitch:id) octave:exact-integer full-duration:positive-rational)
       (compile-note/rest #'('pitch octave full-duration) time-since-last-event)]
      [(midi-pitch:nat)
       (compile-note/rest #'(midi-pitch #,duration) time-since-last-event)]
      [(midi-pitch:nat full-duration:positive-rational)
       (compile-note/rest #'(midi-pitch full-duration) time-since-last-event)]))

  ;; Converts the note or rest to the corresponding PlayedNote or Rest.
  ;; compile-note/rest: Syntax Natural -> Syntax
  (define (compile-note/rest note/rest time-since-last-event)
    (syntax-parse note/rest
      [((~datum rest) duration:positive-rational)
       #`(rest #,(duration-to-tick (syntax->datum #'duration)) #,time-since-last-event)]
      [(((~datum quote) pitch:id) octave:exact-integer duration:positive-rational)
       (compile-played-note #'(note 'pitch octave) #'duration time-since-last-event)]
      [(midi-pitch:nat duration:positive-rational)
       (compile-played-note #'midi-pitch #'duration time-since-last-event)]))

  ;; Creates the PlayedNote that corresponds to the given MidiPitch syntax object,
  ;; Duration syntax object, and time since the last event.
  ;; compile-played-note: Syntax PositiveRational Natural -> Syntax
  (define (compile-played-note pitch duration time-since-last-event)
    #`(played-note #,pitch
                   #,(duration-to-tick (syntax->datum duration))
                   127
                   0
                   #,time-since-last-event
                   #t
                   #t))

  ;; Extracts the duration of the given note, rest, or group of notes.
  ;; get-duration: Syntax -> Rational
  (define (get-duration note/grouped-notes)
    (syntax->datum
     (syntax-parse note/grouped-notes
       [(contents:expr ... duration:positive-rational)
        #'duration])))

  ;; Extracts the full duration of the given note, rest, or group of notes.
  ;; get-full-duration: Syntax -> Rational
  (define (get-full-duration note/grouped-notes)
    (syntax->datum
     (syntax-parse note/grouped-notes
       [(notes:special-note ...+ duration:positive-rational)
        (define durations
          (for/list [(note (syntax->list #'(notes ...)))]
            (get-grouped-note-full-duration note #'duration)))
        #`#,(apply max durations)]
       [(contents:expr ... duration:positive-rational)
        #'duration])))

  ;; Gets the full duration from the note in a group of notes
  ;; get-grouped-note-full-duration: SpecialNote PositiveRational -> Rational
  (define (get-grouped-note-full-duration note duration)
    (syntax->datum
     (syntax-parse note
       [(((~datum quote) pitch:id) octave:exact-integer full-duration:positive-rational)
        #'full-duration]
       [(midi-pitch:nat full-duration:positive-rational)
        #'full-duration]
       [other:expr
        duration])))

  ;; Converts the musical duration to a number of MIDI ticks.
  ;; duration-to-tick : Rational -> Integer
  (define (duration-to-tick duration)
    (floor (* duration ticks-per-whole-note))))


;; Builds a MIDI track from the given compiled measures.
;; build-track: (ListOf (ListOf (U PlayedNote Rest))) ExactPositiveInteger ExactPositiveInteger ExactPositiveInteger
;;   -> Track
(define (build-track compiled-measures beat/measure duration tempo)
  (apply track
         (append (list (build-set-tempo-event duration tempo))
                 (build-measures compiled-measures beat/measure duration)
                 (list (make-mtrk-event 0 (end-of-track-event))))))

;; Builds a MTrkEvent containing a SetTempoEvent from the given note duration and tempo
;; build-set-tempo-event: ExactPositiveInteger ExactPositiveInteger -> MTrkEvent
(define (build-set-tempo-event duration tempo)
  (make-mtrk-event 0
                   (set-tempo-event
                    (floor (/ (* microseconds-per-minute
                                 duration)
                              quarter-notes-per-whole-note
                              tempo)))))

;; Converts the list of lists of PlayedNotes/Rests to a list of MTrkEvents
;; build-measures: (ListOf (ListOf (U PlayedNote Rest))) ExactPositiveInteger Exact-Positive-Integer
;;   -> (ListOf MTrkEvent)
(define (build-measures compiled-measures beat/measure duration)
  (map list->measure
       compiled-measures
       (build-list
        (length compiled-measures)
        (lambda (i) (/ (* ticks-per-whole-note beat/measure) duration)))))

;; Converts pseudo-musical notation into a format useful for writing MIDI.
;; Grammar:
;; (music (<ExactPositiveInteger> <ExactPositiveInteger>) <ExactPositiveInteger>
;;        (<measure> ...+))
;;
;; <measure> ::= (measure <note/grouped-notes/rest> ...)
;;
;; <note/grouped-notes/rest> ::= <note/rest>
;;                             | (<special-note> ... <Rational>)
;;
;; <note/rest> ::= (rest <Rational>)
;;               | <note>
;;
;; <special-note> ::= <NoteSymbol>
;;                  | (<NoteSymbol>)
;;                  | (<NoteSymbol> <ExactInteger>)
;;                  | (<NoteSymbol> <ExactInteger> <Rational>)
;;                  | (<MidiPitch>)
;;                  | (<MidiPitch> <Rational>)
;;
;; <note> ::= (<NoteSymbol> <ExactInteger> <Rational>)
;;          | (<NoteSymbol> <Rational>)
;;          | (<MidiPitch> <Rational>)
(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer)
          tempo:exact-positive-integer
          (measure:expr ...+))
       (define compiled-measures
         (compile-measures #'(measure ...) (/ (syntax->datum #'beat/measure) (syntax->datum #'duration))))
       #`(song (header 0 (beat beat/measure duration) tempo)
               (list (build-track #,compiled-measures beat/measure duration tempo)))])))


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