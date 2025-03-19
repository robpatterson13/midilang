#lang racket

(require (for-syntax syntax/parse) "structs.rkt" "notes.rkt")

#;(music (4 4) 120 ((measure ((14 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4))) (measure (list (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4) (note 'B 4 1/4)))))

(define-syntax music
  (lambda (stx)
    (syntax-parse stx
      [(_ (beat/measure:exact-positive-integer duration:exact-positive-integer) tempo:exact-positive-integer (measure ...+))
       #'(song (header (beat beat/measure duration) tempo)
               (list (measures measure ...)))])))

(define-syntax measures
  (lambda (stx)
    (syntax-parse stx
      [(_ (note/grouped-notes) ...)
       #'(track (parse-note/grouped-notes note/grouped-notes) ...)])))

(define-syntax parse-note/grouped-notes
  (lambda (stx)
    (syntax-parse stx
      [(_ ((note-stuff ...) ...))
       #'(create-measure 384 (parse-note note-stuff ...) ...)] ; change expected duration to syntax parameter
      [(_ (note-stuff ...))
       #'(parse-note note-stuff ...)])))


(define ticks-per-quarter-note 96)

;; duration-to-tick : Rational -> Integer
(define (duration-to-tick duration)
  (floor (* duration ticks-per-quarter-note 4)))

;; duration and time-since-start-of-measure are MIDI ticks
(struct played-note [pitch duration velocity channel ticks-since-last-note starts-in-measure? ends-in-measure?] #:transparent)

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

(define (played-note->note-on-event the-note)
  (make-note-on-event (played-note-pitch the-note)
                      (played-note-velocity the-note)
                      (played-note-channel the-note)))

(define (played-note->note-off-event the-note)
  (make-note-off-event (played-note-pitch the-note)
                       (played-note-velocity the-note)
                       (played-note-channel the-note)))

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