#lang racket

(require "preds.rkt" "notes.rkt" "beat.rkt")

(provide event?
         make-note-on-event
         make-note-off-event
         make-text-event
         make-mtrk-event
         measure
         midi-track
         header
         song
         track)

;; An Event is an abstract representation of any MidiEvent or MetaEvent.
(struct event [] #:transparent)

;; A MidiEvent is an abstract representation of an event played in MIDI.
(struct midi-event event [] #:transparent)

;; A MetaEvent is an abstract representation of a meta event within MIDI.
(struct meta-event event [] #:transparent)

;; A NoteOnEvent is a (note-on-event note velocity channel), where note
;; refers to the pitch to be played, velocity refers to the velocity at
;; which the note was played, and channel refers to the MIDI channel on
;; which the note was played.
(struct note-on-event midi-event [note velocity channel] #:transparent)
;; Constructs a NoteOnEvent from the given note pitch, velocity, and
;; channel.
;; make-note-on-event: MidiPitch Velocity Channel -> NoteOnEvent
(define (make-note-on-event note velocity channel)
  (unless (midi-pitch? note)
    (error 'make-note-on-event
           "note must be a MidiPitch; given ~a" note))
  (unless (velocity? velocity)
    (error 'make-note-on-event
           "velocity must be a Velocity; given ~a" velocity))
  (unless (channel? channel)
    (error 'make-note-on-event
           "channel must be a Channel; given ~a" channel))
  (note-on-event note velocity channel))

(module+ test
  (require rackunit)
  
  (check-equal? (make-note-on-event (note 'C) 127 0)
                (note-on-event (note 'C) 127 0))
  (check-exn exn:fail?
             (lambda () (make-note-on-event 128 127 0)))
  (check-exn exn:fail?
             (lambda () (make-note-on-event (note 'C) 128 0)))
  (check-exn exn:fail?
             (lambda () (make-note-on-event (note 'C) 127 16))))

;; A NoteOffEvent is a (note-off-event note velocity channel), where note
;; refers to the pitch to stop playing, velocity refers to the velocity
;; of the release, and channel refers to the channel on which the note
;; was playing.
(struct note-off-event midi-event [note velocity channel] #:transparent)
;; Constructs a NoteOffEvent from the given note pitch, velocity, and
;; channel.
;; make-note-off-event: MidiPitch Velocity Channel -> NoteOffEvent
(define (make-note-off-event note velocity channel)
  (unless (midi-pitch? note)
    (error 'make-note-off-event
           "note must be a MidiPitch; given ~a" note))
  (unless (velocity? velocity)
    (error 'make-note-off-event
           "velocity must be a Velocity; given ~a" velocity))
  (unless (channel? channel)
    (error 'make-note-off-event
           "channel must be a Channel; given ~a" channel))
  (note-off-event note velocity channel))

(module+ test
  (check-equal? (make-note-off-event (note 'C) 127 0)
                (note-off-event (note 'C) 127 0))
  (check-exn exn:fail?
             (lambda () (make-note-off-event 128 127 0)))
  (check-exn exn:fail?
             (lambda () (make-note-off-event (note 'C) 128 0)))
  (check-exn exn:fail?
             (lambda () (make-note-off-event (note 'C) 127 16))))

;; A TextEvent is a (text-event text), where text is the text used
;; in the MIDI file.
(struct text-event meta-event [text] #:transparent)
;; Constructs a TextEvent from the given text.
(define (make-text-event text)
  (if (string? text)
      (text-event text)
      (error 'make-text-event "text must be a string")))

;; A MTrkEvent is a (mtrk-event delta-time event), where delta-time
;; represents the time since the previous event that the given event
;; should be played, with delta-time in units specified by the header.
(struct mtrk-event [delta-time event] #:transparent)
;; Constructs a MTrkEvent from the given delta-time and event.
;; make-mtrk-event: Natural Event -> MTrkEvent
(define (make-mtrk-event delta-time event)
  (unless (natural? delta-time)
    (error 'make-mtrk-event
           "delta-time must be a Natural; given ~a" delta-time))
  (unless (event? event)
    (error 'make-mtrk-event
           "event must be an Event; given ~a" event))
  (mtrk-event delta-time event))

(module+ test
  (check-equal? (make-mtrk-event 0 (make-note-on-event (note 'C) 127 0))
                (mtrk-event 0 (make-note-on-event (note 'C) 127 0)))
  (check-exn exn:fail?
             (lambda () (make-mtrk-event
                         3/2
                         (make-note-on-event (note 'C) 127 0))))
  (check-exn exn:fail?
             (lambda () (make-mtrk-event 0 (note 'C)))))

;; A Measure is a (song-measure mtrk-events), where mtrk-events is the MTrk
;; events that occur in the measure.
(struct song-measure [mtrk-events] #:transparent)
;; Constructs a Measure from the given MTrkEvents.
;; measure: (MTrkEvent ...) -> Measure
(define (measure . mtrk-events)
  (for ([event mtrk-events])
    (unless (mtrk-event? event)
      (error 'measure "Must provide only MTrk events to measure")))
  (song-measure mtrk-events))

(module+ test
  (check-equal? (measure (make-mtrk-event 0 (make-note-on-event
                                             (note 'C)
                                             127
                                             0))
                         (make-mtrk-event 48 (make-note-off-event
                                              (note 'C)
                                              127
                                              0)))
                (song-measure (list
                               (make-mtrk-event 0 (make-note-on-event
                                                   (note 'C)
                                                   127
                                                   0))
                               (make-mtrk-event 48 (make-note-off-event
                                                    (note 'C)
                                                    127
                                                    0)))))
  (check-exn exn:fail?
             (lambda ()
               (measure (make-mtrk-event 0 (make-note-on-event
                                            (note 'C)
                                            127
                                            0))
                        (make-mtrk-event 48 (make-note-off-event
                                             (note 'C)
                                             127
                                             0))
                        (make-note-on-event
                         (note 'D)
                         127
                         0)))))

;; A Section is a (song-section mtrk-events), where mtrk-events is the MTrk
;; events that occur in the section.
(struct song-section [mtrk-events] #:transparent)

;; A Track is a (midi-track mtrk-events), where mtrk-events is the MTrk events
;; that occur on the track.
(struct midi-track [mtrk-events] #:transparent)
;; track: (U Measure Section) ... -> Track
(define (track . measures/sections)
  (midi-track (apply append
                     (for/list ([measure/section measures/sections])
                       (if (song-measure? measure/section)
                           (song-measure-mtrk-events measure/section)
                           (song-section-mtrk-events measure/section))))))

(module+ test
  (check-equal? (track (measure (make-mtrk-event 0 (make-note-on-event
                                                    (note 'C)
                                                    127
                                                    0))
                                (make-mtrk-event 96 (make-note-off-event
                                                     (note 'C)
                                                     127
                                                     0))))
                (midi-track (list (make-mtrk-event
                                   0
                                   (make-note-on-event (note 'C) 127 0))
                                  (make-mtrk-event
                                   96
                                   (make-note-off-event (note 'C) 127 0)))))

  (check-equal? (track
                 (measure (make-mtrk-event 0 (make-text-event "Tacet"))
                          (make-mtrk-event 384 (make-text-event "End tacet")))
                 (measure (make-mtrk-event 0 (make-note-on-event
                                              (note 'C)
                                              127
                                              0))
                          (make-mtrk-event 384 (make-note-off-event
                                                (note 'C)
                                                127
                                                0))))
                (midi-track
                 (list
                  (make-mtrk-event 0 (make-text-event "Tacet"))
                  (make-mtrk-event 384 (make-text-event "End tacet"))
                  (make-mtrk-event 0 (make-note-on-event
                                      (note 'C)
                                      127
                                      0))
                  (make-mtrk-event 384 (make-note-off-event
                                        (note 'C)
                                        127
                                        0))))))

;; A MThd is a (header format time-signature tempo), representing a header
;; chunk in MIDI with the given format, time signature, and tempo.
(struct header [format time-signature tempo] #:transparent)

;; A Song is a (song header tracks), where header represents the header
;; chunk and tracks represents the track chunks.
(struct song [header tracks] #:transparent)

