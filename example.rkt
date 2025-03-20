#lang racket

(require "music.rkt")


#|

   MIDI Grammar:

    music := (music <beat> tempo:exact-positive-integer (<measure> ...+))

    beat := (beat:exact-positive-integer duration:exact-positive-integer)

    measure := (measure <note/grouped-notes> ...)

    note/grouped-notes := (rest duration:rational)
                        | (<note/sym> <octave> duration:rational)
                        | (<note/sym> duration:rational)
                        | (pitch:midi-pitch <octave> duration:rational)
                        | (<note/grouped-notes> ...+ duration:rational)

    octave : (and number? (>= -1) (<= 8))

    note/sym := <note/name><note/modifier>

    note/name := 'A | 'B | 'C | 'D | 'E | 'F | 'G

    note/modifier :=
                   | b
                   | #

|#

;; Example program (Mary Had a Little Lamb):
(music (4 4)
       120
       ((measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/2))
        (measure ('D 1/4) ('D 1/4) ('D 1/2))          (measure ('E 1/4) ('G 1/4) ('G 1/2))
        (measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/4) ('E 1/4))
        (measure ('D 1/4) ('D 1/4) ('E 1/4) ('D 1/4)) (measure ('C 1))))

;; Example expansion
#;(song
   (header 0 (time-signature 4 4) 120)
   (list
    (midi-track
     (list
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 60 127 0))
      (mtrk-event 96 (note-off-event 60 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 192 (note-off-event 64 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 192 (note-off-event 62 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 67 127 0))
      (mtrk-event 96 (note-off-event 67 127 0))
      (mtrk-event 0 (note-on-event 67 127 0))
      (mtrk-event 192 (note-off-event 67 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 60 127 0))
      (mtrk-event 96 (note-off-event 60 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (note-on-event 64 127 0))
      (mtrk-event 96 (note-off-event 64 127 0))
      (mtrk-event 0 (note-on-event 62 127 0))
      (mtrk-event 96 (note-off-event 62 127 0))
      (mtrk-event 0 (text-event "End of measure"))
      (mtrk-event 0 (text-event "Start of measure"))
      (mtrk-event 0 (note-on-event 60 127 0))
      (mtrk-event 384 (note-off-event 60 127 0))
      (mtrk-event 0 (text-event "End of measure"))))))