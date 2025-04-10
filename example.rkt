#lang racket

(require "music.rkt" "file-io.rkt")


#|

   MIDI Grammar:

    music := (music <beat> tempo:exact-positive-integer (<measure> ...+))

    beat := (beat:exact-positive-integer duration:exact-positive-integer)

    measure := (measure <note/grouped-notes> ...)

    note/grouped-notes := <note>
                        | (<note> ...+ duration:rational)

    note := (rest duration:rational)
          | (<note/sym> <octave> duration:rational)
          | (<note/sym> duration:rational)
          | (<midi-pitch> <octave> duration:rational)

    octave : (and exact-integer? (>= -1) (<= 8))

    note/sym := <note/name><note/modifier>

    note/name := 'A | 'B | 'C | 'D | 'E | 'F | 'G

    note/modifier :=
                   | b
                   | #

    midi-pitch : (and natural? (<= 127))
|#

;; Example program (Mary Had a Little Lamb):
#;(music (4 4)
       120
       ((measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/2))
        (measure ('D 1/4) ('D 1/4) ('D 1/2))          (measure ('E 1/4) ('G 1/4) ('G 1/2))
        (measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/4) ('E 1/4))
        (measure ('D 1/4) ('D 1/4) ('E 1/4) ('D 1/4)) (measure ('C 1))))
#;(define duck
  (music (4 4)
       180
       ((measure ('C 5 1/4) ('C 5 1/4) ('F 1/4) ('A 1/8) ('A 1/8)) (measure ('A# 1/8) ('A# 1/8) ('A 1/4) ('G 1/4) ('C 5 1/8) ('C 5 1/8))
        (measure ('C 5 1/4) ('C 5 1/8) ('C 5 1/8) ('C 5 1/2))      (measure ('D 5 1/8) ('C 5 1/8) ('C 5 1/8) ('C 5 1/8) (rest 1/2))
        (measure ('C 5 1/2) ('C 1/4)   ('A 3 1/4))                 (measure ('F 3 1)))))
#;duck

#;(define dark-tune
  (music (12 8)
         180
         ((measure ('E 2 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8)
                   ('G 3 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8))
          (measure ('E 2 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8)
                   ('G 3 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8))
          (measure ('E 2 1/8) ('Bb 3 1/8) ('A 3 1/8) ('E 2 1/8) ('A 3 1/8) ('G 3 1/8)
                   ('E 2 1/8) ('G 3 1/8)  ('F 3 1/8) ('E 2 1/8) ('F 3 1/8) ('E 3 1/8))
          (measure ('E 2 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8)
                   ('G 3 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8))
          (measure ('E 2 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8)
                   ('G 3 1/8) ('E 3 1/8)  ('E 2 1/8) ('F 3 1/8) ('E 3 1/8) ('E 2 1/8))
          (measure ('E 2 1/8) ('Bb 3 1/8) ('A 3 1/8) ('E 2 1/8) ('A 3 1/8) ('G 3 1/8)
                   ('E 2 1/8) ('G 3 1/8)  ('F 3 1/8) ('E 2 1/8) ('F 3 1/8) ('E 3 1/8)))))
#;dark-tune
#;(write-to-midi-file duck "duck.mid")

(define chord-progression
  (music (4 4)
         100
         ((measure ('C ('E 4 1/2) 'G 1/4) ('C 'F 'G 1/4) ('C ('E 4 1/2) 'G 1/4) ('C 'D 'G 1/4))
          (measure ('G 'B ('D 5) 1/2) ('F 'A ('C 5) 1/2)))))

(write-to-midi-file chord-progression "chord-progression.mid")

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