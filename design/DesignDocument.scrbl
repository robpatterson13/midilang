#lang scribble/manual

@title{Musically Impractical DSL Implementation}

@section{Purpose and Concepts}

The Musically Impractical DSL Implementation (MIDI) is a Domain-Specific Language for writing music
and converting it to and from MIDI files.

The syntax is designed to share concepts with musical notation for ease of use by musicians. For
example, it contains concepts such as notes, measures, and sections.

...

@section{Examples}

Here is an example program:
@codeblock|{
;; define a song
(create-song hot-cross-buns
           #:name "Hot Cross Buns"
           #:tempo 100
           #:key 'G
           #:time-signature (beat 4 4))

;; define a track
(create-track main #:in hot-cross-buns)

;; define a section
(create-section section1 #:in track1)

;; define a measure
(create-measure measure1
                (in-sequence
                 (make-note 'B 1/4)
                 (make-note 'A 1/4)
                 (make-note 'G 1/2)))

;; repeat that measure
(repeat measure1)

;; define another measure
(create-measure measure3
                (in-sequence
                 (repeat-note (make-note 'G 1/8) 4)
                 (repeat-note (make-note 'A 1/8) 4)))

;; repeat the first measure again
(repeat measure1)

;; end the section
(end-section)

;; end the track
(end-track)

;; write to a midi file
(write-to-midi hot-cross-buns "hcb.mid") }|

Another example:
@codeblock|{
;; define a new chord
(new-chord C7 ['C 'E 'G 'Bb])

;; read song from file
(read-song hot-cross-buns #:from "hcb.mid")

;; transpose into another key and speed up
(define hcb-in-C
  (scale-to-tempo
   (transpose hot-cross-buns #:semitones 5)
   120))

;; get the main track of the song and open it for modification
(open-track main #:track 1 #:in hcb-in-C)

;; make a section from the 3rd and 4th measures in the track
(define ending (section-from-measures 3 #:to 4 #:in main))

;; repeat the ending
(repeat ending)

;; get the 6th measure of the track and add a note to it
(define last-measure (get-measure 6 #:in main))
(add-note (make-note 'G 1/4) #:beat 4 #:in last-measure)

;; add a measure to the end of the track
(create-measure real-ending
                (play-chord C7 1))

;; end the track
(end-track)

;; write to a midi file
(write-to-midi hcb-in-C "hcb-in-C.mid")
}|

An example of a simplified music notation:
@codeblock|{
;; Note: I could not get scribble to recognize such a unique grammar, so I had to put it in a string.
"
| B4(1/4) A4(1/4) G4(1/4) A4(1/4) | B4(1/4) B4(1/4) B4(1/2)         |
| A4(1/4) A4(1/4) A4(1/2)         | B4(1/4) D5(1/4) D5(1/2)         |
| B4(1/4) A4(1/4) G4(1/4) A4(1/4) | B4(1/4) B4(1/4) B4(1/4) B4(1/4) |
| A4(1/4) A4(1/4) B4(1/4) A4(1/4) | G4(1)                          ||
"
}|
