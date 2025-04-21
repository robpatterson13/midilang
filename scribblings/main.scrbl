#lang scribble/manual

@(require (for-label racket))

@title{midilang: A DSL for transcribing music into MIDI files}

@defmodule[midilang]

@defform[(music #:literals (rest measure)
                (beat/measure:exact-positive-integer
                 duration:exact-positive-integer)
                tempo:exact-positive-integer
                (measures:expr ...+))]{
Converts pseudo-musical notation into a format useful for writing MIDI.

The beats per measure and the duration define the time signature of the music.
For example, @racket[(4 4)] refers to a 4/4 time signature.

The tempo is in beats per minute.

The measures take the form of @racket[(measure note/grouped-note ...+)].

A @racketid[note/grouped-note] is one of the following:
@itemlist[
 @item{@racket[(rest time:positive-rational)]}
 @item{@racket[(pitch:note-symbol time:positive-rational)]}
 @item{@racket[(pitch:note-symbol octave:exact-integer time:positive-rational)]}
 @item{@racket[(midi-pitch:nat time:positive-rational)]}
 @item{@racket[(notes:special-note ...+ time:positive-rational)]}
 ]

Where a @racketid[note-symbol] is one of the following:
@itemlist[
 @item{@racket['C]}
 @item{@racket['C#]}
 @item{@racket['Db]}
 @item{@racket['D]}
 @item{@racket[...]}
 @item{@racket['E]}
 @item{@racket['F]}
 @item{@racket['F#]}
 @item{@racket[...]}
 @item{@racket['G#]}
 @item{@racket['Ab]}
 @item{@racket[...]}
 @item{@racket['B]}
 ]

A @racketid[positive-rational] is a positive rational number, and a @racketid[special-note] is one of:
@itemlist[
 @item{@racket[pitch:note-symbol]}
 @item{@racket[(pitch:note-symbol)]}
 @item{@racket[(pitch:note-symbol octave:exact-integer)]}
 @item{@racket[(pitch:note-symbol octave:exact-integer full-duration:positive-rational)]}
 @item{@racket[(midi-pitch:nat)]}
 @item{@racket[(midi-pitch:nat full-duration:positive-rational)]}
 ]

In instances where the note is given by a midi pitch instead of a pitch and optional octave (octave
defaults to 4), the @racketid[midi-pitch] is the pitch represented in MIDI format (a natural between
0 and 127, inclusive).

In a grouped-note setting, a special note can optionally contain a @racket[full-duration], which
represents the full duration that the note is played, rather than the musical time offset to the
beginning of the next @racket[note/grouped-note], which is given by the overarching @racket[duration]
parameter of the set of grouped notes. When @racket[full-duration] is not specified, it defaults to
the grouped notes' @racket[duration].

The total length of the notes/grouped-notes in each measure must equal the expected length of the
measure given by the time signature. Similarly, no note's full duration can make it last beyond the
end of a measure.
}

@defproc[(write-to-midi-file (song-to-write song?)
                             (file-path path-string?)
                             (#:exists exists-flag symbol? 'error))
         void?]{
Writes the provided song written using the @racketid[music] macro to the given file path.

The @racketid[exists-flag] specifies what should happen if a file already exists on the given
path. See @racketlink[open-output-file "open-output-file"] for the options and how they behave.
}