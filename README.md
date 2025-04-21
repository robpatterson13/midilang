# midilang

The Musically Impractical DSL Implementation (MIDI, or midilang for long)
is a Domain-Specific Language for transcribing music and converting it into
Musical Instrument Digital Interface (MIDI) format.


The syntax is designed to share concepts with musical notation for ease of use by musicians.
For example, it contains concepts such as notes and measures.
The underlying implementation is designed to mimic MIDI format with tracks, events, and headers.

Check [here](private/README.md) for more technical documentation.

## Example

```
(define mary-had-a-little-lamb
  (music (4 4)
       120
       ((measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4))
        (measure ('E 1/4) ('E 1/4) ('E 1/2))
        (measure ('D 1/4) ('D 1/4) ('D 1/2))
        (measure ('E 1/4) ('G 1/4) ('G 1/2))
        (measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4))
        (measure ('E 1/4) ('E 1/4) ('E 1/4) ('E 1/4))
        (measure ('D 1/4) ('D 1/4) ('E 1/4) ('D 1/4))
        (measure ('C 1)))))

(write-to-midi-file mary-had-a-little-lamb "mary-had-a-little-lamb.mid")
```

This defines a 4/4 song at 120 beats per minute with the given measures.
The last line writes the song to the given MIDI file.

## Installation and running

To install, change directory into this Git repository and run
`raco pkg install`

Then import as
`(require midilang)`