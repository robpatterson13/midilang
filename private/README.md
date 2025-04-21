# Files

- constants.rkt: This file contains the constants used throughout the codebase.
- preds.rkt: This file contains the predicates used for checks.
- structs.rkt: This file contains most runtime structs for storing internal data.
- beat.rkt: This file contains the time-signature struct, representing a time signature.
- notes.rkt: This file contains support for converting musical notes into MIDI pitches.
- music.rkt: This file contains the `music` macro and its supporting functions and structs. It also contains helper functions that convert syntax to runtime structs.
- file-io.rkt: This file contains the `write-to-midi-file` procedure and its supporting functions.
- example.rkt: This file contains example usage of `music` and `write-to-midi-file`.
- *.mid: These are example MIDI files constructed through example.rkt.