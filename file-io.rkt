#lang racket

(require "structs.rkt" "music.rkt")

(define (write-to-file song-to-write file-path)
  (define output-file (open-output-file file-path))
  (define tracks (song-tracks song-to-write))
  (write-header (song-header song-to-write) (length tracks) output-file)
  (write-tracks tracks output-file)
  (close-output-port output-file))

(define (write-header header num-tracks output-file)
  (define to-write
    (bytes-append #"MThd"
                  (integer->integer-bytes 6 4 #f #t)
                  (integer->integer-bytes (header-format header) 2 #f #t)
                  (integer->integer-bytes num-tracks 2 #f #t)
                  (integer->integer-bytes 96 2 #f #t))) ; change to reflect tempo and time signature
  (write-bytes-avail to-write output-file))

(define (write-tracks tracks output-file)
  (for ([track tracks])
    (write-track track output-file)))

(define (write-track track output-file)
  (define event-bytes
    (apply bytes-append
           (for/list ([event (midi-track-mtrk-events track)])
             (event->bytes event))))
  (bytes-append #"MTrk"
                (integer->integer-bytes (bytes-length event-bytes) 4 #f #t)
                event-bytes))

(define (event->bytes the-event)
  (bytes-append (make-variable-length-bytes (mtrk-event-delta-time the-event))
                (match (mtrk-event-event the-event)
                  [(note-on-event note velocity channel)
                   (bytes (+ #b10010000 channel) note velocity)]
                  [(note-off-event note velocity channel)
                   (bytes (+ #b10000000 channel) note velocity)]
                  [(text-event text)
                   (bytes-append (bytes #xFF #x01)
                                 (make-variable-length-bytes (string-length text))
                                 (string->bytes/latin-1 text))]
                  [(end-of-track-event)
                   (bytes #xFF #x2F #x00)])))

(define (make-variable-length-bytes n)
  (define (accumulator n acc)
    (let ([byte (bitwise-and n #x7F)]
          [remaining (arithmetic-shift n -7)])
      (if (= remaining 0)
          (cons byte acc)
          (accumulator remaining (cons (bitwise-ior byte #x80) acc)))))
  (list->bytes (accumulator n '())))

;; Test
(define the-song
  (music (4 4)
       120
       ((measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/2))
        (measure ('D 1/4) ('D 1/4) ('D 1/2))          (measure ('E 1/4) ('G 1/4) ('G 1/2))
        (measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/4) ('E 1/4))
        (measure ('D 1/4) ('D 1/4) ('E 1/4) ('D 1/4)) (measure ('C 1)))))

(write-to-file the-song "test.mid")