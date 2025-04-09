#lang racket

(require "structs.rkt" "music.rkt")

(define (write-to-file song-to-write file-path)
  (define output-file (open-output-file file-path #:exists 'replace))
  (define tracks (song-tracks song-to-write))
  (write-bytes-avail 
   (bytes-append (header-bytes (song-header song-to-write) (length tracks))
                 (tracks-bytes tracks))
   output-file)
  (close-output-port output-file))

(define (header-bytes header num-tracks)
  (bytes-append
   #"MThd"
   (integer->integer-bytes 6 4 #f #t)
   (integer->integer-bytes (header-format header) 2 #f #t)
   (integer->integer-bytes num-tracks 2 #f #t)
   (integer->integer-bytes ticks-per-quarter-note 2 #f #t)))

(define (tracks-bytes tracks)
  (apply bytes-append
         (for/list ([track tracks])
           (track-bytes track))))

(define (track-bytes track)
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
                   (bytes #xFF #x2F #x00)]
                  [(set-tempo-event microseconds-per-quarter-note)
                   (bytes-append (bytes #xFF #x51 #x03)
                                 (subbytes
                                  (integer->integer-bytes microseconds-per-quarter-note 4 #f #t)
                                  1))])))

(define (make-variable-length-bytes n)
  (define (accumulator n acc)
    (let ([byte (bitwise-and n #x7F)]
          [remaining (arithmetic-shift n -7)])
      (if (= remaining 0)
          (cons (bitwise-ior byte #x80) acc)
          (accumulator remaining (cons (bitwise-ior byte #x80) acc)))))
  (let ([last-byte (bitwise-and n #x7F)]
        [remaining (arithmetic-shift n -7)])
    (if (= remaining 0)
        (bytes last-byte)
        (list->bytes (accumulator remaining (list last-byte))))))

(module+ test
  (require rackunit)
  (check-equal? (make-variable-length-bytes #x00000000) (bytes #x00))
  (check-equal? (make-variable-length-bytes #x00000040) (bytes #x40))
  (check-equal? (make-variable-length-bytes #x0000007f) (bytes #x7f))
  (check-equal? (make-variable-length-bytes #x00000080) (bytes #x81 #x00))
  (check-equal? (make-variable-length-bytes #x08000000) (bytes #xc0 #x80 #x80 #x00))
  (check-equal? (make-variable-length-bytes #x0fffffff) (bytes #xff #xff #xff #x7f)))

;; Test
(define the-song
  (music (4 4)
       120
       ((measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/2))
        (measure ('D 1/4) ('D 1/4) ('D 1/2))          (measure ('E 1/4) ('G 1/4) ('G 1/2))
        (measure ('E 1/4) ('D 1/4) ('C 1/4) ('D 1/4)) (measure ('E 1/4) ('E 1/4) ('E 1/4) ('E 1/4))
        (measure ('D 1/4) ('D 1/4) ('E 1/4) ('D 1/4)) (measure ('C 1)))))

(write-to-file the-song "test.mid")