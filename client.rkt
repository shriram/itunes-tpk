;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname itunes-client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require "itunes.rkt")

(define-struct song (title album last-play play-count))












(define my-collection
  (iTunes->structs                  ;; provided by library
   "iTunes.xml"                     ;; name of actual iTunes XML file
   make-song                        ;; your structure's constructor
   (list                            ;; names of iTunes fields
    "Name"
    "Album"
    "Play Date UTC"
    "Play Count"))) 


















(define (num-or-zero n)
  (let ([n (if (string? n) (string->number n) n)])
    (if (number? n)
        n
        (if (false? n)
            0
            (error 'num-or-zero "neither number nor zero" n)))))

(define sorted-counts
  (sort my-collection
        (lambda (s1 s2)
          (let ([n1 (num-or-zero (song-play-count s1))]
                [n2 (num-or-zero (song-play-count s2))])
            (< n1 n2)))))



#|

(define sorted-title-lengths (sort (map string-length (map song-title my-collection)) <=))

(define (uniq-counts sorted-nums)
  (cond
    [(empty? sorted-nums) empty]
    [(cons? sorted-nums) (runs (first sorted-nums) 1 (rest sorted-nums))]))
(define (runs n n-count rest-n)
  (cond
    [(empty? rest-n) (list (list n n-count))]
    [(cons? rest-n) (if (= (first rest-n) n)
                        (runs n (add1 n-count) (rest rest-n))
                        (cons (list n n-count)
                              (uniq-counts rest-n)))]))

(uniq-counts sorted-title-lengths)

(define (find-songs-of-length n)
  (filter (lambda (s) (= n (string-length (song-title s))))
          my-collection))

(find-songs-of-length 92)
(find-songs-of-length 62)
(find-songs-of-length 3)
(find-songs-of-length 2)

|#