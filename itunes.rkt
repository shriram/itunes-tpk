#lang racket

(require xml)

(define (is-a-dict-elt? v)
  (and (element? v) (eq? (element-name v) 'dict)))

(define (find-first-dict-in-elt elt)
  (let loop ([contents (element-content elt)])
    (if (is-a-dict-elt? (first contents))
        (first contents)
        (loop (rest contents)))))

(define (get-pcdatum elt)
  (pcdata-string
   (first ;; only one element
    (element-content elt))))

(define (convert-to-hash dict-elt)
  (unless (is-a-dict-elt? dict-elt)
    (error 'find-key-in-dict "expected dict element but found ~a" dict-elt))
  (let ([h (make-hash)])
    (let loop ([cs (element-content dict-elt)])
      (if (empty? cs)
          h
          (if (and (element? (first cs))
                   (eq? (element-name (first cs)) 'key)
                   (element? (second cs)))
              (begin
                (hash-set! h
                           (get-pcdatum (first cs))
                           (second cs))
                (loop (rest (rest cs))))
              (loop (rest cs)))))))

;; assumes everything is bound to a pcdata; this isn't true for purchased, etc. 
(define (iTunes->structs filename constructor field-names)

  (define raw-input (read-xml (open-input-file filename)))

  (define library-dict
    (find-first-dict-in-elt (find-first-dict-in-elt (document-element raw-input))))
  
  (define song-dicts (filter is-a-dict-elt? (element-content library-dict)))
  
  (define hashes (map convert-to-hash song-dicts))
  
  (let loop ([hs hashes])
    (if (empty? hs)
        empty
        (let ([h (first hs)])
          (cons
           (apply constructor (map (lambda (f)
                                     (let ([v (hash-ref h f (lambda () false))])
                                       (and v (get-pcdatum v))))
                                   field-names))
           (loop (rest hs)))))))

(provide iTunes->structs)

#|
			<key>Track ID</key><integer>816</integer>
			<key>Name</key><string>I Feel the Earth Move</string>
			<key>Artist</key><string>Carole King</string>
			<key>Album Artist</key><string>Carole King</string>
			<key>Composer</key><string>Carole King</string>
			<key>Album</key><string>Tapestry</string>
			<key>Genre</key><string>Rock</string>
			<key>Kind</key><string>Protected AAC audio file</string>
			<key>Size</key><integer>2973008</integer>
			<key>Total Time</key><integer>180115</integer>
			<key>Disc Number</key><integer>1</integer>
			<key>Disc Count</key><integer>1</integer>
			<key>Track Number</key><integer>1</integer>
			<key>Track Count</key><integer>14</integer>
			<key>Year</key><integer>1971</integer>
			<key>Date Modified</key><date>2006-08-31T12:48:39Z</date>
			<key>Date Added</key><date>2004-04-26T13:09:59Z</date>
			<key>Bit Rate</key><integer>128</integer>
			<key>Sample Rate</key><integer>44100</integer>
			<key>Play Count</key><integer>17</integer>
			<key>Play Date</key><integer>3344263590</integer>
			<key>Play Date UTC</key><date>2009-12-21T22:06:30Z</date>
			<key>Artwork Count</key><integer>1</integer>
			<key>Persistent ID</key><string>DC5606A65B15B646</string>
			<key>Track Type</key><string>File</string>
			<key>Protected</key><true/>
			<key>Purchased</key><true/>
			<key>Location</key><string>file://localhost/C:/Users/sk/.../Tapestry/01%20I%20Feel%20the%20Earth%20Move.m4p</string>
			<key>File Folder Count</key><integer>5</integer>
			<key>Library Folder Count</key><integer>1</integer>
|#