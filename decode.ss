#lang racket/load
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

 (load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** my code ***

;;maps letter to its corresponging endcoded cypher letter
(define map_letter
  (lambda (n)
    (lambda (letter)
      (vtl(modulo (+ n (ltv letter)) 26)))))

;;makes a list composed of the number of correct words in a paragraph for each encoding
(define check_paragraph_spelling
  (lambda (p)
    (list
        (count identity(map spell-checker (map (encode-n 0) p)))
        (count identity(map spell-checker (map (encode-n 1) p)))
        (count identity(map spell-checker (map (encode-n 2) p)))
        (count identity(map spell-checker (map (encode-n 3) p)))
        (count identity(map spell-checker (map (encode-n 4) p)))
        (count identity(map spell-checker (map (encode-n 5) p)))
        (count identity(map spell-checker (map (encode-n 6) p)))
        (count identity(map spell-checker (map (encode-n 7) p)))
        (count identity(map spell-checker (map (encode-n 8) p)))
        (count identity(map spell-checker (map (encode-n 9) p)))
        (count identity(map spell-checker (map (encode-n 10) p)))
        (count identity(map spell-checker (map (encode-n 11) p)))
        (count identity(map spell-checker (map (encode-n 12) p)))
        (count identity(map spell-checker (map (encode-n 13) p)))
        (count identity(map spell-checker (map (encode-n 14) p)))
        (count identity(map spell-checker (map (encode-n 15) p)))
        (count identity(map spell-checker (map (encode-n 16) p)))
        (count identity(map spell-checker (map (encode-n 17) p)))
        (count identity(map spell-checker (map (encode-n 18) p)))
        (count identity(map spell-checker (map (encode-n 19) p)))
        (count identity(map spell-checker (map (encode-n 20) p)))
        (count identity(map spell-checker (map (encode-n 21) p)))
        (count identity(map spell-checker (map (encode-n 22) p)))
        (count identity(map spell-checker (map (encode-n 23) p)))
        (count identity(map spell-checker (map (encode-n 24) p)))
        (count identity(map spell-checker (map (encode-n 25) p)))
           )))

;;returns the index a target is at within a list
(define indexing
  (lambda (target)
    (lambda (list)
      (lambda (index)
        (cond ((null? list) 0)
            ((equal? target (car list)) index)
            (else (((indexing target) (cdr list)) (+ index 1))))))))

;;takes index and returns encode-n for that index
(define specify_encoding
   (lambda (n)
     (encode-n n)))

;;counts the number of a particular letter in a list
(define count_letter_in_list
  (lambda (list)
    (lambda (letter)
      (lambda (count)
      (cond ((null? list) count)
          ((equal? (car list) (car letter)) (((count_letter_in_list (cdr list)) letter) (+ count 1)))
           (else (((count_letter_in_list (cdr list)) letter) count)))))))

;;takes in a paragraph, returns a list of the number of times each letter appears in paragraph
(define count_all_letters_in_paragraph
  (lambda (p)
    (list
     (((count_letter_in_list (flatten p)) '(a)) 0)
     (((count_letter_in_list (flatten p)) '(b)) 0)
     (((count_letter_in_list (flatten p)) '(c)) 0)
     (((count_letter_in_list (flatten p)) '(d)) 0)
     (((count_letter_in_list (flatten p)) '(e)) 0)
     (((count_letter_in_list (flatten p)) '(f)) 0)
     (((count_letter_in_list (flatten p)) '(g)) 0)
     (((count_letter_in_list (flatten p)) '(h)) 0)
     (((count_letter_in_list (flatten p)) '(i)) 0)
     (((count_letter_in_list (flatten p)) '(j)) 0)
     (((count_letter_in_list (flatten p)) '(k)) 0)
     (((count_letter_in_list (flatten p)) '(l)) 0)
     (((count_letter_in_list (flatten p)) '(m)) 0)
     (((count_letter_in_list (flatten p)) '(n)) 0)
     (((count_letter_in_list (flatten p)) '(o)) 0)
     (((count_letter_in_list (flatten p)) '(p)) 0)
     (((count_letter_in_list (flatten p)) '(q)) 0)
     (((count_letter_in_list (flatten p)) '(r)) 0)
     (((count_letter_in_list (flatten p)) '(s)) 0)
     (((count_letter_in_list (flatten p)) '(t)) 0)
     (((count_letter_in_list (flatten p)) '(u)) 0)
     (((count_letter_in_list (flatten p)) '(v)) 0)
     (((count_letter_in_list (flatten p)) '(w)) 0)
     (((count_letter_in_list (flatten p)) '(x)) 0)
     (((count_letter_in_list (flatten p)) '(y)) 0)
     (((count_letter_in_list (flatten p)) '(z)) 0))))

;;takes index and returns where 'A' gets mapped to if the index gets mapped to 'E'
(define map_letter_to_e
  (lambda (index)
    (modulo(+ 26 (- 4 index)) 26)
    ))
     
;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)

    ;; *** my code ***
    (cond ((member w dictionary) #t)
          (else #f))))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded

      ;; *** my code ***
      (map  
         (map_letter n) w))))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    
;; *** my code ***
    (cond ((null? d) '())
       (else (append (cons (map encoder(car d)) '())
                     (encode-d (cdr d) encoder))))))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    
;; *** my code ***
    (specify_encoding
     (((indexing (apply max(check_paragraph_spelling p))) (check_paragraph_spelling p)) 0))))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)

    ;; *** my code ***
    (specify_encoding
     (map_letter_to_e
     (((indexing (apply max(count_all_letters_in_paragraph p))) (count_all_letters_in_paragraph p)) 0)))
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)

    ;; *** my code ***
    (cond ((null? d) '())
       (else (append(cons (map decoder(car d)) '())
                    (Code-Breaker(cdr d) decoder))))))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)
