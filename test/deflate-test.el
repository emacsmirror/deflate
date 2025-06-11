;;; deflate-test.el --- Tests for the DEFLATE compression algorithm implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Carlo Sciolla

;; Author: Carlo Sciolla <carlo.sciolla@gmail.com>
;; Keywords: deflate, compression, algorithm, zlib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash)

;; ---- Utility functions ----

(defun deflate/code-comparator (a b)
  "Compares Huffman codes A and B provided as cons of (symbol . code)."
  (< (car a) (car b)))

(ert-deftest deflate-test/bitwise-operation ()
  "Test correct transformation of bits to bytes and viceversa."
  (let* ((original-string "test")
         (original-bytes (string-to-list original-string))
         (bits (deflate--bytes-to-bits original-bytes))
         (bytes (deflate--bits-to-bytes bits)))
    (should (equal bytes original-bytes))))

(ert-deftest deflate-test/bits-to-bytes ()
  "Test the translation from a bitstream to bytes."
  ;; empty list
  (should (equal '() (deflate--bits-to-bytes '())))
  ;; less than a byte
  (should (equal '(2) (deflate--bits-to-bytes '(0 1))))
  ;; more than a byte, not byte aligned
  (should (equal '(2 3) (deflate--bits-to-bytes '(0 1 0 0 0 0 0 0
                                                                1 1))))
  ;; two bytes
  (should (equal '(2 255) (deflate--bits-to-bytes '(0 1 0 0 0 0 0 0
                                                                  1 1 1 1 1 1 1 1))))
  ;; more than two bytes
  (should (equal '(2 255 4) (deflate--bits-to-bytes '(0 1 0 0 0 0 0 0
                                                                    1 1 1 1 1 1 1 1
                                                                    0 0 1)))))

(ert-deftest deflate-test/find-matches ()
  "Test finding the right match given a string and a position."
  ;; note that the min amount of match chars is 3
  (let ((data (string-to-list "Oneone oneone twotwo twotwo")))
    ;; the first 6 characters have no match
    (dolist (pos (-iota 7))
      (should (equal nil (deflate--find-match data pos))))
    (let ((pos 7))
      ;; matches with `one'
      (should (equal '(3 4) (deflate--find-match data pos))))
    (let ((pos 8))
      ;; matches with `neone '
      (should (equal '(6 7) (deflate--find-match data pos))))
    (let ((pos 24)) ;; length - 3
      ;; matches with `two'
      (should (equal '(3 10) (deflate--find-match data pos))))))

(ert-deftest deflate-test/length-codes ()
  "Test deriving length codes."
  ;; lengths of 3..10 encode as 257 + (length - 3) with 0 extra bits
  (should (equal '(257 . (0 . 0))
                 (deflate--get-length-code 3)))
  (should (equal '(264 . (0 . 0))
                 (deflate--get-length-code 10)))
  ;; lengths of 11-12 encode as 265 + one extra bit
  (should (equal '(265 . (1 . 0))
                 (deflate--get-length-code 11)))
  (should (equal '(265 . (1 . 1))
                 (deflate--get-length-code 12)))
  ;; max match (length 258) encodes as 285 with no extra bits
  (should (equal '(285 . (0 . 0))
                 (deflate--get-length-code deflate--max-match))))

(ert-deftest deflate-test/distance-codes ()
  "Test deriving distance codes."
  ;; distances of 1..4 encode as distance - 1 with no extra bits
  (should (equal '(0 . (0 . 0))
                 (deflate--get-distance-code 1)))
  (should (equal '(3 . (0 . 0))
                 (deflate--get-distance-code 4)))
  ;; distances of 5-6 encode as 4 + one extra bit
  (should (equal '(4 . (1 . 0))
                 (deflate--get-distance-code 5)))
  (should (equal '(4 . (1 . 1))
                 (deflate--get-distance-code 6)))
  ;; max distance (distance 32768) encodes as 29 + 13 extra bits
  (should (equal '(29 . (13 . 8191)) ;; 2^13 - 1
                 (deflate--get-distance-code deflate--window-size))))

(ert-deftest deflate-test/lz77-compress ()
  "Test compressing a string with LZ77."
  (let ((data (string-to-list "Oneone oneone twotwo twotwo"))
        (expected (append '(79 110 101 111 110 101 32) ;; (string-to-list "Oneone ")
                          (list '(3 4))                ;; match with `one'
                          (list '(4 7))                ;; match with `one\ '
                          '(116 119 111)               ;; (string-to-list "two")
                          (list '(3 3))                ;; match with `two'
                          (list '(7 7))                ;; match with `\ twotwo'
                          )))
    (should (equal expected (deflate--lz77-compress data)))))

(ert-deftest deflate-test/freq-table ()
  "Test generation of frequency tables."
  (let* ((data (string-to-list "Oneone oneone twotwo twotwo"))
         (tokens (deflate--lz77-compress data))
         (result (deflate--build-frequency-table tokens)))
    ;; note: you can follow along whe freq table with the value at the expectation at `deflate-test/lz77-compress'
    (should (equal (-sort
                    #'deflate/code-comparator
                    `((79 . 1) (110 . 2) (101 . 2) (111 . 2) (32 . 1)
                      (,(car (deflate--get-length-code 3)) . 2)
                      (,(car (deflate--get-length-code 4)) . 1)
                      (116 . 1) (119 . 1)
                      (,(car (deflate--get-length-code 7)) . 1)
                      (256 . 1) ;; EOF
                      ))
                   (-sort
                    #'deflate/code-comparator
                    (gethash 'literal-length result))))
    (should (equal (-sort
                    #'deflate/code-comparator
                    `((,(car (deflate--get-distance-code 4)) . 1)
                      (,(car (deflate--get-distance-code 3)) . 1)
                      (,(car (deflate--get-distance-code 7)) . 2)))
                   (-sort
                    #'deflate/code-comparator
                    (gethash 'distance result))))))

(ert-deftest deflate-test/huffman-tree ()
  "Test Huffman tree generation."
  (let* ((data (string-to-list "Oneone oneone twotwo twotwo"))
         (tokens (deflate--lz77-compress data))
         (freq-table (deflate--build-frequency-table tokens))
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table))))
    (should (equal '(((((((79 . 1) ;; depth 3
                          (((258 . 1) ;; depth 4
                            (32 . 1)) ;; depth 4
                           . 2)) ;; depth 3
                         . 3)  ;; depth 2
                        (((101 . 2)  ;; depth 3
                          (110 . 2)) ;; depth 3
                         . 4)) ;; depth 2
                       . 7) ;; depth 1
                      (((((257 . 2)  ;; depth 3
                          (111 . 2)) ;; depth 3
                         . 4)  ;; depth 2
                        (((((119 . 1)  ;; depth 4
                            (116 . 1)) ;; depth 4
                           . 2) ;; depth 3
                          (((256 . 1)  ;; depth 4
                            (261 . 1)) ;; depth 4
                           . 2)) ;; depth 3
                         . 4)) ;; depth 2
                       . 8)) ;; depth 1
                     . 15) ;; depth 0
                   ll-huff-tree))
    (should (equal '(((((2 . 1)  ;; depth 2
                        (3 . 1)) ;; depth 2
                       . 2) ;; depth 1
                      (5 . 2)) ;; depth 1
                     . 4) ;; depth 0
                   dd-huff-tree))))

(ert-deftest deflate-test/huffman-code-lengths-simple ()
  "Test Huffman code generation."
  (let* ((data (string-to-list "One"))
         (tokens (deflate--lz77-compress data))               ;; no compression as there is no repetition
         (freq-table (deflate--build-frequency-table tokens)) ;; no distance, only literal/length
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table))))
    ;; the frequency table for literal length is:
    ;; ((256 . 1) (101 . 1) (110 . 1) (79 . 1))
    ;;
    ;; the tree is as follows:
    ;;        (*)
    ;;       /   \
    ;;     (*)   (*)
    ;;    /   \  /   \
    ;; 110   79 256 101
    (should (equal (-sort
                    #'deflate/code-comparator
                    '((79  . 2)
                      (101 . 2)
                      (110 . 2)
                      (256 . 2)))
                   (-sort
                    #'deflate/code-comparator
                    (deflate--build-huffman-code-lengths ll-huff-tree))))))

(ert-deftest deflate-test/huffman-code-lengths ()
  "Test Huffman code lengths generation."
  (let* ((data (string-to-list "Oneone oneone twotwo twotwo"))
         (tokens (deflate--lz77-compress data))
         (freq-table (deflate--build-frequency-table tokens))
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table))))
    ;; should be able to follow along with the depths witten in the `deflate-test/huffman-tree' test
    (should (equal (-sort
                    #'deflate/code-comparator
                    '((79 . 3)
                      (258 . 4)
                      (32 . 4)
                      (101 . 3)
                      (110 . 3)
                      (257 . 3)
                      (111 . 3)
                      (119 . 4)
                      (116 . 4)
                      (256 . 4)
                      (261 . 4)))
                   (-sort
                    #'deflate/code-comparator
                    (deflate--build-huffman-code-lengths ll-huff-tree))))
    (should (equal (-sort
                    #'deflate/code-comparator
                    '((2 . 2)
                      (3 . 2)
                      (5 . 1)))
                   (-sort
                    #'deflate/code-comparator
                    (deflate--build-huffman-code-lengths dd-huff-tree))))))

(ert-deftest deflate-test/huffman-codes ()
  "Test huffman code generation."
  (let* ((data (string-to-list "Oneone oneone twotwo twotwo"))
         (tokens (deflate--lz77-compress data))
         (freq-table (deflate--build-frequency-table tokens))
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table)))
         (ll-code-lengths (deflate--build-huffman-code-lengths ll-huff-tree))
         (dd-code-lengths (deflate--build-huffman-code-lengths dd-huff-tree)))
    ;; canonical Huffman codes use adjacent numbers for same-lengths, sorted by token
    (should (equal (-sort
                    #'deflate/code-comparator
                    '(;; code length 3
                      (79  . (0 . 3))
                      (101 . (1 . 3))
                      (110 . (2 . 3))
                      (111 . (3 . 3))
                      (257 . (4 . 3))
                      ;; code length 4
                      (32  . (10 . 4))
                      (116 . (11 . 4))
                      (119 . (12 . 4))
                      (256 . (13 . 4))
                      (258 . (14 . 4))
                      (261 . (15 . 4))))
                   (-sort
                    #'deflate/code-comparator
                    (let ((codes (deflate--assign-huffman-codes ll-code-lengths))
                          (codes-alist '()))
                      (maphash (lambda (k v)
                                 (push (cons k v) codes-alist)) codes)
                      codes-alist))))))

(ert-deftest deflate-test/number->bits ()
  "Test converting a number into a list of bits."
  (should (equal '(0 0 0)
                 (deflate--number->bits 0 3)))
  (should (equal '(1 0)
                 (deflate--number->bits 2 2)))
  (should (equal '(1 1 1 1 1)
                 (deflate--number->bits 31 5))))

(ert-deftest deflate-test/simple-huffman-encode ()
  "Test Huffman encoding."
  ;; start with a simple version that is easy to follow along
  (let* ((data (string-to-list "O"))
         (eob 256)
         (tokens (deflate--lz77-compress data))
         (freq-table (deflate--build-frequency-table tokens))
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table)))
         (ll-code-lengths (deflate--build-huffman-code-lengths ll-huff-tree))
         (dd-code-lengths (deflate--build-huffman-code-lengths dd-huff-tree))
         (ll-huff-code (deflate--assign-huffman-codes ll-code-lengths))
         (dd-huff-code (deflate--assign-huffman-codes dd-code-lengths)))
    ;; In such a simple example, we only have 2 tokens both of which havinng a code len of 1:
    ;; - O -- ie token 79, given the code of 0
    ;; - EOB -- ie token 256, given the code of 1
    (should (equal '(((code        . 0)
                      (code-length . 1)))
                   (deflate--huffman-encode-token (car data) ll-huff-code dd-huff-code)))
    (should (equal '(((code        . 1)
                      (code-length . 1)))
                   (deflate--huffman-encode-token eob ll-huff-code dd-huff-code)))))

(ert-deftest deflate-test/huffman-encode ()
  "Test Huffman encoding."
  (let* ((data (string-to-list "Oneone oneone twotwo twotwo"))
         (tokens (deflate--lz77-compress data))
         (freq-table (deflate--build-frequency-table tokens))
         (ll-huff-tree (deflate--build-huffman-tree (gethash 'literal-length freq-table)))
         (dd-huff-tree (deflate--build-huffman-tree (gethash 'distance freq-table)))
         (ll-code-lengths (deflate--build-huffman-code-lengths ll-huff-tree))
         (dd-code-lengths (deflate--build-huffman-code-lengths dd-huff-tree))
         (ll-huff-code (deflate--assign-huffman-codes ll-code-lengths))
         (dd-huff-code (deflate--assign-huffman-codes dd-code-lengths)))
    ;; you can try to follow along the values at `deflate-test/lz77-compress'
    (should (equal '(;; O
                     ((code        . 0)
                      (code-length . 3))

                     ;; n
                     ((code        . 2)
                      (code-length . 3))

                     ;; e
                     ((code        . 1)
                      (code-length . 3))

                     ;; o
                     ((code        . 3)
                      (code-length . 3))

                     ;; n
                     ((code        . 2)
                      (code-length . 3))

                     ;; e
                     ((code        . 1)
                      (code-length . 3))

                     ;; \space
                     ((code        . 10)
                      (code-length . 4))

                     ;; length: 3
                     ((code             . 4)
                      (code-length      . 3)
                      (num-extra-bits   . 0)
                      (extra-bits-value . 0))

                     ;; distance: 4
                     ((code             . 3)
                      (code-length      . 2)
                      (num-extra-bits   . 0)
                      (extra-bits-value . 0))

                     ;; length: 4
                     ((code             . 14)
                      (code-length      . 4)
                      (num-extra-bits   . 0)
                      (extra-bits-value . 0))

                     ;; distance: 7
                     ((code             . 0)
                      (code-length      . 1)
                      (num-extra-bits   . 1)
                      (extra-bits-value . 0))

                     ;; t
                     ((code        . 11)
                      (code-length . 4))

                     ;; w
                     ((code        . 12)
                      (code-length . 4))

                     ;; o
                     ((code        . 3)
                      (code-length . 3))

                     ;; length: 3

                   ((code             . 4)
                    (code-length      . 3)
                    (num-extra-bits   . 0)
                    (extra-bits-value . 0))

                   ;; distance: 3
                   ((code             . 2)
                    (code-length      . 2)
                    (num-extra-bits   . 0)
                    (extra-bits-value . 0))

                   ;; length: 7
                   ((code             . 15)
                    (code-length      . 4)
                    (num-extra-bits   . 0)
                    (extra-bits-value . 0))

                   ;; distance: 7
                   ((code             . 0)
                    (code-length      . 1)
                    (num-extra-bits   . 1)
                    (extra-bits-value . 0)))
                   (-mapcat (lambda (token)
                           (deflate--huffman-encode-token token ll-huff-code dd-huff-code))
                         tokens)))))

(ert-deftest deflate-test/huffman-code-leghts-encoding ()
  "Test the RLE encoding of code lengths."
  ;; no repetitions
  (should (equal '((1 . (0 . 0))
                   (2 . (0 . 0))
                   (3 . (0 . 0)))
                 (deflate--encode-code-lengths-to-alphabet '(1 2 3))))
  ;; 4 times 1 -> one repetition
  (should (equal '((1  . (0 . 0))
                   (16 . (2 . 0)))
                 (deflate--encode-code-lengths-to-alphabet (-repeat 4 1))))
  ;; 3 times 1 -> no repetition
  (should (equal '((1 . (0 . 0))
                   (1 . (0 . 0))
                   (1 . (0 . 0)))
                 (deflate--encode-code-lengths-to-alphabet (-repeat 3 1))))
  ;; 3 times 0 -> one repetition
  (should (equal '((17 . (3 . 0)))
                 (deflate--encode-code-lengths-to-alphabet (-repeat 3 0))))
  ;; 4 times 0 -> one repetition
  (should (equal '((17 . (3 . 1)))
                 (deflate--encode-code-lengths-to-alphabet (-repeat 4 0))))
  ;; 11 times 0 -> one repetition
  (should (equal '((18 . (7 . 0)))
                 (deflate--encode-code-lengths-to-alphabet (-repeat 11 0))))
  ;; putting it all together
  (let ((code-lengths (append '(1 2 3)       ;; literals
                              (-repeat 13 4) ;; more than one repetition of non-zero
                              (-repeat 200 0) ;; more than one repetitin of zeroes
                              '(1)            ;; literals
                              )))
    (should (equal '((1  . (0 . 0))
                     (2  . (0 . 0))
                     (3  . (0 . 0))
                     (4  . (0 . 0))
                     (16 . (2 . 3))
                     (16 . (2 . 3))
                     (18 . (7 . 127)) ;; 138 - 11, max reps. remaining: 200 - 138 = 62
                     (18 . (7 . 51))  ;; 62 - 11
                     (1  . (0 . 0)))
                   (deflate--encode-code-lengths-to-alphabet code-lengths)))))

(ert-deftest deflate-test/hlit ()
  "Test calculating HLIT base length."
  (let ((min-hlit 257))
    ;; we start with a short literal/lengths code lengths array, we should get the minimum imposed by the spec
    (let* ((non-zero-lengths 10)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq min-hlit (deflate--calculate-hlit-length cl-lengths))))
    ;; we then go exactly at the minimum
    (let* ((non-zero-lengths min-hlit)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq min-hlit (deflate--calculate-hlit-length cl-lengths))))
    ;; then we go above (this is above the maximum, but we don't care for now
    (let* ((non-zero-lengths 300)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq non-zero-lengths (deflate--calculate-hlit-length cl-lengths)))))
  ;; now we calculate HLIT for the exact values coming out of the string "Oneone oneone twotwo twotwo"
  ;; `ll-code-lengths-array' represents the code lenghts for literal/lengths whose symbols are the indexes of such array.
  ;; DEFLATE mandates to trnsmit at least all literals, hence the humongousness of the array here
  (let ((ll-code-lenghts-array (apply #'vector (append (-repeat 32 0)
                                                       '(4)
                                                       (-repeat 46 0)
                                                       '(3)
                                                       (-repeat 21 0)
                                                       '(3)
                                                       (-repeat 8 0)
                                                       '(3 3)
                                                       (-repeat 4 0)
                                                       '(4)
                                                       (-repeat 2 0)
                                                       '(4)
                                                       (-repeat 136 0)
                                                       '(4) '(3)
                                                       '(4)
                                                       (-repeat 2 0)
                                                       '(4)
                                                       (-repeat 24 0))))
        (expected-hlit-length 262))
    (should (eq expected-hlit-length
                (deflate--calculate-hlit-length ll-code-lenghts-array)))))

(ert-deftest deflate-test/hdist ()
  "Test calculating HDIST base length."
  (let ((min-hdist 1))
    ;; we start with an empty distances code lengths array, we should get the minimum imposed by the spec
    (let* ((non-zero-lengths 0)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq min-hdist (deflate--calculate-hdist-length cl-lengths))))
    ;; we then go exactly at the minimum
    (let* ((non-zero-lengths min-hdist)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq min-hdist (deflate--calculate-hdist-length cl-lengths))))
    ;; then we go above (this is above the maximum, but we don't care for now
    (let* ((non-zero-lengths 20)
           (cl-lengths (apply #'vector (append (-repeat non-zero-lengths 1) (-repeat 100 0)))))
      (should (eq non-zero-lengths (deflate--calculate-hdist-length cl-lengths)))))
  ;; now we calculate HDIST for the exact values coming out of the string "Oneone oneone twotwo twotwo"
  ;; `dd-code-lenghts-array' represents the code lenghts for all possible distances, which are the indices of such array:
  ;;                                                1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
  ;;                            0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
  (let ((dd-code-lengths-array [0 0 2 2 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
        ;; HDIST length is a length, not an index: highest non-zero index above is 5, so we get a 6
        (expected-hdist-length 6))
    (should (eq expected-hdist-length
                (deflate--calculate-hdist-length dd-code-lengths-array)))))

(ert-deftest deflate-test/hclen ()
  "Test calculating HCLEN base length."
  (let ((min-hclen 4)
        (max-hclen 19))
    ;; we start with an empty meta code lengths array, we should get zero
    (let* ((non-zero-lengths 0)
           (code-lengths-array (make-vector 19 0))
           (cl-lengths code-lengths-array))
      (should (eq min-hclen (deflate--calculate-hclen-length cl-lengths))))
    ;; then we go at the maximum
    (let* ((non-zero-lengths 19)
           (code-lengths-array (make-vector 19 1)))
      (should (eq max-hclen (deflate--calculate-hclen-length code-lengths-array))))
    ;; then we go somewhere in between
    (let* ((non-zero-lengths 6)
           ;; find the expected amount of code lengths codes by looking at the highest index symbol in the custom order
           (lengths-index (apply #'max (-map (lambda (x)
                                               (seq-position deflate--code-lengths-order x))
                                             (number-sequence 0 (1- non-zero-lengths)))))
           (code-lengths-array (apply #'vector (append (-repeat non-zero-lengths 1)
                                                       (-repeat (- 19 non-zero-lengths) 0)))))
      (should (eq (1+ lengths-index) (deflate--calculate-hclen-length code-lengths-array)))))
  ;; now we calculate HCLEN for the exact values coming out of the string "Oneone oneone twotwo twotwo"
  ;; `code-lenghts-aray' provides the code lenghts for the symbols from the code lenghts special alphabet, which are the
  ;; indices of such array:
  ;;                                            1 1 1 1 1 1 1 1 1
  ;;                        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
  (let* ((cl-lengths-array [2 5 5 2 2 0 0 0 0 0 0 0 0 0 0 0 0 4 3])
         ;; in the custom ordering of the alphabet, the code length symbol with the highest index is 1 (index 17)
         ;; since we are calculating a length and not an index we get an 18
         (expected-hclen-length 18))
    (should (eq expected-hclen-length
                (deflate--calculate-hclen-length cl-lengths-array)))))

(ert-deftest deflate-test/header ()
  "Test writing the DEFLATE header into a bitstream."
  ;; The values are taken from the compression of the string "Oneone oneone twotwo twotwo"
  (let* ((hlit-length 262)
         (hdist-length 6)
         (hclen-length 18)

         (bitstream '())

         (hlit (- hlit-length 257)) ;; 5 bits for the value 5
         (hdist (- hdist-length 1)) ;; 5 bits for the value 5
         (hclen (- hclen-length 4)) ;; 4 bits for the value 14
         ;; cl-lengths-array should be of 19 entries
         (cl-lengths-array [2 5 5 2 2 0 0 0 0 0 0 0 0 0 0 0 0 4 3])
         (cl-lengths-array-ordered (apply #' vector
                                             (mapcar (lambda (i) (aref cl-lengths-array i))
                                                     deflate--code-lengths-order)))

         (result (deflate--write-dynamic-header bitstream t hlit hdist hclen cl-lengths-array))
         (header-length (+ 1 ;; BFINAL
                           2 ;; block type
                           5 ;; HLIT
                           5 ;; HDIST
                           4 ;; HCLEN
                           ))
         (header (-take header-length result))
         (cl-lenghts-codes (-drop header-length result))

         (expected-header (append '(1)                                                          ;; BFINAL
                                  (seq-reverse '(1 0))                                          ;; dynamic Huffman
                                  (seq-reverse (deflate--number->bits hlit 5))    ;; HLIT
                                  (seq-reverse (deflate--number->bits hdist 5))   ;; HDIST
                                  (seq-reverse (deflate--number->bits hclen 4)))) ;; HCLEN

         ;; we expect `hclen-length' code lenghts, 3 bits each
         (expected-cl-lengths-codes (--mapcat (seq-reverse (deflate--number->bits
                                                            (aref cl-lengths-array-ordered it) 3))
                                              ;; we decrease `hclen-length' as we need an index instead of a count
                                              (number-sequence 0 (1- hclen-length)))))
    (should (equal expected-header header))
    (should (equal expected-cl-lengths-codes cl-lenghts-codes))))

(ert-deftest deflate-test/write-huffman ()
  "Test writing a Huffman code."
  ;; first with no extra bits
  (should (equal '(0 0 1)
                 (deflate--write-huffman-code '() ;; bitstream
                                                            1   ;; code -- better to use non-palyndrome codes to check ordering
                                                            3   ;; code-length
                                                            nil ;; num-extra-bits
                                                            nil ;; extra-bits-value
                                                            )))
  ;; then with extra bits
  (should (equal '(0 0 1 1 0 0)
                 (deflate--write-huffman-code '() ;; bitstream
                                                            1   ;; code
                                                            3   ;; code-length
                                                            3   ;; num-extra-bits
                                                            1   ;; extra-bits-value
                                                            ))))

(ert-deftest deflate-test/code-lengths-codes ()
  "Test writing the meta code lengths into the bitstream."
  (let* (;; the following values have been manually calculated on the REPL for the starting string of:
         ;; `Oneone oneone twotwo twotwo'
         ;; Note that the amount of data to output is long as we require at least 257 codes for lit/len, which in our
         ;; case include lots of zeroes (see the 18 / 17 codes for 0 repeats)
         (cl-encoded '((18 . (7 . 21))
                       (4  . (0 . 0))
                       (18 . (7 . 35))
                       (3  . (0 . 0))
                       (18 . (7 . 10))
                       (3  . (0 . 0))
                       (17 . (3 . 5))
                       (3  . (0 . 0))
                       (3  . (0 . 0))
                       (17 . (3 . 1))
                       (4  . (0 . 0))
                       (0  . (0 . 0))
                       (0  . (0 . 0))
                       (4  . (0 . 0))
                       (18 . (7 . 125))
                       (4  . (0 . 0))
                       (3  . (0 . 0))
                       (4  . (0 . 0))
                       (0  . (0 . 0))
                       (0  . (0 . 0))
                       (4  . (0 . 0))
                       (0  . (0 . 0))
                       (0  . (0 . 0))
                       (2  . (0 . 0))
                       (2  . (0 . 0))
                       (0  . (0 . 0))
                       (1  . (0 . 0))))
         (expected (append
                    ;; code for 18, 7 extra bits for 21
                    (deflate--number->bits 6 3)
                    (seq-reverse (deflate--number->bits 21 7))
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 18, 7 extra bits for 35
                    (deflate--number->bits 6 3)
                    (seq-reverse (deflate--number->bits 35 7))
                    ;; code for 3, 0 extra bits
                    (deflate--number->bits 1 2)
                    ;; code for 18, 7 extra bits for 10
                    (deflate--number->bits 6 3)
                    (seq-reverse(deflate--number->bits 10 7))
                    ;; code for 3, 0 extra bits
                    (deflate--number->bits 1 2)
                    ;; code for 17, 3 extra bits for 5
                    (deflate--number->bits 14 4)
                    (seq-reverse(deflate--number->bits 5 3))
                    ;; code for 3, 0 extra bits
                    (deflate--number->bits 1 2)
                    ;; code for 3, 0 extra bits
                    (deflate--number->bits 1 2)
                    ;; code for 17, 3 extra bits for 1
                    (deflate--number->bits 14 4)
                    (seq-reverse(deflate--number->bits 1 3))
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 18, 7 extra bits for 125
                    (deflate--number->bits 6 3)
                    (seq-reverse(deflate--number->bits 125 7))
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 3, 0 extra bits
                    (deflate--number->bits 1 2)
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 4, 0 extra bits
                    (deflate--number->bits 2 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 2, 0 extra bits
                    (deflate--number->bits 31 5)
                    ;; code for 2, 0 extra bits
                    (deflate--number->bits 31 5)
                    ;; code for 0, 0 extra bits
                    (deflate--number->bits 0 2)
                    ;; code for 1, 0 extra bits
                    (deflate--number->bits 30 5)))
         (cl-huff-codes (make-hash-table))
         (bitstream '()))
    (puthash 0  '(0 . 2) cl-huff-codes)  ;; code 0, length 2
    (puthash 3  '(1 . 2) cl-huff-codes)  ;; code 1, length 2
    (puthash 4  '(2 . 2) cl-huff-codes)  ;; code 2, length 2
    (puthash 18 '(6 . 3) cl-huff-codes)  ;; code 6, length 3
    (puthash 17 '(14 . 4) cl-huff-codes) ;; code 14, length 4
    (puthash 1  '(30 . 5) cl-huff-codes) ;; code 00, length 5
    (puthash 2  '(31 . 5) cl-huff-codes) ;; code 31, length 5
    (should (equal expected
                   (deflate--write-code-lengths bitstream cl-encoded cl-huff-codes)))))


(provide 'deflate-test)
;;; deflate-test.el ends here
