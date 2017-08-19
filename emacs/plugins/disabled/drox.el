;;; drox.el --- Graphics in terminal without GUI nor framebuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Josuah Demangeon
;; Created: 11 Jan 2016
;; Version: 0.1

;; Author: Josuah Demangeon <josuah.demangeon@gmail.com>
;; Keywords: multimedia, terminals, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Writing Drawille in elisp was fun, but only available for terminals
;	; with unicode support because 521 braille characters are required to
;; display the drawille graphics.  I'm doing this all over again, but
;; with box drawing characters, which are only 18:

;; 1 full - 4 half - 2 diagonal - 4 (3/4) - 4 (1/4) - 3 gradients blocks.

;; Mosts of the linux text mode fonts support these 18 characters,
;; since there are 256 to 512 characters slots for these

;;; Code:

(defconst drox-pixel-chars-map
  [#x0020 #x259d #x2598 #x2580 #x2596 #x2590 #x259a #x259c
	  #x2597 #x259a #x258c #x259b #x2584 #x259f #x2599 #x2588]
  "A map to reach the appropriate character from a simple int.
It follows a binary pattern starting from the upper left corner.
The first one is empty and the last one is full.")

(defun drox-vector-to-char (vector)
  "Convert a VECTOR canvas to a corresponding bloc char."
  (let ((sum (apply '+ (append vector nil))))
    (cond
     ;; If the global value is lower or greater than a threshold,
     ;; apply a char immediately.  Otherwise, compute a char.
     ((equal sum 0) #x0020)
     ((< sum 2) #x2591)
     ((> sum 22) #x2588)
     ((> sum 14) #x2593)
     (t (aref drox-pixel-chars-map
	      (cl-loop for i from 0 to 7
		       for pixel across vector
		       if (member i '(1 2 5 6))
		       sum (* (if (> pixel 0) 1 0) (expt 2 (/ i 2)))))))))

(drox-vector-to-char [
		      0 2
		      0 2
		      2 2
		      2 2
		      ])

(defun drox-vector-at-pos (matrix row column)
  "Return a vector corresponding to MATRIX at ROW, COLUMN."
  (cl-loop for i from 0 to 3 vconcat
           (cl-loop for j from 0 to 1 vconcat
                    (vector (aref (aref matrix (+ row i))
                                  (+ column j))))))

(defun drox-from-matrix (matrix)
  "Create a drox out of a MATRIX of 0, 1, 2, 3, ... 15."
  ;; First subdivide a MATRIX of into a matrix of vector
  (cl-loop with width = (length (aref matrix 0))
           with height = (length matrix)
           for i from 0 to (1- (floor height 4))
           concat
	   ;; And then converts the matrix of vectors
           (cl-loop for j from 0 to (1- (floor width 2)) concat
                    (char-to-string
                     (drox-vector-to-char
                      (drox-vector-at-pos matrix (* 4 i) (* 2 j))))
                    into line finally return (concat line "\n"))))

(defun drox (path &optional imagemagick-arguments)
  "Generate a drawille by converting PATH image to B&W bitmap.
It will use ImageMagick, that is already used by Emacs.  You can
provide aditionnal IMAGEMAGICK-ARGUMENTS as a string.  As an
example:

 - \"-gamma 10\": This may help increase the visibility of the
   content of the image.

- \"-level 23%,24%\": This sets the minimal white value and the
  maximal black value for the converted image.

- \"-negate\": This permit to invert the black and white.

- \"-auto-level\": If you are feeling lucky.

- \"-resize\ NxN\", with each N an integer: Resize to size NxN,
  but keeping the proportions.

There  other methods at http://www.imagemagick.org/Usage/quantize/"

  (with-temp-buffer
    (shell-command
     (concat "convert " path " -compress none "
	     imagemagick-arguments " +depth +level 0,4 pgm:-")
     (current-buffer))
    (kill-line 4)
    (drox-from-matrix
     (cl-map 'vector
	     (lambda (a)
	       (cl-map 'vector
		       (lambda (b) (max 0 (- b 49)))
		       a))
	     (butlast (split-string
		       (replace-regexp-in-string " " "" (buffer-string))
		       "\n"))))))

(provide 'drox)
;;; drox.el ends here
