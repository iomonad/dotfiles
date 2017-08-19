;;; drawille.el --- Drawille implementation in elisp

;; Copyright (C) 2015-2016 Josuah Demangeon

;; Author: Josuah Demangeon <josuah.demangeon@gmail.com>
;; Created: 09 Dec 2015
;; Version: 0.1
;; Keywords: graphics
;; URL: https://github.com/sshbio/elisp-drawille
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Change-log:

;;; Commentary:

;; This is an experimental drawille implementation im emacs lisp.

;; Drawille is a library originally written in python that permit to
;; use graphics in text environment using the braille characters as a
;; canvas smaller than a character (2x8 dots per characters).

;; It is not complete, but yet works, with a `drawille-buffer' command
;; that transforms the buffer into a small representation of its
;; content.

;; This will result into transforming a matrices:

;; [[a0 a1 a2 a3 a4 a5]   [[[a0 a1   [a2 a3   [a4 a5   \
;;  [b0 b1 b2 b3 b4 b5]      b0 b1  / b2 b3  / b4 b5   |<- One braille
;;  [c0 c1 c2 c3 c4 c5]      c0 c1 /  c2 c3 /  c4 c5   |   character
;;  [d0 d1 d2 d3 d4 d5] =>    d0 d1]   d2 d3]   d4 d5]] /
;;  [e0 e1 e2 e3 e4 e5]    [[e0 e1   [e2 e3   [e4 e5
;;  [f0 f1 f2 f3 f4 f5]      f0 f1  / f2 f3  / f4 f5
;;  [g0 g1 g2 g3 g4 g5]      g0 g1 /  g2 g3 /  g4 g5
;;  [h0 h1 h2 h3 h4 h5]]     h0 h1]   h2 h3]   h4 h5]]]

;; Which is more correctly written as:

;; [[[a0 a1 b0 b1 c0 c1 d0 d1] <- One braille character
;;   [a2 a3 b2 b3 c2 c3 d2 d3]
;;   [a4 a5 b4 b5 c4 c5 d4 d5]] <- One row of braille characters
;;  [[e0 e1 f0 f1 g0 g1 h0 h1]
;;   [e2 e3 f2 f3 g2 g3 h2 h3]
;;   [e4 e5 f4 f5 g4 g5 h4 h5]]] <- Two row of braille characters

;; With each row a vector of 0 or 1, that is multiplied pairwise, and
;; aditionned to #x2800 produce a braille character keycode.

;;; Code:

(require 'cl-lib)
(require 'lisp-float-type)

(defconst drawille-braille-unicode-offset #x2800
  "Offset to reach the first braille char in unicode encoding.")

(defconst drawille-braille-table
  [#x01 #x08 #x02 #x10 #x04 #x20 #x40 #x80]
  "Table to convert coordinates to braille character.")

(defun drawille-vector-to-char (vector)
  "Translate a VECTOR to a corresponding braille character."
  (cl-loop for dot across vector
           for offset across drawille-braille-table collect
           (* dot offset)
           into offsets finally return
           (apply '+ drawille-braille-unicode-offset offsets)))

(defconst drawille-braille-reverse-table [7 6 5 3 1 4 2 0]
  "Table to convert braille character to coordinates.")

(defun drawille-char-to-vector (char)
  "Translate a braille CHAR to a corresponding vector."
  (cl-loop with char-offset = (- char drawille-braille-unicode-offset)
           with result = (make-vector 8 nil)
           for dot-index across drawille-braille-reverse-table
           for dot-offset = (aref drawille-braille-table dot-index)
           do
           (aset result dot-index
                 (if (< char-offset dot-offset) 0
                   (setq char-offset (- char-offset dot-offset)) 1))
           finally return result))

(defun drawille-vector-at-pos (matrix row column)
  "Return a braille char corresponding to MATRIX at ROW, COLUMN."
  (cl-loop for i from 0 to 3 vconcat
           (cl-loop for j from 0 to 1 vconcat
                    (vector (aref (aref matrix (+ row i))
                                  (+ column j))))))

(defun drawille-fill-matrix (matrix)
  "Return a MATRIX filled until there are a multiple of 4 of rows."
  (let* ((width (length (aref matrix 0)))
         (height (length matrix)))
    (if (= (% height 4) 0)
        matrix
      (vconcat (make-vector (- 4 (% height 4)) (make-vector width 0))
               matrix ))))

(defun drawille-from-matrix (unfilled-matrix)
  "Fill an UNFILLED-MATRIX and subdivides it into a matrix of vector.
It will then call `drawille-vector-to-char' to fill rows, then
columns."
  (cl-loop with matrix = (drawille-fill-matrix unfilled-matrix)
           with width = (length (aref matrix 0))
           with height = (length matrix)
           for i from 0 to (1- (floor height 4))
           concat
           (cl-loop for j from 0 to (1- (floor width 2)) concat
                    (char-to-string
                     (drawille-vector-to-char
                      (drawille-vector-at-pos matrix (* 4 i) (* 2 j))))
                    into line finally return (concat line "\n"))))

(defun drawille-grid (drawille row column)
  "Format a DRAWILLE string into a vector of strings (the grid).
If needed, its size is adjusted so that a dot at (ROW, COLUMN)
can be displayed."
  (let* ((grid (apply 'vector (split-string drawille "\n" t)))
         (width (length (aref grid 0)))
         (height (length grid)))
    (when (> (1+ column) (* 2 width))
      (setq grid
            (cl-loop
             for grid-row across grid vconcat
             (vector (concat grid-row (make-string
                                       (1+ (- (floor column 2) width))
                                       #x2800))))))
    (when (> (1+ row) (* 4 height))
      (setq grid
            (vconcat
             (cl-loop
              for i from 0 to (- (floor row 4) height) vconcat
              (vector (make-string (length (aref grid 0)) #x2800)))
             grid)))
    grid))

(defun drawille-draw-dot (drawille x y)
  "On a DRAWILLE string, update a drawille character at X, Y.
Coordinates starts at 0 at the bottom left, it can accept floats,
but not negative cordinates.  Although, they can overflow at the
rigt and at the top of the current matrix."
  (let* ((drawille (if (not drawille) (char-to-string #x2800)
                   drawille))
         (n-x (round x)) (n-y (round y))
         (grid (drawille-grid drawille n-y n-x))
         (inverted-y (- (* 4 (length grid)) n-y 1)) ;Row to ordinate
         (grid-row (aref grid (floor inverted-y 4)))
         (vector (drawille-char-to-vector
                  (aref grid-row (floor n-x 2)))))
    (aset vector (+ (* 2 (% inverted-y 4)) (% n-x 2)) 1)
    (aset grid-row (floor n-x 2) (drawille-vector-to-char vector))
    (mapconcat 'concat grid "\n")))

;; DONE: For each step, check wether the offset of not the ruler
;; (eiter x or y that is iterated one by one with the other that
;; follow) but the other is greater that 1, and if so, switch the
;; ruler to the other (either y or x).  DONE: The drawille-draw-line
;; will dot this

;; The standard functions will provide an offset to apply to the
;; center given a progress parameter these will have to have a
;; progress argument, and will produce a list of two x and y offsets
;; to apply to the center (a point).

(defun drawille-draw-line (drawille x1 y1 x2 y2)
  "On a DRAWILLE string, draw a line from X1, Y1 to X2, Y2."
  (cl-loop with drawille = (if (not drawille) (char-to-string #x2800)
                             drawille)
           with x-offset = (- x2 x1)
           with y-offset = (- y2 y1)
           for x in (if (= x-offset 0)
                        (make-list (abs (round y-offset)) 0)
                      (number-sequence 0 x-offset
                                       (* (cl-signum x-offset)
                                          (min 1 (abs (/ (float x-offset)
                                                         (float y-offset)))))))
           for y in (if (= y-offset 0)
                        (make-list (abs (round x-offset)) 0)
                      (number-sequence 0 y-offset
                                       (* (cl-signum y-offset)
                                          (min 1 (abs (/ (float y-offset)
                                                         (float x-offset)))))))
           do
           (setq drawille (drawille-draw-dot drawille (+ x1 x) (+ y1 y)))
           finally return drawille))

(defun drawille-draw-sparkline (drawille &rest values)
  "On a DRAWILLE string, add a plot representing each of the VALUES.
The VALUES can be integers or float, positive or negative."
  (cl-loop with drawille = (if (not drawille) (char-to-string #x2800)
                             drawille)
           with min = (apply 'min values)
           with offset = (if (< min 0) (- min) 0)
           for x1 from 0
           for x2 from 1 to (length values)
           for y1 in values
           for y2 in (cdr values)
           do
           (setq drawille (drawille-draw-line drawille
                                              x1 (+ y1 offset)
                                              x2 (+ y2 offset)))
           finally return drawille))

(defun drawille-draw-path (drawille &rest coordinates)
  "On a DRAWILLE string, draw a path for each COORDINATES.
Coordinates are multiple lists in the form (X Y)."
  (cl-loop with drawille = (if (not drawille) (char-to-string #x2800)
                             drawille)
	   with x-min = (cl-loop for coordinate in coordinates
				 minimize (car coordinate))
	   with y-min = (cl-loop for coordinate in coordinates
				 minimize (cadr coordinate))
	   with x-offset = (if (< x-min 0) (- x-min) 0)
	   with y-offset = (if (< y-min 0) (- y-min) 0)
           for coordinate1 in coordinates
           for x1 = (car coordinate1)
           for y1 = (cadr coordinate1)
           for coordinate2 in (cdr coordinates)
           for x2 = (car coordinate2)
           for y2 = (cadr coordinate2)
           do
	   (setq drawille (drawille-draw-line
			   drawille
			   (+ x-offset x1) (+ y-offset y1)
			   (+ x-offset x2) (+ y-offset y2)))
	   finally return drawille))

(defun drawille-draw-circle
    (drawille radius &optional x-offset y-offset)
  "On a DRAWILLE string, draw a circle with given RADIUS.
It will be shifted of X-OFFSET and Y-OFFSET on x and y axis."
  (apply 'drawille-draw-path drawille
	 (cl-loop
	  for x from 0 to (* 2 float-pi radius) collect
	  (list
	   (+ (or x-offset 0) radius
	      (* radius (sin (* (/ 1.0 radius) x))))
	   (+ (or y-offset 0) radius
	      (* radius (cos (* (/ 1.0 radius) x))))))))

;; TODO Truncate the string if it overflow or automatically detect the
;; size if no column argument is given
(defun drawille-string-list-fill (string-list column)
  "Fill a strings on STRING-LIST up to COLUMN."
  (cl-loop
   for string in string-list collect
   (substring (concat string (when (< (length string) column)
                               (make-string (- column (length string))
                                            0)))
              0 column)))

(defun drawille-from-string (string &optional column)
  "Transform a STRING to a minimap with COLUMN width.
As vim-minimap does: https://github.com/severin-lemaignan/vim-minimap"
  (let* ((string-without-spaces
          (replace-regexp-in-string " " (char-to-string 0) string))
         (string-without-non-spaces
          (replace-regexp-in-string
           (concat "[^\n" (char-to-string 0) "\s]")
           (char-to-string 1) string-without-spaces))
         (string-list
          (split-string string-without-non-spaces "\n"))
         (filled-strings-vector
          (drawille-string-list-fill
	   string-list (or column
			   (string-bytes (cl-first string-list))))))
    (drawille-from-matrix (vconcat filled-strings-vector))))

;;;###autoload
(defun drawille-from-buffer ()
  "Generate a drawille for current buffer."
  (interactive)
  (message "%s" (drawille-from-string (buffer-string) fill-column)))

(defun drawille-from-image (path &optional imagemagick-arguments)
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

- \"-ordered-dither DITHER_METHOD\" Grayscale emulation in bitmaps.

  Dither methods: \"threshold\", \"checks\", \"oNxN\" with N an
  integer, etc.

- \"-dither DITHER_METHOD\": Like ordered-dither, with an attempt
  to improve the pixel repartition.

  Dither methods: \"Riemersma\", \"FloydSteinberg\", etc.

- \"-resize\ NxN\", with each N an integer: Resize to size NxN, but keeping the proportions.

There  other methods at http://www.imagemagick.org/Usage/quantize/"
  (with-temp-buffer
    (shell-command
     (concat "convert " path " -compress none "
	     imagemagick-arguments " pbm:-")
     (current-buffer))
    (kill-line 2)
    (drawille-from-string
     (replace-regexp-in-string
      "0" " "
      (replace-regexp-in-string
       " " "" (buffer-string))))))

(provide 'drawille)
;;; drawille.el ends here
