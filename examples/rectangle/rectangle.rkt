#lang racket
; examples/rectangle/rectangle.scm: solution to the "rectangle" example problem
;
; This program is an example solution to the "rectangle" example problem
; that gets a full 3/3 score.
;
; This file is part of Udge.
;
;
; Copyright (C) 2020-2023  Rudy Matela
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (area r) (* (car r) (cdr r)))
(define (perimeter r) (* 2 (+ (car r) (cdr r))))

; This is far from a perfect Racket program.
; It does the job but it is hacky.
; I'm not an experienced Racketeer...
(define (main)
  (let ((w (read)))
    (if (not (eof-object? w))
      (let ((h (read)))
         (begin
            (display w)
            (display "x")
            (display h)
            (display " rectangle, area = ")
            (display (area (cons w h)))
            (display ", perimeter = ")
            (display (perimeter (cons w h)))
            (newline)
            (main)))
      (display ""))))
(main)
