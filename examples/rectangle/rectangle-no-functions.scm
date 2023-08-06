;;; examples/rectangle/rectangle.scm: solution to the "rectangle" example problem
;;;
;;; This program is an example solution to the "rectangle" example problem
;;; that gets a 2/3 score due to missing functions.
;;;
;;; This file is part of Udge.
;;;
;;;
;;; Copyright (C) 2020-2023  Rudy Matela
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; This is far from a perfect Scheme program.
;;; It does the job but it is hacky.
;;; I'm not an experienced schemer...
(define (main)
  (let ((w (read)))
    (if (not (eof-object? w))
      (let ((h (read)))
         (begin
            (display w)
            (display "x")
            (display h)
            (display " rectangle, area = ")
            (display (* w h))
            (display ", perimeter = ")
            (display (* 2 (+ w h)))
            (newline)
            (main))))))
(main)
