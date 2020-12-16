;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2020  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

(ql:quickload :uiop)
(ql:quickload :cl-date-time-parser)

(let ((cutoff (- (get-universal-time)
                 (- (date-time-parser:parse-date-time "2007")
                    (date-time-parser:parse-date-time "2000")))))
  (uiop:collect-sub*directories
   "../data"
   (constantly t)
   (constantly t)
   (lambda (it)
     (dolist (filename (uiop:directory-files it "*.db"))
       (print filename)
       (let ((trimmed-file (concatenate 'string (namestring filename) ".trim")))
         (with-open-file (out-stream
                          trimmed-file :direction :output
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
           (with-open-file (in-stream filename)
             (loop for line = (read-line in-stream nil)
                   while line do
                     (let ((date (date-time-parser:parse-date-time (subseq line 2 12))))
                       (when (> date cutoff)
                         (format out-stream "~A~%" line))))))
         (rename-file trimmed-file filename))))))

(quit)
