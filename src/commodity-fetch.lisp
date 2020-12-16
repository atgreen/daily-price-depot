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

(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :flexi-streams)

;; We are normalizing on USD, so it does not appear in this list.

(defparameter +alphavantage-api-uri+ "https://www.alphavantage.co/query")
(defparameter +alphavantage-api-key+ (uiop:getenv "ALPHAVANTAGE_API_KEY"))

(defun fetch-commodity (currency)
  (format t "Fetching exchange rate for ~A." currency)
  (let ((parameters `(("function" . "CURRENCY_EXCHANGE_RATE")
                      ("from_currency" . ,currency)
                      ("to_currency" . "USD")
                      ("datatype" . "csv")
                      ("apikey" . ,+alphavantage-api-key+))))
    (octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun save-data-for-symbol (dir sym)
  (let ((filename (concatenate 'string dir (cdr sym) ".db")))
    (print filename)
    (sleep 13) ;; Rate limit to 5 requests per minute
    (print "slept")
    (let ((exchange (fetch-commodity (car sym))))
      (handler-case
          (let ((json (cdar (json:decode-json-from-string exchange))))
            (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "P ~A ~A ~A USD~%"
                      (cdr (assoc :|6. *LAST *REFRESHED| json))
                      (cdr sym)
                      (cdr (assoc :|5. *EXCHANGE *RATE| json)))))
        (error (c)
          (format t "ERROR: ~A~%" c)
          (format t exchange))))))

(defun pull-daily ()
  (dolist (currency '(("XAU" . "GOLD") ("XAG" . "SILVER")))
    (save-data-for-symbol "../data/commodity/" currency)))
