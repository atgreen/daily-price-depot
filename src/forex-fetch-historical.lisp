;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RLGL-SERVER; Base: 10 -*-
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
(ql:quickload :cl-csv)
(ql:quickload :cl-json)
(ql:quickload :flexi-streams)

;; We are normalizing on USD, so it does not appear in this list.
(defparameter +iso-4217-currency-codes+
  '("AED"  ;	United Arab Emirates Dirham
    "AFN"  ;	Afghanistan Afghani
    "ALL"  ;	Albania Lek
    "AMD"  ;	Armenia Dram
    "ANG"  ;	Netherlands Antilles Guilder
    "AOA"  ;	Angola Kwanza
    "ARS"  ;	Argentina Peso
    "AUD"  ;	Australia Dollar
    "AWG"  ;	Aruba Guilder
    "AZN"  ;	Azerbaijan Manat
    "BAM"  ;	Bosnia and Herzegovina Convertible Mark
    "BBD"  ;	Barbados Dollar
    "BDT"  ;	Bangladesh Taka
    "BGN"  ;	Bulgaria Lev
    "BHD"  ;	Bahrain Dinar
    "BIF"  ;	Burundi Franc
    "BMD"  ;	Bermuda Dollar
    "BND"  ;	Brunei Darussalam Dollar
    "BOB"  ;	Bolivia Bolíviano
    "BRL"  ;	Brazil Real
    "BSD"  ;	Bahamas Dollar
    "BTN"  ;	Bhutan Ngultrum
    "BWP"  ;	Botswana Pula
    "BYN"  ;	Belarus Ruble
    "BZD"  ;	Belize Dollar
    "CAD"  ;	Canada Dollar
    "CDF"  ;	Congo/Kinshasa Franc
    "CHF"  ;	Switzerland Franc
    "CLP"  ;	Chile Peso
    "CNY"  ;	China Yuan Renminbi
    "COP"  ;	Colombia Peso
    "CRC"  ;	Costa Rica Colon
    "CUC"  ;	Cuba Convertible Peso
    "CUP"  ;	Cuba Peso
    "CVE"  ;	Cape Verde Escudo
    "CZK"  ;	Czech Republic Koruna
    "DJF"  ;	Djibouti Franc
    "DKK"  ;	Denmark Krone
    "DOP"  ;	Dominican Republic Peso
    "DZD"  ;	Algeria Dinar
    "EGP"  ;	Egypt Pound
    "ERN"  ;	Eritrea Nakfa
    "ETB"  ;	Ethiopia Birr
    "EUR"  ;	Euro Member Countries
    "FJD"  ;	Fiji Dollar
    "FKP"  ;	Falkland Islands (Malvinas) Pound
    "GBP"  ;	United Kingdom Pound
    "GEL"  ;	Georgia Lari
    "GGP"  ;	Guernsey Pound
    "GHS"  ;	Ghana Cedi
    "GIP"  ;	Gibraltar Pound
    "GMD"  ;	Gambia Dalasi
    "GNF"  ;	Guinea Franc
    "GTQ"  ;	Guatemala Quetzal
    "GYD"  ;	Guyana Dollar
    "HKD"  ;	Hong Kong Dollar
    "HNL"  ;	Honduras Lempira
    "HRK"  ;	Croatia Kuna
    "HTG"  ;	Haiti Gourde
    "HUF"  ;	Hungary Forint
    "IDR"  ;	Indonesia Rupiah
    "ILS"  ;	Israel Shekel
    "IMP"  ;	Isle of Man Pound
    "INR"  ;	India Rupee
    "IQD"  ;	Iraq Dinar
    "IRR"  ;	Iran Rial
    "ISK"  ;	Iceland Krona
    "JEP"  ;	Jersey Pound
    "JMD"  ;	Jamaica Dollar
    "JOD"  ;	Jordan Dinar
    "JPY"  ;	Japan Yen
    "KES"  ;	Kenya Shilling
    "KGS"  ;	Kyrgyzstan Som
    "KHR"  ;	Cambodia Riel
    "KMF"  ;	Comorian Franc
    "KPW"  ;	Korea (North) Won
    "KRW"  ;	Korea (South) Won
    "KWD"  ;	Kuwait Dinar
    "KYD"  ;	Cayman Islands Dollar
    "KZT"  ;	Kazakhstan Tenge
    "LAK"  ;	Laos Kip
    "LBP"  ;	Lebanon Pound
    "LKR"  ;	Sri Lanka Rupee
    "LRD"  ;	Liberia Dollar
    "LSL"  ;	Lesotho Loti
    "LYD"  ;	Libya Dinar
    "MAD"  ;	Morocco Dirham
    "MDL"  ;	Moldova Leu
    "MGA"  ;	Madagascar Ariary
    "MKD"  ;	Macedonia Denar
    "MMK"  ;	Myanmar (Burma) Kyat
    "MNT"  ;	Mongolia Tughrik
    "MOP"  ;	Macau Pataca
    "MRU"  ;	Mauritania Ouguiya
    "MUR"  ;	Mauritius Rupee
    "MVR"  ;	Maldives (Maldive Islands) Rufiyaa
    "MWK"  ;	Malawi Kwacha
    "MXN"  ;	Mexico Peso
    "MYR"  ;	Malaysia Ringgit
    "MZN"  ;	Mozambique Metical
    "NAD"  ;	Namibia Dollar
    "NGN"  ;	Nigeria Naira
    "NIO"  ;	Nicaragua Cordoba
    "NOK"  ;	Norway Krone
    "NPR"  ;	Nepal Rupee
    "NZD"  ;	New Zealand Dollar
    "OMR"  ;	Oman Rial
    "PAB"  ;	Panama Balboa
    "PEN"  ;	Peru Sol
    "PGK"  ;	Papua New Guinea Kina
    "PHP"  ;	Philippines Peso
    "PKR"  ;	Pakistan Rupee
    "PLN"  ;	Poland Zloty
    "PYG"  ;	Paraguay Guarani
    "QAR"  ;	Qatar Riyal
    "RON"  ;	Romania Leu
    "RSD"  ;	Serbia Dinar
    "RUB"  ;	Russia Ruble
    "RWF"  ;	Rwanda Franc
    "SAR"  ;	Saudi Arabia Riyal
    "SBD"  ;	Solomon Islands Dollar
    "SCR"  ;	Seychelles Rupee
    "SDG"  ;	Sudan Pound
    "SEK"  ;	Sweden Krona
    "SGD"  ;	Singapore Dollar
    "SHP"  ;	Saint Helena Pound
    "SLL"  ;	Sierra Leone Leone
    "SOS"  ;	Somalia Shilling
    "SRD"  ;	Suriname Dollar
    "STN"  ;	São Tomé and Príncipe Dobra
    "SVC"  ;	El Salvador Colon
    "SYP"  ;	Syria Pound
    "SZL"  ;	eSwatini Lilangeni
    "THB"  ;	Thailand Baht
    "TJS"  ;	Tajikistan Somoni
    "TMT"  ;	Turkmenistan Manat
    "TND"  ;	Tunisia Dinar
    "TOP"  ;	Tonga Pa'anga
    "TRY"  ;	Turkey Lira
    "TTD"  ;	Trinidad and Tobago Dollar
    "TVD"  ;	Tuvalu Dollar
    "TWD"  ;	Taiwan New Dollar
    "TZS"  ;	Tanzania Shilling
    "UAH"  ;	Ukraine Hryvnia
    "UGX"  ;	Uganda Shilling
    "UYU"  ;	Uruguay Peso
    "UZS"  ;	Uzbekistan Som
    "VEF"  ;	Venezuela Bolívar
    "VND"  ;	Viet Nam Dong
    "VUV"  ;	Vanuatu Vatu
    "WST"  ;	Samoa Tala
    "XAF"  ;	Communauté Financière Africaine (BEAC) CFA Franc BEAC
    "XCD"  ;	East Caribbean Dollar
    "XDR"  ;	International Monetary Fund (IMF) Special Drawing Rights
    "XOF"  ;	Communauté Financière Africaine (BCEAO) Franc
    "XPF"  ;	Comptoirs Français du Pacifique (CFP) Franc
    "YER"  ;	Yemen Rial
    "ZAR"  ;	South Africa Rand
    "ZMW"  ;	Zambia Kwacha
    "ZWD"));	Zimbabwe Dollar

(defparameter +alphavantage-api-uri+ "https://www.alphavantage.co/query")
(defparameter +alphavantage-api-key+ (uiop:getenv "ALPHAVANTAGE_API_KEY"))

(defun fetch-history (currency)
  (format t "Fetching historical ~A rates.~%" currency)
  (let ((parameters `(("function" . "FX_DAILY")
                      ("from_currency" . ,currency)
                      ("to_currency" . "USD")
                      ("outputsize" . "full")
                      ("datatype" . "csv")
                      ("apikey" . ,+alphavantage-api-key+))))
    (octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun fetch-exchange (currency)
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

(defun save-historical-data-for-symbol (dir sym)
  (let ((filename (concatenate 'string dir sym ".db")))
    (unless (probe-file filename)
      (sleep 13) ;; Rate limit to 5 requests per minute
      (let ((history (fetch-history sym)))
        (handler-case
            (let ((prices (reverse (cdr (cl-csv:read-csv history)))))
              (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
                (dolist (price prices)
                  (format stream "P ~A 23:59:59 ~A ~A USD~%"
                          (car price)
                          sym
                          (cadddr price)))))
          (error (c)
            (format t "ERROR: ~A~%" c)
            (format t history)))))))

(defun save-data-for-symbol (dir sym)
  (let ((filename (concatenate 'string dir sym ".db")))
    (print filename)
    (when (probe-file filename)
      (sleep 13) ;; Rate limit to 5 requests per minute
      (let ((exchange (fetch-exchange sym)))
        (print exchange)
        (handler-case
            (let ((json (cdar (json:decode-json-from-string exchange))))
              (with-open-file (stream filename :direction :output :if-exists :append)
                (format stream "P ~A ~A ~A USD~%"
                        (cdr (assoc :|6. *LAST *REFRESHED| json))
                        sym
                        (cdr (assoc :|5. *EXCHANGE *RATE| json)))))
          (error (c)
            (format t "ERROR: ~A~%" c)
            (format t history)))))))

(defun pull-history ()
  (dolist (currency +iso-4217-currency-codes+)
    (save-historical-data-for-symbol "../data/fiat/" currency)))

(defun pull-daily ()
  (dolist (currency +iso-4217-currency-codes+)
    (save-data-for-symbol "../data/fiat/" currency)))
