# Pineapple Future

Pineapple Future is a convenient and trusted source of data for
ledger-cli based plain text accounting.

How to use it
--------------

Pineapple Future is a git repo that you clone to your local work
environment and reference in your ledger source files.

If you are maintaining your ledger files in git, it's easiest just to
add Pineapple Future as a git submodule like so:

    $ git submodule add http://srv.labdroid.net:3000/green/pineapple-future.git

Now create a ledger file like this:

    ; -*- ledger -*-

    ; Tell ledger that the currency symbol '$' refers to Canadian dollars.
    commodity CAD
        alias $

    ; Include the data files that we need.
    include pineapple-future/data/fiat/CAD.db
    include pineapple-future/data/fiat/EUR.db

    2020/12/14 * USD savings account
        Assets:USD Savings                     1000 USD
        Equity:Opening Balances

    2020/12/14 * EUR savings account
        Assets:EUR Savings                     1000 EUR
        Equity:Opening Balances

    2020/12/14 * Local currency savings account
        Assets:Local Currency Savings          $1000
        Equity:Opening Balances

Now generate a report:

    $ ledger -f book.ledger --exchange $ bal
                 CAD3829  Assets
                 CAD1549    EUR Savings
                 CAD1000    Local Currency Savings
                 CAD1280    USD Savings
                CAD-3829  Equity:Opening Balances
    --------------------
                       0

Data sets
--------------

### Fiat Currency Exchange Rates

|-----+----------------------------------------------------------|
| AED | United Arab Emirates Dirham                              |
| AFN | Afghanistan Afghani                                      |
| ALL | Albania Lek                                              |
| AMD | Armenia Dram                                             |
| ANG | Netherlands Antilles Guilder                             |
| AOA | Angola Kwanza                                            |
| ARS | Argentina Peso                                           |
| AUD | Australia Dollar                                         |
| AWG | Aruba Guilder                                            |
| AZN | Azerbaijan Manat                                         |
| BAM | Bosnia and Herzegovina Convertible Mark                  |
| BBD | Barbados Dollar                                          |
| BDT | Bangladesh Taka                                          |
| BGN | Bulgaria Lev                                             |
| BHD | Bahrain Dinar                                            |
| BIF | Burundi Franc                                            |
| BMD | Bermuda Dollar                                           |
| BND | Brunei Darussalam Dollar                                 |
| BOB | Bolivia Bolíviano                                        |
| BRL | Brazil Real                                              |
| BSD | Bahamas Dollar                                           |
| BTN | Bhutan Ngultrum                                          |
| BWP | Botswana Pula                                            |
| BYN | Belarus Ruble                                            |
| BZD | Belize Dollar                                            |
| CAD | Canada Dollar                                            |
| CDF | Congo/Kinshasa Franc                                     |
| CHF | Switzerland Franc                                        |
| CLP | Chile Peso                                               |
| CNY | China Yuan Renminbi                                      |
| COP | Colombia Peso                                            |
| CRC | Costa Rica Colon                                         |
| CUC | Cuba Convertible Peso                                    |
| CUP | Cuba Peso                                                |
| CVE | Cape Verde Escudo                                        |
| CZK | Czech Republic Koruna                                    |
| DJF | Djibouti Franc                                           |
| DKK | Denmark Krone                                            |
| DOP | Dominican Republic Peso                                  |
| DZD | Algeria Dinar                                            |
| EGP | Egypt Pound                                              |
| ERN | Eritrea Nakfa                                            |
| ETB | Ethiopia Birr                                            |
| EUR | Euro Member Countries                                    |
| FJD | Fiji Dollar                                              |
| FKP | Falkland Islands (Malvinas) Pound                        |
| GBP | United Kingdom Pound                                     |
| GEL | Georgia Lari                                             |
| GGP | Guernsey Pound                                           |
| GHS | Ghana Cedi                                               |
| GIP | Gibraltar Pound                                          |
| GMD | Gambia Dalasi                                            |
| GNF | Guinea Franc                                             |
| GTQ | Guatemala Quetzal                                        |
| GYD | Guyana Dollar                                            |
| HKD | Hong Kong Dollar                                         |
| HNL | Honduras Lempira                                         |
| HRK | Croatia Kuna                                             |
| HTG | Haiti Gourde                                             |
| HUF | Hungary Forint                                           |
| IDR | Indonesia Rupiah                                         |
| ILS | Israel Shekel                                            |
| IMP | Isle of Man Pound                                        |
| INR | India Rupee                                              |
| IQD | Iraq Dinar                                               |
| IRR | Iran Rial                                                |
| ISK | Iceland Krona                                            |
| JEP | Jersey Pound                                             |
| JMD | Jamaica Dollar                                           |
| JOD | Jordan Dinar                                             |
| JPY | Japan Yen                                                |
| KES | Kenya Shilling                                           |
| KGS | Kyrgyzstan Som                                           |
| KHR | Cambodia Riel                                            |
| KMF | Comorian Franc                                           |
| KPW | Korea (North) Won                                        |
| KRW | Korea (South) Won                                        |
| KWD | Kuwait Dinar                                             |
| KYD | Cayman Islands Dollar                                    |
| KZT | Kazakhstan Tenge                                         |
| LAK | Laos Kip                                                 |
| LBP | Lebanon Pound                                            |
| LKR | Sri Lanka Rupee                                          |
| LRD | Liberia Dollar                                           |
| LSL | Lesotho Loti                                             |
| LYD | Libya Dinar                                              |
| MAD | Morocco Dirham                                           |
| MDL | Moldova Leu                                              |
| MGA | Madagascar Ariary                                        |
| MKD | Macedonia Denar                                          |
| MMK | Myanmar (Burma) Kyat                                     |
| MNT | Mongolia Tughrik                                         |
| MOP | Macau Pataca                                             |
| MRU | Mauritania Ouguiya                                       |
| MUR | Mauritius Rupee                                          |
| MVR | Maldives (Maldive Islands) Rufiyaa                       |
| MWK | Malawi Kwacha                                            |
| MXN | Mexico Peso                                              |
| MYR | Malaysia Ringgit                                         |
| MZN | Mozambique Metical                                       |
| NAD | Namibia Dollar                                           |
| NGN | Nigeria Naira                                            |
| NIO | Nicaragua Cordoba                                        |
| NOK | Norway Krone                                             |
| NPR | Nepal Rupee                                              |
| NZD | New Zealand Dollar                                       |
| OMR | Oman Rial                                                |
| PAB | Panama Balboa                                            |
| PEN | Peru Sol                                                 |
| PGK | Papua New Guinea Kina                                    |
| PHP | Philippines Peso                                         |
| PKR | Pakistan Rupee                                           |
| PLN | Poland Zloty                                             |
| PYG | Paraguay Guarani                                         |
| QAR | Qatar Riyal                                              |
| RON | Romania Leu                                              |
| RSD | Serbia Dinar                                             |
| RUB | Russia Ruble                                             |
| RWF | Rwanda Franc                                             |
| SAR | Saudi Arabia Riyal                                       |
| SBD | Solomon Islands Dollar                                   |
| SCR | Seychelles Rupee                                         |
| SDG | Sudan Pound                                              |
| SEK | Sweden Krona                                             |
| SGD | Singapore Dollar                                         |
| SHP | Saint Helena Pound                                       |
| SLL | Sierra Leone Leone                                       |
| SOS | Somalia Shilling                                         |
| SRD | Suriname Dollar                                          |
| STN | São Tomé and Príncipe Dobra                              |
| SVC | El Salvador Colon                                        |
| SYP | Syria Pound                                              |
| SZL | eSwatini Lilangeni                                       |
| THB | Thailand Baht                                            |
| TJS | Tajikistan Somoni                                        |
| TMT | Turkmenistan Manat                                       |
| TND | Tunisia Dinar                                            |
| TOP | Tonga Pa'anga                                            |
| TRY | Turkey Lira                                              |
| TTD | Trinidad and Tobago Dollar                               |
| TVD | Tuvalu Dollar                                            |
| TWD | Taiwan New Dollar                                        |
| TZS | Tanzania Shilling                                        |
| UAH | Ukraine Hryvnia                                          |
| UGX | Uganda Shilling                                          |
| UYU | Uruguay Peso                                             |
| UZS | Uzbekistan Som                                           |
| VEF | Venezuela Bolívar                                        |
| VND | Viet Nam Dong                                            |
| VUV | Vanuatu Vatu                                             |
| WST | Samoa Tala                                               |
| XAF | Communauté Financière Africaine (BEAC) CFA Franc BEAC    |
| XCD | East Caribbean Dollar                                    |
| XDR | International Monetary Fund (IMF) Special Drawing Rights |
| XOF | Communauté Financière Africaine (BCEAO) Franc            |
| XPF | Comptoirs Français du Pacifique (CFP) Franc              |
| YER | Yemen Rial                                               |
| ZAR | South Africa Rand                                        |
| ZMW | Zambia Kwacha                                            |
| ZWD | Zimbabwe Dollar                                          |
|-----+----------------------------------------------------------|


Author and License
-------------------

Pineapple Future was conceived of and implemented by [Anthony Green](https://github.com/atgreen). All files in the Pineapple Future, including scripts, programs, documentation and data files are distributed under the terms of the GNU General Public License,
Version 3. See
[COPYING](https://raw.githubusercontent.com/atgreen/pineapple-future/master/COPYING)
for details.


Thanks
-------

Thank you to [Alpha Vantage](https://www.alphavantage.co) who are
providing market data with their explicit permission.
