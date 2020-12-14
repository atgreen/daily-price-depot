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

Pineapple Future contains currency exchange data for 155
currencies. Look for these under `data/fiat`. All exchange rates are
relative to USD. For the purpose of financial planning, we assume that
all rates are transitive, meaning that if you are working in currency
AAA, and asking ledger to do things in currency BBB, ledger will
convert AAA to BBB through USD.

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
