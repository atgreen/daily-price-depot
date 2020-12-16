# Pineapple Future

Pineapple Future is a convenient and trusted source of daily market
data for [ledger](https://ledger-cli.org) based plain text accounting.

How to use it
--------------

Pineapple Future is a git repo containing market data files and the
scripts used to generate them. Simply clone the repo into your local
work environment and reference the data files in your ledger source.

If you are maintaining your ledger files in git, it's easiest just to
add Pineapple Future as a git submodule like so:

    $ git submodule add https://github.com/atgreen/pineapple-future.git

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

All data sets are updated daily, as Pineapple Future is oriented
towards long-term planners and investors, not for active traders.

### Fiat Currency Exchange Rates

Pineapple Future contains currency exchange data for 155 currencies.
Look for these under `data/fiat`. All exchange rates are relative to
USD. For the purpose of financial planning, we assume that all rates
are transitive, meaning that if you are working in currency AAA, and
asking ledger to do things in currency BBB, ledger will convert AAA to
BBB through USD. The historical data goes back no more than seven
years.

### Equity Prices

Pineapple Future currently contains a limited number of daily closing
equity prices under `data/equity`. The historical data goes back no
more than seven years. Feel free to submit PRs or Issues asking for
others.

Equity names are of the form `SYMBOL.EXCHANGE` in order to avoid name
clashes (eg. `IBM.NYSE` and `MSFT.NASDAQ`). Equities are priced in
their native currency.

### Commodity Prices

Gold, silver, and other commodity prices can be found in USD under
`data/commodity`.


Author and License
-------------------

Pineapple Future was created by [Anthony
Green](https://github.com/atgreen). All files in the Pineapple Future,
including scripts, programs, documentation and data files are
distributed under the terms of the GNU General Public License,
Version 3. See
[COPYING3](https://github.com/atgreen/pineapple-future/blob/master/COPYING3)
for details.


Thanks
-------

Thank you to [Alpha Vantage](https://www.alphavantage.co) who are
providing market data with their explicit permission.
