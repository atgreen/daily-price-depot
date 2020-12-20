# Daily Price Depot

Daily Price Depot is a convenient and trusted source of daily market
data for [ledger](https://ledger-cli.org) based plain text accounting.

How to use it
--------------

Daily Price Depot is a git repo containing market data files and the
scripts used to generate them. Simply clone the repo into your local
work environment and reference the data files in your ledger source.

If you are maintaining your ledger files in git, it's easiest just to
add Daily Price Depot as a git submodule like so:

    $ git submodule add --depth 1 https://github.com/atgreen/daily-price-depot.git

Now reference the data in your ledger files like so:

    ; -*- ledger -*-

    ; Tell ledger that the currency symbol '$' refers to Canadian dollars.
    commodity $
        alias CAD

    ; Include the data files that we need.
    include daily-price-depot/data/fiat/CAD.db
    include daily-price-depot/data/fiat/EUR.db

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
                 $3829  Assets
                 $1549    EUR Savings
                 $1000    Local Currency Savings
                 $1280    USD Savings
                $-3829  Equity:Opening Balances
    --------------------
                       0

And refresh content daily with `git submodule update`.


Data sets
--------------

All data sets are updated daily, as Daily Price Depot is oriented
towards long-term planners and investors, not for active traders.

### Fiat Currency Exchange Rates

Daily Price Depot contains currency exchange data for 155 currencies.
Look for these under `data/fiat`. All exchange rates are relative to
USD. For the purpose of financial planning, we assume that all rates
are transitive, meaning that if you are working in currency AAA, and
asking ledger to do things in currency BBB, ledger will convert AAA to
BBB through USD. The historical data goes back no more than seven
years.

### Equity Prices

Daily Price Depot currently contains a limited number of daily closing
equity prices under `data/equity`. The historical data goes back no
more than seven years. Feel free to submit PRs or Issues asking for
others.

Equity names are of the form `SYMBOL.EXCHANGE` in order to avoid name
clashes (eg. `IBM.NYSE` and `MSFT.NASDAQ`). Equities are priced in
their native currency.

### Commodity Prices

Gold, silver, and other commodity prices can be found in USD under
`data/commodity`.

### Fund Prices

Daily Price Depot currently contains a limited number of daily closing
fund prices under `data/fund`. The historical data goes back no
more than seven years. Feel free to submit PRs or Issues asking for
others.


Author and License
-------------------

Daily Price Depot was created by [Anthony
Green](https://github.com/atgreen). All files in the Daily Price
Depot, including scripts, programs, documentation and data files are
distributed under the terms of the GNU General Public License,
Version 3. See
[COPYING3](https://github.com/atgreen/daily-price-depot/blob/master/COPYING3)
for details.


Thanks
-------

Thank you to [Alpha Vantage](https://www.alphavantage.co) who are
providing market data with their explicit permission.
