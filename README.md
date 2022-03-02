# binance-exports

[![binance-exports Build Status](https://github.com/prikhi/binance-exports/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/binance-exports/actions/workflows/main.yml)


Export Your Binance Trade History to a CSV.

Requires [`stack`][get-stack] & a Binance.us API key & secret:

```sh
stack run -- -k <API_KEY> -s <API_SECRET> <SYMBOL1> <SYMBOL2> etc
stack run -- --help
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
$ stack install
$ export PATH="${HOME}/.local/bin/:${PATH}"
$ binance-exports -k <API_KEY> -s <API_SECRET> SOLUSD
time,symbol,type,price,quantity,total,fee,fee-currency
2022-03-01 21:20:44,SOLUSD,BUY,42.2424,0.42,42.90010000,0.0009001,BNB
```


## Build

You can build the project with stack:

```sh
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```sh
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run:

```sh
stack haddock --open binance-exports
```


## LICENSE

BSD-3
