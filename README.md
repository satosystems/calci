# calci

REPL calculator named calci is implemented by Haskell.

## Description

This calculator is implemented for practical purposes of Haskell's following modules.

- [parsec](https://hackage.haskell.org/package/parsec)
- [haskeline](https://hackage.haskell.org/package/haskeline)

## Demo

![Demo](https://github.com/satosystems/calci/wiki/images/calci.gif)

## Requirement

You need [Stack](https://haskellstack.org) to build the source code.

## How to build

```shell-session
$ stack build
```

## How to use

```shell-session
$ git checkout feature/#1-implement-first-version
$ stack build
$ stack exec calci
> a = 10
> b = 20
> c = (a + b) * 100 + 30
> c
3030.0
> :q
Leaving calci.
```

## Licence

BSD3

## Author

[Satoshi Ogata](https://github.com/satosystems)

