# Assignment 3

This assignment consists of 2 problems:

- Implementation of a console-like text buffer and tested with QuickCheck properties.
- Implementation of a simple lhs -> html parser with the Parsec library.

## Install

> You must have [`stack`](https://www.haskellstack.org/) installed
```
stack build
```

## Run
```
stack exec literate-parser example.lhs
# An 'example.html' file will be created
stack exec buffer-quickcheck
# Runs QuickCheck
```