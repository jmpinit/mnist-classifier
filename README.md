# mnist-classifier

Trying to use different machine learning methods to classify handwritten digits.

## Prerequisites

* Install [Stack](https://docs.haskellstack.org/en/stable/README/) for Haskell

## Downloading MNIST

Run `getdata.sh` to create a data directory in the repo and download the MNIST
handwritten digit database to it.

## Running Classifier

1. `stack build`
2. `stack exec mnist-classifier`
