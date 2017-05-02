#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DATA_DIR=$SCRIPT_DIR/data

mkdir -p $DATA_DIR

TRAINING_SET_IMAGES=http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
TRAINING_SET_LABELS=http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
TEST_SET_IMAGES=http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
TEST_SET_LABELS=http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

curl $TRAINING_SET_IMAGES | gzip -d > $DATA_DIR/train-images-idx3-ubyte
curl $TRAINING_SET_LABELS | gzip -d > $DATA_DIR/train-labels-idx1-ubyte
curl $TEST_SET_IMAGES | gzip -d > $DATA_DIR/t10k-images-idx3-ubyte
curl $TEST_SET_LABELS | gzip -d > $DATA_DIR/t10k-labels-idx1-ubyte
