#!/bin/sh

export OMP_NUM_THREADS=4

rm lc.dat
./test_triangle && ./lc.plt

