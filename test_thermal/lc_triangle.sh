#!/bin/sh

export OMP_NUM_THREADS=4

rm lc.dat
./lc_triangle > lc_triangle.out && ./lc.plt

