#!/bin/sh

gprof ./lc_triangle gmon.out > gprof.out
less gprof.out

