#!/bin/sh

gprof ./test_triangle gmon.out > gprof.out
less gprof.out

