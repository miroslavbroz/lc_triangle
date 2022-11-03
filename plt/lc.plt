#!/usr/bin/gnuplot

set term x11

set xl "i"
set yl "V0 [mag]"

set yr [:] reverse
set key bottom

p \
  "lc.dat" u 1:2 w lp,\

pa -1

set term png small
set out "lc.png"
rep

q

  "../test_hires/lc.dat" u 1:2 w lp,\
  "../../test_triangle9_REMESH/lc.dat" u 1:2 w l lc 'gray'

