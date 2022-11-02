#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "-"

set cbr [0:]

set view 0,0
set view equal xyz
set xyplane 0.0
set palette gray

set arrow from 0,0,0 to s1,s2,s3 front lc 'orange'
set arrow from 0+0.01,0,0 to o1+0.01,o2,o3 front lc 'blue'

sp \
  "<awk '(FNR>1) && ($1<3198/2) && (sqrt($2**2+$3**2+$4**2)<0.9)' output.node.50" u 2:3:4 w p not,\
  "<awk '(FNR>1) && ($1<3198/2) && (sqrt($2**2+$3**2+$4**2)>0.9)' output.node.50" u 2:3:4 w p not,\

pa -1

q

  "<./face.awk output.node.50 output.face.50" u 1:2:3 w l lw 1 not,\

  "<awk '(NR>1)' output.centre" u 2:3:4 w p pt 1 lc 'green' t 'centres',\
  "<awk '(NR>1)' output.centre" u 2:3:4:1 w labels tc 'brown' not,\
  "<awk '(ARGIND==1){ s[$1]=$0; }(ARGIND==2) && (FNR>1){ print s[$1],$0; }' output.centre output.normal" u 2:3:4:6:7:8 w vectors lc 'green' t 'normals'
