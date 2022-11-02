#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "tau_i [1]"

set cbr [0:1]

set view 0,0
set view equal xyz
set xyplane 0.0
set palette defined (\
  0.0 "white",\
  0.01 "cyan",\
  1.0 "green" \
  )
set surface hidden3d
set pm3d depthorder
set hidden3d front

set arrow from 0,0,0 to s1,s2,s3 front lc 'orange'
set arrow from 0+0.01,0,0 to o1+0.01,o2,o3 front lc 'blue'

sp \
  "<./pm3d_TAU.awk output.node.50 output.face.50 output.tau_i.50 | awk '(($4==1)) || (NF==0)'" u 1:2:3:6 w pm3d not,\
  "<./face.awk output.node.50 output.face.50" u 1:2:3 w l lw 1 not,\
  "<awk '(NR>1)' output.centre.50" u 2:3:4:1 w labels tc 'brown' not,\


pa -1

q

  "<awk '(NR>1)' output.centre" u 2:3:4 w p pt 1 lc 'green' t 'centres',\
  "<awk '(ARGIND==1){ s[$1]=$0; }(ARGIND==2) && (FNR>1){ print s[$1],$0; }' output.centre output.normal" u 2:3:4:6:7:8 w vectors lc 'green' t 'normals'

