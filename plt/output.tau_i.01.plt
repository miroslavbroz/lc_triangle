#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "tau_i [1]"

set cbr [0:1]

set view 90,0
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
  "<./pm3d_TAU.awk output.node.01 output.face.01 output.tau_i.01 | awk '(($4==1)) || (NF==0)'" u 1:2:3:6 w pm3d not,\
  "<./pm3d_TAU.awk output.node.01 output.face.01 output.tau_i.01 | awk '(($4==1) && ($5==1)) || (NF==0)'" u 1:2:3:6 w l lw 3 lc 'black' not,\
  "<./face.awk output.node.01 output.face.01" u 1:2:3 w l lw 1 not,\
  "<awk '(NR>1)' output.centre.01" u 2:3:4:1 w labels tc 'brown' not,\

pa -1

set term png small size 1024,1024
set out "output.tau_i.01.png"
rep

q

