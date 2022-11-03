#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "I2_{lambda} [W m^{-2} sr^{-1} m^{-1}]" offset 3,0

set cbr [1.e3:]
#set logscale cb

set view 90,0,0.5
set view equal xyz
set xyplane 0.0
set palette defined (\
  0.0 "black",\
  0.001 "#000099",\
  1.0 "white" \
  )
set surface hidden3d
set pm3d depthorder
set hidden3d front

set arrow from 0,0,0 to s1,s2,s3 front lc 'orange'
set arrow from 0+0.01,0,0 to o1+0.01,o2,o3 front lc 'blue'

sp \
  "<./pm3d.awk output.node.49 output.face.49 output.I2_lambda.49" u 1:2:3:5 w pm3d not,\
  "<./face.awk output.node.49 output.face.49" u 1:2:3 w l lw 1 not,\

pa -1

set term png small size 1024,1024
set out "output.I2_lambda.49.png"
rep

q


