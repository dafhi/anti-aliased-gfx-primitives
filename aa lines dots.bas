#include "inc/aaline.bas"
#include "inc/aadot.bas"


sub Main
  
  dim as imagevars    buf

  buf.get_info '' desktop
  
  var scalar = .75, flags = 8 '' no border
  screenres buf.w*scalar, buf.h*scalar,32,,flags
  
  buf.get_info  '' new window
  
  aaline.render_target @buf '' namespace
  aadot.render_target @buf  '' ''
  
  var u = 99
  
  dim as aalinevars l(u)
  dim as dotvars    d(u)
  
  dim as single a, r, win_rad = buf.diagonal / 2
  #define newpos a=rnd*twopi: r=sqr(rnd)*win_rad
 
  for i as myint = 0 to u
    with l(i)
      newpos
      .x0 = buf.wh + r*cos(a)
      .y0 = buf.wh + r*cos(a)
      newpos
      .x1 = buf.wh + r*cos(a)
      .y1 = buf.hh + r*sin(a)
      .wid = .5 + rnd
      .endcap = rnd
      .col = rcol
    end with
    with d(i)
      newpos
      .x = buf.wh + r*cos(a)
      .y = buf.hh + r*sin(a)
      .col = rcol
      .rad = .5 + rnd * 15
      .slope = (rnd*3+1) / .rad
    End With
  Next

  do
    screenlock
      cls
      for i as myint = 0 to u
        aaline.pdraw @l(i)
        aadot.draw d(i).x, d(i).y, d(i).col, d(i).rad, d(i).slope
      Next
    screenunlock
    sleep 1
   
    if inkey<>"" then exit do
  loop

end sub

Main
