#include "inc/aaline.bas"
#include "inc/aadot.bas"

dim as imagevars    buf

buf.get_info                '' desktop

var scalar = .4, no_border = 8

screenres buf.w*scalar, buf.h*scalar,32,, no_border

buf.get_info                '' window

aaline.render_target @buf   '' namespace
aadot.render_target @buf    ''

dim as aalinevars l
dim as dotvars    d

#define res (rnd-.1)*buf.diagonal

screenlock
  for i as myint = 0 to 9
    l.col = rgba(r255,r255,r255,r255)
    l.wid = 1.5 + rnd*rnd*rnd*24
    l.endcap = rnd
    aaline.draw res, res, res, res, l.col, l.wid, l.endcap
    
    d.col = rgb(r255,r255,r255)
    d.rad = .5 + rnd * 35
    d.slope = (rnd*3+1) / d.rad
    aadot.draw res, res, d.col, d.rad, d.slope
  Next
screenunlock

sleep
