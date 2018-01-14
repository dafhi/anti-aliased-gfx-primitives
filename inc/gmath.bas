/' ------- gmath.bas 2017 Dec 7 - by dafhi -------- '/

#include once "imagevars.bas"

#Ifndef floor
#Define floor(x) (((x)*2.0-0.5)shr 1) '' http://www.freebasic.net/forum/viewtopic.php?p=118633
#define ceil(x) (-((-(x)*2.0-0.5)shr 1))
  #EndIf

#ifndef pi
const   TwoPi = 8*atn(1)
const   Pi = 4*atn(1)
const   piBy2 = 2*atn(1)
const   iPi = 1/Pi
const   piBy4 = atn(1)
#EndIf

function round(in as single, places as ubyte = 2) as string
  dim as integer mul = 10 ^ places
  return str(csng(floor(in * mul + .5) / mul))
End Function

function roun(in as single, places as ubyte = 0) as single
  dim as integer mul = 10 ^ places:  return floor(in * mul + .5) / mul
end function

function clamp(in as single, hi as single=1, lo as single=0) as single
  if in < lo then return lo
  if in > hi then return hi
  return in
End Function

'' single precision vector
type v3s
  as single         x,y,z
  declare property  rand as v3s
End Type
property v3s.rand as v3s
  var y=2*(rnd-.5):  var r=sqr(1-y*y)
  return type ( r*cos(z), y, r*sin(rnd*twopi) )
End property

operator /(l as v3s,r as single) as v3s: if r<>0 then: r=1/r: endif: return type(l.x*r,l.y*r,l.z*r): end operator
operator *(l as v3s,r as single) as v3s: return type(l.x*r,l.y*r,l.z*r): end operator
operator *(l as single,r as v3s) as v3s: return type(l*r.x,l*r.y,l*r.z): end operator
operator *(l as v3s,r as v3s) as v3s: return type(l.x*r.x,l.y*r.y,l.z*r.z): end operator
operator -(l as v3s,r as v3s) as v3s: return type(l.x-r.x,l.y-r.y,l.z-r.z): end operator
operator +(l as v3s,r as v3s) as v3s: return type(l.x+r.x,l.y+r.y,l.z+r.z): end operator

#define r255 int(rnd*256)
