/' ------- gmath.bas 2025 Sep 13 - by dafhi -------- '/

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

function sqr_safe( d as double ) as double
    return sgn(d) * sqr( abs(d))
end function

  #define min( a,b) iif( (a)<(b), (a), (b) )
  #define max( a,b) iif( (a)>(b), (a), (b) )

function clamp( in As double, hi As double = 1, lo As double = 0) As double
  return min( max(in, lo), hi ) '' 2023 June 12
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

#undef rnd
function rnd as double
   static as ulong a = 0, b = 0 '' Uint32
   a *= a
   a xor= b
   b += 1
   return a / culng(-1)
End Function

#define r255 floor(rnd*256)
