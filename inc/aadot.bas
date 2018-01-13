/' ------- aadot 2018 Jan 12 - by dafhi -------- '/

#include once "gmath.bas"

type DotVars
  union
    as v3s            p     'projected
    type: as single   x,y,z:  end type
  end union
  union
    As ULong          col
    Type:  As UByte   b,g,r,a:  End Type
  end union
  as single           rad=1, slope=1
  as single           eyepiece_diam=1, eyepiece_FL = 20 '' depth of field (experimental)
  as single           user
End Type


namespace AaDot           '2018 Jan 12 - by dafhi
  
  type tView3D
    as single             eye_z = -10
    as single             scale = 1
  End Type
  
  dim as tView3D          vie
  dim as imagevars ptr    im
  dim as dotvars          q
  dim as single           dy,dxLeft,salpha,cone_h,coneSq,sq,salpha0,slope
  dim as long             x0,y0,x1,y1,alph,alpha_max
  dim as single           r_expan, z, rad0
  
  sub render_target(byref buf as imagevars ptr):  im = buf
  end sub
  
  sub draw(x as single, y as single, col as ulong = -1, rad as single=1, slope as single=1)

    dim as long y0=(y-rad):  if y0<0 then y0=0
    dim as long y1=(y+rad):  if y1>im->hm then y1=im->hm
   
    if y1<y0 then exit sub '2017 Nov 10
    
    salpha0=(col shr 24)/255:  alpha_max=salpha0*256
   
    '' slope = 1 .. 1 pixel aa edge
    '' slope = 2 .. 1/2 pixel (sharp)
    '' slope = 1/q.rad .. max blur
    '' slope < 1/q.rad .. rendering artifact
   
    'sq=1/q.rad                   '' clamp prevents artifact
    'slope=iif(slope<sq,sq,slope)  ''
   
    cone_h = slope*(rad+.5)     'pre-inverted aadot imagined as cone \/
    coneSq = cone_h*cone_h    'avoid sqr() at blit corners
    sq = (cone_h-1)*(cone_h-1)'avoid sqr() in dot center at max brightness
    dim as long x0=(x-rad):  if x0<0 then x0=0
    dim as long x1=(x+rad):  if x1>im->wm then x1=im->wm

    dy=(y0-y)*slope: dxLeft=(x0-x)*slope
    for py as long ptr = @im->p32[ y0*im->pitchBy ] to @im->p32[ y1*im->pitchBy ] step im->pitchBy
      dim as single dx=dxleft, dySq=dy*dy
      for px as ulong ptr = @py[x0] to @py[x1]
        salpha = dx*dx+dySq
        if salpha<sq then
            Alpha256(*px,*px,col,alpha_max)
        elseif salpha<=coneSq then
            alph=(cone_h-sqr(salpha))*alpha_max
            Alpha256(*px,*px,col,alph)
        endif:  dx+=slope
      next: dy+=slope
    next

  end sub

  sub pdraw(byref pdv as dotvars ptr) '' depth of field (experimental)
    
    with *pdv
      r_expan = .eyepiece_diam*abs((.z-.eyepiece_fl)/(.z - vie.eye_z))
      z = 1 / (.z - vie.eye_z)
      q.rad = (.rad + r_expan) * z
      rad0 = .rad * z
      rad0 = rad0 / q.rad
      q.a = floor(.a * rad0*rad0 + .5)
      q.rad *= vie.scale
      q.slope = .slope / q.rad
      z *= vie.scale
      draw im->wh + .x*z, im->hh + .y*z, .col, q.rad, q.slope
    End With

  End Sub
end namespace


/'
sub dotvars.ini
  eyepiece_diam = .035
  eyepiece_fl = 1
  var sca = 8
  rad = (.5 + rnd) * sca * .001
  x = (rnd - .5) * sca
  y = (rnd - .5) * sca
  z = 0
  slope = 1 + rnd * 3
  col = -1
  col = 255 shl 24 or (rnd * &H1000000)
  user = -(rnd + .1) * 1
  o = p
End Sub
'/