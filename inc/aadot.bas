/' ------- aadot 2025 Sep 13 - by dafhi -------- '/

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
End Type


namespace AaDot
  
  type tView3D
    as single             eye_z = -10
    as single             scale = 1
  End Type
  
  dim as imagevars ptr    im
  
  sub render_target(byref buf as imagevars ptr):  im = buf
  end sub
    dim as tView3D          vie
    dim as dotvars          q
    dim as single           a_sdx()
    dim as single           dy,dxLeft,salpha,cone_h,coneSq,sq,salpha0
    dim as long             x0,y0,x1,y1,alph,alpha_max
    dim as single           r_expan, z, rad0
  
  sub draw(x as single, y as single, col as ulong = -1, rad as single=1, slope as single=1)
   
    x0 = max(x-rad,0)
    y0 = max(y-rad,0)
    x1 = min(x+rad, im->wm)
    y1 = min(y+rad, im->hm)
    
    #if 1
    '' 2025 Sep 13  version
    
    dim as single radsq = rad ^ 2
    dim as single component_sa = 256.499 * (col shr 24) / 255
    dim as single sharp = slope * component_sa
    dim as single sharp_by_rSq = sharp / radsq
  
    #define more_optimized
  
      ' Pre-calculate squared x delta with additional adjustment to eliminate nested multiply
    if im->wm > ubound(a_sdx) then redim a_sdx( im->wm)
    for ix as long = x0 to x1
        #ifdef more_optimized
        a_sdx(ix) = (ix - x)^2 * sharp_by_rSq
        #else
        a_sdx(ix) = (ix - x)^2
        #endif
    next

        for iy as long = y0 to y1
    
    dim as ulong ptr pixel = im->pixels + iy * im->pitch
    dim as single    dySq  = ( iy - y ) ^ 2
    dim as single    r     = sqr_safe(radsq - dySq) - .5 ' circle-constrained scan width
    dim as single    f_dy  = dySq * sharp_by_rSq         ' precalc to remove nested multiply

    dim as long      ix0 = max( x - r, x0 ), ix1 = min( x + r, x1 )
        for ix as long = ix0 to ix1

    #ifdef more_optimized
    dim as long final_a = sharp - ( a_sdx(ix) + f_dy )  ' 2025 Sep 8 :  VERY optimized
    alpha256( pixel[ix], pixel[ix], col, min(final_a, component_sa) )
    
    #else
    dim as long alpha = sharp * ( 1 - ( a_sdx(ix) + dySq ) / radsq ) ' from previous work, somewhat optimized
    alpha256( pixel[ix], pixel[ix], col, clamp(alpha, component_sa ))
    
    #endif
    
    next ix
    next iy
    
    #else ' 2018 Jan 12  version

    '' slope = 1 .. 1 pixel aa edge
    '' slope = 2 .. 1/2 pixel (sharp)
    '' slope = 1/q.rad .. max blur
    '' slope < 1/q.rad .. rendering artifact
   
    if y1<y0 then exit sub '2017 Nov 10
    
    salpha0=(col shr 24)/255:  alpha_max=salpha0*256
   
    'sq=1/q.rad                   '' clamp prevents artifact
    'slope=iif(slope<sq,sq,slope)  ''
    
    cone_h = slope*(rad+.5)     'pre-inverted aadot imagined as cone \/
    coneSq = cone_h*cone_h    'avoid sqr() at blit corners
    sq = (cone_h-1)*(cone_h-1)'avoid sqr() in dot center at max brightness

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
    
    #endif

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
  col = 255 shl 24 or (rnd * &H1000000)
  o = p
End Sub
'/
