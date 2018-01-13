/' -- Anti-aliased lines - 2018 Jan 10 - by dafhi 

  ' - usage -----------------------
  dim as imagevars      buf
  buf.screen_init 800,600

  aaline.render_target @buf
  screenlock
    line buf.im, (0,0)-(buf.wm, buf.hm), rgb(180,175,160), bf
    aaline.wid = 50
    aaline.draw 100,100,400,300, rgb(255,255,255)
  screenunlock:  sleep
  ' --------------------------------
  
  I will update this project as time permits.
  http://www.freebasic.net/forum/viewtopic.php?f=8&t=20719
'/

#include once "gmath.bas"

type aalinevars
  as single                   x0,y0,x1,y1, wid=1, endcap
  as ulong                    col = -1
End Type

namespace AaLine
  
  'dim As single               x0,y0,x1,y1,wid=1,endcap
  'dim As ulong                col
  dim as aalinevars           lv
  dim as aalinevars ptr       p
  
  dim As imagevars ptr        im
  
  Sub render_target(byref p as imagevars ptr): im = p '2017 Sep 10
  end sub
  
  dim As single               sx0,sy0,sx1,sy1,ax,bx,cx,dx,ay,by,cy,dy
  dim As single               ayi,byi,cyi,dyi
  dim As single               dxL,bxR,axL,axR,cxL,cxR
  dim As ulong ptr            cBL,cBR,cTL,cTR  'window pixels: bottom-left, ..
  dim As integer              w,wm,hm,pitchx,pitchy,wmprev,hmprev
  dim As single               abslope_small, abslope_large
  dim As single               slen,swid, sdx,sdy, angle, scosa,ssina, cenx,ceny
  dim As Single               lenBy2, smallBy2,largeBy2
  dim As single               da0,da1,ab0,ab1,bc0,bc1,cd0,cd1
  dim As single               cenabx, cenaby, cencdx, cencdy
  dim As integer              yflipped, xyflipped
  
  '' private
  Type ScanIntercepts
    as double                 ab,cd,bc,da
  End Type

  dim As ScanIntercepts       sc0x, sc1x, sc0y, sc1y
  dim As single               _aa(any), __alpha
  
  sub init
    wm=im->wm:  hm=im->hm
    if wm<>wmprev or hm<>hmprev then
      dim as integer ubmax = wm:  if hm > ubmax then ubmax = hm
      if ubmax > ubound(_aa) then ReDim _aa(ubmax)
      wmprev=wm: hmprev=hm
    end If
    pitchx=1: pitchy=im->pitchBy
    cBL=im->pixels: cTL=cBL+hm*pitchy
    cBR=CBL+wm: cTR=cTL+wm: yflipped=0: xyflipped=0: __alpha=256*(p->col shr 24)/255
  End sub
  
  Sub yflip
    pitchy=-pitchy: yflipped=-1
    swap cBL,cTL: swap cBR,cTR
    ceny=hm+1-ceny: ssina=-ssina
  end sub
  
  sub xyflip
    swap cTL,cBR: xyflipped = -1
    swap wm,hm: swap pitchx,pitchy
    swap cenx,ceny: swap scosa,ssina
  end sub
  
  Sub octant
    if scosa<0 then scosa=-scosa: ssina=-ssina
    if ssina<0 then yflip
    if ssina>scosa then xyflip
    w=wm+1
    ax=cenx-scosa: bx=cenx+scosa  '2017 June 4
    ay=ceny-ssina: by=ceny+ssina  '2017 June 4
  End sub
  
  sub corners
    abslope_small=ssina/scosa: smallBy2=abslope_small/2
    abslope_large=scosa/ssina: largeBy2=abslope_large/2
    dim as single widByLen = swid/slen
    dim as single hdxwid = ssina*widByLen, hdywid = scosa*widByLen
    cx=bx-hdxwid: dx=ax-hdxwid: ax=ax+hdxwid: bx=bx+hdxwid  '2017 June 4
    cy=by+hdywid: dy=ay+hdywid: ay=ay-hdywid: by=by-hdywid  '2017 June 4
    ayi=floor(ay): byi=floor(by): cyi=floor(cy): dyi=floor(dy)
    dxL=floor(dx): axL=floor(ax): axR=floor(ax)
    bxR=floor(bx): cxL=floor(cx): cxR=floor(cx)
    If dxL<0 Then: dxL=0: EndIf: If bxR>wm then: bxR=wm: endif
    If axL<0 Then: axL=0: EndIf: If axR>wm Then: axR=wm: EndIf
    If cxL<0 Then: cxL=0: EndIf: If cxR>wm Then: cxR=wm: EndIf
  End Sub
  
  Function xbound(x As Single) As Integer
    return x>=0 And x<w
  End Function
  
  Function ybound(yLo As single,yHi As single,y As single) As Integer
    return y>=yLo And y<= yHi
  End Function
  
  Function areasubt(ceptL As Single,ceptR As Single,edgeL As Single) As Single
    Dim As Single len_tri
    If ceptL<edgeL Then                 '  ceptL
      len_tri=ceptR-edgeL               ' -+-----+
      Return len_tri*len_tri*largeBy2   '   \####|
    Else: Dim As Integer  edgep=edgeL+1 '    \###|
      If ceptR<edgep Then               '     \##|
        Return ceptR-edgeL-smallBy2     ' -----+-+
      Else                              '  ceptR
        len_tri=edgep-ceptL           
      Return 1-len_tri*len_tri*largeby2: EndIf 
    EndIf
  End Function
  
  Sub subt_ab(x0 As single, x1 As single,y As integer)
      sc1y.ab=cenaby+(x0-cenabx)*abslope_small
      For x As Single=x0 To x1
        sc0y.ab=sc1y.ab
        sc1y.ab+=abslope_small
        _aa(x)-=areasubt(sc0y.ab,sc1y.ab,y)
      Next
  end Sub
  Sub subt_cd(x0 As single, x1 As single,y As Integer)
      sc1y.cd=cencdy+(x0-cencdx)*abslope_small
      For x As Single=x0 To x1
        sc0y.cd=sc1y.cd
        sc1y.cd+=abslope_small
        _aa(x)-=1-areasubt(sc0y.cd,sc1y.cd,y)
      Next
  end sub
  
  sub subt_da(x0 As single, x1 As single)
      For x As Integer=x0 To x1
        _aa(x)-=areasubt(sc1x.da,sc0x.da,x)
      Next
  end sub
  Sub subt_bc(x0 As single, x1 As single)
      For x As single=x0 To x1
        _aa(x)-=1-areasubt(sc1x.bc,sc0x.bc,x)
      Next
  End Sub
  
  Function area_oversubtracted(vx As Single,vy As Single,ix As integer,iy As Integer) As single
    vx=Abs(vx-ix)
    vy=Abs(vy-iy)
    var ceptYleft=vy-vx*abslope_small
    Dim As Single areaL
    ' area "low and left" of vertex
    If ceptYleft<0 Then      'triangle
      areaL=vy*vy*largeBy2
    Else                      'trapezoid
      areaL=vx*(ceptYleft+vy)/2
    End If
    ' area "low and right" of vertex
    Var ceptXBottom=vx+vy*abslope_small
    Var ixp=ix+1
    If ceptXBottom<=1 Then    'triangle
      Return areaL + vy*vy*smallBy2
    Else                      'trapezoid
      Var vx1=1-vx
      Return areaL+vx1*(vy-vx1*largeBy2)
    EndIf
  End Function
  
  Sub scanwrite(xL0 As integer, xR1 as integer, y As integer)
    dim as ulong ptr  pix=@cBL[xL0*pitchx+y*pitchY]
    for x as myint=xL0 to xR1
      dim as ulong a256 = __alpha * _aa(x)
      *pix=((_
      (p->col And &Hff00ff) * a256 + _
      (*pix And &Hff00ff) * (256-a256) + &H800080) And &Hff00ff00 Or (_
      (p->col And &H00ff00) * a256 + _
      (*pix And &H00ff00) * (256-a256) + &H008000) And &H00ff0000) Shr 8
      pix+=pitchx
    next
  End Sub
  
  Function int_lo(in As Single,clip As Single) As Single
    If in<clip Then: Return floor(clip): Else: Return floor(in): EndIf
  End Function
  
  Function int_hi(in As Single,clip As Single) As Single
    If in>clip Then: Return floor(clip): Else: Return floor(in): EndIf
  End Function
  
  sub scanlines_abcd(y0 as integer, y1 as single)
    if y0<0 then y0=0
    if y1>hm then y1=hm
    sc0x.cd=sc1x.cd:  sc1x.cd=cencdx+(y0-cencdy)*abslope_large
    sc0x.bc=sc1x.bc:  sc1x.bc=bx-(y0-by)*abslope_small
    for y As Integer=y0 to y1
      sc0x=sc1x
      sc1x.ab+=abslope_large
      sc1x.cd+=abslope_large
      sc1x.da-=abslope_small
      sc1x.bc-=abslope_small
      Dim As Integer  inda=ybound(ayi,dyi,y)
      Dim As Integer  inab=ybound(ayi,byi,y)
      Dim As Integer  incd=ybound(dyi,cyi,y)
      Dim As Integer  inbc=ybound(byi,cyi,y)
      Dim As single  xL1=-1,xL0=wm+1
      If inda Then
        da0=int_lo(sc1x.da,dxL): If da0<xL0 Then xL0=da0
        da1=int_hi(sc0x.da,axR): If da1>xL1 Then xL1=da1
      EndIf
      If incd Then
        cd0=int_lo(sc0x.cd,dxL): If cd0<xL0 Then xL0=cd0
        cd1=int_hi(sc1x.cd,cxR): If cd1>xL1 Then xL1=cd1
      EndIf
      Dim As single  xR1=-1,xR0=wm+1
      If inab Then
        ab0=int_lo(sc0x.ab,axL): If ab0<xR0 Then xR0=ab0
        ab1=int_hi(sc1x.ab,bxR): If ab1>xR1 Then xR1=ab1
      EndIf
      If inbc Then
        bc0=int_lo(sc1x.bc,cxL): If bc0<xR0 Then xR0=bc0
        bc1=int_hi(sc0x.bc,bxR): If bc1>xR1 Then xR1=bc1
      EndIf
      For x as integer=xL0 to xR1
        _aa(x)=1
      Next
      If inda Then subt_da da0,da1
      If inab Then subt_ab ab0,ab1,y
      If inbc Then subt_bc bc0,bc1
      If incd Then subt_cd cd0,cd1,y
      If y=ayi And xbound(ax) Then
        _aa(axL)+=area_oversubtracted(ax,ay,axL,ayi)
      EndIf
      If y=byi And xbound(bx) Then
        _aa(bxR)+=area_oversubtracted(by,bx,byi,bxR+1)
      EndIf
      If y=cyi And xbound(cx) Then
        _aa(cxR)+=area_oversubtracted(cx,cy,cxR+1,cyi+1)
      EndIf
      If y=dyi And xbound(dx) Then
        _aa(dxL)+=area_oversubtracted(dy,dx,dyi+1,dxL)
      EndIf
      scanwrite xL0,xR1,y
    next
  end sub
  
  sub scanlines_adb(y0 as integer, y1 as single)
    if y0<0 then y0=0
    if y1>hm then y1=hm
    if ax < w-cx then 'bc closest
      cenabx=bx: cenaby=by
      cencdx=cx: cencdy=cy
    else
      cenabx=ax: cenaby=ay
      cencdx=dx: cencdy=dy
    end if
    sc1x.da=ax-(y0-ay)*abslope_small
    sc1x.ab=cenabx+(y0-cenaby)*abslope_large
    for y As Integer=y0 to y1
      sc0x.da=sc1x.da: sc1x.da-=abslope_small
      sc0x.ab=sc1x.ab: sc1x.ab+=abslope_large
      Dim As single   xL1=-1,xL0=wm+1
      da0=int_lo(sc1x.da,dxL): If da0<xL0 Then xL0=da0
      da1=int_hi(sc0x.da,axR): If da1>xL1 Then xL1=da1
      Dim As single   xR1=-1,xR0=wm+1
      ab0=int_lo(sc0x.ab,axL): If ab0<xR0 Then xR0=ab0
      ab1=int_hi(sc1x.ab,bxR): If ab1>xR1 Then xR1=ab1
      For x as integer=xL0 to xR1
        _aa(x)=1
      Next
      subt_da da0,da1
      subt_ab ab0,ab1,y
      If y=ayi And xbound(ax) Then
        _aa(axL)+=area_oversubtracted(ax,ay,axL,ayi)
      EndIf
      scanwrite xL0,xR1,y
    next
  end sub
  
  sub scanlines_cdb(y0 as integer, y1 as integer)
    if y0<0 then y0=0
    if y1>hm then y1=hm
    for y As Integer=y0 to y1
      sc0x.cd=sc1x.cd:  sc1x.cd+=abslope_large
      sc0x.bc=sc1x.bc:  sc1x.bc-=abslope_small
      Dim As single  xL1=-1,xL0=wm+1
      cd0=int_lo(sc0x.cd,dxL): If cd0<xL0 Then xL0=cd0
      cd1=int_hi(sc1x.cd,cxR): If cd1>xL1 Then xL1=cd1
      Dim As single  xR1=-1,xR0=wm+1
      bc0=int_lo(sc1x.bc,cxL): If bc0<xR0 Then xR0=bc0
      bc1=int_hi(sc0x.bc,bxR): If bc1>xR1 Then xR1=bc1
      For x as integer=xL0 to xR1
        _aa(x)=1
      Next
      subt_bc bc0,bc1
      subt_cd cd0,cd1,y
      If y=cyi And xbound(cx) Then
        _aa(cxR)+=area_oversubtracted(cx,cy,cxR+1,cyi+1)
      EndIf
      scanwrite xL0,xR1,y
    next
  end sub
  
  sub scanlines_db(y0 as integer, y1 as single)
    if y0<0 then y0=0
    if y1>hm then y1=hm
    for y As Integer=y0 to y1
      sc0x.ab=sc1x.ab:  sc1x.ab+=abslope_large
      sc0x.cd=sc1x.cd:  sc1x.cd+=abslope_large
      Dim As single   xL1=-1,xL0=wm+1
      cd0=int_lo(sc0x.cd,dxL): If cd0<xL0 Then xL0=cd0
      cd1=int_hi(sc1x.cd,cxR): If cd1>xL1 Then xL1=cd1
      Dim As single   xR1=-1,xR0=wm+1
      ab0=int_lo(sc0x.ab,axL): If ab0<xR0 Then xR0=ab0
      ab1=int_hi(sc1x.ab,bxR): If ab1>xR1 Then xR1=ab1
      For x as integer=xL0 to xR1
        _aa(x)=1
      Next
      subt_ab ab0,ab1,y
      subt_cd cd0,cd1,y
      scanwrite xL0,xR1,y
    next
  end sub
  
  sub re_center
    lenBy2=slen/2
    scosa=cos(angle)*lenBy2: ssina=sin(angle)*lenBy2
    var dx0=im->wh - p->x0, dy0=im->hh - p->y0 ''
    var dx1=im->wh - p->x1, dy1=im->hh - p->y1 ''
    const c = .5
    if dx0*dx0+dy0*dy0 < dx1*dx1+dy1*dy1 then 'point 0 closest to center
      cenx=p->x0 + scosa+c ''
      ceny=p->y0 + ssina+c ''
    else
      cenx=p->x1 - scosa+c ''
      ceny=p->y1 - ssina+c ''
    endif
  end sub
  
  sub handle_infinity
    sdx=p->x1-p->x0: sdy=p->y1-p->y0 ''
    slen=sqr(sdx*sdx+sdy*sdy)
    if slen>1e9 then:  slen=1e9
      const sincos=1e9*sqr(1/2)
      sdx=sincos*sgn(sdx)
      sdy=sincos*sgn(sdy)
    endif
    if sdx=0 then
      if sdy<0 then: angle= -pi/2
      else: angle=pi/2: endif
    else: angle=atn(sdy/sdx):  if sdx<0 then angle+=pi
    endif
    re_center
    swid=p->wid
    if swid>1e9 then swid=1e9
    slen+=p->endcap*swid
    if slen < p->wid then
      angle+=pi/2:  swap swid,slen
    end if: lenBy2=slen/2

    '' fix to make compiler happy
    if angle=0 or abs(angle-pi)<0.00001 then angle += 0.0001
   
    scosa=cos(angle)*lenBy2: ssina=sin(angle)*lenBy2
  End sub
  
  Sub calc
    handle_infinity
    'if slen <= 0 then exit sub
    if im=0 orelse im->bpp <> 32 then
      static as integer show_msg=1
      if show_msg then
        if im->im=0 then: ? "AaLine:  invalid render target"
        else: ? "AaLine:  target must be 32bpp"
        end if: sleep 1000: show_msg=0
      endif
    end if
    init
    octant
    corners
    if dyi<=byi then
      scanlines_adb ayi,dyi-1
      if dyi<byi-1 then
        scanlines_abcd dyi, dyi
        scanlines_db dyi+1, byi-1
        scanlines_abcd byi, byi
      else
        scanlines_abcd dyi, byi
      end if
      scanlines_cdb byi+1,cyi
    else
      scanlines_adb ayi,byi-1
      scanlines_abcd byi, dyi
      scanlines_cdb dyi+1,cyi
    end if
  End sub
  
  sub draw(_x0 as single=0,_y0 as single=0,_x1 as single=0,_y1 as single=0, _
          _col as ulong=-1, _wid as single=1, _endcap as single=0)
    p = @lv
    p->x0 = _x0: p->x1=_x1: p->y0=_y0: p->y1=_y1
    p->col = _col: p->wid=_wid: p->endcap=_endcap: calc
  end sub
  
  sub pdraw(byref pp as aalinevars ptr)
    p = pp
    'x0=p->x0: x1=p->x1: y0=p->y0: y1=p->y1
    'col=p->col: wid=p->wid: endcap=p->endcap
    calc
  End Sub
  
end namespace ' ---- aaline