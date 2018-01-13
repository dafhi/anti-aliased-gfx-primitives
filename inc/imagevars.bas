/' ------- imagevars 2018 Jan 9 - by dafhi -------- '/

type myint as integer

type imagevars          '' 2018 Jan 9 - by dafhi
  '1. quick reference for ScreenInfo & ImageInfo
  '2. encapsulate standard metrics
  '3. convenient additional vars, subs and functions
  as myint              w,h,bpp,bypp,pitch,rate,  wm, hm, pitchBy, num_pages, flags 'helpers
  as any ptr            im, pixels
  as ulong ptr          p32
  as string             driver_name
  declare sub           get_info(im as any ptr=0)
  as single             wh, hh, diagonal
  declare sub           destroy
  declare               destructor
end type
Destructor.imagevars:  destroy
End Destructor
Sub imagevars.Destroy():  If ImageInfo(im) = 0 <> 0 Then ImageDestroy im: im = 0: endif:  End Sub
sub imagevars.get_info(im as any ptr)
  if im=0 then:  pixels=screenptr
    ScreenInfo w,h, bpp,, pitch, rate, driver_name:  bypp=bpp\8 '2018 Jan 9
  elseif Imageinfo(im)=0 then
    ImageInfo im, w, h, bypp, pitch, pixels
    bpp = bypp * 8:  this.im = im
  endif:  pitchBy=pitch\bypp
  wm=w-1: wh=w/2:  diagonal = sqr(w*w+h*h)
  hm=h-1: hh=h/2:  p32=pixels
end sub


#Macro Alpha256(ret,back, fore, a256) '2017 Mar 26
  ret=((_
  (fore And &Hff00ff) * a256 + _
  (back And &Hff00ff) * (256-a256) + &H800080) And &Hff00ff00 Or (_
  (fore And &H00ff00) * a256 + _
  (back And &H00ff00) * (256-a256) + &H008000) And &H00ff0000) Shr 8
#EndMacro

Union UnionARGB
  As Ulong col:  Type: As UByte  B,G,R,A:  End Type
End Union


/' ---------- reference -------------------

            i, i, i  , i       , i    , i
ScreenRes(  w, h, bpp, numPages, flags, refresh_rate) as integer
ScreenSet   work_page, visible_page
            i, i, i  , i   , i    , i   , str
ScreenInfo  w, h, bpp, bypp, pitch, rate, driver_info
            i, i, ul   , i
ImageCreate(w, h, color, bpp) as any ptr
            any  , i  i, i   , i    , any   , i  .... (any = any ptr)
ImageInfo(  img  , w, h, bypp, pitch, pixels, size_bytes) as integer

Print Using "The ASCII code for the pound sign (_#) is ###"; Asc("#")
 -------------------------------------- '/
