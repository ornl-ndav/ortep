c ****************************************************************
c *** POSTSCRIPT FILE OUTPUT
c ****************************************************************

      subroutine initps
      common /ns/ npf,ndraw,norient,nvar
      common /ps/ ixmin,ixmax,iymin,iymax,ixt,iyt
      character*10 fname

c *** initialize variables to calculate bounding box
      ixmin=20000
      ixmax=0
      iymin=20000
      iymax=0

      do 11 i=1,999
         write (fname, 10) i
   10    format('TEP',i3.3,'.PRN')
         open(unit=npf,file=fname,status='old',err=12)
         close(npf)
   11 continue
   12 open(unit=npf,file=fname,status='new')
      write (*,13) fname
   13 format(/,' Postscript file name:  ',a)

      ixt=0
      iyt=0
      write (npf,21)
      write (npf,22)
      write (npf,23)
      if (norient.eq.2) then
         write (npf,24)
         iyt=nvar
      else
         write (npf,25)
      end if
      write (npf,26)
      write (npf,27)
      write (npf,28)
      write (npf,29)
      write (npf,30)
      write (npf,31)
      write (npf,32) ixt,iyt
      if (norient.eq.2) write (npf,33)
      write (npf,34)
      write (npf,35)
   21 format('%!PS-Adobe-3.0 EPSF-3.0')
   22 format('%%Creator: ORTEP-III')
   23 format('%%BoundingBox: (atend)',/,'%%Pages: 1')
   24 format('%%Orientation: Landscape')       
   25 format('%%Orientation: Portrait')       
   26 format('%%BeginProlog')
   27 format('/m {moveto} def')
   28 format('/l {lineto} def')
   29 format('%%EndProlog',/,'%%Page: 1 1')
   30 format('%%BeginPageSetup')
   31 format('0.072 0.072 scale')
   32 format(i6,1x,i6,' translate')
   33 format('-90 rotate')
   34 format('0 setgray 1 setlinecap 5 setlinewidth')
   35 format('%%EndPageSetup')
      return
      end

      subroutine colrps(icolor)
      common /ns/ npf,ndraw,norient,nvar
      write(npf,101)
  101 format('stroke')
      if (icolor.eq.0) write(npf,1)
      if (icolor.eq.2) write(npf,2)
      if (icolor.eq.3) write(npf,3)
      if (icolor.eq.4) write(npf,4)
      if (icolor.eq.5) write(npf,5)
      if (icolor.eq.6) write(npf,6)
      if (icolor.eq.7) write(npf,7)
   1  format('0 setgray')
   2  format('1 0 0 setrgbcolor')
   3  format('0 1 0 setrgbcolor')
   4  format('0 0 1 setrgbcolor')
   5  format('0 1 1 setrgbcolor')
   6  format('1 0 1 setrgbcolor')
   7  format('1 1 0 setrgbcolor')
      return
      end

      subroutine penwps(penw)
      common /ns/ npf,ndraw,norient,nvar
      write(npf,101)
  101 format('stroke')
      if (penw.eq.0.) penw=5.
      write(npf,102) penw
  102 format(f10.2,1x,'setlinewidth')
      return
      end

      subroutine penps(x,y,ipen)
      common /trfac/ xtrans,ytrans
      common /ns/ npf,ndraw,norient,nvar
      common /ps/ ixmin,ixmax,iymin,iymax,ixt,iyt

      ix = nint((x + xtrans) * 1000.)
      iy = nint((y + ytrans) * 1000.)

      if (ipen.eq.2) write (npf,101) ix,iy
  101 format(i6,1x,i6,1x,'l')
      if (ipen.eq.3) write (npf,102) ix,iy
  102 format('stroke'/i6,1x,i6,1x,'m')

c *** variables to calculate bounding box
      if (ix.lt.ixmin) ixmin=ix
      if (ix.gt.ixmax) ixmax=ix
      if (iy.lt.iymin) iymin=iy
      if (iy.gt.iymax) iymax=iy

      return
      end

      subroutine endps
      common /ns/ npf,ndraw,norient,nvar
      common /ps/ ixmin,ixmax,iymin,iymax,ixt,iyt

      write (npf,25)
   25 format('stroke'/'showpage')

c *** calculate bounding box
      if (norient.eq.1) then
         ixmn=float(ixmin+ixt)*.072 - 2
         iymn=float(iymin+iyt)*.072 - 2
         ixmx=float(ixmax+ixt)*.072 + 2
         iymx=float(iymax+iyt)*.072 + 2
      else
         ixmn=float(ixt+iymin)*.072 - 2
         iymn=float(iyt-ixmax)*.072 - 2
         ixmx=float(ixt+iymax)*.072 + 2
         iymx=float(iyt-ixmin)*.072 + 2
      end if
      if (ixmn.lt.0) ixmn=0
      if (iymn.lt.0) iymn=0

c *** put bounding box at end of postscript file
      write (npf,26) ixmn,iymn,ixmx,iymx
   26 format('%%BoundingBox: ',4(i6,1x))

      write (npf,27)
   27 format('%%Trailer'/'%%EOF')
      close(npf)
      
      return
      end

c *** end of Postscript specific routines
c ****************************************************************
