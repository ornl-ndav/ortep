c ****************************************************************
c *** HPGL FILE OUTPUT
c ****************************************************************

      subroutine inithp
      common /ns/ npf,ndraw,norient,nvar
      character ESC
      character*10 fname

      do 11 i=1,999
         write (fname, 10) i
   10    format('TEP',i3.3,'.PRN')
         open(unit=npf,file=fname,status='old',err=12)
         close(npf)
   11 continue
   12 open(unit=npf,file=fname,status='new')
      write (*,13) fname
   13 format(/,' HPGL file name:  ',a)

      ESC=char(27)

      write (npf,21) ESC
      write (npf,22) ESC
      write (npf,23)
      if (norient.eq.2) write (npf,24)
   21 format(a1,'E')
   22 format(a1,'%0B')
   23 format('IN;'/'SP1;'/'PW.15;')
   24 format('RO90.;')
      return
      end

      subroutine colrhp(icolor)
      common /ns/ npf,ndraw,norient,nvar
c *** set plot color
c *** in ORTEP icolor=0 => black
c *** plotter pen 1=black
      icol=icolor
      if (icol.eq.0) icol=1
      write (npf,21) icol
   21 format('SP',i1,';')
      return
      end

      subroutine penwhp(penw)
      common /ns/ npf,ndraw,norient,nvar
      if (penw.eq.0.) then
         penw=.15
      else      
         penw=penw*.0252
      end if
      write (npf,21) penw
   21 format('PW',f5.2,';')
      return
      end

      subroutine penhp(x,y,ipen)
      common /ns/ npf,ndraw,norient,nvar
      common /trfac/ xtrans,ytrans

      ix = nint((x + xtrans) * 1000.)
      iy = nint((y + ytrans) * 1000.)

      if (ipen.eq.2) write (npf,101) ix,iy
  101 format('PD',i4,',',i4,';')
      if (ipen.eq.3) write (npf,102) ix,iy
  102 format('PU',i4,',',i4,';')
      return
      end


      subroutine endhp
      common /ns/ npf,ndraw,norient,nvar
      character ESC

      ESC=char(27)
      write (npf,31) 
   31 format('PU;',/,'SP0;',/,'PG;',/,'IN;')
      write (npf,34) ESC
   34 format(a1,'%0A')
      write (npf,35) ESC
   35 format(a1,'E')
      close(npf)
      return
      end

c *** end of HPGL specific routines
c ****************************************************************
