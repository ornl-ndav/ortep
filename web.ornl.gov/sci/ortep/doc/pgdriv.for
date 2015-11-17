c ****************************************************************
c *** PGPLOT CODE FOR SCREEN OUTPUT
c *** if PGPLOT is implemented, use the subroutines here
c *** instead of the ones in the DUMMY SCREEN OUTPUT section
c ****************************************************************

      subroutine initsc
      character*10 outdev
      common /ns/ npf,ndraw,norient,nvar
      integer pgbeg
      
      xwid=11.
      yhgt=8.5
      
c *** The following is for PGPLOT on an X-windows system.
c     outdev = '/XWINDOW'
c *** The following is for PGPLOT on an MS-DOS system.
c     outdev = '/MS'
c *** The following is for PGPLOT on a Macintosh system.
c     outdev = '/MAC'

      open(npf,status='scratch')

      if (pgbeg(0,' ',1,1) .ne. 1) call exitng(8)

c     switch black and white
      call pgscr(0,1.,1.,1.)
      call pgscr(1,0.,0.,0.)

c     set up drawing window
      call pgpage
      call pgqch(osize)
      call pgsch(0.)
      call pgvstd
      call pgwnad(0.,xwid,0.,yhgt)
      call pgsch(osize)
      call pgbox('BCT',1.,0,'BCT',1.,0)

      return
      end

      subroutine colrsc(icolor)
c *** set plot color
c *** in ORTEP icolor=0 => black
c *** PGPLOT is set up for 1=black
      common /ns/ npf,ndraw,norient,nvar
      icol=icolor
      if (icol.eq.0) icol=1
      nvar=icol
      if (ndraw.eq.1) call pgsci(icol)
      if (ndraw.eq.9) write (npf,111) icol
  111 format('COL',1x,i2)
      return
      end

      subroutine penwsc(penw)
c *** change pen width
c *** PGPLOT measures pen width in 200ths of an inch
      common /ns/ npf,ndraw,norient,nvar
      ipenw=nint(.001*penw*200.)
      if (ipenw.le.0) ipenw=1
      if (ipenw.gt.200) ipenw=200
      if (ndraw.eq.1) call pgslw(ipenw)
      if (ndraw.eq.9) write (npf,111) ipenw
  111 format('WID',1x,i3)
      return
      end

      subroutine pensc(x,y,ipen)
c *** move the pen
      common /trfac/ xtrans,ytrans
      common /ns/ npf,ndraw,norient,nvar

      if (ipen.eq.2) then
         if (ndraw.eq.1) call pgdraw(x+xtrans,y+ytrans)
         if (ndraw.eq.9) write (npf,111) x+xtrans,y+ytrans
  111    format('LIN',2(1x,f10.6))
      end if
      if (ipen.eq.3) then
         if (ndraw.eq.1) call pgmove(x+xtrans,y+ytrans)
         if (ndraw.eq.9) write (npf,112) x+xtrans,y+ytrans
  112    format('MOV',2(1x,f10.6))
      end if

      return
      end

      subroutine endsc
      common /ns/ npf,ndraw,norient,nvar

      call curssc
      close(npf)
      call pgend

      return
      end

      subroutine curssc
c *** correlate screen cursor position with atom positions and display results
      character ch
      character*16 str
      integer pgcurs
      character*6 label,alabel
      character*9 tomid,atomid
      common /trfac/ xtrans,ytrans
      common /ns/ npf,ndraw,norient,nvar

      call pgsfs(1)
      call pgscf(1)
      call pgsch(1.)

c *** get cursor position
 1    junk = pgcurs(x,y,ch)

      if (ch.eq.'x' .or. ch.eq.'X') return
      if (ichar(ch).eq.13) return

c *** initial values for variables
      xpt = x
      ypt = y
      odiffx = .125
      odiffy = .125
      atomid = 'not found'
      alabel = '      '
      iflag = 0

 11   rewind(npf)

      do 3 i = 1,600
         read(npf,12,end=4) label,tomid,xx,yy
 12      format(11x,a6,3x,a9,4x,2f8.0)
         diffx = abs(xx-xpt)
         diffy = abs(yy-ypt)
         if (diffx.le.odiffx .and. diffy.le.odiffy) then
            atomid = tomid
            alabel = label
            odiffx = diffx
            odiffy = diffy
            iflag = 1
         end if
  3   continue

  4   if (iflag.eq.0) then
         if (xpt.gt.xtrans) xpt = xpt-xtrans
         if (ypt.gt.ytrans) ypt = ypt-ytrans
         iflag = 1
         go to 11
      end if

      write(str,5) alabel,atomid
  5   format(a6,1x,a9)

c *** erase rectangle
      call pgsci(0)
      call pgsfs(1)
      call pgrect(0.,2.2,8.2,8.5)
c *** redraw empty rectangle
      call pgsci(1)
      call pgsfs(2)
      call pgrect(0.,2.2,8.2,8.5)

c *** print atom information in rectangle
      call pgtext(0.1,8.3,str)

      go to 1

      end

c *** end of PGPLOT specific routines
c ****************************************************************
