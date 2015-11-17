      subroutine readin(iu,chem,id1,id2,x1,x2,x3,it,is,b1,b2,b3,b4,
     1                 b5,b6,btype)
      integer*2 id1,id2
      character*1 chain
      character*3 res
      character*4 atom
      character*6 rec
      character*8 chem
      b1=.1
      b2=0
      b3=0
      b4=0
      b5=0
      b6=0
      btype=7.
      id1=0
      id2=0
      it=2 
c     ***** read the pdb file *****
      read (iu,201) rec,iserno,atom,res,chain,id2,x1,x2,x3,occ,tf
  201 format(a6,i5,1x,a4,1x,a3,1x,a1,i4,4x,3f8.0,2f6.0)
      id1=9
      if (atom.eq.' N  ') id1=1
      if (atom.eq.' CA ') id1=2
      if (atom.eq.' C  ') id1=3
      if (atom.eq.' O  ') then
         id1=4
         b1=.15
      end if
      chem=atom(2:4)//res
      is=0 
      read (iu,202,end=203) rec
  202 format(a6)
      backspace(iu)
      return
  203 is=1
      return  
      end
