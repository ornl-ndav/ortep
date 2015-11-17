c *** Inclusion of this subroutine in ORTEP-III updates the program to
c *** VERSION 1.0.1

      SUBROUTINE BOND(Z1,Z2,NB,NA1,NA2)
      REAL*8 Z1,Z2,WD(2),TD,D100,D1000,D100K
      DIMENSION B1(3,3),E(3,3),S(3,3),U(3,3),VUE(3)
      DIMENSION V7(3),W(13,2),Z(3),RESB(2),r(3)
      REAL*8 AIN,ATOMID 
      CHARACTER*4 TITLE,TITLE2
      CHARACTER*8 CHEM
      COMMON NG,A(9),AA(3,3),AAREV(3,3),AAWRK(3,3),AID(3,3)
     1 ,AIN(140),ATOMID(500),ATOMS(3,500),BB(3,3),BRDR,CD(8,20)
     2 ,CONT(5),D(3,130),DA(3,3),DP(2,130),DISP,EDGE,FORE,FS(3,3,96)
     3 ,IN,ITILT,KD(5,20),LATM,NATOM,NCD,NJ,NJ2,NOUT,NSR,NSYM
     4 ,ORGN(3),PAC(3,5),PAT(3,3),Q(3,3),REFV(3,3),RES(4),RMS(5),SCAL1
     5 ,SCAL2,SCL,SYMB(3,3),TAPER,THETA,TITLE(18),TITLE2(18),TS(3,96)
     6 ,VIEW,VT(3,4),V1(4),V2(3),V3(3),V4(3),V5(3),V6(3),WRKV(3,3)
     7 ,XLNG(3),XO(3),XT(3)
      INTEGER*2 IDENT           
      COMMON /PARMS/ CHEM(505),EV(3,505),P(3,505),PA(3,3,505)        
     1 ,IDENT(2,505),MAXATM
C     ***** OBTAIN POSITIONAL PARAMETERS *****
      DATA RESB/.2,.08/
      D100=100.
      D1000=1000.
      D100K=100000. 
      NG1=0
      DO 105 J=1,26
  105 W(J,1)=0.
      WD(1)=Z1
      WD(2)=Z2
      DO 135 I=1,2
      CALL XYZ(WD(I),W(4,I),2)
      IF(NG)125,110,125
  110 DO 115 J=1,3
  115 W(J+6,I)=XT(J)
      K=WD(I)/D100K  
      L=DMOD(WD(I)/D100,D1000)
      L1=DMOD(WD(I),D100)
      CALL PLTXY(W(4,I),W(2,I))
      IF(EDGE-BRDR*.25)120,128,128
  120 NG=10
  125 NG1=1
      IF (NOUT.GE.0)
     &WRITE(NOUT,136)CHEM(K),K,L,L1,(W(J,I),J=2,9)
      CALL ERPNT(WD(I),800)
      GO TO 134
  128 IF(NJ2-10)130,134,134
  130 IF (NOUT.GE.0)
     &WRITE(NOUT,136)CHEM(K),K,L,L1,(W(J,I),J=2,9)
  134 continue
  135 CONTINUE
  136 FORMAT(1H 10X,A6,3H  (I3,1H,I3,I2,4H)   2F8.2,5X,3F8.3,13X,3F8.4)
      IF(NG1)999,137,999
  137 CALL DIFV(W(7,1),W(7,2),V7)
      DIST=SQRT(VMV(V7,AA,V7))
      IF(MOD(NJ2,2).EQ.0) GO TO 143
      IF(MOD(NJ2,10).EQ.1) GO TO 143
c *** Line bonds with NO symbol on atom position (803,813)
      if (iabs(kd(5,nb)).ge.1) then
         call draw(W(2,1),0.,0.,3)
         call draw(W(2,2),0.,0.,2)
         go to 570
      end if
C *** LINE BONDS AND CENTERED SYMBOLS (803,813) 
      HGT=SCL*.12
      CALL SIMBOL(W(2,1),W(3,1),HGT,' ',0.,-1)
      CALL SIMBOL(W(2,2),W(3,2),HGT,' ',0.,-2)
      GO TO 570
C     ***** STICK BONDS FOR 801,802,811,812 *****
  143 KODE=KD(5,NB)
c *** check for dashed bonds
      kdash=0
      if (iabs(kode).ge.10) then
         kdash=iabs(kode)
         if (kode.lt.0) then
            kode=-1
         else
            kode=1
         end if
      end if
      IF(KODE)145,144,146
  144 NBND=0
      GO TO 148
  145 KODE=-KODE
  146 NBND=128/2**KODE
C     ***** FIND UPPERMOST ATOM PUT IN POSITION ONE *****
  148 IF(VIEW)152,150,152
  150 W(12,1)=1.
      W(12,2)=1.
      IF(W(6,1)-W(6,2))165,175,175
C     *****VECTOR FROM ATOM TO VIEWPOINT *****
  152 DO 160 I=1,2
      DO 155 J=10,12
  155 W(J,I)=-W(J-6,I)
      W(12,I)=W(12,I)+VIEW
C     ***** DISTANCE SQUARED TO VIEWPOINT *****
  160 W(13,I)=VV(W(10,I),W(10,I))
      IF(W(13,2)-W(13,1))165,175,175
C     ***** SWITCH ATOMS *****
  165 DO 170 J=1,13
      T1=W(J,1)
      W(J,1)=W(J,2)
  170 W(J,2)=T1
      TD=WD(1)
      WD(1)=WD(2)
      WD(2)=TD
C     ***** FORM IDEMFACTOR MATRIX *****
  175 DO 180 J=1,3
      E(J,J)=1.
      E(J+1,1)=0.
  180 E(J+5,1)=0.
C     ***** FORM VECTOR SET RADIAL TO BOND *****
      CALL DIFV(W(4,2),W(4,1),DA(1,3))
      CALL UNITY(DA(1,3),V3,1)
C     ***** UNIT VECTOR FROM BOND MIDPOINT TO REFERENCE VIEWPOINT *****
      DO 183 I=1,3
      V2(I)=0.0
      DO 181 J=1,3
  181 V2(I)=V2(I)+AAREV(J,3)*WRKV(J,I)
      IF(VIEW)183,183,182
  182 V2(I)=V2(I)*VIEW-0.5*(W(I+3,1)+W(I+3,2))
  183 CONTINUE
      CALL UNITY(V2,V2,1)
      T6=ABS(VV(V3,V2))
      IF(.9994-T6)185,185,187
C     ***** ALTERNATE CALC IF BOND IS ALONG REFERENCE VIEW DIRECTION ***
  185 DO 186 J=1,3
  186 V2(J)=W(J+9,1)+W(J+9,2)
      CALL UNITY(V2,V2,1)
      T6=ABS(VV(V3,V2))
      IF(.9994-T6)390,390,187
  187 CALL AXES(V3,V2,B1,1)
  188 T1=CD(3,NB)/SCAL2
      DO 190 J=1,3
      DA(J,1)=-B1(J,2)*T1
  190 DA(J,2)=-B1(J,3)*T1
      IF(NBND)500,500,195
C     ***** SET PLOTTING RESOLUTION FOR BOND *****
  195 T1=CD(3,NB)*SCL
      NRESOL=4
      NBIS=3
      DO 200 J=1,2
      IF(T1.GE.RESB(J)) GO TO 202
      IF(NBND.LE.NRESOL) GO TO 202
      NBIS=NBIS-1
  200 NRESOL=NRESOL*2
  202 NRES1=NRESOL+1
      CALL RADIAL(NBIS)
C     ***** DERIVE QUADRICS FOR EACH ATOM *****
      DO 380 II=1,2
      CALL PAXES(WD(II),2)
      IF(NG)205,210,205
  205 CALL ERPNT(WD(II),800)
      GO TO 999
C     ***** DOES BOND GO TO ELLIPSOID OR TO ENVELOPE *****
  210 T1=3-II*2
      DO 212 J=1,3
      V3(J)=V3(J)*T1
  212 VUE(J)=0.
      IF(KD(5,NB))260,260,215
  215 IF(VMV(V3,Q,W(10,II)))220,260,260
  220 IBND=0
      IF(VIEW)240,240,225
C     ***** DERIVE TANGENT CONE DIRECTLY WITHOUT ROTATING COORDINATES **
  225 T2=-(SCAL2*RMS(1)*RMS(2)*RMS(3))**2
      DO 230 J=1,3
      V1(J)=-W(J+9,II)/SCAL1
      VUE(J)=V1(J)/SCAL2
C     ***** INVERT ELLIPSOID MATRIX *****
      DO 230 K=J,3
      T1=0.0
      DO 228 I=1,3
  228 T1=T1+PAC(J,I)*PAC(K,I)*RMS(I)**2
      U(J,K)=T1
  230 U(K,J)=T1
C     *****  ADD POLARIZED COFACTOR MATRIX TO ELLIPSOID MATRIX *****
      DO 235 J=1,3
      J1=MOD(J,3)+1
      VJ1=V1(J1)
      J2=MOD(J+1,3)+1
      VJ2=V1(J2)
      DO 235 K=J,3
      K1=MOD(K,3)+1
      K2=MOD(K+1,3)+1
      S(J,K)=T2*Q(J,K)+(VJ2*(U(J1,K1)*V1(K2)-U(J1,K2)*V1(K1))
     1                + VJ1*(U(J2,K2)*V1(K1)-U(J2,K1)*V1(K2)))
  235 S(K,J)=S(J,K)
      T5=0.0
      GO TO 300
C     ***** DERIVE TANGENT CYLINDER WITH AXIS ALONG Z *****
  240 T1=-1.0/Q(3,3)
      DO 250 J=1,2
      DO 245 K=1,2
  245 S(K,J)=Q(K,J)+Q(K,3)*Q(J,3)*T1
      S(3,J)=0.0
  250 S(J,3)=0.0
      S(3,3)=0.0
      GO TO 270
C     ***** TRANSFER ELLIPSOID *****
  260 DO 265 J=1,9
  265 S(J,1)=Q(J,1)
      IBND=II
  270 T5=1.
C     ***** CHECK FOR BOND TAPER *****
  300 IF(II-2)305,310,310
  305 RADIUS=1.+T6*TAPER
      GO TO 320
  310 RADIUS=1.-T6*TAPER
  320 CALL MV(S,V3,V4)
      T2=VV(V3,V4)
C     ***** COMPUTE BOND INTERSECTION *****
      KL=5-II-II
      KSTP=NRESOL
      IF(NJ2-21)324,322,322
  322 KSTP=32
  324 DO 335 K=1,65,KSTP
      DO 325 J=1,3
      V6(J)=D(J,K)*RADIUS
  325 V5(J)=V6(J)+VUE(J)
      T3=VV(V5,V4)
      T4=T3*T3-T2*(VMV(V5,S,V5)-T5)
      IF(T4)345,330,330
  330 T4=SQRT(T4)
      T1=(T4-T3)/T2
      T3=(-T4-T3)/T2
      L=K+KL-1
      DO 335 J=1,3
      D(J,L)=(V6(J)+T1*V3(J))*SCL
  335 D(J,L+1)=(-V6(J)-T3*V3(J))*SCL
      IF(IBND+21-NJ2)360,338,360
  338 IF(KD(5,NB))360,360,340
C     ***** FOR LOCAL OVERLAP, MAKE BOND QUADRANGLE TANGENT TO ENVELOPING CONE
  340 T3=VV(VUE,V4)
      T4=T3**2-T2*(VMV(VUE,S,VUE)-T5)
      IF(T4)345,350,350
  345 NG=13
      CALL ERPNT(WD(II),800)
      GO TO 999
  350 T1=(SQRT(T4)-T3)/T2
      DO 355 J=1,3
      T4=(T1*V3(J)*SCL-0.5*(D(J,KL)+D(J,KL+64)))*1.001
      D(J,KL)=D(J,KL)+T4
  355 D(J,KL+64)=D(J,KL+64)+T4
  360 CALL PROJ(D(1,KL),DP(1,II),W(4,II),XO,VIEW,1,65,KSTP)
      IF(IBND-1)370,365,370
  365 CALL PROJ(D(1,KL+KSTP+1),DP(1,II+64+KSTP),W(4,II),XO,VIEW,1,
     & 65-KSTP,KSTP)
      GO TO 380
C     ***** RETRACE TOP HALF *****
  370 KK=64-(II-1)*KSTP
      DO 375 K=KSTP,KK,KSTP
      L=K+II
      M=L+64
      N=66-L
      DP(1,M)=DP(1,N)
  375 DP(2,M)=DP(2,N)
  380 CONTINUE
C     ***** CHECK FOR LOCAL OVERLAP OR HIDDEN BOND *****
      DO 395 K=1,65,32
      T1=0.
      T2=0.
      DO 385 J=1,2
      T1=T1+(DP(J,K)-W(J+1,1))**2
  385 T2=T2+(DP(J,K+1)-W(J+1,1))**2
      IF(T2-T1)390,390,395
  395 CONTINUE
C     ***** CALL GLOBAL OVERLAP ROUTINE *****
      ICQ=0
      CALL LAP800(NA1,NA2,ICQ)
      IF(NJ2-21)400,999,999
  400 IF(ICQ)390,405,405
  405 continue
c *** draw dashed stick bonds
      if (kdash.ne.0) then
c        draw bond ends
         call draw(dp(1,1),0.,0.,3)
         do 406 k=nres1,129,nresol
  406    call draw(dp(1,k),0.,0.,2)
         call draw(dp(1,2),0.,0.,3)
         do 408 k=2,66,nresol
  408    call draw(dp(1,k),0.,0.,2)
c        draw dashed parts
         r(3)=0.
         sdash=kdash/10
         fract=mod(kdash,10)
         if (fract.eq.0.) fract=5.
         do 410 k=1,65,64
            x1=dp(1,k+1)
            y1=dp(2,k+1)
            x2=dp(1,k)
            y2=dp(2,k)
            denom=sdash*fract+(sdash-1.)*(10.-fract)
            ddx=(x2-x1)/denom
            ddy=(y2-y1)/denom
            call draw(dp(1,k+1),0.,0.,3)
            npart=2.*sdash-1.
            do 410 j=1,npart
               if (mod(j,2).eq.1) then
                  r(1)=x1+ddx*fract
                  r(2)=y1+ddy*fract
                  call draw(r,0.,0.,2)
               else
                  r(1)=x1+ddx*(10.-fract)
                  r(2)=y1+ddy*(10.-fract)
                  call draw(r,0.,0.,3)
               end if
               x1=r(1)
               y1=r(2)
  410    continue
         go to 500
      end if      
c *** draw non-dashed stick bonds
C     ***** DRAW BOND OUTLINE *****
      CALL DRAW(DP(1,1),0.,0.,3)
      DO 415 K=NRES1,129,NRESOL
  415 CALL DRAW(DP(1,K),0.,0.,2)
      DO 420 K=2,66,NRESOL
  420 CALL DRAW(DP(1,K),0.,0.,2)
      CALL DRAW(DP(1,65),0.,0.,2)
C     ***** DRAW BOND DETAIL *****
  425 K=65
  430 K=K-NBND
      IF(K-1)500,500,435
  435 CALL DRAW(DP(1,K),0.,0.,3)
      CALL DRAW(DP(1,K+1),0.,0.,2)
      K=K-NBND
      IF(K-1)500,500,440
  440 CALL DRAW(DP(1,K+1),0.,0.,3)
      CALL DRAW(DP(1,K),0.,0.,2)
      GO TO 430

  500 HGT=CD(4,NB)
      OFF=CD(5,NB)
      IF(HGT)570,570,510
C     ***** PERSPECTIVE BOND LABEL ROUTINE *****
C     ***** BASE DECISIONS ON REFERENCE SYSTEM *****
  510 K=0
      CALL DIFV(W(7,2),W(7,1),V7)
      CALL VM(V7,AAREV,V1)
      CALL AXES(V1,E(1,3),U,1)
      DO 535 I=1,3
      T1=1.
      IF(I-2)515,515,520
  515 IF(VV(U(1,I),SYMB(1,I)))525,530,530
  520 IF(MOD(K,2))530,525,530
  525 T1=-1.
      K=K+1
  530 DO 535 J=1,3
      U(J,I)=U(J,I)*T1
  535 VT(J,I)=B1(J,I)*T1
      DO 540 J=1,3
  540 VT(J,4)=.5*(W(J+3,1)+W(J+3,2))
C     ***** CHECK FOR EXCESS FORESHORTENING *****
      IF(FORE-ABS(U(3,1)))545,550,550
  545 CALL NORM(U(1,2),SYMB(1,3),VT(1,1),1)
      VT(1,3)=SYMB(1,3)
      VT(2,3)=SYMB(2,3)
      VT(3,3)=SYMB(3,3)
      HGT=CD(6,NB)
      OFF=CD(7,NB)
      IF(HGT)550,999,550
  550 T1=CD(8,NB)
      Z(1)=VT(1,4)-HGT*(11.+3.*T1)/7.
      Z(2)=VT(2,4)+OFF-HGT*.5
      Z(3)=VT(3,4)
      XO(3)=Z(3)
      ITILT=1
      I9=T1+2.
      T9=10.**I9
      DISTR=AINT((DIST*T9)+0.5)/T9 +.0001
      CALL NUMBUR(Z(1),Z(2),HGT,DISTR,0.,I9)
  570 ITILT=0
      IF(NJ2-10)580,999,999
  580 IF (NOUT.GE.0)
     &WRITE (NOUT,571)DIST
  571 FORMAT(1H 59X,10HDISTANCE =F8.3/1H )
      GO TO 999
  390 NG=14
      CALL ERPNT(WD(2),800)
  999 RETURN
      END
