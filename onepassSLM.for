      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,JLTYP,
     1 TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION COORDS(3),FLUX(2),TIME(2)
      CHARACTER*80 SNAME
      REAL thermcon,alpha,vel,R0,R,pi,dist
     & x,y,z,power,lamda,abs,AI,M,Eff,ref
      power=200.0
      vel=0.0038
C ********************
      Eff=2. ! 11 this added t othis code
      pi=3.141592654
      abs=0.64
      R0=0.004
      ref=0.57   !ref=Surface reflectivity of the powder mixture
      AI=(Eff*power)/(pi*(R0*R0)) ! 11
      dist=vel*TIME(1)
      x=COORDS(1)-dist
      y=COORDS(3)
      z=COORDS(2)
      R=sqrt(x*x+z*z)
      FLUX(1)=(1-ref)*abs*AI*exp(-3*(R*R)/(R0*R0))*exp((-abs)*y)   !y= depth direction
      FLUX(2)=0.
      JLTYP=0.
      RETURN
      END
c----------------------------------------------   
      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     1 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,LACCFLA)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),
     & T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*),
     & NT11(15)
C Subroutine obtains the temperature information and flags
C the elements as 0, 1 or 2 according to at which temperature
C range they are
      CALL getvrm('NT11',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     & MATLAYO,LACCFLA)
      NT11 = array(1)
C Get the previous value of the solution dependent state variable and set this to be the
C previous maximum temperature
      CALL getvrm('SDV',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     & MATLAYO,LACCFLA)
      Tmax = array(1)
C (T < Tm = 1, T > Tm = 2, Tmax > Tm and T < Tm = 3)
      IF(NT11(1) .LT. 2133)THEN
      FIELD(1) = 1 !powder
      ELSE IF((NT11(1) .LT. 2133) .AND. (Tmax .GT. 2133)) THEN
      FIELD(1) = 3 !solid
      ELSE IF(NT11(1) .GE. 2133) THEN
      FIELD(1) = 2 !liquid
      END IF
      RETURN
      END
C---------------------