C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++ Copyright (C) 2013 Korinna C. Zapp [Korinna.Zapp@cern.ch]       ++
C++                                                                 ++
C++ This file is part of JEWEL 2.0.2                                ++
C++                                                                 ++
C++ The JEWEL homepage is jewel.hepforge.org                        ++
C++                                                                 ++
C++ The medium model was partly implemented by Jochen Klein         ++
C++                                                                 ++
C++ Please follow the MCnet GUIDELINES and cite arXiv:1311.0048     ++
C++ for the code and JHEP 1303 (2013) 080 [arXiv:1212.1599] and     ++
C++ optionally EPJC C60 (2009) 617 [arXiv:0804.3568] for the        ++
C++ physics.                                                        ++
C++                                                                 ++
C++ JEWEL relies heavily on PYTHIA 6 for the event generation. The  ++
C++ modified version of PYTHIA 6.4.25 that is distributed with      ++
C++ JEWEL is, however, not an official PYTHIA release and must not  ++
C++ be used for anything else. Please refer to results as           ++
C++ "JEWEL+PYTHIA".                                                 ++
C++                                                                 ++
C++ JEWEL also uses code provided by S. Zhang and J. M. Jing        ++
C++ (Computation of Special Functions, John Wiley & Sons, New York, ++
C++ 1996 and http://jin.ece.illinois.edu) for computing the         ++
C++ exponential integral Ei(x).                                     ++
C++                                                                 ++
C++                                                                 ++
C++ JEWEL  is free software; you can redistribute it and/or         ++
C++ modify it under the terms of the GNU General Public License     ++
C++ as published by the Free Software Foundation; either version 2  ++
C++ of the License, or (at your option) any later version.          ++
C++                                                                 ++
C++ JEWEL is distributed in the hope that it will be useful,        ++
C++ but WITHOUT ANY WARRANTY; without even the implied warranty of  ++
C++ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ++
C++ GNU General Public License for more details.                    ++
C++                                                                 ++
C++ You should have received a copy of the GNU General Public       ++  
C++ License along with this program; if not, write to the Free      ++
C++ Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, ++
C++ MA 02110-1301 USA                                               ++
C++                                                                 ++
C++ Linking JEWEL statically or dynamically with other modules is   ++
C++ making a combined work based on JEWEL. Thus, the terms and      ++
C++ conditions of the GNU General Public License cover the whole    ++
C++ combination.                                                    ++
C++                                                                 ++
C++ In addition, as a special exception, I give you permission to   ++
C++ combine JEWEL with the code for the computation of special      ++
C++ functions provided by S. Zhang and J. M. Jing. You may copy and ++
C++ distribute such a system following the terms of the GNU GPL for ++
C++ JEWEL and the licenses of the other code concerned, provided    ++
C++ that you include the source code of that other code when and as ++
C++ the GNU GPL requires distribution of source code.               ++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C-- the hydro event is dynamiclaly allocated
C   in the reader, but the reader natively
C   does not do memory mamangement, so
C   we clear the memory ourselves
      SUBROUTINE CLEARHYDROMEMORY()
      USE HDF5
      CALL DEALLOCATEHYDROEVENT()
      END

      SUBROUTINE VISHNUNEXTEVT(j)
      USE HDF5
C -- fairly clumsy function to open hydro events
C    will crash at end of file to avoid sampling
C    the hydro events more than once
C    reads line by line from the  input file

      CHARACTER(LEN=120) :: FILEITERATOR
      CHARACTER(LEN=120) :: FILENAME
      INTEGER :: I, J
      OPEN(UNIT=10, FILE='vishnu_events.dat', STATUS ='OLD')
      DO I=1,10000
C -- this loop step seems absurd, but it protects against having 
C    problems with filename N+1 has less characters
C    than filename N
          IF(J.NE.I) THEN 
              READ(10,'(A)') FILEITERATOR
C              write(3,*)'already played with event ', FILEITERATOR, I, J
          ELSE IF (J.EQ.I) THEN
              READ(10,'(A)') FILENAME
              write(3,*)'but this one seems fresh ', FILENAME, I, J
              EXIT
          ENDIF
      END DO
      write(3,*)'Reading HYDRO event from ',FILENAME
C      character(*80)
      CALL readHydroFiles_initialEZ(FILENAME)
      CLOSE(10)
      END



      SUBROUTINE VISHNUINIT(id)
C-- here we open the hydro file to extract the info
C   and buffer it
C   called once at start of medium
C   initialization
C   but all this does is make a nice picture of a hydro evolution
C   which is then not used anymore
      USE HDF5
      COMMON/vishlog/vishnuid
      INTEGER vishnuid
      INTEGER id
      DOUBLE PRECISION  X,Y,T,GETNEFFQUIET
      DOUBLE PRECISION GETNEFFJEWEL
      CHARACTER(LEN=120) :: FILENAME
      OPEN(UNIT=10, FILE='vishnu_events.dat', STATUS ='OLD')
      READ(10,'(A)') FILENAME
      write(3,*)'Reading test HYDRO event from ',FILENAME
      CALL readHydroFiles_initialEZ(TRIM(FILENAME))
      CLOSE(10)
C      vishnuid=id
C      WRITE(vishnuid,'(A)')'INITIALIZED VISHNU HYDRO'
C      
C-- we also want to just go through the x y profile of the
C   medium, to get a sense of its makeup
C
C      OPEN(unit=7,file='vishnu_profile.csv',status='unknown')
C      write(7,'(A)')'scanning hydro cells - x y t neff neffjewel'

C-- make a nested loop over x and y, and store the neff and
C-- jewel neff in a grid of cell width .1
C      T = 0.6
C      DO K = 1,110,1
C          X = -10.
C          DO I=1,201,1
C              Y = -10.
C              DO J=1,202,1
C                     write(7,*)X,Y,T,GETNEFFQUIET(X,Y,0d0,T),
C     &GETNEFFJEWEL(X,Y,0d0,T)
C                  Y = Y + .1
C              END DO
C              X = X + .1
C          END DO
C          T = T + .1
C      END DO
C--   close file for writing
C      CLOSE(7,status='keep')

      END


      SUBROUTINE MEDINIT(FILE,id,etam)
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--longitudinal boost of momentum distribution
	common/boostmed/boost
	logical boost
C--factor to vary Debye mass
	COMMON/MDFAC/MDFACTOR,MDSCALEFAC
	DOUBLE PRECISION MDFACTOR,MDSCALEFAC
C--nuclear thickness function
      COMMON /THICKFNC/ RMAX,TA(100,2)
      DOUBLE PRECISION RMAX,TA
C--geometrical cross section
      COMMON /CROSSSEC/ IMPMAX,CROSS(200,3)
      DOUBLE PRECISION IMPMAX,CROSS
C--identifier of log file
	common/logfile/logfid
	integer logfid

      DATA RAU/10./
      DATA D3/0.9d0/
      DATA ZETA3/1.2d0/
C--local variables
      INTEGER I,LUN,POS,IOS,id
	double precision etam
      CHARACTER*100 BUFFER,LABEL,tempbuf
	CHARACTER*80 FILE
	character firstchar
	logical fileexist

	boost = .true.

	etamax2 = etam
	logfid = id

      IOS=0
      LUN=77

C--default settings
      TAUI=0.6d0
      TI=0.36d0
      TC=0.17d0
      WOODSSAXON=.TRUE.
      CENTRMIN=0.d0
      CENTRMAX=10.d0
      NF=3
      A=208
      N0=0.17d0
      D=0.54d0
      SIGMANN=6.2
	MDFACTOR=0.45d0
	MDSCALEFAC=0.9d0

C--read settings from file
	write(logfid,*)
	inquire(file=FILE,exist=fileexist)
	if(fileexist)then
        write(logfid,*)'Reading medium parameters from ',FILE
        OPEN(unit=LUN,file=FILE,status='old',err=10)
	  do 20 i=1,1000
          READ(LUN, '(A)', iostat=ios) BUFFER
	    if (ios.ne.0) goto 30
	    firstchar = buffer(1:1)
	    if (firstchar.eq.'#') goto 20
          POS=SCAN(BUFFER,' ')
          LABEL=BUFFER(1:POS)
          BUFFER=BUFFER(POS+1:)
          IF (LABEL=="TAUI")THEN
            READ(BUFFER,*,IOSTAT=IOS) TAUI
          ELSE IF (LABEL=="TI") THEN
            READ(BUFFER,*,IOSTAT=IOS) TI
          ELSE IF (LABEL=="TC") THEN
            READ(BUFFER,*,IOSTAT=IOS) TC
          ELSE IF (LABEL=="WOODSSAXON") THEN
            READ(BUFFER,*,IOSTAT=IOS) WOODSSAXON
          ELSE IF (LABEL=="CENTRMIN") THEN
            READ(BUFFER,*,IOSTAT=IOS) CENTRMIN
          ELSE IF (LABEL=="CENTRMAX") THEN
            READ(BUFFER,*,IOSTAT=IOS) CENTRMAX
          ELSE IF (LABEL=="NF") THEN
            READ(BUFFER,*,IOSTAT=IOS) NF
          ELSE IF (LABEL=="A") THEN
            READ(BUFFER,*,IOSTAT=IOS) A
          ELSE IF (LABEL=="N0") THEN
            READ(BUFFER,*,IOSTAT=IOS) N0
          ELSE IF (LABEL=="D") THEN
            READ(BUFFER,*,IOSTAT=IOS) D
          ELSE IF (LABEL=="SIGMANN") THEN
            READ(BUFFER,*,IOSTAT=IOS) SIGMANN
          ELSE IF (LABEL=="MDFACTOR") THEN
            READ(BUFFER,*,IOSTAT=IOS) MDFACTOR
          ELSE IF (LABEL=="MDSCALEFAC") THEN
            READ(BUFFER,*,IOSTAT=IOS) MDSCALEFAC
	    else
	      write(logfid,*)'unknown label ',label
	    endif
 20	  continue

 30	  close(LUN,status='keep')
	  write(logfid,*)'...done'
	  goto 40

 10     write(logfid,*)'Could not open medium parameter file, '//
     &	'will run with default settings.'

	else
	  write(logfid,*)'No medium parameter file found, '//
     &	'will run with default settings.'
	endif

 40   write(logfid,*)'using parameters:'
      write(logfid,*)'TAUI       =',TAUI
      write(logfid,*)'TI         =',TI
      write(logfid,*)'TC         =',TC
      write(logfid,*)'WOODSSAXON =',WOODSSAXON
      write(logfid,*)'CENTRMIN   =',CENTRMIN
      write(logfid,*)'CENTRMAX   =',CENTRMAX
      write(logfid,*)'NF         =',NF
      write(logfid,*)'A          =',A
      write(logfid,*)'N0         =',N0
      write(logfid,*)'D          =',D
      write(logfid,*)'SIGMANN    =',SIGMANN
      write(logfid,*)'MDFACTOR   =',MDFACTOR
      write(logfid,*)'MDSCALEFAC =',MDSCALEFAC
	write(logfid,*)
	write(logfid,*)
	write(logfid,*)

C--calculate T_A(x,y)
      CALL CALCTA
C--calculate geometrical cross section
      CALL CALCXSECTION
C-- read the vishnu data (for now hardcoded filename)
C      CALL VISHNUINIT(5)
      END

      SUBROUTINE MEDNEXTEVT
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--geometrical cross section
      COMMON /CROSSSEC/ IMPMAX,CROSS(200,3)
      DOUBLE PRECISION IMPMAX,CROSS
C--local variables
      integer i,j
      DOUBLE PRECISION PYR,R,b1,b2,gettemp

C--pick an impact parameter
      r=(pyr(0)*(centrmax-centrmin)+centrmin)/100.
      i=0
      do 130 j=1,200
       if ((r-cross(j,3)/cross(200,3)).ge.0.) then
        i=i+1
       else 
        goto 132
       endif
 130  continue
 132  continue
      b1 = (i-1)*0.1d0
      b2 = i*0.1d0
      breal = (b2*(cross(i,3)/cross(200,3)-r)
     &      +b1*(r-cross(i+1,3)/cross(200,3)))/
     &	(cross(i,3)/cross(200,3)-cross(i+1,3)/cross(200,3))
      centr = r;

      END

      double precision function getcentrality()
      implicit none
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      getcentrality=centr

      end



      SUBROUTINE PICKVTX(X,Y)
      IMPLICIT NONE
      DOUBLE PRECISION X,Y
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
C--local variables
      DOUBLE PRECISION X1,X2,Y1,Y2,Z,XVAL,YVAL,ZVAL,NTHICK,PYR

      X1=BREAL/2.-RAU
      X2=RAU-BREAL/2.
      Y1=-SQRT(4*RAU**2-BREAL**2)/2.
      Y2=SQRT(4*RAU**2-BREAL**2)/2.
 131  XVAL=PYR(0)*(X2-X1)+X1
      YVAL=PYR(0)*(Y2-Y1)+Y1
      IF((NTHICK(XVAL-BREAL/2.,YVAL).EQ.0.d0).OR.
     &     NTHICK(XVAL+BREAL/2.,YVAL).EQ.0.d0) GOTO 131
      ZVAL=PYR(0)*NTHICK(-BREAL/2.,0d0)*NTHICK(BREAL/2.,0d0)
      Z=NTHICK(XVAL-BREAL/2.,YVAL)*NTHICK(XVAL+BREAL/2.,YVAL)
      IF(ZVAL.GT.Z) GOTO 131
      X=XVAL
      Y=YVAL
      END

C	SUBROUTINE SETB(BVAL)
C	IMPLICIT NONE
C--medium parameters
C      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
C      INTEGER NF
C      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
C	DOUBLE PRECISION BVAL
C	BREAL=BVAL
C	END



      SUBROUTINE GETSCATTERER(X,Y,Z,T,TYPE,PX,PY,PZ,E,MS)
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
C--internal medium parameters
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--longitudinal boost of momentum distribution
	common/boostmed/boost
	logical boost
C--function calls
      DOUBLE PRECISION GETHYDROTEMP,GETMD,GETMOM,GETMS
C--identifier of log file
	common/logfile/logfid
	integer logfid
C--local variables
      DOUBLE PRECISION X,Y,Z,T,MS,PX,PY,PZ,E,MD,TEMP
      INTEGER TYPE
      DOUBLE PRECISION R,PYR,pmax,wt,tau,theta,phi,pi,p,ys,pz2,e2
      DATA PI/3.141592653589793d0/

C-- REDMER -- here we check if quark of gluon is produced, i assume      
      R=PYR(0)
      IF(R.LT.(2.*12.*NF*D3/3.)/(2.*12.*NF*D3/3.+3.*16.*ZETA3/2.))THEN
         TYPE=2
      ELSE
         TYPE=21
      ENDIF
C-- REDMER so ... ms is the debye mass divided by sqrt(2). why not ...      
      MS=GETMS(X,Y,Z,T)
C-- REDMER pick the debye mass      
      MD=GETMD(X,Y,Z,T)
      TEMP=GETHYDROTEMP(X,Y,Z,T)
	tau=sqrt(t**2-z**2)
	if (boost) then
  	  ys = 0.5*log((t+z)/(t-z))
	else
	  ys = 0.d0
	endif
	pmax = 10.*temp

      IF(TEMP.LT.1.D-2)THEN
       write(logfid,*)'asking for a scattering centre without medium:'
       write(logfid,*)'at (x,y,z,t)=',X,Y,Z,T
       write(logfid,*)'making one up to continue but '//
     &	'something is wrong!'
       TYPE=21
       PX=0.d0
       PY=0.d0
       PZ=0.d0
       MS=GETMS(0.d0,0.d0,0.d0,0.d0)
       MD=GETMD(0.d0,0.d0,0.d0,0.d0)
       E=SQRT(PX**2+PY**2+PZ**2+MS**2)
       RETURN
      ENDIF
C-- REDMER wha tis p ? 
 10	p = pyr(0)**0.3333333*pmax
C-- REDMER total energy E, so p is momentum
	E2 = sqrt(p**2+ms**2)
C-- REDMER below some weight - this i don't understand      
	if (type.eq.2) then
	  wt = (exp(ms/temp)-1.)/(exp(E2/temp)-1.)
	else
	  wt = (exp(ms/temp)+1.)/(exp(E2/temp)+1.)
	endif
	if (wt.gt.1.) write(logfid,*)'Error in getscatterer: weight = ',wt
	if (wt.lt.0.) write(logfid,*)'Error in getscatterer: weight = ',wt
	if (pyr(0).gt.wt) goto 10
C-- REDMER here i guess we pick kinematic variables for the particle
        phi = pyr(0)*2.*pi
	theta = -acos(2.*pyr(0)-1.)+pi
	px  = p*sin(theta)*cos(phi)
	py  = p*sin(theta)*sin(phi)
	pz2 = p*cos(theta)
	E   = cosh(ys)*E2 + sinh(ys)*pz2
	pz  = sinh(ys)*E2 + cosh(ys)*pz2
C REDMER transverse boost
      END


      SUBROUTINE AVSCATCEN(X,Y,Z,T,PX,PY,PZ,E,m)
      IMPLICIT NONE
C--longitudinal boost of momentum distribution
	common/boostmed/boost
	logical boost
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--local variables
	double precision x,y,z,t,px,py,pz,e,getms,m,ys
	if (boost) then
  	  ys = 0.5*log((t+z)/(t-z))
	  if ((z.eq.0.d0).and.(t.eq.0.d0)) ys =0.d0
	  if (ys.gt.etamax2) ys=etamax2
	  if (ys.lt.-etamax2) ys=-etamax2
	else
	  ys = 0.d0
	endif
	m  = getms(x,y,z,t)
	e  = m*cosh(ys)
	px = 0.d0
	py = 0.d0
	pz = m*sinh(ys)
	end


      SUBROUTINE maxscatcen(PX,PY,PZ,E,m)
      IMPLICIT NONE
C--longitudinal boost of momentum distribution
	common/boostmed/boost
	logical boost
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--local variables
	double precision px,py,pz,e,getmsmax,m,ys
	if (boost) then
  	  ys = etamax2
	else
	  ys = 0.d0
	endif
	m  = getmsmax()
	e  = m*cosh(ys)
	px = 0.d0
	py = 0.d0
	pz = m*sinh(ys)
	end
	


      DOUBLE PRECISION FUNCTION GETMD(X1,Y1,Z1,T1)
      IMPLICIT NONE
C--factor to vary Debye mass
	COMMON/MDFAC/MDFACTOR,MDSCALEFAC
	DOUBLE PRECISION MDFACTOR,MDSCALEFAC
      DOUBLE PRECISION X1,Y1,Z1,T1,GETHYDROTEMP
      GETMD=MDSCALEFAC*3.*GETHYDROTEMP(X1,Y1,Z1,T1)
      GETMD=MAX(GETMD,MDFACTOR)
      END



      DOUBLE PRECISION FUNCTION GETMS(X2,Y2,Z2,T2)
      IMPLICIT NONE
      DOUBLE PRECISION X2,Y2,Z2,T2,GETMD
      GETMS=GETMD(X2,Y2,Z2,T2)/SQRT(2.)
      END

      DOUBLE PRECISION FUNCTION GETNEFFQUIET(X3,Y3,Z3,T3)
      IMPLICIT NONE
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
      COMMON/vishlog/vishnuid
      INTEGER vishnuid
C--   local variables
      DOUBLE PRECISION X3,Y3,Z3,T3,PI,GETHYDROTEMP,tau,cosheta
      DATA PI/3.141592653589793d0/
	tau = sqrt(t3**2-z3**2)
	cosheta = t3/tau
C--REDMER this is where we intervene
C         rather than the density (neff) is a function of temperature
C         at a given location and tau. 
C         rather than taking the JEWEL estimate of the temperature,
C         we'll use the hydro one 
C--       GETTEMP from hydro
      GETNEFFQUIET=(2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *GETHYDROTEMP(X3,Y3,Z3,T3)**3/PI**2

	getneffquiet = getneffquiet/cosheta
      END

C -- REDMER effedctive density Neff
C -- this function should be merged with the quiet flavor
C -- so that it just write the output of that 
      DOUBLE PRECISION FUNCTION GETNEFF(X3,Y3,Z3,T3)
      IMPLICIT NONE
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
      COMMON/vishlog/vishnuid
      INTEGER vishnuid
C--   local variables
      DOUBLE PRECISION X3,Y3,Z3,T3,PI,GETHYDROTEMP,tau,cosheta
      DOUBLE PRECISION GETTEMP, GETHYDROENTROPY, GETHYDROEPSILON
      DOUBLE PRECISION GETNEFFJEWEL
      DATA PI/3.141592653589793d0/
	tau = sqrt(t3**2-z3**2)
	cosheta = t3/tau
C--REDMER this is where we intervene
C         rather than the density (neff) is a function of temperature
C         at a given location and tau. 
C         rather than taking the JEWEL estimate of the temperature,
C         we'll use the hydro one 
C--       GETTEMP from hydro
      GETNEFF=(2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *GETHYDROTEMP(X3,Y3,Z3,T3)**3/PI**2

	getneff = getneff/cosheta
      GETNEFFJEWEL = (2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *GETTEMP(X3,Y3,Z3,T3)**3/PI**2
      GETNEFFJEWEL = GETNEFFJEWEL/cosheta
C      write(vishnuid,*)X3,Y3,Z3,T3,GETTEMP(X3,Y3,Z3,T3),
C     &GETHYDROTEMP(X3,Y3,Z3,T3),GETHYDROEPSILON(X3,Y3,Z3,T3),
C     &GETHYDROENTROPY(X3,Y3,Z3,T3),GETNEFF,GETNEFFJEWEL
      END
 
C -- returns the original Neff from jewel
C    used as a reference
      DOUBLE PRECISION FUNCTION GETNEFFJEWEL(X3,Y3,Z3,T3)
      IMPLICIT NONE
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--   local variables
      DOUBLE PRECISION X3,Y3,Z3,T3,PI,GETTEMP,tau,cosheta
      DATA PI/3.141592653589793d0/
	tau = sqrt(t3**2-z3**2)
	cosheta = t3/tau
      GETNEFFJEWEL=(2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *GETTEMP(X3,Y3,Z3,T3)**3/PI**2
	getneffjewel = getneffjewel/cosheta
      END
      
      
      DOUBLE PRECISION FUNCTION GETTEMP(X4,Y4,Z4,T4)
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--local variables
      DOUBLE PRECISION X4,Y4,Z4,T4,TAU,NPART,EPS0,EPSIN,TEMPIN,PI,
     &NTHICK,ys
      DATA PI/3.141592653589793d0/

      GETTEMP=0.D0

      IF(ABS(Z4).GT.T4)RETURN

      TAU=SQRT(T4**2-Z4**2)
C--check for overlap region
      IF((NTHICK(X4-BREAL/2.,Y4).EQ.0.d0).OR.
     &NTHICK(X4+BREAL/2.,Y4).EQ.0.d0) RETURN

	ys = 0.5*log((t4+z4)/(t4-z4))
	if (abs(ys).gt.etamax2) return
C--determine initial temperature at transverse position
      IF(WOODSSAXON)THEN
         EPS0=(16.*8.+7.*2.*6.*NF)*PI**2*TI**4/240.
         EPSIN=EPS0*NPART(X4-BREAL/2.,Y4,X4+BREAL/2.,Y4)
     &        *PI*RAU**2/(2.*A)
         TEMPIN=(EPSIN*240./(PI**2*(16.*8.+7.*2.*6.*NF)))**0.25
      ELSE
         TEMPIN=TI
      ENDIF
C--calculate temperature if before initial time
      IF(TAU.LE.TAUI)THEN
	 GETTEMP=TEMPIN*TAU/TAUI
      ELSE
C--evolve temperature
       GETTEMP=TEMPIN*(TAUI/TAU)**0.3333
      ENDIF
      IF(GETTEMP.LT.TC) GETTEMP=0.d0
      END


C--REDMER max temp at Tau I (initial Tau)
      DOUBLE PRECISION FUNCTION GETTEMPMAX()
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--function call
      DOUBLE PRECISION GETHYDROTEMP
      GETTEMPMAX=GETHYDROTEMP(0.D0,0.D0,0.D0,TAUI)
      END


C--REDMER max debye mass at max temp
      DOUBLE PRECISION FUNCTION GETMDMAX()
      IMPLICIT NONE
C--factor to vary Debye mass
	COMMON/MDFAC/MDFACTOR,MDSCALEFAC
	DOUBLE PRECISION MDFACTOR,MDSCALEFAC
      DOUBLE PRECISION GETTEMPMAX
      GETMDMAX=MDSCALEFAC*3.*GETTEMPMAX()
      GETMDMAX=MAX(GETMDMAX,MDFACTOR)
      END


C--REDMER min debye mass at critial temp (freeze out)
      DOUBLE PRECISION FUNCTION GETMDMIN()
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--factor to vary Debye mass
	COMMON/MDFAC/MDFACTOR,MDSCALEFAC
	DOUBLE PRECISION MDFACTOR,MDSCALEFAC
      DOUBLE PRECISION GETTEMPMAX
	GETMDMIN=MDSCALEFAC*3.*TC
      GETMDMIN=MAX(GETMDMIN,MDFACTOR)
      END


C-- REDMER ms - what is this ? 
      DOUBLE PRECISION FUNCTION GETMSMAX()
      IMPLICIT NONE
      DOUBLE PRECISION GETMDMAX,SQRT
      GETMSMAX=GETMDMAX()/SQRT(2.D0)
      END


C--REDMER what do we do here exactly ? 
C-- something extracted at the minimum debye mass
C-- with the zeta function (see Marco's paper) and 
C-- Nf 
	DOUBLE PRECISION FUNCTION GETNATMDMIN()
	IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--factor to vary Debye mass
	COMMON/MDFAC/MDFACTOR,MDSCALEFAC
	DOUBLE PRECISION MDFACTOR,MDSCALEFAC,PI
      DATA PI/3.141592653589793d0/
C--local variables
	DOUBLE PRECISION T,GETMDMIN
	T=GETMDMIN()/(MDSCALEFAC*3.)
      GETNATMDMIN=(2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *T**3/PI**2
	END



	DOUBLE PRECISION FUNCTION GETLTIMEMAX()
	IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--function call
      DOUBLE PRECISION GETTEMPMAX
	GETLTIMEMAX=TAUI*(GETTEMPMAX()/TC)**3*cosh(etamax2)
	END


C-- N eff max
      DOUBLE PRECISION FUNCTION GETNEFFMAX()
      IMPLICIT NONE
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--max rapidity
	common/rapmax2/etamax2
	double precision etamax2
C--   local variables
      DOUBLE PRECISION PI,GETTEMPMAX
      DATA PI/3.141592653589793d0/
      GETNEFFMAX=(2.*6.*NF*D3*2./3. + 16.*ZETA3*3./2.)
     &     *GETTEMPMAX()**3/PI**2
      END
      
      
C-- REDMER number of participating
C-- nucleons ? sampled from the nuclear thickness functions
C-- for both nucleons
      DOUBLE PRECISION FUNCTION NPART(XX1,YY1,XX2,YY2)
      IMPLICIT NONE
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--local variables
      DOUBLE PRECISION XX1,YY1,XX2,YY2,NTHICK
      
      NPART = NTHICK(XX1,YY1)*(1.-EXP(-SIGMANN*NTHICK(XX2,YY2))) +
     &        NTHICK(XX2,YY2)*(1.-EXP(-SIGMANN*NTHICK(XX1,YY1)))
      END
      

C-- REDMER implementation of nuclear thickness function 
C-- called in the previous function where the Npart is 
C-- evaluated
      DOUBLE PRECISION FUNCTION NTHICK(X1,Y1)
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--identifier of log file
	common/logfile/logfid
	integer logfid
C--nuclear thickness function
      COMMON /THICKFNC/ RMAX,TA(100,2)
      DOUBLE PRECISION RMAX,TA
      INTEGER LINE,LMIN,LMAX,I
      DOUBLE PRECISION X1,Y1,XA(4),YA(4),Y,DY,R,C,B,DELTA
  
      R=SQRT(X1**2+Y1**2)
      IF(R.GT.TA(100,1))THEN
	 NTHICK=0.
      ELSE
	 LINE=INT(R*99.d0/TA(100,1)+1)
	 LMIN=MAX(LINE,1)
	 LMIN=MIN(LMIN,99)
	 IF((R.LT.TA(LMIN,1)).OR.(R.GT.TA(LMIN+1,1)))
     &	write(logfid,*)LINE,LMIN,R,TA(LMIN,1),TA(LMIN+1,1)
	 XA(1)=TA(LMIN,1)
	 XA(2)=TA(LMIN+1,1)
	 YA(1)=TA(LMIN,2)
	 YA(2)=TA(LMIN+1,2)
	 C=(YA(2)-YA(1))/(XA(2)-XA(1))
	 B=YA(1)-C*XA(1)
	 NTHICK=C*R+B
      ENDIF
      END


C-- calculate Ta, assumedly this is the conventional Ta
      SUBROUTINE CALCTA()
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--   nuclear thickness function
      COMMON /THICKFNC/ RMAX,TA(100,2)
      DOUBLE PRECISION RMAX,TA
C--variables for integration
      COMMON/INTEG/B,R
      DOUBLE PRECISION B,R
C--local variables
      INTEGER NSTEPS,I
      DOUBLE PRECISION EPS,HFIRST,Y

      NSTEPS=100
      EPS=1.E-4
      HFIRST=0.1D0

      R=1.12*A**(0.33333)-0.86*A**(-0.33333)
      RMAX=2.*R

      DO 10 I=1,NSTEPS
C--set transverse position
       B=(I-1)*2.D0*R/NSTEPS
       Y=0.D0
C--integrate along longitudinal line
       CALL ODEINT(Y,-2*R,2*R,EPS,HFIRST,0.d0,101)
       TA(I,1)=B
       TA(I,2)=Y
 10   CONTINUE
      END


C-- REDMER caluclate the cross section
C-- this is called from MEDINIT, so once per run .. ? 
C-- doesn't make much sense, wouldn't you want this
C-- per event ? 
C-- probably i jus tdon't undertsand this ..
      SUBROUTINE CALCXSECTION()
      IMPLICIT NONE
C--medium parameters
      COMMON/MEDPARAM/CENTRMIN,CENTRMAX,BREAL,CENTR,RAU,NF
      INTEGER NF
      DOUBLE PRECISION CENTRMIN,CENTRMAX,BREAL,CENTR,RAU
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--   geometrical cross section
      COMMON /CROSSSEC/ IMPMAX,CROSS(200,3)
      DOUBLE PRECISION IMPMAX,CROSS
C--local variables
      INTEGER IX,IY,IB
      DOUBLE PRECISION B,P,PROD,X,Y,NTHICK,NPART,pprev

      pprev=0.
      DO 30 IB=1,200
       B=0.1d0*IB
       PROD=1.d0
       DO 10 IX=1,100
        DO 20 IY=1,100
         X=-20.d0+IX*0.4d0
         Y=-20.d0+IY*0.4d0
         PROD=PROD*
     &EXP(-NTHICK(X+B/2.D0,Y)*SIGMANN)**(0.16d0*NTHICK(X-B/2.D0,Y))
 20     CONTINUE
 10    CONTINUE
       P=(1.D0-PROD)*8.8D0/14.D0*B
       CROSS(IB,1)=B
       CROSS(IB,2)=P
       if (ib.eq.1) then
        cross(ib,3)=0.
       else
        cross(ib,3)=cross(ib-1,3)+(p+pprev)/2.*0.1
       endif
       pprev=p
 30   CONTINUE
      IMPMAX=19.95
      END



      DOUBLE PRECISION FUNCTION MEDDERIV(XVAL,W)
      IMPLICIT NONE
      DOUBLE PRECISION XVAL
      INTEGER W
C--medium parameters
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN
      INTEGER A
      LOGICAL WOODSSAXON
C--variables for integration
      COMMON/INTEG/B,R
      DOUBLE PRECISION B,R

      IF (W.EQ.1) THEN
C--XVAL corresponds to z-coordinate
       MEDDERIV=N0/(1+EXP((SQRT(B**2+XVAL**2)-R)/D))
      ELSE 
       MEDDERIV=0.D0
      ENDIF
      END

C-- start of added code, get temperature from VISHNU
      DOUBLE PRECISION FUNCTION GETHYDROTEMP(X6,Y6,Z6,T6)
      USE HDF5 ! load the HDF5 module, necessary to read hydro file
      DOUBLE PRECISION X6, Y6, Z6, T6
      COMMON/MEDPARAMINT/TAUI,TI,TC,D3,ZETA3,D,
     &N0,SIGMANN,A,WOODSSAXON
      DOUBLE PRECISION TAUI,TI,TC,ALPHA,BETA,GAMMA,D3,ZETA3,D,N0,
     &SIGMANN

C-- implicite double precision initialization of GETHYDROTEMP
C-- extract the following params:
C   e - energy density
C   p - pressure
C   s - entropy density
C   T - temperature
C   vx - velocity of the fluid cell in the x direction
C   vy - velocity of the fluid cell in the y direction

      DOUBLE PRECISION :: e, p, s, T, vx, vy
      
C-- get the temperature at some coordinate
C-- input variables (first three) are
C-- tau
C-- x
C-- y
C-- so there is NO z dependence - how do we treat this ? 
      CALL readHydroinfoBuffered_ideal(T6, X6, Y6,
     &  e,p,s,T,vx,vy);
C-- return value is variable that has the function name
      GETHYDROTEMP=T 

C-- if the temperature is lower than the critical temperature
C-- , set it to 0 
      IF(GETHYDROTEMP.LT.TC) GETHYDROTEMP=0.d0

      END

      DOUBLE PRECISION FUNCTION GETHYDROEPSILON(X6,Y6,Z6,T6)
      USE HDF5 ! load the HDF5 module, necessary to read hydro file
      IMPLICIT NONE
      DOUBLE PRECISION X6, Y6, Z6, T6
C-- implicite double precision initialization of GETHYDROTEMP
C-- extract the following params:
C   e - energy density
C   p - pressure
C   s - entropy density
C   T - temperature
C   vx - velocity of the fluid cell in the x direction
C   vy - velocity of the fluid cell in the y direction

      DOUBLE PRECISION :: e, p, s, T, vx, vy
      
C-- get the temperature at some coordinate
C-- input variables (first three) are
C-- tau
C-- x
C-- y
C-- so there is NO z dependence - how do we treat this ? 
      CALL readHydroinfoBuffered_ideal(T6, X6, Y6,
     &  e,p,s,T,vx,vy);
C-- return value is variable that has the function name
      GETHYDROEPSILON=e

      END

      DOUBLE PRECISION FUNCTION GETHYDROENTROPY(X6,Y6,Z6,T6)
      USE HDF5 ! load the HDF5 module, necessary to read hydro file
      IMPLICIT NONE
      DOUBLE PRECISION X6, Y6, Z6, T6
C-- implicite double precision initialization of GETHYDROTEMP
C-- extract the following params:
C   e - energy density
C   p - pressure
C   s - entropy density
C   T - temperature
C   vx - velocity of the fluid cell in the x direction
C   vy - velocity of the fluid cell in the y direction

      DOUBLE PRECISION :: e, p, s, T, vx, vy
      
C-- get the temperature at some coordinate
C-- input variables (first three) are
C-- tau
C-- x
C-- y
C-- so there is NO z dependence - how do we treat this ? 
      CALL readHydroinfoBuffered_ideal(T6, X6, Y6,
     &  e,p,s,T,vx,vy);
C-- return value is variable that has the function name
      GETHYDROENTROPY=s

      END



      DOUBLE PRECISION FUNCTION GETPARTICIPANTPLANE()
C -- get the participant plane from the
C -- hydro event that is currently in the 
C -- event buffer
      USE HDF5 ! load the HDF5 module, necessary to read hydro file
      IMPLICIT NONE
      DOUBLE PRECISION X, Y, Z, T, GETNEFFQUIET, qy, qx, phi, r
      INTEGER :: I, J
      qy = 0
      qx = 0 
      T = 6
      X = -10.
      DO I=1,201,1
          Y = -10.
          DO J=1,202,1
C -- here extract azimuth (polar coordinates) and
C -- increment the q vectors                
              PHI = ATAN2(Y,X)
              r = x*x + y*y
              QY = QY + r*GETNEFFQUIET(X,Y,0d0,T) * SIN(2.*PHI)
              QX = QX + r*GETNEFFQUIET(X,Y,0d0,T) * COS(2.*PHI)
              Y = Y + .1
          END DO
          X = X + .1
      END DO
      GETPARTICIPANTPLANE = ATAN2(QY,QX)/2.


      END

