      PROGRAM VISHNUTEST
	IMPLICIT NONE
C--Common block of Pythia
      COMMON/PYJETS/N,NPAD,K(23000,5),P(23000,5),V(23000,5)
	INTEGER N,NPAD,K
	DOUBLE PRECISION P,V
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
	INTEGER MSTU,MSTJ
	DOUBLE PRECISION PARU,PARJ
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
	INTEGER MDCY,MDME,KFDP
	DOUBLE PRECISION BRAT
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
	INTEGER MSEL,MSELPD,MSUB,KFIN
	DOUBLE PRECISION CKIN 
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
	INTEGER MSTP,MSTI
	DOUBLE PRECISION PARP,PARI
      COMMON/PYDATR/MRPY(6),RRPY(100)
	INTEGER MRPY
	DOUBLE PRECISION RRPY
C--identifier of file for hepmc output and logfile
	common/hepmcid/hpmcfid,logfid,vishnuid
	integer hpmcfid,logfid,vishnuid
C--discard event flag
	COMMON/DISC/NDISC,NSTRANGE,NGOOD,errcount,wdisc,DISCARD
	LOGICAL DISCARD
	INTEGER NDISC,NSTRANGE,NGOOD,errcount
	double precision wdisc
C--event weight
	COMMON/WEIGHT/EVWEIGHT,sumofweights
	double precision EVWEIGHT,sumofweights
C--number of scattering events
	COMMON/CHECK/NSCAT,NSCATEFF,NSPLIT
	DOUBLE PRECISION NSCAT,NSCATEFF,NSPLIT
C--number of extrapolations in tables
	common/extrapolations/ntotspliti,noverspliti,ntotpdf,noverpdf,
     &ntotxsec,noverxsec,ntotsuda,noversuda
	integer ntotspliti,noverspliti,ntotpdf,noverpdf,
     &ntotxsec,noverxsec,ntotsuda,noversuda
C--local variables
	integer j,nsim,init,i
	double precision gettemp,r,tau

	nsim=init()
C--event loop
C--REDMER -- this is called for each event
C----------- NSIM times
	DO 100 J=1,NSIM
	  call genevent(j)
 100	CONTINUE
 
C--finish
	WRITE(HPMCFID,'(A)')'HepMC::IO_GenEvent-END_EVENT_LISTING'
	WRITE(HPMCFID,*)
	CLOSE(HPMCFID,status='keep')

	write(logfid,*)
	write(logfid,*)'mean number of scatterings:',
     &      NSCAT/(SUMOFWEIGHTS-WDISC)
	write(logfid,*)'mean number of effective scatterings:',
     &      NSCATEFF/(SUMOFWEIGHTS-WDISC)
	write(logfid,*)'mean number of splittings:',
     &      NSPLIT/(SUMOFWEIGHTS-WDISC)
	write(logfid,*)
	write(logfid,*)'number of extrapolations in splitting integral: ',
     &	noverspliti,' (',(noverspliti*1.d0)/(ntotspliti*1.d0),'%)'
	write(logfid,*)
     &	'number of extrapolations in splitting partonic PDFs: ',
     &	noverpdf,' (',(noverpdf*1.d0)/(ntotpdf*1.d0),'%)'
	write(logfid,*)
     &	'number of extrapolations in splitting cross sections: ',
     &	noverxsec,' (',(noverxsec*1.d0)/(ntotxsec*1.d0),'%)'
	write(logfid,*)
     &	'number of extrapolations in Sudakov form factor: ',
     &	noversuda,' (',(noversuda*1.d0)/(ntotsuda*1.d0),'%)'
	write(logfid,*)
	write(logfid,*)'number of good events: ',ngood
	write(logfid,*)'total number of discarded events: ',NDISC
	write(logfid,*)'number of events for which conversion '//
     &'to hepmc failed: ',NSTRANGE

	write(logfid,*)'cross section:',PARI(1),'mb'
	write(logfid,*)'sum of event weights:',sumofweights-wdisc
        write(logfid,*)'FRANKENJEWEL - beta version, use with care'


	close(logfid,status='keep')
        close(vishnuid,status='keep')

	END



***********************************************************************
***********************************************************************
***   END OF MAIN PROGRAM - NOW COME THE SUBROUTINES   ****************
***********************************************************************
***********************************************************************


***********************************************************************
***	  function init
***********************************************************************
	integer function init()
	implicit none
	INTEGER PYCOMP
	INTEGER NMXHEP
C--Common block of Pythia
      COMMON/PYJETS/N,NPAD,K(23000,5),P(23000,5),V(23000,5)
	INTEGER N,NPAD,K
	DOUBLE PRECISION P,V
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
	INTEGER MSTU,MSTJ
	DOUBLE PRECISION PARU,PARJ
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
	INTEGER MDCY,MDME,KFDP
	DOUBLE PRECISION BRAT
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
	INTEGER MSEL,MSELPD,MSUB,KFIN
	DOUBLE PRECISION CKIN 
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
	INTEGER MSTP,MSTI
	DOUBLE PRECISION PARP,PARI
      COMMON/PYDATR/MRPY(6),RRPY(100)
	INTEGER MRPY
	DOUBLE PRECISION RRPY
C--use nuclear pdf?      
      COMMON/NPDF/MASS,NSET,EPS09,INITSTR
      INTEGER NSET
      DOUBLE PRECISION MASS
      LOGICAL EPS09
      CHARACTER*10 INITSTR
C--Parameter common block
	COMMON/PARAM/Q0,LPS,LQCD,LTIME,SCALEFACM,ANGORD,SCATRECOIL,
     &ALLHAD,compress,NF
      INTEGER NF
	DOUBLE PRECISION Q0,LQCD,LTIME,LPS,SCALEFACM
      LOGICAL ANGORD,SCATRECOIL,ALLHAD,compress
C--splitting integral
      COMMON/SPLITINT/SPLITIGGV(1000,1000),SPLITIQQV(1000,1000),
     &SPLITIQGV(1000,1000),QVAL(1000),ZMVAL(1000),QMAX,ZMMIN,NPOINT
      INTEGER NPOINT
      DOUBLE PRECISION SPLITIGGV,SPLITIQQV,SPLITIQGV,
     &QVAL,ZMVAL,QMAX,ZMMIN
C--pdf common block
	COMMON/PDFS/QINQX(2,1000),GINQX(2,1000),QINGX(2,1000),
     &GINGX(2,1000)
	DOUBLE PRECISION QINQX,GINQX,QINGX,GINGX
C--cross secttion common block
	COMMON/XSECS/INTQ1(1001,101),INTQ2(1001,101),
     &INTG1(1001,101),INTG2(1001,101)
	DOUBLE PRECISION INTQ1,INTQ2,INTG1,INTG2
C--Sudakov common block
	COMMON/INSUDA/SUDAQQ(1000,2),SUDAQG(1000,2),SUDAGG(1000,2)
     &,SUDAGC(1000,2)
	DOUBLE PRECISION SUDAQQ,SUDAQG,SUDAGG,SUDAGC
C--exponential integral for negative arguments
      COMMON/EXPINT/EIXS(3,1000),VALMAX,NVAL
      INTEGER NVAL
      DOUBLE PRECISION EIXS,VALMAX
C--discard event flag
	COMMON/DISC/NDISC,NSTRANGE,NGOOD,errcount,wdisc,DISCARD
	LOGICAL DISCARD
	INTEGER NDISC,NSTRANGE,NGOOD,errcount
	double precision wdisc
C--factor in front of formation times
	COMMON/FTIMEFAC/FTFAC
	DOUBLE PRECISION FTFAC
C--factor in front of alphas argument
	COMMON/ALPHASFAC/PTFAC
	DOUBLE PRECISION PTFAC
C--number of scattering events
	COMMON/CHECK/NSCAT,NSCATEFF,NSPLIT
	DOUBLE PRECISION NSCAT,NSCATEFF,NSPLIT
C--number of extrapolations in tables
	common/extrapolations/ntotspliti,noverspliti,ntotpdf,noverpdf,
     &ntotxsec,noverxsec,ntotsuda,noversuda
	integer ntotspliti,noverspliti,ntotpdf,noverpdf,
     &ntotxsec,noverxsec,ntotsuda,noversuda
C--event weight
	COMMON/WEIGHT/EVWEIGHT,sumofweights
	double precision EVWEIGHT,sumofweights
C--event weight exponent
	COMMON/WEXPO/WEIGHTEX
	DOUBLE PRECISION WEIGHTEX
C--identifier of file for hepmc output and logfile
	common/hepmcid/hpmcfid,logfid,vishnuid
	integer hpmcfid,logfid,vishnuid
C--max rapidity
	common/rapmax/etamax
	double precision etamax
C--memory for error message from getdeltat
	common/errline/errl
	integer errl
C--organisation of event record
	common/evrecord/nsim,npart,offset,hadrotype,sqrts,collider,hadro,
     &shorthepmc
	integer nsim,npart,offset,hadrotype
	double precision sqrts
	character*4 collider
	logical hadro,shorthepmc

C--Variables local to this program
	INTEGER NJOB,PDFSET,ios,pos,i,j,jj
	DOUBLE PRECISION PTMIN,PTMAX,GETLTIMEMAX,EOVEST,r,pyr
	character firstchar
	CHARACTER*2 SNSET
      CHARACTER*80 PDFFILE,XSECFILE,FILEMED,FILESPLIT,buffer,
     &label,value
      CHARACTER*100 HEPMCFILE,LOGFILE,FILENAME2,VISHNUFILE
	CHARACTER(LEN=100) filename
	LOGICAL PDFEXIST,SPLITIEXIST,XSECEXIST,WEIGHTED

      HPMCFID = 4
	logfid = 3
      vishnuid = 5


C--default settings
	nsim = 10000
	njob = 0
	logfile = 'out.log'
        vishnufile = 'vishnu.cvs'
	hepmcfile = 'out.hepmc'
	filesplit = 'splitint.dat'
	pdffile = 'pdfs.dat'
	xsecfile = 'xsecs.dat'
	filemed = 'medium-params.dat'
	nf = 3
	lqcd = 0.4
	q0 = 1.5
	ptmin = 5.
	ptmax = 350.
	etamax = 3.1
	collider = 'PPJJ'
	sqrts = 2760
	pdfset = 10042
	nset = 1
	mass = 208.
	weighted = .true.
	weightex = 5.
	angord = .true.
	allhad = .false.
	hadro = .true.
	hadrotype = 0
	shorthepmc = .true.
	compress = .true.
	
	lps = lqcd
	scatrecoil = .false.
	if (.not.hadro) shorthepmc = .true.

	SCALEFACM=1.
	ptfac=1.
	ftfac=1.d0

	if (iargc().eq.0) then
	  write(*,*)'No parameter file given, '// 
     &'will run with default settings.'
	else
	  call getarg(1,filename)
	  write(*,*)'Reading parameters from ',filename
	  open(unit=1,file=filename,status='old',err=110)
	  do 120 i=1,1000
          read(1, '(A)', iostat=ios) buffer
	    if(ios.ne.0) goto 130
	    firstchar = buffer(1:1)
	    if (firstchar.eq.'#') goto 120
          pos=scan(buffer,' ')
          label=buffer(1:pos)
          value=buffer(pos+1:)
          if(label.eq."NEVENT")then
            read(value,*,iostat=ios) nsim
          elseif(label.eq."NJOB")then
            read(value,*,iostat=ios) njob
          elseif(label.eq."LOGFILE")then
            read(value,'(a)',iostat=ios) logfile
          elseif(label.eq."VISHNUFILE")then
            read(value,'(a)',iostat=ios) vishnufile
          elseif(label.eq."HEPMCFILE")then
            read(value,'(a)',iostat=ios) hepmcfile
          elseif(label.eq."SPLITINTFILE")then
            read(value,'(a)',iostat=ios) filesplit
          elseif(label.eq."PDFFILE")then
            read(value,'(a)',iostat=ios) pdffile
          elseif(label.eq."XSECFILE")then
            read(value,'(a)',iostat=ios) xsecfile
          elseif(label.eq."MEDIUMPARAMS")then
            read(value,'(a)',iostat=ios) filemed
          elseif(label.eq."NF")then
            read(value,*,iostat=ios) nf
          elseif(label.eq."LAMBDAQCD")then
            read(value,*,iostat=ios) lqcd
          elseif(label.eq."Q0")then
            read(value,*,iostat=ios) q0
          elseif(label.eq."PTMIN")then
            read(value,*,iostat=ios) ptmin
          elseif(label.eq."PTMAX")then
            read(value,*,iostat=ios) ptmax
          elseif(label.eq."ETAMAX")then
            read(value,*,iostat=ios) etamax
          elseif(label.eq."PROCESS")then
            read(value,*,iostat=ios) collider
          elseif(label.eq."SQRTS")then
            read(value,*,iostat=ios) sqrts
          elseif(label.eq."PDFSET")then
            read(value,*,iostat=ios) pdfset
          elseif(label.eq."NSET")then
            read(value,*,iostat=ios) nset
          elseif(label.eq."MASS")then
            read(value,*,iostat=ios) mass
          elseif(label.eq."WEIGHTED")then
            read(value,*,iostat=ios) weighted
          elseif(label.eq."WEXPO")then
            read(value,*,iostat=ios) weightex
          elseif(label.eq."ANGORD")then
            read(value,*,iostat=ios) angord
          elseif(label.eq."KEEPRECOILS")then
            read(value,*,iostat=ios) allhad
          elseif(label.eq."HADRO")then
            read(value,*,iostat=ios) hadro
          elseif(label.eq."HADROTYPE")then
            read(value,*,iostat=ios) hadrotype
          elseif(label.eq."SHORTHEPMC")then
            read(value,*,iostat=ios) shorthepmc
          elseif(label.eq."COMPRESS")then
            read(value,*,iostat=ios) compress
	    else
	      write(*,*)'unknown label ',label
	    endif
 120	  continue


 110	  write(*,*)
     &		'Unable to open parameter file, will exit the run.'
	  call exit(1)

 130	  close(1,status='keep')
	  write(*,*)'...done'
	endif

	if (ptmin.lt.3.d0) ptmin = 3.d0

	OPEN(unit=logfid,file=LOGFILE,status='unknown')
	MSTU(11)=logfid

        OPEN(unit=vishnuid,file=VISHNUFILE,status='unknown')

	write(logfid,*)
	write(logfid,*)'parameters of the run:'
	write(logfid,*)'NEVENT       = ',nsim
	write(logfid,*)'NJOB         = ',njob
	write(logfid,*)'LOGFILE      = ',logfile
        write(logfid,*)'VISHNUFILE   = ',vishnufile
	write(logfid,*)'HEPMCFILE    = ',hepmcfile
	write(logfid,*)'SPLITINTFILE = ',filesplit
	write(logfid,*)'PDFFILE      = ',pdffile
	write(logfid,*)'XSECFILE     = ',xsecfile
	write(logfid,*)'MEDIUMPARAMS = ',filemed
	write(logfid,*)'NF           = ',nf
	write(logfid,*)'LAMBDAQCD    = ',lqcd
	write(logfid,*)'Q0           = ',q0
	write(logfid,*)'PTMIN        = ',ptmin
	write(logfid,*)'PTMAX        = ',ptmax
	write(logfid,*)'ETAMAX       = ',etamax
	write(logfid,*)'PROCESS      = ',collider
	write(logfid,*)'SQRTS        = ',sqrts
	write(logfid,*)'PDFSET       = ',pdfset
	write(logfid,*)'NSET         = ',nset
	write(logfid,*)'MASS         = ',mass
	write(logfid,*)'WEIGHTED     = ',weighted
	write(logfid,*)'WEXPO        = ',weightex
	write(logfid,*)'ANGORD       = ',angord
	write(logfid,*)'KEEPRECOILS  = ',allhad
	write(logfid,*)'HADRO        = ',hadro
	write(logfid,*)'HADROTYPE    = ',hadrotype
	write(logfid,*)'SHORTHEPMC   = ',shorthepmc
	write(logfid,*)'COMPRESS     = ',compress
	write(logfid,*)
	call flush(logfid)

	if ((collider.ne.'PPJJ').and.(collider.ne.'EEJJ')) then
	  write(logfid,*)'Fatal error: colliding system unknown, '//
     &	'will exit now'
	  call exit(1)
	endif

C--initialize medium
      CALL MEDINIT(FILEMED,logfid,etamax)
      CALL VISHNUINIT(vishnuid)
      CALL MEDNEXTEVT

	OPEN(unit=HPMCFID,file=HEPMCFILE,status='unknown')
	WRITE(HPMCFID,*)
	WRITE(HPMCFID,'(A)')'HepMC::Version 2.06.05'
	WRITE(HPMCFID,'(A)')'HepMC::IO_GenEvent-START_EVENT_LISTING'

	NPART=2
	
	if(ptmax.gt.0.)then
	  EOVEST=MIN(1.5*(PTMAX+50.)*COSH(ETAMAX),sqrts/2.)
	else
	  EOVEST=sqrts/2.
	endif

	IF(NSET.EQ.0)THEN
	 EPS09=.FALSE.
	ELSE
	 EPS09=.TRUE.
	 IF(NSET.LT.10)THEN
	  WRITE(SNSET,'(i1)') NSET
	 ELSE
	  WRITE(SNSET,'(i2)') NSET
	 ENDIF
	  INITSTR='EPS09LO,'//SNSET
	ENDIF 

C--initialise PYTHIA
C--no multiple interactions
	 MSTP(81) = 0
C--switch off final state radiation
	 MSTP(71)=0
C--No hadronisation
       MSTP(111)=0
C--Min shat in simulation
       CKIN(1)=2.      
C--pT-cut
       CKIN(3)=PTMIN
       CKIN(4)=PTMAX
C--use LHAPDF
	 MSTP(52)=2
C--choose pdf: CTEQ6ll (LO fit/LO alphas) - 10042
C	         MSTW2008 (LO central) - 21000
	 MSTP(51)=PDFSET
C--All QCD processes are active
       MSEL=1
C--weighted events
       IF(WEIGHTED) MSTP(142)=1

C--number of errors to be printed
	 MSTU(22)=MAX(10,INT(5.*NSIM/100.))

C--number of lines in event record
	MSTU(4)=23000
	MSTU(5)=23000

C--switch off pi0 decay
      MDCY(PYCOMP(111),1)=0
C--initialisation call
C-- REDMER -- this is the initialization
C--           of the PYTHIA6 routines
	 IF(COLLIDER.EQ.'EEJJ')THEN
	  OFFSET=9
        CALL PYINIT('CMS','e+','e-',sqrts)
	 ELSE
	  OFFSET=8
        CALL PYINIT('CMS','p','p',sqrts)
       ENDIF

	write(logfid,*)
	 INQUIRE(file=FILESPLIT,exist=SPLITIEXIST)
	 IF(SPLITIEXIST)THEN
	  write(logfid,*)'read splitting integrals from ',FILESPLIT
	  OPEN(unit=10,file=FILESPLIT,status='old')
	  READ(10,*)QMAX,ZMMIN,NPOINT
	  DO 893 I=1,NPOINT+1
	   READ(10,*) QVAL(I),ZMVAL(I)
 893    CONTINUE	 
	  DO 891 I=1,NPOINT+1
	   DO 892 J=1,NPOINT+1
	    READ(10,*)SPLITIGGV(I,J),SPLITIQQV(I,J),SPLITIQGV(I,J)
 892	   CONTINUE
 891	  CONTINUE
	  CLOSE(10,status='keep')
	 ELSE
 	  write(logfid,*)'have to integrate splitting functions, '// 
     &'this may take some time'
	 ENDIF
	write(logfid,*)

	INQUIRE(file=PDFFILE,exist=PDFEXIST)
	IF(PDFEXIST)THEN
	write(logfid,*)'read pdfs from ',PDFFILE
	 OPEN(unit=10,file=PDFFILE,status='old')
	 DO 872 I=1,2
	  DO 873 J=1,1000
	   READ(10,*)QINQX(I,J),GINQX(I,J),QINGX(I,J),GINGX(I,J)
 873	  CONTINUE
 872	 CONTINUE
	 CLOSE(10,status='keep')
	ELSE
 	 write(logfid,*)'have to integrate pdfs, this may take some time'
	  CLOSE(10,status='keep')
	ENDIF 
	write(logfid,*)

	INQUIRE(file=XSECFILE,exist=XSECEXIST)
	IF(XSECEXIST)THEN
	write(logfid,*)'read cross sections from ',XSECFILE
	 OPEN(unit=10,file=XSECFILE,status='old')
	  DO 881 J=1,1001
         DO 885 JJ=1,101
	   READ(10,*)INTQ1(J,JJ),INTQ2(J,JJ),
     &INTG1(J,JJ),INTG2(J,JJ)
 885     CONTINUE
 881	  CONTINUE
	 CLOSE(10,status='keep')
	ELSE
	 write(logfid,*)'have to integrate cross sections, '//
     &'this may take some time'
	ENDIF
	write(logfid,*)
	CALL FLUSH(3)



C--initialise random number generator status
      IF(NJOB.GT.0)THEN
       MRPY(1)=NJOB*1000
       MRPY(2)=0
      ENDIF

C--Call PYR once for initialization
	R=PYR(0)

      WDISC=0.d0
	NDISC=0
      NGOOD=0
      NSTRANGE=0
      
	ERRCOUNT=0
	errl = 0

	SUMOFWEIGHTS=0.d0

	NSCAT=0.d0
	NSCATEFF=0.d0
	NSPLIT=0.d0

	ntotspliti=0
	noverspliti=0
	ntotpdf=0
	noverpdf=0
	ntotxsec=0
	noverxsec=0
	ntotsuda=0
	noversuda=0

	init=nsim
	end



***********************************************************************
***	  subroutine genevent
***********************************************************************
	subroutine genevent(j)
	implicit none
C--identifier of file for hepmc output and logfile
	common/hepmcid/hpmcfid,logfid,vishnuid
	integer hpmcfid,logfid,vishnuid
	INTEGER PYCOMP
	INTEGER NMXHEP
C--Common block of Pythia
      COMMON/PYJETS/N,NPAD,K(23000,5),P(23000,5),V(23000,5)
	INTEGER N,NPAD,K
	DOUBLE PRECISION P,V
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
	INTEGER MSTU,MSTJ
	DOUBLE PRECISION PARU,PARJ
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
	INTEGER MDCY,MDME,KFDP
	DOUBLE PRECISION BRAT
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
	INTEGER MSEL,MSELPD,MSUB,KFIN
	DOUBLE PRECISION CKIN 
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
	INTEGER MSTP,MSTI
	DOUBLE PRECISION PARP,PARI
      COMMON/PYDATR/MRPY(6),RRPY(100)
	INTEGER MRPY
	DOUBLE PRECISION RRPY
C--Parameter common block
	COMMON/PARAM/Q0,LPS,LQCD,LTIME,SCALEFACM,ANGORD,SCATRECOIL,
     &ALLHAD,compress,NF
      INTEGER NF
	DOUBLE PRECISION Q0,LQCD,LTIME,LPS,SCALEFACM
      LOGICAL ANGORD,SCATRECOIL,ALLHAD,compress
C--discard event flag
	COMMON/DISC/NDISC,NSTRANGE,NGOOD,errcount,wdisc,DISCARD
	LOGICAL DISCARD
	INTEGER NDISC,NSTRANGE,NGOOD,errcount
	double precision wdisc
C--variables for angular ordering
      COMMON/ANGOR/ZA(23000),ZD(23000),THETAA(23000),QQBARD(23000)
	DOUBLE PRECISION ZA,ZD,THETAA
      LOGICAL QQBARD
C--factor in front of formation times
	COMMON/FTIMEFAC/FTFAC
	DOUBLE PRECISION FTFAC
C--time common block
      COMMON/TIME/MV(23000,5)
      DOUBLE PRECISION MV
C--colour index common block
	COMMON/COLOUR/TRIP(23000),ANTI(23000),COLMAX
	INTEGER TRIP,ANTI,COLMAX
C--number of scattering events
	COMMON/CHECK/NSCAT,NSCATEFF,NSPLIT
	DOUBLE PRECISION NSCAT,NSCATEFF,NSPLIT
C--event weight
	COMMON/WEIGHT/EVWEIGHT,sumofweights
	double precision EVWEIGHT,sumofweights
C--event weight exponent
	COMMON/WEXPO/WEIGHTEX
	DOUBLE PRECISION WEIGHTEX
C--max rapidity
	common/rapmax/etamax
	double precision etamax
C--organisation of event record
	common/evrecord/nsim,npart,offset,hadrotype,sqrts,collider,hadro,
     &shorthepmc
	integer nsim,npart,offset,hadrotype
	double precision sqrts
	character*4 collider
	logical hadro,shorthepmc

C--Variables local to this program
	INTEGER NOLD,PID,IPART,LME1,LME2,j,i
	DOUBLE PRECISION PYR,ENI,QMAX1,R,GETMASS,PYP,Q1,Q2,P21,P22,ETOT,
     &QMAX2,POLD,EN1,EN2,BETA(3),ENEW1,ENEW2,emax,lambda,x0,y0,x1,x2,x3,
     &MEWEIGHT,PSWEIGHT,WEIGHT,EPS1,EPS2,THETA1,THETA2,Z1,Z2,getltimemax
	CHARACTER*2 TYPE1,TYPE2
	LOGICAL FIRSTTRIP,WHICH1,WHICH2,ISDIQUARK

	 N=0
	 COLMAX=600
	 DISCARD=.FALSE.
       DO 91 I=1,23000
        MV(I,1)=0.d0
        MV(I,2)=0.d0
        MV(I,3)=0.d0
        MV(I,4)=0.d0
        MV(I,5)=0.d0
 91    CONTINUE

       CALL MEDNEXTEVT


C--write message to log-file
 102  IF(NSIM.GT.100)THEN
       IF(MOD(J,NSIM/100).EQ.0)THEN
 	  write(logfid,*) 'done with event number ',J
          write(vishnuid,*) 'done with event number ',J
 	 ENDIF
	else
          write(vishnuid,*) 'done with event number ',J
 	  write(logfid,*) 'done with event number ',J
      ENDIF
	call flush(logfid)
        call flush(vishnuid)
	end

***********************************************************************
***	  function odeint
***********************************************************************
	subroutine odeint(ystart,a,b,eps,h1,hmin,w1)
	implicit none
C--identifier of file for hepmc output and logfile
	common/hepmcid/hpmcfid,logfid,vishnuid
	integer hpmcfid,logfid,vishnuid
C--local variables
	integer nmax,nstep,w1
	double precision ystart,a,b,eps,h1,hmin,x,h,y,dydx,
     &deriv,yscale,hdid,hnew
	data nmax/100000/

	x = a
	y = ystart
	h = sign(h1,b-a)
	do 20 nstep=1,nmax
	  dydx = deriv(x,w1)
	  yscale = abs(y) + abs(h*dydx) + 1.e-25
	  if (((x + h - b)*h).gt.0.) h = b-x
	  call rkstepper(x,y,dydx,h,hdid,hnew,yscale,eps,w1)
	  if ((x - b)*h.ge.0) then
	    ystart = y
	    return
	  endif
	  h = hnew
	  if (abs(h).lt.abs(hmin)) then
	    write(logfid,*)'Error in odeint: stepsize too small',w1
     &	,ystart,a,b,h1
	    return
	  endif	  
 20	continue
	write(logfid,*)'Error in odeint: too many steps',w1
     &	,ystart,a,b,h1
	end



***********************************************************************
***	  function rkstepper
***********************************************************************
	subroutine rkstepper(x,y,dydx,htest,hdid,hnew,yscale,eps,w1)
	implicit none
C--identifier of file for hepmc output and logfile
	common/hepmcid/hpmcfid,logfid,vishnuid
	integer hpmcfid,logfid,vishnuid
C--local variables
	integer w1
	double precision x,y,dydx,htest,hdid,hnew,yscale,eps,
     &yhalf,y1,y2,rk4step,dydxhalf,xnew,delta,err,h,safety, powerdown,
     &powerup,maxup,maxdown,deriv,fac
	logical reject
	data powerdown/0.25/
	data powerup/0.2/
	data safety/0.9/
	data maxdown/10./
	data maxup/5./

	reject = .false.
	h = htest
 10	xnew = x + h
	if (x.eq.xnew) then
	  write(logfid,*)'Error in rkstepper: step size not significant'
	  return
	endif
	yhalf = rk4step(x,y,dydx,h/2.,w1)
	dydxhalf = deriv(x+h/2.,w1)
	y2 = rk4step(x+h/2.,yhalf,dydxhalf,h/2.,w1)
	y1 = rk4step(x,y,dydx,h,w1)
	delta = y2-y1
	err = abs(delta)/(yscale*eps)
	if (err.gt.1.) then
	  reject = .true.
	  fac = max(1./maxdown,safety/err**powerdown)
	  h = h*fac
	  goto 10 
	else
	  if (reject) then
	    hnew = h
	  else
	    fac = min(maxup,safety/err**powerup)
	    hnew = fac*h
	  endif
	  x = xnew
	  y = y2 + delta/15.
	  hdid = h
	endif
	end


***********************************************************************
***	  function deriv
***********************************************************************
      DOUBLE PRECISION FUNCTION DERIV(XVAL,W4)
      IMPLICIT NONE
C--Parameter common block
	COMMON/PARAM/Q0,LPS,LQCD,LTIME,SCALEFACM,ANGORD,SCATRECOIL,
     &ALLHAD,compress,NF
      INTEGER NF
	DOUBLE PRECISION Q0,LQCD,LTIME,LPS,SCALEFACM
      LOGICAL ANGORD,SCATRECOIL,ALLHAD,compress
C--variables for splitting function integration
	COMMON/INTSPLITF/QQUAD,FM
	DOUBLE PRECISION QQUAD,FM
C--variables for Sudakov integration
	COMMON/SUDAINT/QA,ZA2,EB,T,INSTATE,TYP
	DOUBLE PRECISION QA,ZA2,EB,T
	CHARACTER*2 TYP
	LOGICAL INSTATE
C--variables for pdf integration
	COMMON/PDFINTV/XMAX,Z
	DOUBLE PRECISION XMAX,Z
C--variables for cross section integration 
	COMMON/XSECV/QLOW,MDX
	DOUBLE PRECISION QLOW,MDX
C--local variables
	INTEGER W4
      DOUBLE PRECISION XVAL,GETSPLITI,PI,ALPHAS,GETINSPLITI,
     &GETINSUDAFAST,SCATPRIMFUNC,PQQ,PQG,PGG,PGQ,
     &MEDDERIV
	DATA PI/3.141592653589793d0/

	IF(W4.EQ.1)THEN
C--Sudakov integration
	 IF(INSTATE)THEN
        DERIV=2.*GETINSPLITI(XVAL,TYP)/XVAL
	 ELSE
        DERIV=2.*GETSPLITI(QA,XVAL,ZA2,EB,TYP)/XVAL
	 ENDIF
	ELSEIF(W4.EQ.2)THEN
C--P(q->qg) integration
	 DERIV=(1.+FM)*ALPHAS(XVAL*(1.-XVAL)*QQUAD/1.,LPS)*
     &		PQQ(XVAL)/(2.*PI)
	ELSEIF(W4.EQ.3)THEN
C--P(g->gg) integration
       DERIV=(1.+FM)*ALPHAS(XVAL*(1.-XVAL)*QQUAD/1.,LPS)
     &           *PGG(XVAL)/(2.*PI)
	ELSEIF(W4.EQ.4)THEN
C--P(g->qq) integration
	 DERIV=(1.+FM)*ALPHAS(XVAL*(1-XVAL)*QQUAD/1.,LPS)*
     &	PQG(XVAL)/(2.*PI)	
	ELSEIF(W4.EQ.5)THEN
	 DERIV=EXP(-XVAL)/XVAL
	ELSEIF(W4.EQ.6)THEN
       DERIV=2.*GETINSPLITI(XVAL,TYP)/XVAL
	ELSEIF(W4.EQ.7)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'QQ')
     &	*ALPHAS((1.-Z)*XVAL**2/1.,LPS)
     &	*PQQ(Z)/(2.*PI*XVAL)
	ELSEIF(W4.EQ.8)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'GC')
     &	*ALPHAS((1.-Z)*XVAL**2/1.,LPS)
     &	*PGQ(Z)/(2.*PI*XVAL)
	ELSEIF(W4.EQ.9)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'QQ')
     &	*ALPHAS((1.-Z)*XVAL**2/1.,LPS)
     &	*PQG(Z)/(2.*PI*XVAL)	
	ELSEIF(W4.EQ.10)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'GC')
     &	*ALPHAS((1.-Z)*XVAL**2/1.,LPS)*
     &      *2.*PGG(Z)/(2.*PI*XVAL)
	ELSEIF(W4.EQ.11)THEN
	 DERIV=3.*GETINSPLITI(SCALEFACM*SQRT(XVAL),'GQ')
     &	*SCATPRIMFUNC(XVAL,MDX)/(2.*XVAL)
	ELSEIF(W4.EQ.12)THEN
	 DERIV=2.*GETINSPLITI(SCALEFACM*SQRT(XVAL),'QG')
     &	*SCATPRIMFUNC(XVAL,MDX)/(3.*XVAL)
	ELSEIF(W4.EQ.13)THEN
	 DERIV=GETINSUDAFAST(QLOW,SCALEFACM*SQRT(XVAL),'GC')
     &	*3.*2.*PI*ALPHAS(XVAL+MDX**2,LQCD)**2/(2.*(XVAL+MDX**2)**2)
	ELSEIF(W4.EQ.14)THEN
	 DERIV=GETINSUDAFAST(QLOW,SCALEFACM*SQRT(XVAL),'QQ')
     &	*2.*2.*PI*ALPHAS(XVAL+MDX**2,LQCD)**2/(3.*(XVAL+MDX**2)**2)
	ELSEIF(W4.EQ.21)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'QQ')*GETINSPLITI(XVAL,'QQ')
     &	/XVAL
	ELSEIF(W4.EQ.22)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'GC')*GETINSPLITI(XVAL,'GQ')
     &	/XVAL
	ELSEIF(W4.EQ.23)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'QQ')*GETINSPLITI(XVAL,'QG')
     &	/XVAL
	ELSEIF(W4.EQ.24)THEN
	 DERIV=2.*GETINSUDAFAST(XVAL,XMAX,'GC')*2.
     &	*GETINSPLITI(XVAL,'GG')/XVAL
      ELSE
       DERIV=MEDDERIV(XVAL,W4-100)
      ENDIF
      END




        
