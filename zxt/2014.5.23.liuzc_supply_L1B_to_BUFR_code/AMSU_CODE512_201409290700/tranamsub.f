      PROGRAM BUFR_TRANAMSUB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_TRANAMSUB
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2005-04-29
C
C ABSTRACT: Read raw AMSU-B 1B format file, decode, write selected
C   observations to output file. Tb reports are written to a BUFR
C   file and to an IEEE file.
C
C PROGRAM HISTORY LOG:
C 1998-06-15  Treadon  -- Original author
C 1999-03-24  X. Su    -- Modified to add bias correction
C 2000-09-06  Woollen  -- Added second output in BUFR
C 2000-11-20  Treadon  -- Changed to properly relabel NOAA-16 satellite
C       id from 2 to 16 to be consistent with the convention followed
C       in the global and regional analysis systems; Added error
C       handling when no output is created so that subsequent TRANJB's
C       are skipped
C 2002-02-11  Woollen  -- Modifications and corrections to output BUFR
C       dataset: "SAID" (0-01-007) corrected to proper WMO Code Table
C       value (was 14 for NOAA-14, etc.), "SIID" (0-02-019) repl.
C       "SIDU" (0-02-021) which didn't seem to be correct, "HMSL"
C       (0-07-002) corrected to proper units of meters (was being
C       stored in km), "LSQL" (0-08-012) corrected to proper WMO Code
C       Table value (0-land/1-sea) (was backwards), "TMBR" (0-12-163)
C       corrected to proper units of K (was being stored as K +
C       273.15), channel 20 "TMBR" set to missing for HIRS-2 and HIRS-3
C       types
C 2002-07-08  Keyser   -- Accounts for NOAA-17 (converts NESDIS sat.
C       no. from 6 to 17)
C 2002-10-23  Treadon  -- Use lbyte instead of mbyte for unpacking
C       counts
C 2004-01-23  Keyser   -- Based on new namelist switch "compress", now
C       has option to write compressed BUFR messages using WRITCP
C       instead of WRITSB (removes the need for the downstream program
C       BUFR_COMPRESS)
C 2005-04-29  Keyser   -- Improved Docblocks and comments in code
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 05     - Standard input (namelist "input")
C     UNIT 11     - Binary file containing raw 1B AMSU-B data
C     UNIT 12     - BUFR mnemonic table
C     UNIT 41     - Binary file containing topography information
C                   used by function LANSEA
C
C           ***NOTE***
C                   Function LANSEA assumes this information is in
C                   a file named 'lowtopog.dat' which is local to
C                   the working directory.
C
C   OUTPUT FILES:
C     UNIT 06      - Printout
C     UNIT 51      - Binary (IEEE) file containing decoded AMSU-B Tb
C                    data
C     UNIT 52      - BUFR file containing AMSU-B Tb data
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - AMSUB   CHARS   DATTIM  ICHARS  LANSEA  LBIT
C                  MBYTE   LBYTE   XFLOAT  BUFR1B
C     SYSTEM:    - SYSTEM
C     LIBRARY:
C       W3LIB    - W3TAGB  W3TAGE  ERREXIT W3FS26
C       BUFRLIB  - OPENBF  CLOSBF  OPENMB  WRITSB  WRITCP  UFBSEQ
C                  MESGBC
C
C
C   EXIT STATES
C     0 = No errors detected
C     1 = Data type id decoded from header is not for AMSU-B
C     3 = Problem reading header record of 1b AMSU-B file
C     6 = Unknown satellite id
C     7 = Unknown satellite instrument
C
C REMARKS:
C   Switches read in Namelist INPUT:
C     INFILE     - Path to input 1B data file
C     OUTFILE    - Path to output IEEE file
C     COMPRESS   - BUFR compression switch (YES or NO)
C     COEFILE    - Path to input coefficient file
C     PROCESS_Tb - Process brightness temps into BUFR and IEEE files?
C                    (hardwired to YES - can only process Tb)
C     PROCESS_Ta - Process antenna temps into BUFR files?
C                    (hardwired to NO - can only process Tb)
C
C####################################################################
C  NOTE: This program can only process Tb into a BUFR file.  There
C        is no Ta data for AMSU-B.  Switches pertaining to the
C        processing of Ta data are included because the parent script
C        also executes BUFR_TRANAMSUA which can process BOTH Tb and
C        Ta data into BUFR files.
C####################################################################
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

C  Declare namelist variables and namelist
C  ---------------------------------------

      integer stdout
      character*8  compress,process_Tb,process_Ta
      character*80 infile,outfile,coefile
      namelist /input/ infile,outfile,coefile,compress,process_Tb,
     x                 process_Ta

      common/switches/compress,process_Tb,process_Ta

C  Set I/O unit numbers
C  --------------------

      data lunam, stdout / 5,  6  /
      data lunin         / 11 /
      data lunout        / 51 /

      call w3tagb('BUFR_TRANAMSUB',2005,0119,0068,'NP22')

      print *
      print *, 'WELCOME TO BUFR_TRANAMSUB - Version 04/29/2005'
      print *

C  Get Namelist input
C  ------------------

      read(lunam,input)

      process_Tb = 'YES'  ! process_Tb is hardwired to YES
      process_Ta = 'NO'   ! process_Ta is hardwired to NO

      write(stdout,*)'namelist input below'
      write(stdout,input)

C  Open unit to output IEEE file
C  -----------------------------

      open(lunout,file=outfile,form='unformatted')

C  Read/decode/output data records scan by scan
C  --------------------------------------------

      call amsub(lunin,infile,lunout)

      call w3tage('BUFR_TRANAMSUB')

      stop
      end
cfpp$ expand(lbyte,mbyte,xfloat,lansea,dattim)

      SUBROUTINE AMSUB(LUNIN,RAWAMSU,LUNOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    AMSUB
C   PRGMMR: D. A. KEYSER      ORG: NP22       DATE: 2002-07-08
C
C ABSTRACT: Read raw AMSU-B 1B format file, decode, write selected
C   observations to output file.  Tb reports are written to a BUFR
C   file and to an IEEE file.
C
C PROGRAM HISTORY LOG:
C 1998-06-15  Treadon  -- Original author
C 2000-03-24  X. Su    -- Modification for bias correction on counts
C 2002-07-08  Keyser   -- Accounts for NOAA-17 (converts NESDIS sat.
C       no. from 6 to 17)
C
C USAGE:    CALL AMSUB(LUNIN,RAWAMSU,LUNOUT)
C   INPUT ARGUMENT LIST:
C     LUNIN    - Unit connected to raw 1B AMSU-B data file
C     RAWAMSU  - Name of raw 1B AMSU-B data file
C     LUNOUT   - Unit connected to output binary (IEEE) file
C
C   INPUT FILES:
C     UNIT 12     - BUFR mnemonic table
C     UNIT 41     - Binary file containing topography information
C                   used by function LANSEA
C     UNIT LUNIN  - Binary file containing raw 1B AMSU-B data
C
C           ***NOTE***
C                   Function LANSEA assumes this information is in
C                   a file named 'lowtopog.dat' which is local to
C                   the working directory.
C
C   OUTPUT FILES:
C     UNIT 06      - Printout
C     UNIT 52      - BUFR file containing AMSU-B Tb data
C     UNIT LUNOUT  - Binary (IEEE) file containing decoded AMSU-B Tb
C                    data
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

C  Include machine dependent parameters
C  ------------------------------------

      include 'rfac.inc'

C  Declare/set parameters:
C      NBYTE1  = Total number of bytes (3072) in AMSU-B data record
C      NBYTE4  = Number of 4-byte words in NBYTE1 bytes (3072/4=768)
C      NSET    = Number of topography datasets for function LANSEA3
C                (not currently used)
C      NID,NND = Number of arguments in arrays in dattim call
C      EPS     = A "small" number
C      MCH     = Number of channels
C      MPOS    = Number of spots (positions) on a scan line
C      NTX     = Transmitter number
C      NFOV    = Bias correction number from data

      integer,parameter::real_32=selected_real_kind(6,37)
      integer,parameter::real_64=selected_real_kind(15,307)
      real(real_64) eps
      parameter (nbyte1=3072,nbyte4=nbyte1/4)
      parameter (nset=3)
      parameter (nid=3,nnd=6)
      parameter (eps=1.d-12)
      parameter (mch=5)
      parameter (mpos=90)
      parameter (ntx=4)
      parameter (nfov=19)

C  Set parameters for structure of output data file
C  ------------------------------------------------

      parameter (nreal=14,ntot=nreal+mch)

C  Declare variables
C  -----------------

      character*1 kbuf(nbyte1)
      character*4 indat(nbyte4),jbuf(nbyte4)
      character*40 mapfile(nset)
      character*80 rawamsu

      integer stdout
      integer(8) itime
      integer idt(nid),ndt(nnd)
      integer ichan(mch),lndsea(mpos),ikeepb(mpos)
      integer imx(nset),jmx(nset),ibadc(mch)
      integer ibiascorr(mch,nfov,ntx)
      integer icorrtab(mch,mpos,ntx)
      integer itranrefpow(ntx)
      integer itranpow(ntx)

      real(real_64) p1,p2,term1,term2,term3,ta0,b,c
      real(real_64) sctime,counts,rads,sathgt
      real(real_64) scale,scale6,scale10,scale16
      real(real_64) slat(mpos),slon(mpos)
      real(real_64) cwave(mch),cnst1(mch),cnst2(mch)
      real(real_64) cfrq0(mch)
      real(real_64) rad(mch,mpos),tb(mch,mpos),sfchgt(mpos)
      real(real_64) c0(mch),c1(mch),c2(mch)
      real(real_64) saza(mpos),soza(mpos),rlocaz(mpos)
      real(real_32) bdata(ntot)
      real(real_64) badr(mch),badtb(mch)
      real(real_64) grad(5,19,4)

      double precision two22,two30,two44

C  Declare equivalences
C  --------------------

      equivalence (kbuf(1),jbuf(1))

C  Set information for different resolution map datasets
C  -----------------------------------------------------

      data imx / 360, 720, 1440 /
      data jmx / 181, 361, 721  /
      data mapfile / 'mapdat_100.iee', 'mapdat_050.iee',
     x     'mapdat_025.iee' /

C  Lower/upper limits for gross temperature check on Tb
C  ----------------------------------------------------

      data tlo,thi / 100., 400. /

C  Constants for Planck equation
C  -----------------------------

      data p1,p2 / 1.1910659d-5, 1.438833d0 /

C  Missing data flag
C  -----------------

      data rmiss / -999. /

C  Channel numbers
C  ---------------

      data ichan / 16, 17, 18, 19, 20 /

C  Set I/O unit numbers (including standard output)
C  ------------------------------------------------

      data stdout / 6/
      data lundx  /12/
      data lubfrb /52/

      write(stdout,*)' '
      write(stdout,*)' BEGIN AMSU-B 1B DECODE'

C  Initialize arrays
C  -----------------

      badr  = 0.
      badtb = 0.
      nprint = 1000 ! skip between data record diagnostic prints

C  Write header record to output IEEE file
C  ---------------------------------------

      write(stdout,*)' '
      write(stdout,*)'header information below'
      write(stdout,*)'nreal,mch = ',nreal,mch
      write(stdout,*)'ntot      = ',ntot
      write(stdout,*)'channel numbers below'
      write(stdout,*) (ichan(i),i=1,mch)
      write(stdout,*)' '
      write(lunout) nreal,mch,(ichan(i),i=1,mch)
ccccc write(99,*) nreal,mch,(ichan(i),i=1,mch)

C  Open output BUFR file
C  ---------------------

      call openbf(lubfrb,'OUT',lundx)

C  Open unit to raw 1B AMSU-B data file - read header record, see if
C   valid data type - if not, exit routine
C  -----------------------------------------------------------------

      open(lunin,file=rawamsu,recl=nbyte1/rfac,
     &      access='direct',status='old')
      nri = 1
      read (lunin,rec=nri,err=1900) (kbuf(i),i=1,nbyte1)

C  Load header record into work array
C  ----------------------------------

      do i = 1,nbyte4
         indat(i) = jbuf(i)
      end do

C  Extract NOAA spacecraft identification code (72*8+1=577)
C  --------------------------------------------------------

      jsat  = lbyte(577,16,indat)
      if (jsat.eq.4) then  ! NOAA-15
         jsat0 = jsat
         jsat  = 15
         write(stdout,*) '***WARNING:   reset satellite id from ',
     x        jsat0,' to ',jsat
      elseif (jsat.eq.2) then  ! NOAA-16
         jsat0 = jsat
         jsat  = 16
         write(stdout,*) '***WARNING:   reset satellite id from ',
     x        jsat0,' to ',jsat
      elseif (jsat.eq.6) then  ! NOAA-17
         jsat0 = jsat
         jsat  = 17
         write(stdout,*) '***WARNING:   reset satellite id from ',
     x        jsat0,' to ',jsat
      endif

C  Extract data type code (76*8+1=609)
C  -----------------------------------

      jtype = lbyte(609,16,indat)

      if (jtype.ne.11) then
         write(stdout,*)'***ERROR***  Input data file does not contain',
     x    ' AMSU-B data (type=11).  data type = ',jtype
         call w3tage('BUFR_TRANAMSUB')
         call errexit(1)
      endif
      write(stdout,*) 'Data and satellite type = ',jtype,jsat

C  Extract number of data records in data set (132*8+1=1057)
C   and number of scans (134*8+1=1073)
C  ---------------------------------------------------------

      nrecs = lbyte(1057,16,indat)
      nscan = lbyte(1073,16,indat)
      write(stdout,*)'nrecs,nscan=',nrecs,nscan

C  Extract coefficients for radiance to temperature conversion
C  -----------------------------------------------------------

      scale6 = 1.d-6
      do j = 1,mch
         jb  = 325 + (j-1)*12
         jb0 = jb
         jb1 = jb0 + 4
         jb2 = jb1 + 4
         cwave(j) = xfloat(1,kbuf(jb0))*scale6
         cnst1(j) = xfloat(1,kbuf(jb1))*scale6
         cnst2(j) = xfloat(1,kbuf(jb2))*scale6
      end do
      write(stdout,*)'chn 1 cwave,cnst1,cnst2=',
     x     cwave(1),cnst1(1),cnst2(1)
      write(stdout,*)' '

C  Extract the bias correction
C     ntx:  the trasmitter number: STX-1, STX-2, STX-3 SARR
C     nfov: the number of bias correction provided in the header
C            record nfov=90/5+1
C  -------------------------------------------------------------

      do itx=1,ntx
      do ifov=1,nfov
      do ich=1,mch
          jj=1000*8+(ifov-1)*5*16+(ich-1)*16+(itx-1)*105*16+1
          ibiascorr(ich,ifov,itx)=mbyte(jj,16,indat)
      enddo
      enddo
      enddo

      write(stdout,*) 'ibiascorr(1,1,1),ibiascorr(1,2,1)',
     :                 ibiascorr(1,1,1),ibiascorr(1,2,1)
      write(stdout,*) 'ibiascorr(4,1,4),ibiascorr(4,2,4)',
     :                 ibiascorr(4,1,4),ibiascorr(4,2,4)

C  Interpolation: quadratic interpolation recommended in the appendix 4
C  --------------------------------------------------------------------

C  Calculate gradients
C  -------------------

      do ich=1,mch
      do itx=1,ntx
      do ifov=2,nfov-1
         grad(ich,ifov,itx)=0.1*(ibiascorr(ich,ifov+1,itx)-
     :                           ibiascorr(ich,ifov-1,itx))
      end do
        grad(ich,1,itx)=2.0*grad(ich,2,itx)-grad(ich,3,itx)
        grad(ich,nfov,itx)=2.0*grad(ich,nfov-1,itx)-grad(ich,nfov-2,itx)
      enddo
      enddo

C  Interpolate
C  -----------

      do ipx=1,mpos
         ip1=ipx/5+1           !  find nearest 2 points in table
         ip2=ipx/5+2           !  2 to 20
         ipx1=(ip1-1)*5        !  0 to 90
         ipx2=(ip2-1)*5        !  5 to 95
         if(ipx1 .eq.0) ipx1=1 !  first point is pixel 1
         if(ip2 .gt.19) ip2=19
         f=(ipx2-ipx)/(1.0*(ipx2-ipx1))  ! linear term
         ff=0.5*f*(ipx-ipx1)             ! quadratic term
         do ich=1,mch
         do itx=1,ntx
            icorrtab(ich,ipx,itx)=nint(ibiascorr(ich,ip1,itx)*f
     :               +ibiascorr(ich,ip2,itx)*(1.0-f)
     :               +(grad(ich,ip1,itx)-grad(ich,ip2,itx))*ff)
         enddo
         enddo
         enddo

       write(stdout,*) 'icorrtab(1,1,1), icorrtab(1,2,1)',
     :                 icorrtab(1,1,1), icorrtab(1,2,1)
       write(stdout,*) 'icorrtab(1,3,1), icorrtab(1,4,1)',
     :                 icorrtab(1,3,1), icorrtab(1,4,1)

       write(stdout,*) 'icorrtab(4,1,4), icorrtab(4,85,4)',
     :                 icorrtab(4,1,4), icorrtab(4,85,4)
       write(stdout,*) 'icorrtab(4,15,4), icorrtab(4,90,4)',
     :                 icorrtab(4,15,4), icorrtab(4,90,4)

C  Extract transmitter reference power
C  -----------------------------------

      do itx=1,ntx
       scale=1.0d-1
       jj=1848*8+1+(itx-1)*16
       itranrefpow(itx)=nint(mbyte(jj,16,indat)*scale)
      enddo

      write(stdout,*) 'itranrefpow',(itranrefpow(i),i=1,ntx)

C  Prepatory initializations prior to reading in satellite data
C  ------------------------------------------------------------

      nopos   = 0
      nqcbad  = 0
      nqctim  = 0
      nqccal  = 0
      nqcloc  = 0
      nermin  = 0
      nermaj  = 0
      nbadc   = 0
      nbadl   = 0
      nbadtb  = 0
      nbadr   = 0
      nrecb   = 0
      nrepb   = 0
      nskipc  = 0
      nskipq  = 0
      nskipm  = 0
      nskiptb = 0
      nlandb  = 0
      nseab   = 0
      nlo = 0

 1200 continue

C**********************************************************************
C                    MAIN LOOP OVER NUMBER OF SCANS
C**********************************************************************

         nri = nri + 1    ! Increment record counter

C  Read data record and load into local work array
C  -----------------------------------------------

         read(lunin,rec=nri,err=1600) (kbuf(i),i=1,nbyte1)

         do i = 1,nbyte4
            indat(i) = jbuf(i)
         end do

         nlo  = nlo + 1   ! Increment line counter
         line = nlo

C  Extract scan line number, date/time, position, and type
C  -------------------------------------------------------

         iline  = lbyte(1,16,indat)
         iyear  = lbyte(17,16,indat)  ! (2*8+1=17)
         iddd   = lbyte(33,16,indat)  ! (4*8+1=33)
         itime  = lbyte(65,32,indat)  ! (8*8+1=65)
         sctime = 1.d-3*float(itime)
         jyd    = 1000*iyear + iddd
         idt(1) = sctime + 0.5
         idt(2) = mod(jyd,1000)
         idt(3) = jyd/1000
         call dattim(idt,ndt)

C  Extract quality bits - if all good (=0) continue, else skip this scan
C  ---------------------------------------------------------------------

         isum   = 0
         iqcbad = lbyte(193,1,indat)  ! (8*24+1+0=193)
         iqctim = lbyte(194,1,indat)  ! (8*24+1+1=194)
         iqccal = lbyte(196,1,indat)  ! (8*24+1+3=196)
         iqcloc = lbyte(197,1,indat)  ! (8*24+1+4=197)
         iermin = lbyte(222,1,indat)  ! (8*24+1+29=222)
         iermaj = lbyte(223,1,indat)  ! (8*24+1+30=223)
         isum   = iqcbad + iqctim + iqccal + iqcloc
     x        + iermin + iermaj
         if (iqcbad.ne.0) nqcbad = nqcbad + 1
         if (iqctim.ne.0) nqctim = nqctim + 1
         if (iqccal.ne.0) nqccal = nqccal + 1
         if (iqcloc.ne.0) nqcloc = nqcloc + 1
         if (iermin.ne.0) nermin = nermin + 1
         if (iermaj.ne.0) nermaj = nermaj + 1
         if (isum.ne.0) then
            nskipq = nskipq + 1
            write(stdout,1000) nlo,iline,iqcbad,iqctim,
     x           iqccal,iqcloc,iermin,iermaj,nskipq
 1000              format('***FAIL QC  :  nlo=',i6,' iline=',i6,
     x           ' bad=',i2,' time=',i2,' cali=',i2,' loc=',i2,
     x           ' min=',i2,' maj=',i2,' nskipq=',i6)
            goto 1200
         endif

C  Extract calibration quality bits for each channel
C  -------------------------------------------------

         isum = 0
         do j = 1,mch
            ibadc(j) = 0
            jb       = (32 + (j-1)*2)*8+1
            iqccal   = lbyte(jb,16,indat)
            ibadc(j) = iqccal
            isum     = isum + ibadc(j)
         end do
         if (isum.ne.0) then
            nskipc = nskipc + 1
            write(stdout,1005) nlo,iline,(ibadc(j),j=1,mch)
 1005       format('***FAIL CAL :  nlo=',i6,' iline=',i6,
     x           ' badcal=',20(i2,1x))
         endif

C  Extract calibration coefficients
C  --------------------------------

         scale16 = 1.d-16
         scale10 = 1.d-10
         scale6  = 1.d-6
         do j = 1,mch
            jb2   = 61 + (j-1)*12
            c2(j) = xfloat(1,kbuf(jb2))*scale16
            jb1   = jb2 + 4
            c1(j) = xfloat(1,kbuf(jb1))*scale10
            jb0   = jb1 + 4
            c0(j) = xfloat(1,kbuf(jb0))*scale6

C  Code to pull out secondary calibration coefficients
C  ---------------------------------------------------

ccccc         jb2   = 121 + (j-1)*12
ccccc         c2j = xfloat(1,kbuf(jb2))*scale16
ccccc         jb1   = jb2 + 4
ccccc         c1j = xfloat(1,kbuf(jb1))*scale10
ccccc         jb0   = jb1 + 4
ccccc         c0j = xfloat(1,kbuf(jb0))*scale6
         end do

C  EXTRACT NAVIGATION DATA
C  -----------------------
C  -----------------------

C  Extract spacecraft altitude (km)
C  --------------------------------

         scale  = 1.d-1
         jb     = 210*8+1
         sathgt = lbyte(jb,16,indat)*scale

C  Extract angular relationships
C  -----------------------------

         scale = 1.d-2
         do i = 1,mpos
            jb0 = 212*8+1 + (i-1)*48
            jb1 = jb0 + 16
            jb2 = jb1 + 16
            soza(i) = mbyte(jb0,16,indat)*scale
            saza(i) = mbyte(jb1,16,indat)*scale
            rlocaz(i) = mbyte(jb2,16,indat)*scale
         end do

C  Extract earth location data
C  ---------------------------

         scale = 1.d-4
         do i = 1,mpos
            jb0     = 753 + (i-1)*8
            slat(i) = xfloat(1,kbuf(jb0))*scale
            jb1     = jb0 + 4
            slon(i) = xfloat(1,kbuf(jb1))*scale
            lndsea(i) = rmiss
            sfchgt(i) = rmiss
            ikeepb(i)  = 0
            if ( (abs(slat(i)).gt.90.) .or.
     x           (abs(slon(i)).gt.180.) ) then
               ikeepb(i) = 0
               nbadl    = nbadl + 1
            elseif ( (abs(slat(i)).le.eps) .and.
     x               (abs(slon(i)).le.eps) ) then
               ikeepb(i) = 0
               nopos    = nopos + 1
            else
               ikeepb(i) = 1

C  Set surface type information based on resolution option
C    If iresol = 1, use 1.0 degree dataset
C       iresol = 2, use 0.5 degree dataset
C       iresol = 3, use 0.25 degree dataset
C  -------------------------------------------------------

ccccc            call lansea3(xlat,xlon,imx(iresol),jmx(iresol),
ccccc  x              mapfile(iresol),rmask,water,elev,stdev)
ccccc            lndsea(i) = rmask + eps
ccccc            sfchgt(i) = elev

               ils = lansea(slat(i),slon(i),ll)
               if (ils.eq.2) then
                  lndsea(i) = 0
                  sfchgt(i) = 0.0
               elseif (ils.eq.1) then
                  lndsea(i) = 1
                  sfchgt(i) = 1.*ll
               else
                  lndsea(i) = rmiss
                  sfchgt(i) = rmiss
               endif
            endif
         end do

C  Is scan line for full scan mode - if so, keep, else skip this scan
C  ------------------------------------------------------------------

         imode = lbyte(21470,1,indat)  ! (2682*8+1+13=21470)
         if ( imode.ne.1 ) then
            nskipm = nskipm + 1
            write(stdout,1010) nlo,iline,imode,nskipm
 1010              format('***FAIL MODE:  nlo=',i6,' iline=',i6,
     x           ' imode=',i2,' nskipm=',i6)
            goto 1200
         endif

C  Extract the transmitter power
C  -----------------------------

         do itx=1,ntx
          jj=2792*8+1+(itx-1)*16  ! (2792*8+1=22337)
          itranpow(itx)=mbyte(jj,16,indat)
         enddo
         itranpow(ntx)=itranpow(ntx)+mbyte(22401,16,indat)  ! (2800*8+1)

         if(nlo.eq.10) then
          write(stdout,*)'itranpow',(itranpow(i),i=1,4)
         end if

C  Extract AMSU-B counts for channels 16-20, then convert counts to
C   radiances
C  ----------------------------------------------------------------

         do i = 1,mpos
            jb = (1480 + (i-1)*12 + 1*2)*8+1
            do j = 2,6
               jj = j-1
               jb0 = jb + (j-2)*16
               counts1 = lbyte(jb0,16,indat)

C  Bias correction on counts
C  -------------------------

               iecorr=0
               do itx=1,ntx
                    f=itranpow(itx)/(1.0*itranrefpow(itx))
                 if(f.gt.0.01) then
                    iecorr=iecorr+nint(icorrtab(jj,i,itx)*f)
                 end if
               enddo

               counts=counts1+iecorr

ccccc   write(stdout,*) 'iecorr,counts1,counts jj,c0(jj),c1(jj),c2(jj)',
ccccc :  iecorr,counts1,counts,jj,c0(jj),c1(jj),c2(jj)

               rads   = c0(jj) + (c1(jj)+c2(jj)*counts)*counts
               if (rads.lt.0.) then
                  nbadr   = nbadr + 1
                  badr(j) = badr(j) + 1
                  rads    = rmiss
               endif
               rad(jj,i) = rads
            end do
         end do

C-----------------------------------------------------------------------
C  Convert radiances to apparent temperature (Ta0), then convert
C   apparent temperature (Ta0) to brightness temperature (Tb)
C   QC all channels - if all channels are bad for a given spot, set
C   flag to omit Tb data in final write
C   Note:  The AMSU-B does not produce an intermediate antenna
C          temperature (Ta)
C-----------------------------------------------------------------------

         do i = 1,mpos
            ibadtb = 0
            do j = 1,mch
               rads  = rad(j,i)
               term1 = p2*cwave(j)
               if (rads.gt.eps) then
                  term2 = 1. + p1*cwave(j)**3/rads
                  if (term2.le.0) term2 = eps
                  term3   = log(term2)
                  ta0     = term1/term3
                  b       = cnst1(j)
                  c       = cnst2(j)
                  tb(j,i) = (ta0-b)/c
               else
                  ta0 = 0.
                  tb(j,i) = rmiss
               endif

C  Apply gross check to Tb using limits TLO and THI set in data stmt
C  -----------------------------------------------------------------

               if ( (tb(j,i).lt.tlo) .or.
     x              (tb(j,i).gt.thi) ) then
                  nbadtb   = nbadtb + 1
                  badtb(j) = badtb(j) + 1
                  tb(j,i) = rmiss
               endif

C  If calibration quality flag for this channel is nonzero, we do not
C   want to use this channel for Tb
C  ------------------------------------------------------------------

               if (ibadc(j).ne.0) then
                  nbadc   = nbadc + nbadc
                  tb(j,i) = rmiss
               endif

C  Count number of bad channels for current scan position
C  ------------------------------------------------------

               if (tb(j,i).lt.0.) ibadtb = ibadtb + 1

            end do

C  If all Tb channels are bad for current scan position, set keep flag
C   to zero (this tells the code below to not write Tb for this spot
C   to the output files)
C  -------------------------------------------------------------------

            if (ibadtb.eq.mch) then
               nskiptb = nskiptb + 1
               ikeepb(i) = 0
            endif

         end do

C  Write AMSU-B data for each spot position on current scan line
C  -------------------------------------------------------------

         do i = 1,mpos
            if (ikeepb(i).eq.1) then
               bdata(1) = jsat
               bdata(2) = jtype
               bdata(3) = ndt(1)
               bdata(4) = ndt(2)
               bdata(5) = ndt(3)
               bdata(6) = 3600*ndt(4) + 60*ndt(5) + ndt(6)
               bdata(7) = lndsea(i)
               bdata(8) = i
               bdata(9) = slat(i)
               bdata(10)= slon(i)
ccccc          bdata(11)= rlocaz(i)
               bdata(11)= saza(i)
               bdata(12)= soza(i)
               bdata(13)= sfchgt(i)
               bdata(14)= sathgt

               if (lndsea(i).lt.0.5) nseab  = nseab + 1
               if (lndsea(i).gt.0.5) nlandb = nlandb + 1
               do j = 1,mch
                  bdata(14+j) = tb(j,i)
               end do
               nrecb = nrecb + 1
               write(lunout) (bdata(j),j=1,ntot)  ! ieee write
               call bufr1b(lubfrb,'NC021024',nreal,mch,bdata,nrepb)
ccccc          write(99,*) (bdata(j),j=1,ntot)
            endif
         end do

C  Every NPRINT scan lines, print mpos-th record
C  ---------------------------------------------

         if (mod(nlo,nprint).eq.0) then
            write(stdout,*)' '
            write(stdout,*)' Tb data for line,rec=',nlo,nrecb
            write(stdout,*) (bdata(i),i=1,ntot)
            write(stdout,*)' '
         endif

C**********************************************************************
C           DONE WITH THIS SCAN LINE, READ NEXT SCAN LINE
C**********************************************************************

      goto 1200

 1600 continue

C  All scan lines have been read and processed, summarize
C  ------------------------------------------------------

      write(stdout,*)' '
      write(stdout,*)'Done reading raw 1b file'
      write(stdout,*)' '
      write(stdout,*)'AMSU-B INGEST STATS:'
      write(stdout,*)' no. scan lines           =   ',nlo,nrecs,nscan
      write(stdout,*)' no. fail good qc         =   ',nqcbad
      write(stdout,*)' no. fail time qc         =   ',nqctim
      write(stdout,*)' no. fail cali qc         =   ',nqccal
      write(stdout,*)' no. fail loc qc          =   ',nqcloc
      write(stdout,*)' no. frame errors         =   ',nermin,nermaj
      write(stdout,*)' no. scans bad qc         =   ',nskipq
      write(stdout,*)' no. scans bad calibr.    =   ',nskipc
      write(stdout,*)' no. scans bad mode       =   ',nskipm
      write(stdout,*)' no. bad (lat,lon)        =   ',nbadl
      write(stdout,*)' no. zero (lat,lon)       =   ',nopos
      write(stdout,*)' no. bad radiances        =   ',nbadr
      write(stdout,*)' no. bad calibration      =   ',nbadc
      write(stdout,*)' no. scans with bad Tb    =   ',nskiptb
      write(stdout,*)' no. bad Tb values        =   ',nbadtb
      write(stdout,*)' no. land/sea Tb obs      =   ',nlandb,nseab
      write(stdout,*)' no. Tb recs written      =   ',nrecb
      write(stdout,*)' no. Tb BUFR rpts written =   ',nrepb

      write(stdout,*)' '
      write(stdout,*)'bad radiance,temperature counts per channel'
      write(stdout,1020)
 1020 format(t1,'channel',t10,'bad rad',t20,'bad Tb')
      sumr  = 0.
      sumtb = 0.
      do j = 1,mch
         write(stdout,1030) j,badr(j),badtb(j)
 1030    format(t1,i2,t10,f8.1,t20,f8.1)
         sumr  = sumr  + badr(j)
         sumtb = sumtb + badtb(j)
      end do
      write(stdout,*)'nbadr,nbadtb=',sumr,sumtb

      write(stdout,*)' '
      write(stdout,*)' AMSU-B 1B DECODE COMPLETED'
      write(stdout,*)' '

C  Close UNITs
C  -----------

      close(lunin)
      close(lunout)
      call closbf(lubfrb)

      call system('echo YES > Tb')
      if(nrecb.eq.0) then
         write(stdout,1003)
 1003    format(/' NO Tb RECORDS WRITTEN -- DISABLING ALL SUBSEQUENT ',
     .    'Tb PROCESSING.'/)
         call system('echo NO > Tb')
      else
         call mesgbc(lubfrb,msgt,icomp)
         if(icomp.eq.1) then
            print'(/"OUTPUT Tb BUFR FILE MESSAGES   '//
     .       'C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .       msgt
         elseif(icomp.eq.0) then
            print'(/"OUTPUT Tb BUFR FILE MESSAGES   '//
     .       'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",'//
     .       'I5/)',  msgt
         elseif(icomp.eq.-1) then
            print'(//"ERROR READING OUTPUT Tb BUFR FILE - MESSAGE '//
     .       'COMPRESSION UNKNOWN"/)'
         elseif(icomp.eq.-3) then
            print'(/"OUTPUT Tb BUFR FILE DOES NOT EXIST"/)'
         elseif(icomp.eq.-2) then
            print'(/"OUTPUT Tb BUFR FILE HAS NO DATA MESSAGES"/'//
     .       '"FIRST MESSAGE TYPE FOUND IS",I5/)', msgt
         endif
      endif

      close(lubfrb)

      return

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C                                 ERRORS
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C  Error reading 1B file
C  ---------------------

 1900 continue
      write(stdout,*)' *** error reading hdr record of file ',rawamsu
      close(lunin)
      close(lunout)
      call closbf(lubfrb)
      call w3tage('BUFR_TRANAMSUB')
      call errexit(3)

      end

      SUBROUTINE CHARS(IWORD,LEN,CWORD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CHARS
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-06
C
C ABSTRACT: Turns integer into character string of specified length,
C   starting at low-order byte of integer.
C
C PROGRAM HISTORY LOG:
C 1997-11-06  Katz -- Original author
C
C USAGE:    CALL CHARS(IWORD,LEN,CWORD)
C   INPUT ARGUMENT LIST:
C     IWORD    - INTEGER argument
C     LEN      - INTEGER argument holding number of low-order bytes
C                of first argument to convert into character
C
C   OUTPUT ARGUMENT LIST:
C     CWORD    - CHARACTER argument
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$
      character*1 cword(len)
      integer iword
      do ic = len , 1 , -1
        ibeg = (len - ic) * 8
        ichr = ibits(iword,ibeg,8)
        cword(ic) = char(ichr)
      enddo
      return
      end

      SUBROUTINE DATTIM(IDT,NDT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DATTIM
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-06
C
C ABSTRACT: Converts year, day-of-year, and second-of-day into year,
C   month-of-year, day-of-month, hour-of-day, minute-of-hour, and
C   second-of-minute.
C
C PROGRAM HISTORY LOG:
C 1997-11-06  Katz -- Original author
C
C USAGE:    CALL DATTIM(IDT,NDT)
C   INPUT ARGUMENT LIST:
C     IDT      - INTEGER array argument containing three members:
C                year, day-of-year, and second-of-day.
C
C   OUTPUT ARGUMENT LIST:
C     KDT      - INTEGER array argument containing six members:
C                year, month-of-year, day-of-month, hour-of-day,
C                minute-of-hour, and second-of-minute.
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      integer idt(3),ndt(6)
      integer iday,ihr,imin,imon,isec,iyr,jday,jsec

      external w3fs26

      intrinsic mod

       JULIAN(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4
     &                    -3 * ((IYR + 4899) / 100) / 4 + IDYR

      iyr  = idt(3)
      jday = idt(2)

C  If year is two digits, convert to 4 digits
C  ------------------------------------------

      if (iyr.ge.0.and.iyr.le.99) then
         if (iyr.lt.21) then
            kyr = iyr + 2000
         else
            kyr = iyr + 1900
         endif
      else
         kyr = iyr
      endif

C  Compute julian day number as number days after 4713 bc
C  ------------------------------------------------------

      idy = jday
      jdn = julian(kyr,idy)

      call w3fs26(jdn,iyear,jmo,jda,idaywk,idayyr)

      imon = jmo
      iday = jda
      jsec = idt(1)
      ihr = jsec/3600
      imin = mod(jsec,3600)/60
      isec = jsec - 3600*ihr - 60*imin
      ndt(1) = iyr
      ndt(2) = imon
      ndt(3) = iday
      ndt(4) = ihr
      ndt(5) = imin
      ndt(6) = isec
      return
      end

      INTEGER FUNCTION ICHARS(CWORD,LEN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ICHARS
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Turns character string of specified length into integer.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    ICHARS(CWORD,LEN)
C   INPUT ARGUMENT LIST:
C     CWORD    - CHARACTER*1 array argument
C     LEN      - INTEGER argument holding length of cword
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      character*1 cword(len)
      lchars = 0
      do ic = len , 1 , -1
        ibeg = (len - ic) * 8
        icmove = mova2i(cword(ic))
        call mvbits(icmove,0,8,lchars,ibeg)
      enddo
      ichars = lchars
      return
      end
cfpp$ expand(ichars,lbit)

      INTEGER FUNCTION LANSEA(RLAT,RLON,LEVEL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LANSEA
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Calculates topography, land/sea status from latitude and
C   longitude.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    LANSEA(RLAT,RLON,LEVEL)
C   INPUT ARGUMENT LIST:
C     RLAT     - INTEGER argument containing scaled latitude
C     RLON     - INTEGER argument containing scaled longitude
C
C   OUTPUT ARGUMENT LIST:
C     LEVEL    - INTEGER argument containing scaled topography
C
C   INPUT FILES:
C     UNIT 41  - Binary low-resolution topography file
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      include 'rfac.inc'

      integer,parameter::real_64=selected_real_kind(15,307)
      integer ilat,ilon,level
      real(real_64) rlat,rlon
      real slat,slon,xlon
      integer iopn,iu,lan,last,lat,lenr,lon
      character*12 name
      character*4 iflag(12),kelev(192)
      character*2 ielev(360)

      integer lbit,ichars

      external lbit,ichars

      intrinsic float,max0

      equivalence (iflag(1),kelev(1)), (ielev(1),kelev(13))

      data name/'lowtopog.dat'/,iu/41/,lenr/768/,last/0/,iopn/0/

      save iopn,kelev,last

      if (iopn.eq.0) then
        open (iu,recl=lenr/rfac,
     &        file=name,access='direct',status='old')
        iopn = 1
      endif
      lan = 0
      level = 0
      slat = rlat
      slon = rlon
      lat = slat + 1.
      if (slat.lt.0.) lat = slat
      lat = max0(lat,-87)
      lat = 91 - lat
      if (lat.eq.last) go to 100
      read (iu,rec=lat) kelev
      last = lat
  100 xlon = slon
      if (xlon.lt.0.) xlon = xlon + 360.
      lon = xlon
      if (lon.eq.360) lon = 0
      lon = lon + 1
      lan = lbit(lon,iflag)
      if (lan.ne.0) then
        ltemp = ichars(ielev(lon),2)
        if (btest(ltemp,15)) then
          ltemp = ior(ltemp,-65536)
        endif
        level = ltemp
      endif
      lansea = 2 - lan
      return
      end
cfpp$ expand(ichars)

      INTEGER FUNCTION LBIT(J,ARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LBIT
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Extracts j'th bit from array of CHARACTER*4.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    LBIT(J,ARRAY)
C   INPUT ARGUMENT LIST:
C     J        - INTEGER argument
C     ARRAY    - CHARACTER*4 array argument
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      integer j
      character*4 array(*)
      integer ibit,jout,jw,jword,nbit

      integer ichars
      external ichars

      intrinsic btest

      jw = (j-1)/32
      nbit = j - jw*32
      jword = ichars(array(jw+1),4)
      ibit = 32 - nbit
      jout = 0
      if (btest(jword,ibit)) jout = 1
      lbit = jout
      return
      end
cfpp$ expand(ichars)

      INTEGER FUNCTION MBYTE(J,LENGTH,JARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MBYTE
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Extracts bit string from array of CHARACTER*4 and
C   converts it to INTEGER.  Entry point MBYTE propagates sign bit
C   in result; entry point LBYTE does not.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    MBYTE(J,LENGTH,JARRAY)
C   INPUT ARGUMENT LIST:
C     J        - INTEGER argument containing starting bit
C     LENGTH   - integer argument containing number of bits
C                (maximum value 32)
C     JARRAY   - CHARACTER*4 array argument
C
C   OUTPUT FILES:
C     UNIT 06  - printout
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      integer j,length
      character*4 jarray(*)
      integer inleft,jbit,kompl,mflag,n,nlj,nrj,
     +        nsh,nword
      integer(8) item,jleft,jrite,mask
      integer(8)  kounts(33)

      integer ichars
      external ichars

      intrinsic iand,ior,mod

      integer lbyte

      data kounts/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     +     16384,32768,65536,131072,262144,524288,1048576,2097152,
     +     4194304,8388608,16777216,33554432,67108864,134217728,
     +     268435456,536870912,1073741824,2147483648_8,0/

C  ENTRY MBYTE
C  -----------

      mflag = 1
  110 nword = (j-1)/32 + 1
      if (length.lt.1 .or. length.gt.32) write (*,fmt=120) length
  120 format (' improper byte length in mbyte or lbyte',i10)
      nlj = mod(j-1,32)
      nrj = 32 - length - nlj
      if (nrj.lt.0) go to 150
      item = ichars(jarray(nword),4)
      kompl = 33 - length - nlj
      mask = -kounts(kompl)
      item = iand(item,mask)
      item = item/kounts(nrj+1)
      mask = kounts(length+1) - 1
      item = iand(item,mask)
  130 if (mflag.eq.0) go to 140

c ... means logical byte

      mbyte = item
      jbit = iand(kounts(length),item)
      if (jbit.eq.0) return

c ... need sign extension

      mask = -mask - 1
      item = ior(item,mask)
      mbyte = item
      return

C  ENTRY LBYTE
C  -----------

      entry lbyte(j,length,jarray)

      mflag = 0
      go to 110

  140 lbyte = item
      return

c ... byte spans two words

  150 inleft = length + nrj
      mask = kounts(inleft+1) - 1
      jleft = ichars(jarray(nword),4)
      jleft = iand(jleft,mask)
      nsh = 1 - nrj
      jleft = jleft*kounts(nsh)
      n = 1 - nrj
      jrite = ichars(jarray(nword+1),4)
      kompl = 33 + nrj
      mask = -kounts(kompl)
      jrite = iand(jrite,mask)
      jrite = jrite/kounts(nrj+33)
      mask = kounts(n) - 1
      jrite = iand(jrite,mask)
      item = ior(jrite,jleft)
      mask = kounts(length+1) - 1
      go to 130
      end
cfpp$ expand(ichars)

      REAL FUNCTION XFLOAT(JB,IARRAY)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    XFLOAT
C   PRGMMR: BERT KATZ        ORG: NP20       DATE: 1997-11-05
C
C ABSTRACT: Takes two consecutive elements of CHARACTER*2 array and
C   forms a floating point number from them.
C
C PROGRAM HISTORY LOG:
C 1997-11-05  Katz -- Original author
C
C USAGE:    XFLOAT(JB,IARRAY)
C   INPUT ARGUMENT LIST:
C     JB       - INTEGER argument containing array location
C     IARRAY   - CHARACTER*2 array argument
C
C REMARKS:
C   NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP-CCS
C
C$$$

      integer jb
      integer(8) mask
      data mask/z'ffffffff00000000'/
ccccc character*2 iarray(2)
      character*2 iarray(2000)
      character*4 conv
      real xf
      integer j
      integer(8) jj
      integer in(2)

      integer ichars
      external ichars

      intrinsic btest,ior

      j = jb
      conv(1:2) = iarray(j)
      conv(3:4) = iarray(j+1)
      jj = ichars(conv,4)
      if (btest(jj,31_8)) then
        jj = ior(jj,mask)
      endif
      xf = jj
      xfloat = xf
      return
      end
