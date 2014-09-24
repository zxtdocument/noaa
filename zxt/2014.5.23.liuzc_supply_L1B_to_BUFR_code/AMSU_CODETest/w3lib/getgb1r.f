C-----------------------------------------------------------------------
      SUBROUTINE GETGB1R(LUGB,LSKIP,LGRIB,KF,KPDS,KGDS,KENS,LB,F,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB1R        READS AND UNPACKS A GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 95-10-31
C
C ABSTRACT: READ AND UNPACK A GRIB MESSAGE.
C
C PROGRAM HISTORY LOG:
C   95-10-31  IREDELL
C
C USAGE:    CALL GETGB1R(LUGB,LSKIP,LGRIB,KF,KPDS,KGDS,KENS,LB,F,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE
C     LSKIP        INTEGER NUMBER OF BYTES TO SKIP
C     LGRIB        INTEGER NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KF           INTEGER NUMBER OF DATA POINTS UNPACKED
C     KPDS         INTEGER (200) UNPACKED PDS PARAMETERS
C     KGDS         INTEGER (200) UNPACKED GDS PARAMETERS
C     KENS         INTEGER (200) UNPACKED ENSEMBLE PDS PARMS
C     LB           LOGICAL*1 (KF) UNPACKED BITMAP IF PRESENT
C     F            REAL (KF) UNPACKED DATA
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    97     ERROR READING GRIB FILE
C                    OTHER  W3FI63 GRIB UNPACKER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   BAREAD         BYTE-ADDRESSABLE READ
C   W3FI63         UNPACK GRIB
C   PDSEUP         UNPACK PDS EXTENSION
C
C REMARKS: THERE IS NO PROTECTION AGAINST UNPACKING TOO MUCH DATA.
C   SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C   DO NOT ENGAGE THE SAME LOGICAL UNIT FROM MORE THAN ONE PROCESSOR.
C   THIS SUBPROGRAM IS INTENDED FOR PRIVATE USE BY GETGB ROUTINES ONLY.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY, WORKSTATIONS
C
C$$$
      INTEGER KPDS(200),KGDS(200),KENS(200)
      LOGICAL*1 LB(*)
      REAL F(*)
      INTEGER KPTR(200)
      CHARACTER GRIB(LGRIB)*1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ GRIB RECORD
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  UNPACK GRIB RECORD
      IF(LREAD.EQ.LGRIB) THEN
        CALL W3FI63(GRIB,KPDS,KGDS,LB,F,KPTR,IRET)
        IF(IRET.EQ.0.AND.KPDS(23).EQ.2) THEN
          CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,GRIB(9))
        ENDIF
      ELSE
        IRET=97
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RETURN NUMBER OF POINTS
      IF(IRET.EQ.0) THEN
        KF=KPTR(10)
      ELSE
        KF=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
