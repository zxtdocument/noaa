#***********************************************************************
#  Makefile for AMSU level 1 to BUFR converters
#  Note that it is not build 64 bit executables since libw3 isn't built
#  64 bit.
#***********************************************************************
#***********************************************************************
#
#     DATE        SCR#    ENGINEER    DESCRIPTION
#    ---------   ------   --------   -----------------
#    12JUN2006  20061163    ccf      initial version
#***********************************************************************

FC = gfortran

FCFLAGS = -c -O3 -fno-range-check

LIB_PATH=/home/zxt/NOAA/zxt/2014.5.23.liuzc_supply_L1B_to_BUFR_code/AMSU_CODE/ncep_bufr/ncepbufr_w64.a /home/zxt/NOAA/zxt/2014.5.23.liuzc_supply_L1B_to_BUFR_code/AMSU_CODE/w3lib/libw3.a

all: OBSI_bufr_tranamsua.exe  OBSI_bufr_tranamsub.exe

OBSI_bufr_tranamsua.exe: tranamsua.o bufr1b.o
	$(FC) -o OBSI_bufr_tranamsua.exe tranamsua.o bufr1b.o \
	$(LIB_PATH)

OBSI_bufr_tranamsub.exe: tranamsub.o bufr1b.o
	$(FC) -o OBSI_bufr_tranamsub.exe tranamsub.o bufr1b.o \
	$(LIB_PATH)

tranamsua.o: tranamsua.f
	$(FC) $(FCFLAGS) -c tranamsua.f

tranamsub.o: tranamsub.f
	$(FC) $(FCFLAGS) -c tranamsub.f

bufr1b.o: bufr1b.f
	$(FC) $(FCFLAGS) -c bufr1b.f

clean:
	rm -f *.o *.exe
	
