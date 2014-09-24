#***********************************************************************
#
#    Makefile for NCEP BUFR library.
#
#     DATE        SCR#   ENGINEER    DESCRIPTION
#    ---------  ------  --------   -----------------
#    12JUN06   20061163   ccf      initial version
#***********************************************************************

.SUFFIXES: .o .f .c

RM          = rm -f
AR32        = ar 
AR64        = ar 

LIBTARGETW   = ncepbufr_w.a
LIBTARGETW64 = ncepbufr_w64.a
TARGETDIR = ./

default:
	@echo "To make $(LIBTARGET), type one of the following choices:"
	@echo "   make wh2"
	@echo "   make wh2_64"

wh2:
	$(MAKE) $(LIBTARGETW) \
	"FC = gfortran" \
	"FCFLAGS = " \
	"CC  = gcc" \
	"CCFLAGS = -fno-leading-underscore" 

wh2_64:
	$(MAKE) $(LIBTARGETW64) \
	"FC = gfortran" \
	"FCFLAGS =  " \
	"CC  = gcc" \
	"CCFLAGS = -fno-leading-underscore" 

.f.o:
	$(FC) $(FCFLAGS) -c $*.f
.c.o:
	$(CC) $(CCFLAGS) -c $*.c

OBJS=   addate.o cpbfdx.o idn30.o iupbs1.o msgwrt.o nxtwin.o rcstpl.o rjust.o ufbin3.o upb.o \
        adn30.o cpymem.o ifbget.o iupm.o mvb.o openbf.o rdbfdx.o rsvfvm.o ufbint.o upbb.o \
        bort_exit.o bfrini.o cpyupd.o ifxy.o iupvs01.o nemock.o openbt.o rdcmps.o seqsdx.o ufbinx.o upc.o \
        bort.o datebf.o igetdate.o iupvs1.o nemtab.o openmb.o rdmemm.o status.o ufbmem.o upds3.o \
        bort2.o datelen.o inctab.o jstchr.o nemtba.o openmg.o rdmems.o stdmsg.o ufbmms.o upftbv.o \
        cadn30.o digit.o invcon.o jstnum.o nemtbax.o ovrbs1.o rdmgsb.o stndrd.o ufbmns.o uptdd.o \
        capit.o drfini.o invmrg.o ljust.o nemtbb.o pad.o rdmsgb.o strcln.o ufbovr.o usrtpl.o \
        chekstab.o drstpl.o invtag.o lmsg.o nemtbd.o padmsg.o rdmsgw.o string.o ufbpos.o valx.o \
        chrtrn.o dumpbf.o invwin.o lstjpb.o nenuaa.o parseq.o rdtree.o strnum.o ufbqcd.o wrcmps.o \
        chrtrna.o dxdump.o ipkm.o lstrpc.o nenubd.o parusr.o rdusdx.o strsuc.o ufbqcp.o wrdlen.o \
        cktaba.o dxinit.o ireadft.o lstrps.o nevn.o parutg.o readdx.o subupd.o ufbrep.o writca.o \
        closbf.o dxmini.o ireadibm.o makestab.o newwin.o pkb.o readerme.o tabent.o ufbrms.o writcp.o \
        closmg.o elemdx.o ireadmg.o maxout.o nmbyt.o pkbs1.o readft.o tabsub.o ufbrp.o writdx.o \
        cmpmsg.o getabdb.o ireadmm.o mesgbc.o nmsub.o pkc.o readibm.o trybump.o ufbrw.o writlc.o \
        cmsgini.o getlens.o ireadns.o mesgbf.o nmwrd.o pkftbv.o readlc.o ufbcnt.o ufbseq.o writsa.o \
        cnved4.o gets1loc.o ireadsb.o minimg.o numbck.o pktdd.o readmg.o ufbcpy.o ufbsp.o writsb.o \
        conwin.o getwin.o irev.o mova2i.o numtab.o pkvs01.o readmm.o ufbcup.o ufbstp.o wrtree.o \
        copybf.o i4dy.o istdesc.o mrginv.o numtbd.o pkvs1.o readns.o ufbdmp.o ufbtab.o wtstat.o \
        copymg.o ichkstr.o iupb.o msgini.o nvnwin.o posapn.o readsb.o ufbevn.o ufbtam.o \
        copysb.o icopysb.o iupbs01.o msgupd.o nwords.o posapx.o rewnbf.o ufbget.o ufdump.o \
	bort_exit.o ccbfl.o cobfl.o crbmg.o cwbmg.o rbytes.o restd.o wrdesc.o

$(LIBTARGETW): $(OBJS)
	$(AR32) -ru $(TARGETDIR)$(LIBTARGETW) $(OBJS)
	#rm -f *.o 

$(LIBTARGETW64): $(OBJS)
	$(AR64) -ru $(TARGETDIR)$(LIBTARGETW64) $(OBJS)
	#rm -f *.o

wclean:
	$(RM) $(OBJS) $(LIBTARGETW) 

wclean64:
	$(RM) $(OBJS) $(LIBTARGETW64) 
