c ... Include file to set record length factor for the RECL keyword.
c ... This version is for compilers where RECL is in *** BYTES ***.

c ... Systems with compilers where this is the default include:
c ... HP-UX
c ... Sun Solaris
c ... IBM AIX
c ... SGI MIPS (with -bytereclen compiler switch)

c ... The parameter RFAC is used as a divisor for the record length
c ... variable in all open statements which use the RECL keyword, thus
c ... all record lengths in these modules are defined in BYTES.
c ... See rfaclong.inc for the corresponding version of this file
c ... for compilers where RECL is in *** LONGWORDS ***.

      integer rfac
      parameter ( rfac = 1 )
