/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int dlasum_(char *type, integer *iounit, integer *ie, 
	integer *nrun)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,a2,i4,a8,i4,a35)";
    static char fmt_9998[] = "(/1x,a14,a3,a23,i4,a11)";

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___2 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK auxiliary test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DLASUM prints a summary of the results from one of the test routines. 
  

    ===================================================================== 
*/


    if (*ie > 0) {
	io___1.ciunit = *iounit;
	s_wsfe(&io___1);
	do_fio(&c__1, type, 3L);
	do_fio(&c__1, ": ", 2L);
	do_fio(&c__1, (char *)&(*ie), (ftnlen)sizeof(integer));
	do_fio(&c__1, " out of ", 8L);
	do_fio(&c__1, (char *)&(*nrun), (ftnlen)sizeof(integer));
	do_fio(&c__1, " tests failed to pass the threshold", 35L);
	e_wsfe();
    } else {
	io___2.ciunit = *iounit;
	s_wsfe(&io___2);
	do_fio(&c__1, "All tests for ", 14L);
	do_fio(&c__1, type, 3L);
	do_fio(&c__1, " passed the threshold (", 23L);
	do_fio(&c__1, (char *)&(*nrun), (ftnlen)sizeof(integer));
	do_fio(&c__1, " tests run)", 11L);
	e_wsfe();
    }
    return 0;

/*     End of DLASUM */

} /* dlasum_ */

