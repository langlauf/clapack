/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, nout;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static integer c__2 = 2;
static integer c_n1 = -1;
static integer c__0 = 0;
static integer c__1 = 1;

/* Subroutine */ int zerrtz_(char *path, integer *nunit)
{
    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer info;
    static doublecomplex a[4]	/* was [2][2] */;
    static char c2[2];
    extern /* Subroutine */ int alaesm_(char *, logical *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chkxer_(char *, integer *, integer *, logical 
	    *, logical *), ztzrqf_(integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static doublecomplex tau[2];

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ZERRTZ tests the error exits for ZTZRQF.   

    Arguments   
    =========   

    PATH    (input) CHARACTER*3   
            The LAPACK path name for the routines to be tested.   

    NUNIT   (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
*/


    infoc_1.nout = *nunit;
    s_copy(c2, path + 1, 2L, 2L);
    a[0].r = 1., a[0].i = -1.;
    a[2].r = 2., a[2].i = -2.;
    a[3].r = 3., a[3].i = -3.;
    a[1].r = 4., a[1].i = -4.;
    infoc_1.ok = TRUE_;

/*     Test error exits for the trapezoidal routines. */

    io___3.ciunit = infoc_1.nout;
    s_wsle(&io___3);
    e_wsle();
    if (lsamen_(&c__2, c2, "TZ")) {

/*        ZTZRQF */

	s_copy(srnamc_1.srnamt, "ZTZRQF", 6L, 6L);
	infoc_1.infot = 1;
	ztzrqf_(&c_n1, &c__0, a, &c__1, tau, &info);
	chkxer_("ZTZRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	ztzrqf_(&c__1, &c__0, a, &c__1, tau, &info);
	chkxer_("ZTZRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	ztzrqf_(&c__2, &c__2, a, &c__1, tau, &info);
	chkxer_("ZTZRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
    }

/*     Print a summary line. */

    alaesm_(path, &infoc_1.ok, &infoc_1.nout);

    return 0;

/*     End of ZERRTZ */

} /* zerrtz_ */

