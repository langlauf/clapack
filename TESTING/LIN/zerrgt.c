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
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int zerrgt_(char *path, integer *nunit)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    static doublecomplex b[2];
    static doublereal d[2];
    static doublecomplex e[2], w[2], x[2];
    static doublereal rcond, anorm;
    static char c2[2];
    static doublereal r1[2], r2[2], df[2];
    static doublecomplex ef[2];
    extern /* Subroutine */ int alaesm_(char *, logical *, integer *);
    static doublereal rw[2];
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chkxer_(char *, integer *, integer *, logical 
	    *, logical *), zptcon_(integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublereal *, doublereal *, 
	    integer *), zptrfs_(char *, integer *, integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, doublereal *,
	     doublecomplex *, doublereal *, integer *), zpttrf_(
	    integer *, doublereal *, doublecomplex *, integer *), zpttrs_(
	    char *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ZERRGT tests the error exits for the COMPLEX*16 tridiagonal   
    routines.   

    Arguments   
    =========   

    PATH    (input) CHARACTER*3   
            The LAPACK path name for the routines to be tested.   

    NUNIT   (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
*/


    infoc_1.nout = *nunit;
    io___1.ciunit = infoc_1.nout;
    s_wsle(&io___1);
    e_wsle();
    s_copy(c2, path + 1, 2L, 2L);
    d[0] = 1.;
    d[1] = 2.;
    e[0].r = 3., e[0].i = 0.;
    e[1].r = 4., e[1].i = 0.;
    anorm = 1.;
    infoc_1.ok = TRUE_;

    if (lsamen_(&c__2, c2, "GT")) {

/*        Test error exits for the general tridiagonal routines. */

    } else if (lsamen_(&c__2, c2, "PT")) {

/*        Test error exits for the positive definite tridiagonal   
          routines.   

          ZPTTRF */

	s_copy(srnamc_1.srnamt, "ZPTTRF", 6L, 6L);
	infoc_1.infot = 1;
	zpttrf_(&c_n1, d, e, &info);
	chkxer_("ZPTTRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        ZPTTRS */

	s_copy(srnamc_1.srnamt, "ZPTTRS", 6L, 6L);
	infoc_1.infot = 1;
	zpttrs_("/", &c__1, &c__0, d, e, x, &c__1, &info);
	chkxer_("ZPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zpttrs_("U", &c_n1, &c__0, d, e, x, &c__1, &info);
	chkxer_("ZPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zpttrs_("U", &c__0, &c_n1, d, e, x, &c__1, &info);
	chkxer_("ZPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zpttrs_("U", &c__2, &c__1, d, e, x, &c__1, &info);
	chkxer_("ZPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        ZPTRFS */

	s_copy(srnamc_1.srnamt, "ZPTRFS", 6L, 6L);
	infoc_1.infot = 1;
	zptrfs_("/", &c__1, &c__0, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("ZPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zptrfs_("U", &c_n1, &c__0, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("ZPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zptrfs_("U", &c__0, &c_n1, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("ZPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zptrfs_("U", &c__2, &c__1, d, e, df, ef, b, &c__1, x, &c__2, r1, r2, 
		w, rw, &info);
	chkxer_("ZPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zptrfs_("U", &c__2, &c__1, d, e, df, ef, b, &c__2, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("ZPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        ZPTCON */

	s_copy(srnamc_1.srnamt, "ZPTCON", 6L, 6L);
	infoc_1.infot = 1;
	zptcon_(&c_n1, d, e, &anorm, &rcond, rw, &info);
	chkxer_("ZPTCON", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	d__1 = -anorm;
	zptcon_(&c__0, d, e, &d__1, &rcond, rw, &info);
	chkxer_("ZPTCON", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
    }

/*     Print a summary line. */

    alaesm_(path, &infoc_1.ok, &infoc_1.nout);

    return 0;

/*     End of ZERRGT */

} /* zerrgt_ */

