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

/* Subroutine */ int cerrgt_(char *path, integer *nunit)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer info;
    static complex b[2];
    static real d[2];
    static complex e[2], w[2], x[2];
    static real rcond, anorm;
    static char c2[2];
    static real r1[2], r2[2], df[2];
    static complex ef[2];
    extern /* Subroutine */ int alaesm_(char *, logical *, integer *);
    static real rw[2];
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chkxer_(char *, integer *, integer *, logical 
	    *, logical *), cptcon_(integer *, real *, complex *, real 
	    *, real *, real *, integer *), cptrfs_(char *, integer *, integer 
	    *, real *, complex *, real *, complex *, complex *, integer *, 
	    complex *, integer *, real *, real *, complex *, real *, integer *
	    ), cpttrf_(integer *, real *, complex *, integer *), 
	    cpttrs_(char *, integer *, integer *, real *, complex *, complex *
	    , integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CERRGT tests the error exits for the COMPLEX tridiagonal   
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
    d[0] = 1.f;
    d[1] = 2.f;
    e[0].r = 3.f, e[0].i = 0.f;
    e[1].r = 4.f, e[1].i = 0.f;
    anorm = 1.f;
    infoc_1.ok = TRUE_;

    if (lsamen_(&c__2, c2, "GT")) {

/*        Test error exits for the general tridiagonal routines. */

    } else if (lsamen_(&c__2, c2, "PT")) {

/*        Test error exits for the positive definite tridiagonal   
          routines.   

          CPTTRF */

	s_copy(srnamc_1.srnamt, "CPTTRF", 6L, 6L);
	infoc_1.infot = 1;
	cpttrf_(&c_n1, d, e, &info);
	chkxer_("CPTTRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CPTTRS */

	s_copy(srnamc_1.srnamt, "CPTTRS", 6L, 6L);
	infoc_1.infot = 1;
	cpttrs_("/", &c__1, &c__0, d, e, x, &c__1, &info);
	chkxer_("CPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cpttrs_("U", &c_n1, &c__0, d, e, x, &c__1, &info);
	chkxer_("CPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cpttrs_("U", &c__0, &c_n1, d, e, x, &c__1, &info);
	chkxer_("CPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cpttrs_("U", &c__2, &c__1, d, e, x, &c__1, &info);
	chkxer_("CPTTRS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CPTRFS */

	s_copy(srnamc_1.srnamt, "CPTRFS", 6L, 6L);
	infoc_1.infot = 1;
	cptrfs_("/", &c__1, &c__0, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("CPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cptrfs_("U", &c_n1, &c__0, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("CPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cptrfs_("U", &c__0, &c_n1, d, e, df, ef, b, &c__1, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("CPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cptrfs_("U", &c__2, &c__1, d, e, df, ef, b, &c__1, x, &c__2, r1, r2, 
		w, rw, &info);
	chkxer_("CPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cptrfs_("U", &c__2, &c__1, d, e, df, ef, b, &c__2, x, &c__1, r1, r2, 
		w, rw, &info);
	chkxer_("CPTRFS", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CPTCON */

	s_copy(srnamc_1.srnamt, "CPTCON", 6L, 6L);
	infoc_1.infot = 1;
	cptcon_(&c_n1, d, e, &anorm, &rcond, rw, &info);
	chkxer_("CPTCON", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	r__1 = -(doublereal)anorm;
	cptcon_(&c__0, d, e, &r__1, &rcond, rw, &info);
	chkxer_("CPTCON", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
    }

/*     Print a summary line. */

    alaesm_(path, &infoc_1.ok, &infoc_1.nout);

    return 0;

/*     End of CERRGT */

} /* cerrgt_ */

