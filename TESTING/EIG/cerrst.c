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
static integer c__0 = 0;
static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__99 = 99;
static integer c__15 = 15;
static real c_b461 = 0.f;
static integer c__5 = 5;

/* Subroutine */ int cerrst_(char *path, integer *nunit)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,\002 routines passed the tests of the e"
	    "rror exits\002,\002 (\002,i3,\002 tests done)\002)";
    static char fmt_9998[] = "(\002 *** \002,a3,\002 routines failed the tes"
	    "ts of the error \002,\002exits ***\002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static complex a[9]	/* was [3][3] */, c[9]	/* was [3][3] */;
    static real d[3], e[3];
    static integer i, j, m;
    static complex q[9]	/* was [3][3] */, w[15];
    static real x[3];
    extern /* Subroutine */ int chbev_(char *, char *, integer *, integer *, 
	    complex *, integer *, real *, complex *, integer *, complex *, 
	    real *, integer *);
    static complex z[9]	/* was [3][3] */;
    extern /* Subroutine */ int cheev_(char *, char *, integer *, complex *, 
	    integer *, real *, complex *, integer *, real *, integer *), chpev_(char *, char *, integer *, complex *, 
	    real *, complex *, integer *, complex *, real *, integer *);
    static char c2[2];
    static integer i1[3], i2[3], i3[3], kd, il;
    extern /* Subroutine */ int chbevd_(char *, char *, integer *, integer *, 
	    complex *, integer *, real *, complex *, integer *, complex *, 
	    integer *, real *, integer *, integer *, integer *, integer *);
    static integer iu;
    extern /* Subroutine */ int cheevd_(char *, char *, integer *, complex *, 
	    integer *, real *, complex *, integer *, real *, integer *, 
	    integer *, integer *, integer *);
    static integer iw[15], nt;
    static real vl;
    extern /* Subroutine */ int cstedc_(char *, integer *, real *, real *, 
	    complex *, integer *, complex *, integer *, real *, integer *, 
	    integer *, integer *, integer *), chbtrd_(char *, char *, 
	    integer *, integer *, complex *, integer *, real *, real *, 
	    complex *, integer *, complex *, integer *);
    static real rw[15];
    extern /* Subroutine */ int chetrd_(char *, integer *, complex *, integer 
	    *, real *, real *, complex *, complex *, integer *, integer *);
    static real vu;
    extern /* Subroutine */ int chpevd_(char *, char *, integer *, complex *, 
	    real *, complex *, integer *, complex *, integer *, real *, 
	    integer *, integer *, integer *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chbevx_(char *, char *, char *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, real *, 
	    real *, integer *, integer *, real *, integer *, real *, complex *
	    , integer *, complex *, real *, integer *, integer *, integer *), cheevx_(char *, char *, char *, integer *
	    , complex *, integer *, real *, real *, integer *, integer *, 
	    real *, integer *, real *, complex *, integer *, complex *, 
	    integer *, real *, integer *, integer *, integer *), chkxer_(char *, integer *, integer *, logical *, 
	    logical *), chptrd_(char *, integer *, complex *, real *, 
	    real *, complex *, integer *), cstein_(integer *, real *, 
	    real *, integer *, real *, integer *, integer *, complex *, 
	    integer *, real *, integer *, integer *, integer *), chpevx_(char 
	    *, char *, char *, integer *, complex *, real *, real *, integer *
	    , integer *, real *, integer *, real *, complex *, integer *, 
	    complex *, real *, integer *, integer *, integer *), cpteqr_(char *, integer *, real *, real *, 
	    complex *, integer *, real *, integer *), csteqr_(char *, 
	    integer *, real *, real *, complex *, integer *, real *, integer *
	    ), cungtr_(char *, integer *, complex *, integer *, 
	    complex *, complex *, integer *, integer *), cupgtr_(char 
	    *, integer *, complex *, complex *, complex *, integer *, complex 
	    *, integer *), cunmtr_(char *, char *, char *, integer *, 
	    integer *, complex *, integer *, complex *, complex *, integer *, 
	    complex *, integer *, integer *), cupmtr_(
	    char *, char *, char *, integer *, integer *, complex *, complex *
	    , complex *, integer *, complex *, integer *);
    static complex tau[3];
    static real tol;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, 0, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CERRST tests the error exits for CHETRD, CUNGTR, CUNMTR, CHPTRD,   
    CUNGTR, CUPMTR, CSTEQR, CSTEIN, CPTEQR, CHBTRD,   
    CHEEV, CHEEVX, CHEEVD, CHBEV, CHBEVX, CHBEVD,   
    CHPEV, CHPEVX, CHPEVD, and CSTEDC.   

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

/*     Set the variables to innocuous values. */

    for (j = 1; j <= 3; ++j) {
	for (i = 1; i <= 3; ++i) {
	    i__1 = i + j * 3 - 4;
	    d__1 = 1.f / (real) (i + j);
	    a[i__1].r = d__1, a[i__1].i = 0.f;
/* L10: */
	}
/* L20: */
    }
    infoc_1.ok = TRUE_;
    nt = 0;

/*     Test error exits for the ST path. */

    if (lsamen_(&c__2, c2, "ST")) {

/*        CHETRD */

	s_copy(srnamc_1.srnamt, "CHETRD", 6L, 6L);
	infoc_1.infot = 1;
	chetrd_("/", &c__0, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("CHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chetrd_("U", &c_n1, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("CHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chetrd_("U", &c__2, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("CHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        CUNGTR */

	s_copy(srnamc_1.srnamt, "CUNGTR", 6L, 6L);
	infoc_1.infot = 1;
	cungtr_("/", &c__0, a, &c__1, tau, w, &c__1, &info);
	chkxer_("CUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cungtr_("U", &c_n1, a, &c__1, tau, w, &c__1, &info);
	chkxer_("CUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cungtr_("U", &c__2, a, &c__1, tau, w, &c__1, &info);
	chkxer_("CUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cungtr_("U", &c__3, a, &c__3, tau, w, &c__1, &info);
	chkxer_("CUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        CUNMTR */

	s_copy(srnamc_1.srnamt, "CUNMTR", 6L, 6L);
	infoc_1.infot = 1;
	cunmtr_("/", "U", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cunmtr_("L", "/", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cunmtr_("L", "U", "/", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cunmtr_("L", "U", "N", &c_n1, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cunmtr_("L", "U", "N", &c__0, &c_n1, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cunmtr_("L", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cunmtr_("R", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cunmtr_("L", "U", "N", &c__2, &c__0, a, &c__2, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cunmtr_("L", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cunmtr_("R", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("CUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        CHPTRD */

	s_copy(srnamc_1.srnamt, "CHPTRD", 6L, 6L);
	infoc_1.infot = 1;
	chptrd_("/", &c__0, a, d, e, tau, &info);
	chkxer_("CHPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chptrd_("U", &c_n1, a, d, e, tau, &info);
	chkxer_("CHPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 2;

/*        CUPGTR */

	s_copy(srnamc_1.srnamt, "CUPGTR", 6L, 6L);
	infoc_1.infot = 1;
	cupgtr_("/", &c__0, a, tau, z, &c__1, w, &info);
	chkxer_("CUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cupgtr_("U", &c_n1, a, tau, z, &c__1, w, &info);
	chkxer_("CUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cupgtr_("U", &c__2, a, tau, z, &c__1, w, &info);
	chkxer_("CUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        CUPMTR */

	s_copy(srnamc_1.srnamt, "CUPMTR", 6L, 6L);
	infoc_1.infot = 1;
	cupmtr_("/", "U", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cupmtr_("L", "/", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cupmtr_("L", "U", "/", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cupmtr_("L", "U", "N", &c_n1, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cupmtr_("L", "U", "N", &c__0, &c_n1, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cupmtr_("L", "U", "N", &c__2, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("CUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        CPTEQR */

	s_copy(srnamc_1.srnamt, "CPTEQR", 6L, 6L);
	infoc_1.infot = 1;
	cpteqr_("/", &c__0, d, e, z, &c__1, rw, &info);
	chkxer_("CPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cpteqr_("N", &c_n1, d, e, z, &c__1, rw, &info);
	chkxer_("CPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cpteqr_("V", &c__2, d, e, z, &c__1, rw, &info);
	chkxer_("CPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        CSTEIN */

	s_copy(srnamc_1.srnamt, "CSTEIN", 6L, 6L);
	infoc_1.infot = 1;
	cstein_(&c_n1, d, e, &c__0, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("CSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cstein_(&c__0, d, e, &c_n1, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("CSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cstein_(&c__0, d, e, &c__1, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("CSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cstein_(&c__2, d, e, &c__0, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("CSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        CSTEQR */

	s_copy(srnamc_1.srnamt, "CSTEQR", 6L, 6L);
	infoc_1.infot = 1;
	csteqr_("/", &c__0, d, e, z, &c__1, rw, &info);
	chkxer_("CSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	csteqr_("N", &c_n1, d, e, z, &c__1, rw, &info);
	chkxer_("CSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	csteqr_("V", &c__2, d, e, z, &c__1, rw, &info);
	chkxer_("CSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        CSTEDC */

	s_copy(srnamc_1.srnamt, "CSTEDC", 6L, 6L);
	infoc_1.infot = 1;
	cstedc_("/", &c__0, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cstedc_("N", &c_n1, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cstedc_("V", &c__2, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cstedc_("N", &c__2, d, e, z, &c__2, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cstedc_("V", &c__2, d, e, z, &c__2, w, &c__1, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cstedc_("N", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cstedc_("V", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__1, iw, &c__0, &
		info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cstedc_("N", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__99, iw, &c__0, 
		&info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cstedc_("V", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__99, iw, &c__1, 
		&info);
	chkxer_("CSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        CHEEVD */

	s_copy(srnamc_1.srnamt, "CHEEVD", 6L, 6L);
	infoc_1.infot = 1;
	cheevd_("/", "U", &c__0, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cheevd_("N", "/", &c__0, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cheevd_("N", "U", &c_n1, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cheevd_("N", "U", &c__2, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cheevd_("N", "U", &c__2, a, &c__2, x, w, &c__0, rw, &c__0, iw, &c__0, 
		&info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cheevd_("V", "U", &c__2, a, &c__2, x, w, &c__1, rw, &c__0, iw, &c__0, 
		&info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cheevd_("N", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cheevd_("V", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__1, iw, &c__0,
		 &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cheevd_("N", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__99, iw, &
		c__0, &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cheevd_("V", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__99, iw, &
		c__1, &info);
	chkxer_("CHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        CHEEV */

	s_copy(srnamc_1.srnamt, "CHEEV", 6L, 5L);
	infoc_1.infot = 1;
	cheev_("/", "U", &c__0, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("CHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cheev_("N", "/", &c__0, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("CHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cheev_("N", "U", &c_n1, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("CHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cheev_("N", "U", &c__2, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("CHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cheev_("N", "U", &c__2, a, &c__2, x, w, &c__0, rw, &info);
	chkxer_("CHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 5;

/*        CHEEVX */

	s_copy(srnamc_1.srnamt, "CHEEVX", 6L, 6L);
	infoc_1.infot = 1;
	cheevx_("/", "A", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cheevx_("V", "/", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cheevx_("V", "A", "/", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	infoc_1.infot = 4;
	cheevx_("V", "A", "U", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cheevx_("V", "A", "U", &c__2, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cheevx_("V", "V", "U", &c__1, a, &c__1, &c_b461, &c_b461, &il, &iu, &
		tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__0, &iu, 
		&tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cheevx_("V", "I", "U", &c__2, a, &c__2, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	cheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__0, w, &c__15, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 17;
	cheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__1, w, &c__0, rw, iw, i1, &info);
	chkxer_("CHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        CHPEVD */

	s_copy(srnamc_1.srnamt, "CHPEVD", 6L, 6L);
	infoc_1.infot = 1;
	chpevd_("/", "U", &c__0, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chpevd_("N", "/", &c__0, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chpevd_("N", "U", &c_n1, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	chpevd_("V", "U", &c__2, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__1, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	chpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__0, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	chpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__1, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	chpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__99, iw, &
		c__0, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	chpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__99, iw, &
		c__1, &info);
	chkxer_("CHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        CHPEV */

	s_copy(srnamc_1.srnamt, "CHPEV", 6L, 5L);
	infoc_1.infot = 1;
	chpev_("/", "U", &c__0, a, x, z, &c__1, w, rw, &info);
	chkxer_("CHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chpev_("N", "/", &c__0, a, x, z, &c__1, w, rw, &info);
	chkxer_("CHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chpev_("N", "U", &c_n1, a, x, z, &c__1, w, rw, &info);
	chkxer_("CHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	chpev_("V", "U", &c__2, a, x, z, &c__1, w, rw, &info);
	chkxer_("CHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        CHPEVX */

	s_copy(srnamc_1.srnamt, "CHPEVX", 6L, 6L);
	infoc_1.infot = 1;
	chpevx_("/", "A", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chpevx_("V", "/", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chpevx_("V", "A", "/", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chpevx_("V", "A", "U", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	chpevx_("V", "V", "U", &c__1, a, &c_b461, &c_b461, &il, &iu, &tol, &m,
		 x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	chpevx_("V", "I", "U", &c__1, a, &c_b461, &c_b461, &c__0, &iu, &tol, &
		m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chpevx_("V", "I", "U", &c__2, a, &c_b461, &c_b461, &c__2, &c__1, &tol,
		 &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	chpevx_("V", "I", "U", &c__1, a, &c_b461, &c_b461, &c__1, &c__1, &tol,
		 &m, x, z, &c__0, w, rw, iw, i1, &info);
	chkxer_("CHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the HB path. */

    } else if (lsamen_(&c__2, c2, "HB")) {

/*        CHBTRD */

	s_copy(srnamc_1.srnamt, "CHBTRD", 6L, 6L);
	infoc_1.infot = 1;
	chbtrd_("/", "U", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chbtrd_("N", "/", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chbtrd_("N", "U", &c_n1, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chbtrd_("N", "U", &c__0, &c_n1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	chbtrd_("N", "U", &c__1, &c__1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	chbtrd_("V", "U", &c__2, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("CHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        CHBEVD */

	s_copy(srnamc_1.srnamt, "CHBEVD", 6L, 6L);
	infoc_1.infot = 1;
	chbevd_("/", "U", &c__0, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chbevd_("N", "/", &c__0, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chbevd_("N", "U", &c_n1, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chbevd_("N", "U", &c__1, &c_n1, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	chbevd_("N", "U", &c__2, &c__1, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__0, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	chbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	chbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__1, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	chbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__0, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	chbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__1, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	chbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__99, iw, &c__0, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	chbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__99, iw, &c__1, &info);
	chkxer_("CHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 12;

/*        CHBEV */

	s_copy(srnamc_1.srnamt, "CHBEV", 6L, 5L);
	infoc_1.infot = 1;
	chbev_("/", "U", &c__0, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chbev_("N", "/", &c__0, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chbev_("N", "U", &c_n1, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chbev_("N", "U", &c__2, &c_n1, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	chbev_("N", "U", &c__2, &c__1, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chbev_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__0, w, rw, &info);
	chkxer_("CHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        CHBEVX */

	s_copy(srnamc_1.srnamt, "CHBEVX", 6L, 6L);
	infoc_1.infot = 1;
	chbevx_("/", "A", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chbevx_("V", "/", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chbevx_("V", "A", "/", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	infoc_1.infot = 4;
	chbevx_("V", "A", "U", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	chbevx_("V", "A", "U", &c__2, &c_n1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	chbevx_("V", "A", "U", &c__2, &c__1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	chbevx_("V", "A", "U", &c__2, &c__1, a, &c__5, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	chbevx_("V", "V", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	chbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__0, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &
		info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	chbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__1, &c__2, &tol, &m, x, z, &c__1, w, rw, iw, i1, &
		info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	chbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__1, &c__1, &tol, &m, x, z, &c__0, w, rw, iw, i1, &
		info);
	chkxer_("CHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;
    }

/*     Print a summary line. */

    if (infoc_1.ok) {
	io___28.ciunit = infoc_1.nout;
	s_wsfe(&io___28);
	do_fio(&c__1, path, 3L);
	do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	io___29.ciunit = infoc_1.nout;
	s_wsfe(&io___29);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    }


    return 0;

/*     End of CERRST */

} /* cerrst_ */

