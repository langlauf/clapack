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
static doublereal c_b226 = 0.;
static integer c__1000 = 1000;
static integer c__12 = 12;
static integer c__5 = 5;

/* Subroutine */ int derrst_(char *path, integer *nunit)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,\002 routines passed the tests of the e"
	    "rror exits\002,\002 (\002,i3,\002 tests done)\002)";
    static char fmt_9998[] = "(\002 *** \002,a3,\002 routines failed the tes"
	    "ts of the error \002,\002exits ***\002)";

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static doublereal a[9]	/* was [3][3] */, c[9]	/* was [3][3] */, d[3]
	    , e[3];
    static integer i, j, m;
    static doublereal q[9]	/* was [3][3] */, w[12], x[3], z[9]	/* 
	    was [3][3] */;
    extern /* Subroutine */ int dsbev_(char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *), dspev_(char *, char *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *);
    static char c2[2];
    extern /* Subroutine */ int dstev_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *);
    static integer i1[3], i2[3], i3[3];
    extern /* Subroutine */ int dsyev_(char *, char *, integer *, doublereal *
	    , integer *, doublereal *, doublereal *, integer *, integer *);
    static integer kd, il, iu, iw[9], nt;
    static doublereal vl;
    extern /* Subroutine */ int dstedc_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, integer *, integer *), dsbevd_(char *, char *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *);
    static doublereal vu;
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int dsbtrd_(char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *), chkxer_(
	    char *, integer *, integer *, logical *, logical *), 
	    dspevd_(char *, char *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *), dstein_(integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, integer *), dsterf_(integer *, doublereal *, 
	    doublereal *, integer *), dstevd_(char *, integer *, doublereal *,
	     doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, integer *, integer *), dsbevx_(char *, char *, 
	    char *, integer *, integer *, doublereal *, integer *, doublereal 
	    *, integer *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *), dstebz_(char *, char *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, integer *), 
	    dsyevd_(char *, char *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, integer *, 
	    integer *), dopgtr_(char *, integer *, doublereal 
	    *, doublereal *, doublereal *, integer *, doublereal *, integer *), dpteqr_(char *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    dorgtr_(char *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, integer *), dsptrd_(char *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     integer *), dsteqr_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *), dopmtr_(char *, char *, char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *), dormtr_(char *, char *, char 
	    *, integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, integer *);
    static integer nsplit;
    extern /* Subroutine */ int dspevx_(char *, char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *), dsytrd_(char *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *), dstevx_(char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, integer *), dsyevx_(char *, char *, char *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *);
    static doublereal tau[3], tol;

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

    DERRST tests the error exits for DSYTRD, DORGTR, DORMTR, DSPTRD,   
    DOPGTR, DOPMTR, DSTEQR, SSTERF, SSTEBZ, SSTEIN, DPTEQR, DSBTRD,   
    DSYEV, SSYEVX, SSYEVD, DSBEV, SSBEVX, SSBEVD,   
    DSPEV, SSPEVX, SSPEVD, DSTEV, SSTEVX, SSTEVD, and SSTEDC.   

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
	    a[i + j * 3 - 4] = 1. / (doublereal) (i + j);
/* L10: */
	}
/* L20: */
    }
    infoc_1.ok = TRUE_;
    nt = 0;

/*     Test error exits for the ST path. */

    if (lsamen_(&c__2, c2, "ST")) {

/*        DSYTRD */

	s_copy(srnamc_1.srnamt, "DSYTRD", 6L, 6L);
	infoc_1.infot = 1;
	dsytrd_("/", &c__0, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("DSYTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsytrd_("U", &c_n1, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("DSYTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dsytrd_("U", &c__2, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("DSYTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DORGTR */

	s_copy(srnamc_1.srnamt, "DORGTR", 6L, 6L);
	infoc_1.infot = 1;
	dorgtr_("/", &c__0, a, &c__1, tau, w, &c__1, &info);
	chkxer_("DORGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dorgtr_("U", &c_n1, a, &c__1, tau, w, &c__1, &info);
	chkxer_("DORGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dorgtr_("U", &c__2, a, &c__1, tau, w, &c__1, &info);
	chkxer_("DORGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dorgtr_("U", &c__3, a, &c__3, tau, w, &c__1, &info);
	chkxer_("DORGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        DORMTR */

	s_copy(srnamc_1.srnamt, "DORMTR", 6L, 6L);
	infoc_1.infot = 1;
	dormtr_("/", "U", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dormtr_("L", "/", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dormtr_("L", "U", "/", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dormtr_("L", "U", "N", &c_n1, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dormtr_("L", "U", "N", &c__0, &c_n1, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dormtr_("L", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dormtr_("R", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dormtr_("L", "U", "N", &c__2, &c__0, a, &c__2, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dormtr_("L", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dormtr_("R", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("DORMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        DSPTRD */

	s_copy(srnamc_1.srnamt, "DSPTRD", 6L, 6L);
	infoc_1.infot = 1;
	dsptrd_("/", &c__0, a, d, e, tau, &info);
	chkxer_("DSPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsptrd_("U", &c_n1, a, d, e, tau, &info);
	chkxer_("DSPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 2;

/*        DOPGTR */

	s_copy(srnamc_1.srnamt, "DOPGTR", 6L, 6L);
	infoc_1.infot = 1;
	dopgtr_("/", &c__0, a, tau, z, &c__1, w, &info);
	chkxer_("DOPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dopgtr_("U", &c_n1, a, tau, z, &c__1, w, &info);
	chkxer_("DOPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dopgtr_("U", &c__2, a, tau, z, &c__1, w, &info);
	chkxer_("DOPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DOPMTR */

	s_copy(srnamc_1.srnamt, "DOPMTR", 6L, 6L);
	infoc_1.infot = 1;
	dopmtr_("/", "U", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dopmtr_("L", "/", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dopmtr_("L", "U", "/", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dopmtr_("L", "U", "N", &c_n1, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dopmtr_("L", "U", "N", &c__0, &c_n1, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dopmtr_("L", "U", "N", &c__2, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("DOPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        DPTEQR */

	s_copy(srnamc_1.srnamt, "DPTEQR", 6L, 6L);
	infoc_1.infot = 1;
	dpteqr_("/", &c__0, d, e, z, &c__1, w, &info);
	chkxer_("DPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dpteqr_("N", &c_n1, d, e, z, &c__1, w, &info);
	chkxer_("DPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dpteqr_("V", &c__2, d, e, z, &c__1, w, &info);
	chkxer_("DPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DSTEBZ */

	s_copy(srnamc_1.srnamt, "DSTEBZ", 6L, 6L);
	infoc_1.infot = 1;
	dstebz_("/", "E", &c__0, &vl, &vu, &il, &iu, &tol, d, e, &m, &nsplit, 
		x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dstebz_("A", "/", &c__0, &vl, &vu, &il, &iu, &tol, d, e, &m, &nsplit, 
		x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dstebz_("A", "E", &c_n1, &vl, &vu, &il, &iu, &tol, d, e, &m, &nsplit, 
		x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dstebz_("V", "E", &c__0, &c_b226, &c_b226, &il, &iu, &tol, d, e, &m, &
		nsplit, x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dstebz_("I", "E", &c__0, &vl, &vu, &c__0, &c__0, &tol, d, e, &m, &
		nsplit, x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dstebz_("I", "E", &c__1, &vl, &vu, &c__1, &c__0, &tol, d, e, &m, &
		nsplit, x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dstebz_("I", "E", &c__0, &vl, &vu, &c__1, &c__1, &tol, d, e, &m, &
		nsplit, x, i1, i2, w, iw, &info);
	chkxer_("DSTEBZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 7;

/*        DSTEIN */

	s_copy(srnamc_1.srnamt, "DSTEIN", 6L, 6L);
	infoc_1.infot = 1;
	dstein_(&c_n1, d, e, &c__0, x, i1, i2, z, &c__1, w, iw, i3, &info);
	chkxer_("DSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dstein_(&c__0, d, e, &c_n1, x, i1, i2, z, &c__1, w, iw, i3, &info);
	chkxer_("DSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dstein_(&c__0, d, e, &c__1, x, i1, i2, z, &c__1, w, iw, i3, &info);
	chkxer_("DSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dstein_(&c__2, d, e, &c__0, x, i1, i2, z, &c__1, w, iw, i3, &info);
	chkxer_("DSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        DSTEQR */

	s_copy(srnamc_1.srnamt, "DSTEQR", 6L, 6L);
	infoc_1.infot = 1;
	dsteqr_("/", &c__0, d, e, z, &c__1, w, &info);
	chkxer_("DSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsteqr_("N", &c_n1, d, e, z, &c__1, w, &info);
	chkxer_("DSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dsteqr_("V", &c__2, d, e, z, &c__1, w, &info);
	chkxer_("DSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DSTERF */

	s_copy(srnamc_1.srnamt, "DSTERF", 6L, 6L);
	infoc_1.infot = 1;
	dsterf_(&c_n1, d, e, &info);
	chkxer_("DSTERF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	++nt;

/*        DSTEDC */

	s_copy(srnamc_1.srnamt, "DSTEDC", 6L, 6L);
	infoc_1.infot = 1;
	dstedc_("/", &c__0, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dstedc_("N", &c_n1, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dstedc_("V", &c__2, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstedc_("V", &c__2, d, e, z, &c__2, w, &c_n1, iw, &c__1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstedc_("I", &c__2, d, e, z, &c__2, w, &c__1, iw, &c__1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstedc_("V", &c__2, d, e, z, &c__2, w, &c__1, iw, &c__1, &info);
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dstedc_("V", &c__2, d, e, z, &c__2, w, &c__1000, iw, &c_n1, &info)
		;
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dstedc_("I", &c__2, d, e, z, &c__2, w, &c__1000, iw, &c__1, &info)
		;
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dstedc_("V", &c__2, d, e, z, &c__2, w, &c__1000, iw, &c__1, &info)
		;
	chkxer_("DSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        DSTEVD */

	s_copy(srnamc_1.srnamt, "DSTEVD", 6L, 6L);
	infoc_1.infot = 1;
	dstevd_("/", &c__0, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dstevd_("N", &c_n1, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dstevd_("V", &c__2, d, e, z, &c__1, w, &c_n1, iw, &c_n1, &info);
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstevd_("V", &c__2, d, e, z, &c__2, w, &c_n1, iw, &c__1, &info);
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstevd_("V", &c__2, d, e, z, &c__2, w, &c__1, iw, &c__1, &info);
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dstevd_("V", &c__2, d, e, z, &c__2, w, &c__1000, iw, &c_n1, &info)
		;
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dstevd_("V", &c__2, d, e, z, &c__2, w, &c__1000, iw, &c__1, &info)
		;
	chkxer_("DSTEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 7;

/*        DSTEV */

	s_copy(srnamc_1.srnamt, "DSTEV", 6L, 5L);
	infoc_1.infot = 1;
	dstev_("/", &c__0, d, e, z, &c__1, w, &info);
	chkxer_("DSTEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dstev_("N", &c_n1, d, e, z, &c__1, w, &info);
	chkxer_("DSTEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dstev_("V", &c__2, d, e, z, &c__1, w, &info);
	chkxer_("DSTEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DSTEVX */

	s_copy(srnamc_1.srnamt, "DSTEVX", 6L, 6L);
	infoc_1.infot = 1;
	dstevx_("/", "A", &c__0, d, e, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dstevx_("V", "/", &c__0, d, e, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dstevx_("V", "A", &c_n1, d, e, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dstevx_("V", "V", &c__1, d, e, &c_b226, &c_b226, &il, &iu, &tol, &m, 
		w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dstevx_("V", "I", &c__1, d, e, &c_b226, &c_b226, &c__0, &iu, &tol, &m,
		 w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dstevx_("V", "I", &c__2, d, e, &c_b226, &c_b226, &c__2, &c__1, &tol, &
		m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	dstevx_("V", "I", &c__1, d, e, &c_b226, &c_b226, &c__2, &c__1, &tol, &
		m, w, z, &c__0, x, iw, i1, &info);
	chkxer_("DSTEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 7;

/*        DSYEVD */

	s_copy(srnamc_1.srnamt, "DSYEVD", 6L, 6L);
	infoc_1.infot = 1;
	dsyevd_("/", "U", &c__0, a, &c__1, w, x, &c__12, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsyevd_("N", "/", &c__0, a, &c__1, w, x, &c__12, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsyevd_("N", "U", &c_n1, a, &c__1, w, x, &c__12, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dsyevd_("N", "U", &c__2, a, &c__1, w, x, &c__12, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dsyevd_("N", "U", &c__1, a, &c__2, w, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dsyevd_("N", "U", &c__2, a, &c__2, w, x, &c__1, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dsyevd_("V", "U", &c__2, a, &c__2, w, x, &c__1, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dsyevd_("N", "U", &c__1, a, &c__2, w, x, &c__1000, iw, &c_n1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dsyevd_("N", "U", &c__2, a, &c__2, w, x, &c__1000, iw, &c__0, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dsyevd_("V", "U", &c__2, a, &c__2, w, x, &c__1000, iw, &c__1, &info);
	chkxer_("DSYEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        DSYEV */

	s_copy(srnamc_1.srnamt, "DSYEV", 6L, 5L);
	infoc_1.infot = 1;
	dsyev_("/", "U", &c__0, a, &c__1, w, x, &c__12, &info);
	chkxer_("DSYEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsyev_("N", "/", &c__0, a, &c__1, w, x, &c__12, &info);
	chkxer_("DSYEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsyev_("N", "U", &c_n1, a, &c__1, w, x, &c__12, &info);
	chkxer_("DSYEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dsyev_("N", "U", &c__2, a, &c__1, w, x, &c__12, &info);
	chkxer_("DSYEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dsyev_("N", "U", &c__2, a, &c__2, w, x, &c__0, &info);
	chkxer_("DSYEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 5;

/*        DSYEVX */

	s_copy(srnamc_1.srnamt, "DSYEVX", 6L, 6L);
	infoc_1.infot = 1;
	dsyevx_("/", "A", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsyevx_("V", "/", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsyevx_("V", "A", "/", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		w, z, &c__1, x, &c__12, iw, i1, &info);
	infoc_1.infot = 4;
	dsyevx_("V", "A", "U", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dsyevx_("V", "A", "U", &c__2, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dsyevx_("V", "V", "U", &c__1, a, &c__1, &c_b226, &c_b226, &il, &iu, &
		tol, &m, w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dsyevx_("V", "I", "U", &c__1, a, &c__1, &c_b226, &c_b226, &c__0, &iu, 
		&tol, &m, w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dsyevx_("V", "I", "U", &c__2, a, &c__2, &c_b226, &c_b226, &c__2, &
		c__1, &tol, &m, w, z, &c__1, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	dsyevx_("V", "I", "U", &c__1, a, &c__1, &c_b226, &c_b226, &c__2, &
		c__1, &tol, &m, w, z, &c__0, x, &c__12, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 17;
	dsyevx_("V", "I", "U", &c__1, a, &c__1, &c_b226, &c_b226, &c__2, &
		c__1, &tol, &m, w, z, &c__1, x, &c__0, iw, i1, &info);
	chkxer_("DSYEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        DSPEVD */

	s_copy(srnamc_1.srnamt, "DSPEVD", 6L, 6L);
	infoc_1.infot = 1;
	dspevd_("/", "U", &c__0, a, w, z, &c__1, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dspevd_("N", "/", &c__0, a, w, z, &c__1, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dspevd_("N", "U", &c_n1, a, w, z, &c__1, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dspevd_("V", "U", &c__2, a, w, z, &c__1, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dspevd_("N", "U", &c__2, a, w, z, &c__2, x, &c_n1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dspevd_("N", "U", &c__2, a, w, z, &c__2, x, &c__1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dspevd_("V", "U", &c__2, a, w, z, &c__2, x, &c__1, iw, &c_n1, &info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dspevd_("N", "U", &c__1, a, w, z, &c__2, x, &c__1000, iw, &c_n1, &
		info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dspevd_("N", "U", &c__2, a, w, z, &c__2, x, &c__1000, iw, &c__0, &
		info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dspevd_("V", "U", &c__2, a, w, z, &c__2, x, &c__1000, iw, &c__1, &
		info);
	chkxer_("DSPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        DSPEV */

	s_copy(srnamc_1.srnamt, "DSPEV", 6L, 5L);
	infoc_1.infot = 1;
	dspev_("/", "U", &c__0, a, w, z, &c__1, x, &info);
	chkxer_("DSPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dspev_("N", "/", &c__0, a, w, z, &c__1, x, &info);
	chkxer_("DSPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dspev_("N", "U", &c_n1, a, w, z, &c__1, x, &info);
	chkxer_("DSPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dspev_("V", "U", &c__2, a, w, z, &c__1, x, &info);
	chkxer_("DSPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        DSPEVX */

	s_copy(srnamc_1.srnamt, "DSPEVX", 6L, 6L);
	infoc_1.infot = 1;
	dspevx_("/", "A", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dspevx_("V", "/", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dspevx_("V", "A", "/", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	infoc_1.infot = 4;
	dspevx_("V", "A", "U", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, w, z, &
		c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dspevx_("V", "V", "U", &c__1, a, &c_b226, &c_b226, &il, &iu, &tol, &m,
		 w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dspevx_("V", "I", "U", &c__1, a, &c_b226, &c_b226, &c__0, &iu, &tol, &
		m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dspevx_("V", "I", "U", &c__2, a, &c_b226, &c_b226, &c__2, &c__1, &tol,
		 &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	dspevx_("V", "I", "U", &c__1, a, &c_b226, &c_b226, &c__2, &c__1, &tol,
		 &m, w, z, &c__0, x, iw, i1, &info);
	chkxer_("DSPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the SB path. */

    } else if (lsamen_(&c__2, c2, "SB")) {

/*        DSBTRD */

	s_copy(srnamc_1.srnamt, "DSBTRD", 6L, 6L);
	infoc_1.infot = 1;
	dsbtrd_("/", "U", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsbtrd_("N", "/", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsbtrd_("N", "U", &c_n1, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dsbtrd_("N", "U", &c__0, &c_n1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dsbtrd_("N", "U", &c__1, &c__1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dsbtrd_("V", "U", &c__2, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("DSBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        DSBEVD */

	s_copy(srnamc_1.srnamt, "DSBEVD", 6L, 6L);
	infoc_1.infot = 1;
	dsbevd_("/", "U", &c__0, &kd, a, &c__1, w, z, &c__1, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsbevd_("N", "/", &c__0, &kd, a, &c__1, w, z, &c__1, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsbevd_("N", "U", &c_n1, &kd, a, &c__1, w, z, &c__1, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dsbevd_("N", "U", &c__1, &c_n1, a, &c__1, w, z, &c__1, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dsbevd_("N", "U", &c__2, &c__1, a, &c__1, w, z, &c__1, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dsbevd_("N", "U", &c__2, &c__1, a, &c__5, w, z, &c__0, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dsbevd_("N", "U", &c__1, &c__1, a, &c__5, w, z, &c__2, x, &c_n1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dsbevd_("N", "U", &c__2, &c__1, a, &c__5, w, z, &c__2, x, &c__0, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dsbevd_("V", "U", &c__2, &c__1, a, &c__5, w, z, &c__2, x, &c__1, iw, &
		c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dsbevd_("N", "U", &c__1, &c__1, a, &c__5, w, z, &c__2, x, &c__1000, 
		iw, &c_n1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dsbevd_("N", "U", &c__2, &c__1, a, &c__5, w, z, &c__2, x, &c__1000, 
		iw, &c__0, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dsbevd_("V", "U", &c__2, &c__1, a, &c__5, w, z, &c__2, x, &c__1000, 
		iw, &c__1, &info);
	chkxer_("DSBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 12;

/*        DSBEV */

	s_copy(srnamc_1.srnamt, "DSBEV", 6L, 5L);
	infoc_1.infot = 1;
	dsbev_("/", "U", &c__0, &kd, a, &c__1, w, z, &c__1, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsbev_("N", "/", &c__0, &kd, a, &c__1, w, z, &c__1, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsbev_("N", "U", &c_n1, &kd, a, &c__1, w, z, &c__1, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dsbev_("N", "U", &c__2, &c_n1, a, &c__1, w, z, &c__1, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dsbev_("N", "U", &c__2, &c__1, a, &c__1, w, z, &c__1, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dsbev_("N", "U", &c__2, &kd, a, &c__5, w, z, &c__0, x, &info);
	chkxer_("DSBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        DSBEVX */

	s_copy(srnamc_1.srnamt, "DSBEVX", 6L, 6L);
	infoc_1.infot = 1;
	dsbevx_("/", "A", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dsbevx_("V", "/", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dsbevx_("V", "A", "/", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	infoc_1.infot = 4;
	dsbevx_("V", "A", "U", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dsbevx_("V", "A", "U", &c__2, &c_n1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dsbevx_("V", "A", "U", &c__2, &c__1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dsbevx_("V", "A", "U", &c__2, &c__1, a, &c__5, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dsbevx_("V", "V", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b226, &
		c_b226, &il, &iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dsbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b226, &
		c_b226, &c__0, &iu, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dsbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b226, &
		c_b226, &c__1, &c__2, &tol, &m, w, z, &c__1, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	dsbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b226, &
		c_b226, &c__1, &c__1, &tol, &m, w, z, &c__0, x, iw, i1, &info);
	chkxer_("DSBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
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

/*     End of DERRST */

} /* derrst_ */

