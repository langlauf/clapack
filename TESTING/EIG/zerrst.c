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
static doublereal c_b461 = 0.;
static integer c__5 = 5;

/* Subroutine */ int zerrst_(char *path, integer *nunit)
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
    static doublecomplex a[9]	/* was [3][3] */, c[9]	/* was [3][3] */;
    static doublereal d[3], e[3];
    static integer i, j, m;
    static doublecomplex q[9]	/* was [3][3] */, w[15];
    static doublereal x[3];
    static doublecomplex z[9]	/* was [3][3] */;
    extern /* Subroutine */ int zhbev_(char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublecomplex *, 
	    integer *, doublecomplex *, doublereal *, integer *), zheev_(char *, char *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublecomplex *, integer *, doublereal *,
	     integer *);
    static char c2[2];
    static integer i1[3], i2[3], i3[3];
    extern /* Subroutine */ int zhpev_(char *, char *, integer *, 
	    doublecomplex *, doublereal *, doublecomplex *, integer *, 
	    doublecomplex *, doublereal *, integer *);
    static integer kd, il, iu, iw[15], nt;
    static doublereal vl, rw[15], vu;
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int zhbevd_(char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, integer *, 
	    integer *, integer *, integer *), chkxer_(char *, 
	    integer *, integer *, logical *, logical *), zheevd_(char 
	    *, char *, integer *, doublecomplex *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *), zstedc_(char *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *), zhbtrd_(char *, char *, integer *, 
	    integer *, doublecomplex *, integer *, doublereal *, doublereal *,
	     doublecomplex *, integer *, doublecomplex *, integer *), zhetrd_(char *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, doublecomplex *, 
	    integer *, integer *), zhpevd_(char *, char *, integer *, 
	    doublecomplex *, doublereal *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *), zhbevx_(char *, char *, 
	    char *, integer *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, integer *,
	     integer *, doublereal *, integer *, doublereal *, doublecomplex *
	    , integer *, doublecomplex *, doublereal *, integer *, integer *, 
	    integer *), zheevx_(char *, char *, char *
	    , integer *, doublecomplex *, integer *, doublereal *, doublereal 
	    *, integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *), zhptrd_(char *, integer *, doublecomplex *, doublereal *,
	     doublereal *, doublecomplex *, integer *), zstein_(
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, doublecomplex *, integer *, doublereal *, 
	    integer *, integer *, integer *), zhpevx_(char *, char *, char *, 
	    integer *, doublecomplex *, doublereal *, doublereal *, integer *,
	     integer *, doublereal *, integer *, doublereal *, doublecomplex *
	    , integer *, doublecomplex *, doublereal *, integer *, integer *, 
	    integer *), zpteqr_(char *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublereal *, integer *), zsteqr_(char *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublereal *, integer *), zungtr_(char *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *), zupgtr_(char *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zunmtr_(char *, char *, char 
	    *, integer *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, integer *), zupmtr_(char *, 
	    char *, char *, integer *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *);
    static doublecomplex tau[3];
    static doublereal tol;

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

    ZERRST tests the error exits for ZHETRD, ZUNGTR, CUNMTR, ZHPTRD,   
    ZUNGTR, ZUPMTR, ZSTEQR, CSTEIN, ZPTEQR, ZHBTRD,   
    ZHEEV, CHEEVX, CHEEVD, ZHBEV, CHBEVX, CHBEVD,   
    ZHPEV, CHPEVX, CHPEVD, and ZSTEDC.   

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
	    d__1 = 1. / (doublereal) (i + j);
	    a[i__1].r = d__1, a[i__1].i = 0.;
/* L10: */
	}
/* L20: */
    }
    infoc_1.ok = TRUE_;
    nt = 0;

/*     Test error exits for the ST path. */

    if (lsamen_(&c__2, c2, "ST")) {

/*        ZHETRD */

	s_copy(srnamc_1.srnamt, "ZHETRD", 6L, 6L);
	infoc_1.infot = 1;
	zhetrd_("/", &c__0, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("ZHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhetrd_("U", &c_n1, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("ZHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhetrd_("U", &c__2, a, &c__1, d, e, tau, w, &c__1, &info);
	chkxer_("ZHETRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        ZUNGTR */

	s_copy(srnamc_1.srnamt, "ZUNGTR", 6L, 6L);
	infoc_1.infot = 1;
	zungtr_("/", &c__0, a, &c__1, tau, w, &c__1, &info);
	chkxer_("ZUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zungtr_("U", &c_n1, a, &c__1, tau, w, &c__1, &info);
	chkxer_("ZUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zungtr_("U", &c__2, a, &c__1, tau, w, &c__1, &info);
	chkxer_("ZUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zungtr_("U", &c__3, a, &c__3, tau, w, &c__1, &info);
	chkxer_("ZUNGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        ZUNMTR */

	s_copy(srnamc_1.srnamt, "ZUNMTR", 6L, 6L);
	infoc_1.infot = 1;
	zunmtr_("/", "U", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zunmtr_("L", "/", "N", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zunmtr_("L", "U", "/", &c__0, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zunmtr_("L", "U", "N", &c_n1, &c__0, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zunmtr_("L", "U", "N", &c__0, &c_n1, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zunmtr_("L", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zunmtr_("R", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zunmtr_("L", "U", "N", &c__2, &c__0, a, &c__2, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zunmtr_("L", "U", "N", &c__0, &c__2, a, &c__1, tau, c, &c__1, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zunmtr_("R", "U", "N", &c__2, &c__0, a, &c__1, tau, c, &c__2, w, &
		c__1, &info);
	chkxer_("ZUNMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        ZHPTRD */

	s_copy(srnamc_1.srnamt, "ZHPTRD", 6L, 6L);
	infoc_1.infot = 1;
	zhptrd_("/", &c__0, a, d, e, tau, &info);
	chkxer_("ZHPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhptrd_("U", &c_n1, a, d, e, tau, &info);
	chkxer_("ZHPTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 2;

/*        ZUPGTR */

	s_copy(srnamc_1.srnamt, "ZUPGTR", 6L, 6L);
	infoc_1.infot = 1;
	zupgtr_("/", &c__0, a, tau, z, &c__1, w, &info);
	chkxer_("ZUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zupgtr_("U", &c_n1, a, tau, z, &c__1, w, &info);
	chkxer_("ZUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zupgtr_("U", &c__2, a, tau, z, &c__1, w, &info);
	chkxer_("ZUPGTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        ZUPMTR */

	s_copy(srnamc_1.srnamt, "ZUPMTR", 6L, 6L);
	infoc_1.infot = 1;
	zupmtr_("/", "U", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zupmtr_("L", "/", "N", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zupmtr_("L", "U", "/", &c__0, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zupmtr_("L", "U", "N", &c_n1, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zupmtr_("L", "U", "N", &c__0, &c_n1, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zupmtr_("L", "U", "N", &c__2, &c__0, a, tau, c, &c__1, w, &info);
	chkxer_("ZUPMTR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        ZPTEQR */

	s_copy(srnamc_1.srnamt, "ZPTEQR", 6L, 6L);
	infoc_1.infot = 1;
	zpteqr_("/", &c__0, d, e, z, &c__1, rw, &info);
	chkxer_("ZPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zpteqr_("N", &c_n1, d, e, z, &c__1, rw, &info);
	chkxer_("ZPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zpteqr_("V", &c__2, d, e, z, &c__1, rw, &info);
	chkxer_("ZPTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        ZSTEIN */

	s_copy(srnamc_1.srnamt, "ZSTEIN", 6L, 6L);
	infoc_1.infot = 1;
	zstein_(&c_n1, d, e, &c__0, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("ZSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zstein_(&c__0, d, e, &c_n1, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("ZSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zstein_(&c__0, d, e, &c__1, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("ZSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zstein_(&c__2, d, e, &c__0, x, i1, i2, z, &c__1, rw, iw, i3, &info);
	chkxer_("ZSTEIN", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        ZSTEQR */

	s_copy(srnamc_1.srnamt, "ZSTEQR", 6L, 6L);
	infoc_1.infot = 1;
	zsteqr_("/", &c__0, d, e, z, &c__1, rw, &info);
	chkxer_("ZSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zsteqr_("N", &c_n1, d, e, z, &c__1, rw, &info);
	chkxer_("ZSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zsteqr_("V", &c__2, d, e, z, &c__1, rw, &info);
	chkxer_("ZSTEQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        ZSTEDC */

	s_copy(srnamc_1.srnamt, "ZSTEDC", 6L, 6L);
	infoc_1.infot = 1;
	zstedc_("/", &c__0, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zstedc_("N", &c_n1, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zstedc_("V", &c__2, d, e, z, &c__1, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zstedc_("N", &c__2, d, e, z, &c__2, w, &c__0, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zstedc_("V", &c__2, d, e, z, &c__2, w, &c__1, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zstedc_("N", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__0, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zstedc_("V", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__1, iw, &c__0, &
		info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zstedc_("N", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__99, iw, &c__0, 
		&info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zstedc_("V", &c__2, d, e, z, &c__2, w, &c__99, rw, &c__99, iw, &c__1, 
		&info);
	chkxer_("ZSTEDC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        ZHEEVD */

	s_copy(srnamc_1.srnamt, "ZHEEVD", 6L, 6L);
	infoc_1.infot = 1;
	zheevd_("/", "U", &c__0, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zheevd_("N", "/", &c__0, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zheevd_("N", "U", &c_n1, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zheevd_("N", "U", &c__2, a, &c__1, x, w, &c__15, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zheevd_("N", "U", &c__2, a, &c__2, x, w, &c__0, rw, &c__0, iw, &c__0, 
		&info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zheevd_("V", "U", &c__2, a, &c__2, x, w, &c__1, rw, &c__0, iw, &c__0, 
		&info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zheevd_("N", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__0, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zheevd_("V", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__1, iw, &c__0,
		 &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zheevd_("N", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__99, iw, &
		c__0, &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zheevd_("V", "U", &c__2, a, &c__2, x, w, &c__99, rw, &c__99, iw, &
		c__1, &info);
	chkxer_("ZHEEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        ZHEEV */

	s_copy(srnamc_1.srnamt, "ZHEEV", 6L, 5L);
	infoc_1.infot = 1;
	zheev_("/", "U", &c__0, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("ZHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zheev_("N", "/", &c__0, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("ZHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zheev_("N", "U", &c_n1, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("ZHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zheev_("N", "U", &c__2, a, &c__1, x, w, &c__15, rw, &info);
	chkxer_("ZHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zheev_("N", "U", &c__2, a, &c__2, x, w, &c__0, rw, &info);
	chkxer_("ZHEEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 5;

/*        ZHEEVX */

	s_copy(srnamc_1.srnamt, "ZHEEVX", 6L, 6L);
	infoc_1.infot = 1;
	zheevx_("/", "A", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zheevx_("V", "/", "U", &c__0, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zheevx_("V", "A", "/", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	infoc_1.infot = 4;
	zheevx_("V", "A", "U", &c_n1, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zheevx_("V", "A", "U", &c__2, a, &c__1, &vl, &vu, &il, &iu, &tol, &m, 
		x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zheevx_("V", "V", "U", &c__1, a, &c__1, &c_b461, &c_b461, &il, &iu, &
		tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__0, &iu, 
		&tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zheevx_("V", "I", "U", &c__2, a, &c__2, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__1, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	zheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__0, w, &c__15, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 17;
	zheevx_("V", "I", "U", &c__1, a, &c__1, &c_b461, &c_b461, &c__2, &
		c__1, &tol, &m, x, z, &c__1, w, &c__0, rw, iw, i1, &info);
	chkxer_("ZHEEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        ZHPEVD */

	s_copy(srnamc_1.srnamt, "ZHPEVD", 6L, 6L);
	infoc_1.infot = 1;
	zhpevd_("/", "U", &c__0, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhpevd_("N", "/", &c__0, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhpevd_("N", "U", &c_n1, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zhpevd_("V", "U", &c__2, a, rw, z, &c__1, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__0, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__1, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zhpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__0, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zhpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__1, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zhpevd_("N", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__99, iw, &
		c__0, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zhpevd_("V", "U", &c__2, a, rw, z, &c__2, q, &c__99, x, &c__99, iw, &
		c__1, &info);
	chkxer_("ZHPEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        ZHPEV */

	s_copy(srnamc_1.srnamt, "ZHPEV", 6L, 5L);
	infoc_1.infot = 1;
	zhpev_("/", "U", &c__0, a, x, z, &c__1, w, rw, &info);
	chkxer_("ZHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhpev_("N", "/", &c__0, a, x, z, &c__1, w, rw, &info);
	chkxer_("ZHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhpev_("N", "U", &c_n1, a, x, z, &c__1, w, rw, &info);
	chkxer_("ZHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zhpev_("V", "U", &c__2, a, x, z, &c__1, w, rw, &info);
	chkxer_("ZHPEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        ZHPEVX */

	s_copy(srnamc_1.srnamt, "ZHPEVX", 6L, 6L);
	infoc_1.infot = 1;
	zhpevx_("/", "A", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhpevx_("V", "/", "U", &c__0, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhpevx_("V", "A", "/", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhpevx_("V", "A", "U", &c_n1, a, &vl, &vu, &il, &iu, &tol, &m, x, z, &
		c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zhpevx_("V", "V", "U", &c__1, a, &c_b461, &c_b461, &il, &iu, &tol, &m,
		 x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zhpevx_("V", "I", "U", &c__1, a, &c_b461, &c_b461, &c__0, &iu, &tol, &
		m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhpevx_("V", "I", "U", &c__2, a, &c_b461, &c_b461, &c__2, &c__1, &tol,
		 &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	zhpevx_("V", "I", "U", &c__1, a, &c_b461, &c_b461, &c__1, &c__1, &tol,
		 &m, x, z, &c__0, w, rw, iw, i1, &info);
	chkxer_("ZHPEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the HB path. */

    } else if (lsamen_(&c__2, c2, "HB")) {

/*        ZHBTRD */

	s_copy(srnamc_1.srnamt, "ZHBTRD", 6L, 6L);
	infoc_1.infot = 1;
	zhbtrd_("/", "U", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhbtrd_("N", "/", &c__0, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhbtrd_("N", "U", &c_n1, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhbtrd_("N", "U", &c__0, &c_n1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zhbtrd_("N", "U", &c__1, &c__1, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zhbtrd_("V", "U", &c__2, &c__0, a, &c__1, d, e, z, &c__1, w, &info);
	chkxer_("ZHBTRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        ZHBEVD */

	s_copy(srnamc_1.srnamt, "ZHBEVD", 6L, 6L);
	infoc_1.infot = 1;
	zhbevd_("/", "U", &c__0, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhbevd_("N", "/", &c__0, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhbevd_("N", "U", &c_n1, &kd, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhbevd_("N", "U", &c__1, &c_n1, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zhbevd_("N", "U", &c__2, &c__1, a, &c__1, x, z, &c__1, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__0, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zhbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__0, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zhbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__1, rw, &
		c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zhbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__0, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zhbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__1, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	zhbevd_("N", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__99, iw, &c__0, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	zhbevd_("V", "U", &c__2, &c__1, a, &c__5, x, z, &c__2, w, &c__99, rw, 
		&c__99, iw, &c__1, &info);
	chkxer_("ZHBEVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 12;

/*        ZHBEV */

	s_copy(srnamc_1.srnamt, "ZHBEV", 6L, 5L);
	infoc_1.infot = 1;
	zhbev_("/", "U", &c__0, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhbev_("N", "/", &c__0, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhbev_("N", "U", &c_n1, &kd, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhbev_("N", "U", &c__2, &c_n1, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zhbev_("N", "U", &c__2, &c__1, a, &c__1, x, z, &c__1, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhbev_("N", "U", &c__2, &kd, a, &c__5, x, z, &c__0, w, rw, &info);
	chkxer_("ZHBEV", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        ZHBEVX */

	s_copy(srnamc_1.srnamt, "ZHBEVX", 6L, 6L);
	infoc_1.infot = 1;
	zhbevx_("/", "A", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhbevx_("V", "/", "U", &c__0, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhbevx_("V", "A", "/", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	infoc_1.infot = 4;
	zhbevx_("V", "A", "U", &c_n1, &kd, a, &c__1, q, &c__1, &vl, &vu, &il, 
		&iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zhbevx_("V", "A", "U", &c__2, &c_n1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zhbevx_("V", "A", "U", &c__2, &c__1, a, &c__1, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zhbevx_("V", "A", "U", &c__2, &c__1, a, &c__5, q, &c__1, &vl, &vu, &
		il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zhbevx_("V", "V", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &il, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zhbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__0, &iu, &tol, &m, x, z, &c__1, w, rw, iw, i1, &
		info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zhbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__1, &c__2, &tol, &m, x, z, &c__1, w, rw, iw, i1, &
		info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	zhbevx_("V", "I", "U", &c__1, &c__1, a, &c__5, q, &c__1, &c_b461, &
		c_b461, &c__1, &c__1, &tol, &m, x, z, &c__0, w, rw, iw, i1, &
		info);
	chkxer_("ZHBEVX", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
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

/*     End of ZERRST */

} /* zerrst_ */

