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
static integer c__18 = 18;

/* Subroutine */ int zerrgg_(char *path, integer *nunit)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,\002 routines passed the tests of the e"
	    "rror exits (\002,i3,\002 tests done)\002)";
    static char fmt_9998[] = "(\002 *** \002,a3,\002 routines failed the tes"
	    "ts of the error \002,\002exits ***\002)";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static doublecomplex beta[3];
    static integer info;
    static doublereal tola, tolb;
    static doublecomplex a[9]	/* was [3][3] */, b[9]	/* was [3][3] */;
    static integer i, j, m;
    static doublecomplex q[9]	/* was [3][3] */, alpha[3], u[9]	/* 
	    was [3][3] */, v[9]	/* was [3][3] */, w[18], z[9]	/* was [3][3] 
	    */;
    static char c2[2];
    static doublereal r1[3], r2[3];
    static integer iw[18], nt;
    static doublereal rw[18];
    static integer ncycle;
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chkxer_(char *, integer *, integer *, logical 
	    *, logical *), zgghrd_(char *, char *, integer *, integer 
	    *, integer *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     integer *), zggglm_(integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex 
	    *, integer *, integer *), zgglse_(integer *, integer *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, integer *), zggqrf_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, integer *)
	    , zggrqf_(integer *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, integer *), ztgevc_(
	    char *, char *, logical *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *, integer *, doublecomplex *,
	     doublereal *, integer *), zggsvd_(char *, char *,
	     char *, integer *, integer *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, doublereal *, integer *, integer *), ztgsja_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *);
    static integer dummyk, dummyl;
    extern /* Subroutine */ int zhgeqz_(char *, char *, char *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublereal *, integer *), zggsvp_(char *,
	     char *, char *, integer *, integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    , integer *, integer *, doublecomplex *, integer *, doublecomplex 
	    *, integer *, doublecomplex *, integer *, integer *, doublereal *,
	     doublecomplex *, doublecomplex *, integer *);
    static logical sel[3];
    static doublecomplex tau[3];

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

    ZERRGG tests the error exits for ZGGGLM, CGGHRD, CGGLSE, CGGQRF,   
    ZGGRQF, CGGSVD, CGGSVP, ZHGEQZ, ZTGEVC, and CTGSJA.   

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
	    a[i__1].r = 0., a[i__1].i = 0.;
	    i__1 = i + j * 3 - 4;
	    b[i__1].r = 0., b[i__1].i = 0.;
/* L10: */
	}
/* L20: */
    }
    for (i = 1; i <= 3; ++i) {
	i__1 = i + i * 3 - 4;
	a[i__1].r = 1., a[i__1].i = 0.;
	i__1 = i + i * 3 - 4;
	b[i__1].r = 1., b[i__1].i = 0.;
/* L30: */
    }
    infoc_1.ok = TRUE_;
    tola = 1.;
    tolb = 1.;
    nt = 0;

/*     Test error exits for the GG path. */

    if (lsamen_(&c__2, c2, "GG")) {

/*        ZGGHRD */

	s_copy(srnamc_1.srnamt, "ZGGHRD", 6L, 6L);
	infoc_1.infot = 1;
	zgghrd_("/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zgghrd_("N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zgghrd_("N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zgghrd_("N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zgghrd_("N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	zgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zgghrd_("V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	zgghrd_("N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("ZGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        ZHGEQZ */

	s_copy(srnamc_1.srnamt, "ZHGEQZ", 6L, 6L);
	infoc_1.infot = 1;
	zhgeqz_("/", "N", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zhgeqz_("E", "/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zhgeqz_("E", "N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zhgeqz_("E", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zhgeqz_("E", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zhgeqz_("E", "N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zhgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zhgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	zhgeqz_("E", "V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	zhgeqz_("E", "N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("ZHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        ZTGEVC */

	s_copy(srnamc_1.srnamt, "ZTGEVC", 6L, 6L);
	infoc_1.infot = 1;
	ztgevc_("/", "A", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	ztgevc_("R", "/", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	ztgevc_("R", "A", sel, &c_n1, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	ztgevc_("R", "A", sel, &c__2, a, &c__1, b, &c__2, q, &c__1, z, &c__2, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	ztgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__1, q, &c__1, z, &c__2, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	ztgevc_("L", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	ztgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	ztgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__2, 
		&c__1, &m, w, rw, &info);
	chkxer_("ZTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GSV path. */

    } else if (lsamen_(&c__3, path, "GSV")) {

/*        ZGGSVD */

	s_copy(srnamc_1.srnamt, "ZGGSVD", 6L, 6L);
	infoc_1.infot = 1;
	zggsvd_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggsvd_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggsvd_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zggsvd_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zggsvd_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zggsvd_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	zggsvd_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__0, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	zggsvd_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__0, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	zggsvd_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__0, w, rw, 
		iw, &info);
	chkxer_("ZGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        ZGGSVP */

	s_copy(srnamc_1.srnamt, "ZGGSVP", 6L, 6L);
	infoc_1.infot = 1;
	zggsvp_("/", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggsvp_("N", "/", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggsvp_("N", "N", "/", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	zggsvp_("N", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zggsvp_("N", "N", "N", &c__0, &c_n1, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	zggsvp_("N", "N", "N", &c__0, &c__0, &c_n1, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__0, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	zggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__0, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	zggsvp_("U", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__0, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	zggsvp_("N", "V", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__0, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	zggsvp_("N", "N", "Q", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__0, iw, 
		rw, tau, w, &info);
	chkxer_("ZGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        ZTGSJA */

	s_copy(srnamc_1.srnamt, "ZTGSJA", 6L, 6L);
	infoc_1.infot = 1;
	ztgsja_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	ztgsja_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	ztgsja_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	ztgsja_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	ztgsja_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	ztgsja_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	ztgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	ztgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	ztgsja_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__0, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	ztgsja_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__0, q, &
		c__1, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 22;
	ztgsja_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__0, w, &ncycle, &info);
	chkxer_("ZTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*     Test error exits for the GLM path. */

    } else if (lsamen_(&c__3, path, "GLM")) {

/*        ZGGGLM */

	s_copy(srnamc_1.srnamt, "ZGGGLM", 6L, 6L);
	infoc_1.infot = 1;
	zggglm_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggglm_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggglm_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggglm_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggglm_(&c__1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zggglm_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zggglm_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zggglm_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__1, &info);
	chkxer_("ZGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the LSE path. */

    } else if (lsamen_(&c__3, path, "LSE")) {

/*        ZGGLSE */

	s_copy(srnamc_1.srnamt, "ZGGLSE", 6L, 6L);
	infoc_1.infot = 1;
	zgglse_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zgglse_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zgglse_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zgglse_(&c__0, &c__0, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zgglse_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zgglse_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	zgglse_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	zgglse_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__1, &info);
	chkxer_("ZGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GQR path. */

    } else if (lsamen_(&c__3, path, "GQR")) {

/*        ZGGQRF */

	s_copy(srnamc_1.srnamt, "ZGGQRF", 6L, 6L);
	infoc_1.infot = 1;
	zggqrf_(&c_n1, &c__0, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggqrf_(&c__0, &c_n1, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggqrf_(&c__0, &c__0, &c_n1, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zggqrf_(&c__0, &c__0, &c__0, a, &c__0, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zggqrf_(&c__0, &c__0, &c__0, a, &c__1, alpha, b, &c__0, beta, w, &
		c__18, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zggqrf_(&c__1, &c__1, &c__2, a, &c__1, alpha, b, &c__1, beta, w, &
		c__1, &info);
	chkxer_("ZGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        ZGGRQF */

	s_copy(srnamc_1.srnamt, "ZGGRQF", 6L, 6L);
	infoc_1.infot = 1;
	zggrqf_(&c_n1, &c__0, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	zggrqf_(&c__0, &c_n1, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	zggrqf_(&c__0, &c__0, &c_n1, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	zggrqf_(&c__0, &c__0, &c__0, a, &c__0, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	zggrqf_(&c__0, &c__0, &c__0, a, &c__1, alpha, b, &c__0, beta, w, &
		c__18, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	zggrqf_(&c__1, &c__1, &c__2, a, &c__1, alpha, b, &c__1, beta, w, &
		c__1, &info);
	chkxer_("ZGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;
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

/*     End of ZERRGG */

} /* zerrgg_ */

