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

/* Subroutine */ int cerrgg_(char *path, integer *nunit)
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
    static complex beta[3];
    static integer info;
    static real tola, tolb;
    static complex a[9]	/* was [3][3] */, b[9]	/* was [3][3] */;
    static integer i, j, m;
    static complex q[9]	/* was [3][3] */, alpha[3], u[9]	/* was [3][3] 
	    */, v[9]	/* was [3][3] */, w[18], z[9]	/* was [3][3] */;
    static char c2[2];
    static real r1[3], r2[3];
    extern /* Subroutine */ int cgghrd_(char *, char *, integer *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, integer *);
    static integer iw[18];
    extern /* Subroutine */ int cggglm_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, complex *, 
	    complex *, complex *, integer *, integer *);
    static integer nt;
    extern /* Subroutine */ int cgglse_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, complex *, 
	    complex *, complex *, integer *, integer *);
    static real rw[18];
    extern /* Subroutine */ int cggqrf_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, complex *, integer *, complex *, 
	    complex *, integer *, integer *), cggrqf_(integer *, integer *, 
	    integer *, complex *, integer *, complex *, complex *, integer *, 
	    complex *, complex *, integer *, integer *);
    static integer ncycle;
    extern /* Subroutine */ int cggsvd_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, complex *, integer *, 
	    complex *, integer *, real *, real *, complex *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, real *, 
	    integer *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int ctgevc_(char *, char *, logical *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, integer *, integer *, complex *, real *, 
	    integer *), chgeqz_(char *, char *, char *, 
	    integer *, integer *, integer *, complex *, integer *, complex *, 
	    integer *, complex *, complex *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, real *, integer *), ctgsja_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, complex *, integer *, 
	    complex *, integer *, real *, real *, real *, real *, complex *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, integer *), chkxer_(char *, 
	    integer *, integer *, logical *, logical *), cggsvp_(char 
	    *, char *, char *, integer *, integer *, integer *, complex *, 
	    integer *, complex *, integer *, real *, real *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, integer *, real *, complex *, complex *, integer *);
    static integer dummyk, dummyl;
    static logical sel[3];
    static complex tau[3];

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

    CERRGG tests the error exits for CGGGLM, CGGHRD, CGGLSE, CGGQRF,   
    CGGRQF, CGGSVD, CGGSVP, CHGEQZ, CTGEVC, and CTGSJA.   

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
	    a[i__1].r = 0.f, a[i__1].i = 0.f;
	    i__1 = i + j * 3 - 4;
	    b[i__1].r = 0.f, b[i__1].i = 0.f;
/* L10: */
	}
/* L20: */
    }
    for (i = 1; i <= 3; ++i) {
	i__1 = i + i * 3 - 4;
	a[i__1].r = 1.f, a[i__1].i = 0.f;
	i__1 = i + i * 3 - 4;
	b[i__1].r = 1.f, b[i__1].i = 0.f;
/* L30: */
    }
    infoc_1.ok = TRUE_;
    tola = 1.f;
    tolb = 1.f;
    nt = 0;

/*     Test error exits for the GG path. */

    if (lsamen_(&c__2, c2, "GG")) {

/*        CGGHRD */

	s_copy(srnamc_1.srnamt, "CGGHRD", 6L, 6L);
	infoc_1.infot = 1;
	cgghrd_("/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cgghrd_("N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cgghrd_("N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cgghrd_("N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cgghrd_("N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cgghrd_("V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	cgghrd_("N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("CGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        CHGEQZ */

	s_copy(srnamc_1.srnamt, "CHGEQZ", 6L, 6L);
	infoc_1.infot = 1;
	chgeqz_("/", "N", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	chgeqz_("E", "/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	chgeqz_("E", "N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	chgeqz_("E", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	chgeqz_("E", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	chgeqz_("E", "N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	chgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	chgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 14;
	chgeqz_("E", "V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	chgeqz_("E", "N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, alpha,
		 beta, q, &c__1, z, &c__1, w, &c__1, rw, &info);
	chkxer_("CHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        CTGEVC */

	s_copy(srnamc_1.srnamt, "CTGEVC", 6L, 6L);
	infoc_1.infot = 1;
	ctgevc_("/", "A", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	ctgevc_("R", "/", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	ctgevc_("R", "A", sel, &c_n1, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	ctgevc_("R", "A", sel, &c__2, a, &c__1, b, &c__2, q, &c__1, z, &c__2, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	ctgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__1, q, &c__1, z, &c__2, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	ctgevc_("L", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	ctgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	ctgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__2, 
		&c__1, &m, w, rw, &info);
	chkxer_("CTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GSV path. */

    } else if (lsamen_(&c__3, path, "GSV")) {

/*        CGGSVD */

	s_copy(srnamc_1.srnamt, "CGGSVD", 6L, 6L);
	infoc_1.infot = 1;
	cggsvd_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggsvd_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggsvd_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cggsvd_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cggsvd_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cggsvd_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	cggsvd_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__0, v, &c__1, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	cggsvd_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__0, q, &c__1, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	cggsvd_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__0, w, rw, 
		iw, &info);
	chkxer_("CGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        CGGSVP */

	s_copy(srnamc_1.srnamt, "CGGSVP", 6L, 6L);
	infoc_1.infot = 1;
	cggsvp_("/", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggsvp_("N", "/", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggsvp_("N", "N", "/", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cggsvp_("N", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cggsvp_("N", "N", "N", &c__0, &c_n1, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cggsvp_("N", "N", "N", &c__0, &c__0, &c_n1, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__0, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__0, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	cggsvp_("U", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__0, v, &c__1, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	cggsvp_("N", "V", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__0, q, &c__1, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	cggsvp_("N", "N", "Q", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__0, iw, 
		rw, tau, w, &info);
	chkxer_("CGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        CTGSJA */

	s_copy(srnamc_1.srnamt, "CTGSJA", 6L, 6L);
	infoc_1.infot = 1;
	ctgsja_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	ctgsja_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	ctgsja_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	ctgsja_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	ctgsja_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	ctgsja_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	ctgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	ctgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	ctgsja_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__0, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	ctgsja_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__0, q, &
		c__1, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 22;
	ctgsja_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__0, w, &ncycle, &info);
	chkxer_("CTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*     Test error exits for the GLM path. */

    } else if (lsamen_(&c__3, path, "GLM")) {

/*        CGGGLM */

	s_copy(srnamc_1.srnamt, "CGGGLM", 6L, 6L);
	infoc_1.infot = 1;
	cggglm_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggglm_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggglm_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggglm_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggglm_(&c__1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cggglm_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cggglm_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cggglm_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__1, &info);
	chkxer_("CGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the LSE path. */

    } else if (lsamen_(&c__3, path, "LSE")) {

/*        CGGLSE */

	s_copy(srnamc_1.srnamt, "CGGLSE", 6L, 6L);
	infoc_1.infot = 1;
	cgglse_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cgglse_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cgglse_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cgglse_(&c__0, &c__0, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cgglse_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cgglse_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	cgglse_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, tau, alpha, beta, w, 
		&c__18, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	cgglse_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, tau, alpha, beta, w, 
		&c__1, &info);
	chkxer_("CGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GQR path. */

    } else if (lsamen_(&c__3, path, "GQR")) {

/*        CGGQRF */

	s_copy(srnamc_1.srnamt, "CGGQRF", 6L, 6L);
	infoc_1.infot = 1;
	cggqrf_(&c_n1, &c__0, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggqrf_(&c__0, &c_n1, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggqrf_(&c__0, &c__0, &c_n1, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cggqrf_(&c__0, &c__0, &c__0, a, &c__0, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cggqrf_(&c__0, &c__0, &c__0, a, &c__1, alpha, b, &c__0, beta, w, &
		c__18, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cggqrf_(&c__1, &c__1, &c__2, a, &c__1, alpha, b, &c__1, beta, w, &
		c__1, &info);
	chkxer_("CGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        CGGRQF */

	s_copy(srnamc_1.srnamt, "CGGRQF", 6L, 6L);
	infoc_1.infot = 1;
	cggrqf_(&c_n1, &c__0, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cggrqf_(&c__0, &c_n1, &c__0, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cggrqf_(&c__0, &c__0, &c_n1, a, &c__1, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cggrqf_(&c__0, &c__0, &c__0, a, &c__0, alpha, b, &c__1, beta, w, &
		c__18, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cggrqf_(&c__0, &c__0, &c__0, a, &c__1, alpha, b, &c__0, beta, w, &
		c__18, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cggrqf_(&c__1, &c__1, &c__2, a, &c__1, alpha, b, &c__1, beta, w, &
		c__1, &info);
	chkxer_("CGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
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

/*     End of CERRGG */

} /* cerrgg_ */

