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
static integer c__18 = 18;
static integer c__3 = 3;

/* Subroutine */ int derrgg_(char *path, integer *nunit)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,\002 routines passed the tests of the e"
	    "rror exits (\002,i3,\002 tests done)\002)";
    static char fmt_9998[] = "(\002 *** \002,a3,\002 routines failed the tes"
	    "ts of the error \002,\002exits ***\002)";

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static doublereal tola, tolb, a[9]	/* was [3][3] */, b[9]	/* was [3][3] 
	    */;
    static integer i, j, m;
    static doublereal q[9]	/* was [3][3] */, u[9]	/* was [3][3] */, v[9]
	    	/* was [3][3] */, w[18], z[9]	/* was [3][3] */;
    static char c2[2];
    static doublereal r1[3], r2[3], r3[3];
    extern /* Subroutine */ int dgghrd_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *);
    static integer iw[3];
    extern /* Subroutine */ int dggglm_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *);
    static integer nt;
    extern /* Subroutine */ int dgglse_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *), 
	    dggqrf_(integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *, integer *), dggrqf_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *);
    static integer ncycle;
    extern /* Subroutine */ int dggsvd_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int dtgevc_(char *, char *, logical *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, integer *), dtgsja_(char *, char *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, integer *), dhgeqz_(char *, 
	    char *, char *, integer *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, integer *), 
	    chkxer_(char *, integer *, integer *, logical *, logical *), dggsvp_(char *, char *, char *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, doublereal *, integer *);
    static integer dummyk, dummyl;
    static logical sel[3];
    static doublereal tau[3];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, 0, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DERRGG tests the error exits for DGGGLM, SGGHRD, SGGLSE, SGGQRF,   
    DGGRQF, SGGSVD, SGGSVP, DHGEQZ, DTGEVC, and STGSJA.   

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
	    a[i + j * 3 - 4] = 0.;
	    b[i + j * 3 - 4] = 0.;
/* L10: */
	}
/* L20: */
    }
    for (i = 1; i <= 3; ++i) {
	a[i + i * 3 - 4] = 1.;
	b[i + i * 3 - 4] = 1.;
/* L30: */
    }
    infoc_1.ok = TRUE_;
    tola = 1.;
    tolb = 1.;
    nt = 0;

/*     Test error exits for the GG path. */

    if (lsamen_(&c__2, c2, "GG")) {

/*        DGGHRD */

	s_copy(srnamc_1.srnamt, "DGGHRD", 6L, 6L);
	infoc_1.infot = 1;
	dgghrd_("/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dgghrd_("N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dgghrd_("N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dgghrd_("N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dgghrd_("N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dgghrd_("V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dgghrd_("N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("DGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        DHGEQZ */

	s_copy(srnamc_1.srnamt, "DHGEQZ", 6L, 6L);
	infoc_1.infot = 1;
	dhgeqz_("/", "N", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dhgeqz_("E", "/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dhgeqz_("E", "N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dhgeqz_("E", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dhgeqz_("E", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dhgeqz_("E", "N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dhgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dhgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	dhgeqz_("E", "V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 17;
	dhgeqz_("E", "N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("DHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        DTGEVC */

	s_copy(srnamc_1.srnamt, "DTGEVC", 6L, 6L);
	infoc_1.infot = 1;
	dtgevc_("/", "A", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dtgevc_("R", "/", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dtgevc_("R", "A", sel, &c_n1, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dtgevc_("R", "A", sel, &c__2, a, &c__1, b, &c__2, q, &c__1, z, &c__2, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dtgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__1, q, &c__1, z, &c__2, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dtgevc_("L", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dtgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dtgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__2, 
		&c__1, &m, w, &info);
	chkxer_("DTGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GSV path. */

    } else if (lsamen_(&c__3, path, "GSV")) {

/*        DGGSVD */

	s_copy(srnamc_1.srnamt, "DGGSVD", 6L, 6L);
	infoc_1.infot = 1;
	dggsvd_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggsvd_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggsvd_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dggsvd_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dggsvd_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dggsvd_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	dggsvd_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__0, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	dggsvd_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__0, q, &c__1, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	dggsvd_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__0, w, iw, &
		info);
	chkxer_("DGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        DGGSVP */

	s_copy(srnamc_1.srnamt, "DGGSVP", 6L, 6L);
	infoc_1.infot = 1;
	dggsvp_("/", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggsvp_("N", "/", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggsvp_("N", "N", "/", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dggsvp_("N", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dggsvp_("N", "N", "N", &c__0, &c_n1, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dggsvp_("N", "N", "N", &c__0, &c__0, &c_n1, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__0, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__0, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	dggsvp_("U", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__0, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	dggsvp_("N", "V", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__0, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	dggsvp_("N", "N", "Q", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__0, iw, 
		tau, w, &info);
	chkxer_("DGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        DTGSJA */

	s_copy(srnamc_1.srnamt, "DTGSJA", 6L, 6L);
	infoc_1.infot = 1;
	dtgsja_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dtgsja_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dtgsja_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dtgsja_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dtgsja_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dtgsja_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dtgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dtgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	dtgsja_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__0, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	dtgsja_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__0, q, &
		c__1, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 22;
	dtgsja_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__0, w, &ncycle, &info);
	chkxer_("DTGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*     Test error exits for the GLM path. */

    } else if (lsamen_(&c__3, path, "GLM")) {

/*        DGGGLM */

	s_copy(srnamc_1.srnamt, "DGGGLM", 6L, 6L);
	infoc_1.infot = 1;
	dggglm_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggglm_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggglm_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggglm_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggglm_(&c__1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dggglm_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dggglm_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dggglm_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &c__1,
		 &info);
	chkxer_("DGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the LSE path. */

    } else if (lsamen_(&c__3, path, "LSE")) {

/*        DGGLSE */

	s_copy(srnamc_1.srnamt, "DGGLSE", 6L, 6L);
	infoc_1.infot = 1;
	dgglse_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dgglse_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dgglse_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dgglse_(&c__0, &c__0, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dgglse_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dgglse_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	dgglse_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	dgglse_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &c__1,
		 &info);
	chkxer_("DGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GQR path. */

    } else if (lsamen_(&c__3, path, "GQR")) {

/*        DGGQRF */

	s_copy(srnamc_1.srnamt, "DGGQRF", 6L, 6L);
	infoc_1.infot = 1;
	dggqrf_(&c_n1, &c__0, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggqrf_(&c__0, &c_n1, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggqrf_(&c__0, &c__0, &c_n1, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dggqrf_(&c__0, &c__0, &c__0, a, &c__0, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dggqrf_(&c__0, &c__0, &c__0, a, &c__1, r1, b, &c__0, r2, w, &c__18, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dggqrf_(&c__1, &c__1, &c__2, a, &c__1, r1, b, &c__1, r2, w, &c__1, &
		info);
	chkxer_("DGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        DGGRQF */

	s_copy(srnamc_1.srnamt, "DGGRQF", 6L, 6L);
	infoc_1.infot = 1;
	dggrqf_(&c_n1, &c__0, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dggrqf_(&c__0, &c_n1, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dggrqf_(&c__0, &c__0, &c_n1, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dggrqf_(&c__0, &c__0, &c__0, a, &c__0, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dggrqf_(&c__0, &c__0, &c__0, a, &c__1, r1, b, &c__0, r2, w, &c__18, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dggrqf_(&c__1, &c__1, &c__2, a, &c__1, r1, b, &c__1, r2, w, &c__1, &
		info);
	chkxer_("DGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;
    }

/*     Print a summary line. */

    if (infoc_1.ok) {
	io___26.ciunit = infoc_1.nout;
	s_wsfe(&io___26);
	do_fio(&c__1, path, 3L);
	do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	io___27.ciunit = infoc_1.nout;
	s_wsfe(&io___27);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    }


    return 0;

/*     End of DERRGG */

} /* derrgg_ */

