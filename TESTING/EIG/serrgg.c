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

/* Subroutine */ int serrgg_(char *path, integer *nunit)
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
    static real tola, tolb, a[9]	/* was [3][3] */, b[9]	/* was [3][3] 
	    */;
    static integer i, j, m;
    static real q[9]	/* was [3][3] */, u[9]	/* was [3][3] */, v[9]	/* 
	    was [3][3] */, w[18], z[9]	/* was [3][3] */;
    static char c2[2];
    static real r1[3], r2[3], r3[3];
    static integer iw[3], nt, ncycle;
    extern /* Subroutine */ int sgghrd_(char *, char *, integer *, integer *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , real *, integer *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int sggglm_(integer *, integer *, integer *, real 
	    *, integer *, real *, integer *, real *, real *, real *, real *, 
	    integer *, integer *), chkxer_(char *, integer *, integer *, 
	    logical *, logical *), sgglse_(integer *, integer *, 
	    integer *, real *, integer *, real *, integer *, real *, real *, 
	    real *, real *, integer *, integer *), sggqrf_(integer *, integer 
	    *, integer *, real *, integer *, real *, real *, integer *, real *
	    , real *, integer *, integer *), sggrqf_(integer *, integer *, 
	    integer *, real *, integer *, real *, real *, integer *, real *, 
	    real *, integer *, integer *), stgevc_(char *, char *, logical *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , real *, integer *, integer *, integer *, real *, integer *), sggsvd_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, real *, integer *, 
	    real *, integer *, real *, real *, real *, integer *, real *, 
	    integer *, real *, integer *, real *, integer *, integer *), stgsja_(char *, char *, char *, integer *
	    , integer *, integer *, integer *, integer *, real *, integer *, 
	    real *, integer *, real *, real *, real *, real *, real *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , integer *), shgeqz_(char *, char *, 
	    char *, integer *, integer *, integer *, real *, integer *, real *
	    , integer *, real *, real *, real *, real *, integer *, real *, 
	    integer *, real *, integer *, integer *);
    static integer dummyk, dummyl;
    extern /* Subroutine */ int sggsvp_(char *, char *, char *, integer *, 
	    integer *, integer *, real *, integer *, real *, integer *, real *
	    , real *, integer *, integer *, real *, integer *, real *, 
	    integer *, real *, integer *, integer *, real *, real *, integer *
	    );
    static logical sel[3];
    static real tau[3];

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

    SERRGG tests the error exits for SGGGLM, SGGHRD, SGGLSE, SGGQRF,   
    SGGRQF, SGGSVD, SGGSVP, SHGEQZ, STGEVC, and STGSJA.   

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
	    a[i + j * 3 - 4] = 0.f;
	    b[i + j * 3 - 4] = 0.f;
/* L10: */
	}
/* L20: */
    }
    for (i = 1; i <= 3; ++i) {
	a[i + i * 3 - 4] = 1.f;
	b[i + i * 3 - 4] = 1.f;
/* L30: */
    }
    infoc_1.ok = TRUE_;
    tola = 1.f;
    tolb = 1.f;
    nt = 0;

/*     Test error exits for the GG path. */

    if (lsamen_(&c__2, c2, "GG")) {

/*        SGGHRD */

	s_copy(srnamc_1.srnamt, "SGGHRD", 6L, 6L);
	infoc_1.infot = 1;
	sgghrd_("/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sgghrd_("N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sgghrd_("N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	sgghrd_("N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sgghrd_("N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	sgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	sgghrd_("N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	sgghrd_("V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	sgghrd_("N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, q, &c__1, 
		z, &c__1, &info);
	chkxer_("SGGHRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 9;

/*        SHGEQZ */

	s_copy(srnamc_1.srnamt, "SHGEQZ", 6L, 6L);
	infoc_1.infot = 1;
	shgeqz_("/", "N", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	shgeqz_("E", "/", "N", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	shgeqz_("E", "N", "/", &c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	shgeqz_("E", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	shgeqz_("E", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	shgeqz_("E", "N", "N", &c__0, &c__1, &c__1, a, &c__1, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	shgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__1, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	shgeqz_("E", "N", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__1, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 15;
	shgeqz_("E", "V", "N", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 17;
	shgeqz_("E", "N", "V", &c__2, &c__1, &c__1, a, &c__2, b, &c__2, r1, 
		r2, r3, q, &c__1, z, &c__1, w, &c__18, &info);
	chkxer_("SHGEQZ", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        STGEVC */

	s_copy(srnamc_1.srnamt, "STGEVC", 6L, 6L);
	infoc_1.infot = 1;
	stgevc_("/", "A", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	stgevc_("R", "/", sel, &c__0, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	stgevc_("R", "A", sel, &c_n1, a, &c__1, b, &c__1, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	stgevc_("R", "A", sel, &c__2, a, &c__1, b, &c__2, q, &c__1, z, &c__2, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	stgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__1, q, &c__1, z, &c__2, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	stgevc_("L", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	stgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__1, 
		&c__0, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	stgevc_("R", "A", sel, &c__2, a, &c__2, b, &c__2, q, &c__1, z, &c__2, 
		&c__1, &m, w, &info);
	chkxer_("STGEVC", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GSV path. */

    } else if (lsamen_(&c__3, path, "GSV")) {

/*        SGGSVD */

	s_copy(srnamc_1.srnamt, "SGGSVD", 6L, 6L);
	infoc_1.infot = 1;
	sggsvd_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggsvd_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggsvd_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	sggsvd_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sggsvd_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	sggsvd_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	sggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	sggsvd_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, r1, r2, u, &c__1, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	sggsvd_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__0, v, &c__1, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	sggsvd_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__0, q, &c__1, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	sggsvd_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, r1, r2, u, &c__1, v, &c__1, q, &c__0, w, iw, &
		info);
	chkxer_("SGGSVD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        SGGSVP */

	s_copy(srnamc_1.srnamt, "SGGSVP", 6L, 6L);
	infoc_1.infot = 1;
	sggsvp_("/", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggsvp_("N", "/", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggsvp_("N", "N", "/", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	sggsvp_("N", "N", "N", &c_n1, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sggsvp_("N", "N", "N", &c__0, &c_n1, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	sggsvp_("N", "N", "N", &c__0, &c__0, &c_n1, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	sggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__0, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	sggsvp_("N", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__0, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 16;
	sggsvp_("U", "N", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__0, v, &c__1, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	sggsvp_("N", "V", "N", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__0, q, &c__1, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	sggsvp_("N", "N", "Q", &c__0, &c__0, &c__0, a, &c__1, b, &c__1, &tola,
		 &tolb, &dummyk, &dummyl, u, &c__1, v, &c__1, q, &c__0, iw, 
		tau, w, &info);
	chkxer_("SGGSVP", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*        STGSJA */

	s_copy(srnamc_1.srnamt, "STGSJA", 6L, 6L);
	infoc_1.infot = 1;
	stgsja_("/", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	stgsja_("N", "/", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	stgsja_("N", "N", "/", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	stgsja_("N", "N", "N", &c_n1, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	stgsja_("N", "N", "N", &c__0, &c_n1, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	stgsja_("N", "N", "N", &c__0, &c__0, &c_n1, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	stgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__0, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	stgsja_("N", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__0, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 18;
	stgsja_("U", "N", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__0, v, &c__1, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 20;
	stgsja_("N", "V", "N", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__0, q, &
		c__1, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 22;
	stgsja_("N", "N", "Q", &c__0, &c__0, &c__0, &dummyk, &dummyl, a, &
		c__1, b, &c__1, &tola, &tolb, r1, r2, u, &c__1, v, &c__1, q, &
		c__0, w, &ncycle, &info);
	chkxer_("STGSJA", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 11;

/*     Test error exits for the GLM path. */

    } else if (lsamen_(&c__3, path, "GLM")) {

/*        SGGGLM */

	s_copy(srnamc_1.srnamt, "SGGGLM", 6L, 6L);
	infoc_1.infot = 1;
	sggglm_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggglm_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggglm_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggglm_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggglm_(&c__1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sggglm_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	sggglm_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	sggglm_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &c__1,
		 &info);
	chkxer_("SGGGLM", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the LSE path. */

    } else if (lsamen_(&c__3, path, "LSE")) {

/*        SGGLSE */

	s_copy(srnamc_1.srnamt, "SGGLSE", 6L, 6L);
	infoc_1.infot = 1;
	sgglse_(&c_n1, &c__0, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sgglse_(&c__0, &c_n1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sgglse_(&c__0, &c__0, &c_n1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sgglse_(&c__0, &c__0, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sgglse_(&c__0, &c__1, &c__0, a, &c__1, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sgglse_(&c__0, &c__0, &c__0, a, &c__0, b, &c__1, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 7;
	sgglse_(&c__0, &c__0, &c__0, a, &c__1, b, &c__0, r1, r2, r3, w, &
		c__18, &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 12;
	sgglse_(&c__1, &c__1, &c__1, a, &c__1, b, &c__1, r1, r2, r3, w, &c__1,
		 &info);
	chkxer_("SGGLSE", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;

/*     Test error exits for the GQR path. */

    } else if (lsamen_(&c__3, path, "GQR")) {

/*        SGGQRF */

	s_copy(srnamc_1.srnamt, "SGGQRF", 6L, 6L);
	infoc_1.infot = 1;
	sggqrf_(&c_n1, &c__0, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggqrf_(&c__0, &c_n1, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggqrf_(&c__0, &c__0, &c_n1, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sggqrf_(&c__0, &c__0, &c__0, a, &c__0, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	sggqrf_(&c__0, &c__0, &c__0, a, &c__1, r1, b, &c__0, r2, w, &c__18, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	sggqrf_(&c__1, &c__1, &c__2, a, &c__1, r1, b, &c__1, r2, w, &c__1, &
		info);
	chkxer_("SGGQRF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 6;

/*        SGGRQF */

	s_copy(srnamc_1.srnamt, "SGGRQF", 6L, 6L);
	infoc_1.infot = 1;
	sggrqf_(&c_n1, &c__0, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	sggrqf_(&c__0, &c_n1, &c__0, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	sggrqf_(&c__0, &c__0, &c_n1, a, &c__1, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	sggrqf_(&c__0, &c__0, &c__0, a, &c__0, r1, b, &c__1, r2, w, &c__18, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	sggrqf_(&c__0, &c__0, &c__0, a, &c__1, r1, b, &c__0, r2, w, &c__18, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	sggrqf_(&c__1, &c__1, &c__2, a, &c__1, r1, b, &c__1, r2, w, &c__1, &
		info);
	chkxer_("SGGRQF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
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

/*     End of SERRGG */

} /* serrgg_ */

