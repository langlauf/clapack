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

/* Subroutine */ int cerrbd_(char *path, integer *nunit)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,a3,\002 passed the tests of the error ex"
	    "its\002)";
    static char fmt_9998[] = "(\002 *** \002,a3,\002 failed the tests of the"
	    " error exits ***\002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static complex a[16]	/* was [4][4] */;
    static real d[4], e[4];
    static integer i, j;
    static complex u[16]	/* was [4][4] */, v[16]	/* was [4][4] */, w[4]
	    ;
    static char c2[2];
    extern /* Subroutine */ int cgebd2_(integer *, integer *, complex *, 
	    integer *, real *, real *, complex *, complex *, complex *, 
	    integer *), cgebrd_(integer *, integer *, complex *, integer *, 
	    real *, real *, complex *, complex *, complex *, integer *, 
	    integer *);
    static complex tp[4], tq[4];
    static real rw[16];
    extern /* Subroutine */ int cbdsqr_(char *, integer *, integer *, integer 
	    *, integer *, real *, real *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, real *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int cungbr_(char *, integer *, integer *, integer 
	    *, complex *, integer *, complex *, complex *, integer *, integer 
	    *), chkxer_(char *, integer *, integer *, logical *, 
	    logical *), cunmbr_(char *, char *, char *, integer *, 
	    integer *, integer *, complex *, integer *, complex *, complex *, 
	    integer *, complex *, integer *, integer *);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 0, 0, 0, 0 };
    static cilist io___15 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___16 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CERRBD tests the error exits for CGEBRD, CUNGBR, CUNMBR, and CBDSQR. 
  

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

    for (j = 1; j <= 4; ++j) {
	for (i = 1; i <= 4; ++i) {
	    i__1 = i + (j << 2) - 5;
	    d__1 = 1.f / (real) (i + j);
	    a[i__1].r = d__1, a[i__1].i = 0.f;
/* L10: */
	}
/* L20: */
    }
    infoc_1.ok = TRUE_;

/*     Test error exits of the SVD routines. */

    if (lsamen_(&c__2, c2, "BD")) {

/*        CGEBRD */

	s_copy(srnamc_1.srnamt, "CGEBRD", 6L, 6L);
	infoc_1.infot = 1;
	cgebrd_(&c_n1, &c__0, a, &c__1, d, e, tq, tp, w, &c__1, &info);
	chkxer_("CGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cgebrd_(&c__0, &c_n1, a, &c__1, d, e, tq, tp, w, &c__1, &info);
	chkxer_("CGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cgebrd_(&c__2, &c__1, a, &c__1, d, e, tq, tp, w, &c__2, &info);
	chkxer_("CGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	cgebrd_(&c__2, &c__1, a, &c__2, d, e, tq, tp, w, &c__1, &info);
	chkxer_("CGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CGEBD2 */

	s_copy(srnamc_1.srnamt, "CGEBD2", 6L, 6L);
	infoc_1.infot = 1;
	cgebd2_(&c_n1, &c__0, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("CGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cgebd2_(&c__0, &c_n1, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("CGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cgebd2_(&c__2, &c__1, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("CGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CUNGBR */

	s_copy(srnamc_1.srnamt, "CUNGBR", 6L, 6L);
	infoc_1.infot = 1;
	cungbr_("/", &c__0, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cungbr_("Q", &c_n1, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cungbr_("Q", &c__0, &c_n1, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cungbr_("Q", &c__0, &c__1, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cungbr_("Q", &c__1, &c__0, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cungbr_("P", &c__1, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cungbr_("P", &c__0, &c__1, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cungbr_("Q", &c__0, &c__0, &c_n1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cungbr_("Q", &c__2, &c__1, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cungbr_("Q", &c__2, &c__2, &c__1, a, &c__2, tq, w, &c__1, &info);
	chkxer_("CUNGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CUNMBR */

	s_copy(srnamc_1.srnamt, "CUNMBR", 6L, 6L);
	infoc_1.infot = 1;
	cunmbr_("/", "L", "T", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cunmbr_("Q", "/", "T", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cunmbr_("Q", "L", "/", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cunmbr_("Q", "L", "C", &c_n1, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cunmbr_("Q", "L", "C", &c__0, &c_n1, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	cunmbr_("Q", "L", "C", &c__0, &c__0, &c_n1, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cunmbr_("Q", "L", "C", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cunmbr_("Q", "R", "C", &c__0, &c__2, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cunmbr_("P", "L", "C", &c__2, &c__0, &c__2, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	cunmbr_("P", "R", "C", &c__0, &c__2, &c__2, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cunmbr_("Q", "R", "C", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	cunmbr_("Q", "L", "C", &c__0, &c__2, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	cunmbr_("Q", "R", "C", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("CUNMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);

/*        CBDSQR */

	s_copy(srnamc_1.srnamt, "CBDSQR", 6L, 6L);
	infoc_1.infot = 1;
	cbdsqr_("/", &c__0, &c__0, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cbdsqr_("U", &c_n1, &c__0, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	cbdsqr_("U", &c__0, &c_n1, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cbdsqr_("U", &c__0, &c__0, &c_n1, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	cbdsqr_("U", &c__0, &c__0, &c__0, &c_n1, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	cbdsqr_("U", &c__2, &c__1, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	cbdsqr_("U", &c__0, &c__0, &c__2, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	cbdsqr_("U", &c__2, &c__0, &c__0, &c__1, d, e, v, &c__1, u, &c__1, a, 
		&c__1, rw, &info);
	chkxer_("CBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
    }

/*     Print a summary line. */

    if (infoc_1.ok) {
	io___15.ciunit = infoc_1.nout;
	s_wsfe(&io___15);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    } else {
	io___16.ciunit = infoc_1.nout;
	s_wsfe(&io___16);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    }


    return 0;

/*     End of CERRBD */

} /* cerrbd_ */

