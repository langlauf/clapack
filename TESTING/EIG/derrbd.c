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

/* Subroutine */ int derrbd_(char *path, integer *nunit)
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
    static doublereal a[16]	/* was [4][4] */, d[4], e[4];
    static integer i, j;
    static doublereal u[16]	/* was [4][4] */, v[16]	/* was [4][4] */, w[4]
	    ;
    static char c2[2];
    extern /* Subroutine */ int dgebd2_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *), dgebrd_(integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, integer *, integer *);
    static integer nt;
    static doublereal tp[4], tq[4];
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int dbdsqr_(char *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *), dorgbr_(char *, integer *, integer *, integer 
	    *, doublereal *, integer *, doublereal *, doublereal *, integer *,
	     integer *), chkxer_(char *, integer *, integer *, 
	    logical *, logical *), dormbr_(char *, char *, char *, 
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *);

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

    DERRBD tests the error exits for DGEBRD, DORGBR, DORMBR, and DBDSQR. 
  

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
	    a[i + (j << 2) - 5] = 1. / (doublereal) (i + j);
/* L10: */
	}
/* L20: */
    }
    infoc_1.ok = TRUE_;
    nt = 0;

/*     Test error exits of the SVD routines. */

    if (lsamen_(&c__2, c2, "BD")) {

/*        DGEBRD */

	s_copy(srnamc_1.srnamt, "DGEBRD", 6L, 6L);
	infoc_1.infot = 1;
	dgebrd_(&c_n1, &c__0, a, &c__1, d, e, tq, tp, w, &c__1, &info);
	chkxer_("DGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dgebrd_(&c__0, &c_n1, a, &c__1, d, e, tq, tp, w, &c__1, &info);
	chkxer_("DGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dgebrd_(&c__2, &c__1, a, &c__1, d, e, tq, tp, w, &c__2, &info);
	chkxer_("DGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 10;
	dgebrd_(&c__2, &c__1, a, &c__2, d, e, tq, tp, w, &c__1, &info);
	chkxer_("DGEBRD", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 4;

/*        DGEBD2 */

	s_copy(srnamc_1.srnamt, "DGEBD2", 6L, 6L);
	infoc_1.infot = 1;
	dgebd2_(&c_n1, &c__0, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("DGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dgebd2_(&c__0, &c_n1, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("DGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dgebd2_(&c__2, &c__1, a, &c__1, d, e, tq, tp, w, &info);
	chkxer_("DGEBD2", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 3;

/*        DORGBR */

	s_copy(srnamc_1.srnamt, "DORGBR", 6L, 6L);
	infoc_1.infot = 1;
	dorgbr_("/", &c__0, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dorgbr_("Q", &c_n1, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dorgbr_("Q", &c__0, &c_n1, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dorgbr_("Q", &c__0, &c__1, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dorgbr_("Q", &c__1, &c__0, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dorgbr_("P", &c__1, &c__0, &c__0, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dorgbr_("P", &c__0, &c__1, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dorgbr_("Q", &c__0, &c__0, &c_n1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dorgbr_("Q", &c__2, &c__1, &c__1, a, &c__1, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dorgbr_("Q", &c__2, &c__2, &c__1, a, &c__2, tq, w, &c__1, &info);
	chkxer_("DORGBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 10;

/*        DORMBR */

	s_copy(srnamc_1.srnamt, "DORMBR", 6L, 6L);
	infoc_1.infot = 1;
	dormbr_("/", "L", "T", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dormbr_("Q", "/", "T", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dormbr_("Q", "L", "/", &c__0, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dormbr_("Q", "L", "T", &c_n1, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dormbr_("Q", "L", "T", &c__0, &c_n1, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 6;
	dormbr_("Q", "L", "T", &c__0, &c__0, &c_n1, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dormbr_("Q", "L", "T", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dormbr_("Q", "R", "T", &c__0, &c__2, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dormbr_("P", "L", "T", &c__2, &c__0, &c__2, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 8;
	dormbr_("P", "R", "T", &c__0, &c__2, &c__2, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dormbr_("Q", "R", "T", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dormbr_("Q", "L", "T", &c__0, &c__2, &c__0, a, &c__1, tq, u, &c__1, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dormbr_("Q", "R", "T", &c__2, &c__0, &c__0, a, &c__1, tq, u, &c__2, w,
		 &c__1, &info);
	chkxer_("DORMBR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 13;

/*        DBDSQR */

	s_copy(srnamc_1.srnamt, "DBDSQR", 6L, 6L);
	infoc_1.infot = 1;
	dbdsqr_("/", &c__0, &c__0, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	dbdsqr_("U", &c_n1, &c__0, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 3;
	dbdsqr_("U", &c__0, &c_n1, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	dbdsqr_("U", &c__0, &c__0, &c_n1, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 5;
	dbdsqr_("U", &c__0, &c__0, &c__0, &c_n1, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 9;
	dbdsqr_("U", &c__2, &c__1, &c__0, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 11;
	dbdsqr_("U", &c__0, &c__0, &c__2, &c__0, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 13;
	dbdsqr_("U", &c__2, &c__0, &c__0, &c__1, d, e, v, &c__1, u, &c__1, a, 
		&c__1, w, &info);
	chkxer_("DBDSQR", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	nt += 8;
    }

/*     Print a summary line. */

    if (infoc_1.ok) {
	io___15.ciunit = infoc_1.nout;
	s_wsfe(&io___15);
	do_fio(&c__1, path, 3L);
	do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	io___16.ciunit = infoc_1.nout;
	s_wsfe(&io___16);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    }


    return 0;

/*     End of DERRBD */

} /* derrbd_ */

