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

/* Subroutine */ int cerrqp_(char *path, integer *nunit)
{
    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer info;
    static complex a[4]	/* was [2][2] */, w[2];
    static char c2[2];
    static integer ip[2];
    extern /* Subroutine */ int alaesm_(char *, logical *, integer *),
	     cgeqpf_(integer *, integer *, complex *, integer *, integer *, 
	    complex *, complex *, real *, integer *);
    static real rw[4];
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int chkxer_(char *, integer *, integer *, logical 
	    *, logical *);
    static complex tau[2];

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CERRQP tests the error exits for CGEQPF.   

    Arguments   
    =========   

    PATH    (input) CHARACTER*3   
            The LAPACK path name for the routines to be tested.   

    NUNIT   (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
*/


    infoc_1.nout = *nunit;
    s_copy(c2, path + 1, 2L, 2L);
    a[0].r = 1.f, a[0].i = -1.f;
    a[2].r = 2.f, a[2].i = -2.f;
    a[3].r = 3.f, a[3].i = -3.f;
    a[1].r = 4.f, a[1].i = -4.f;
    infoc_1.ok = TRUE_;
    io___3.ciunit = infoc_1.nout;
    s_wsle(&io___3);
    e_wsle();

/*     Test error exits for QR factorization with pivoting */

    if (lsamen_(&c__2, c2, "QP")) {

/*        CGEQPF */

	s_copy(srnamc_1.srnamt, "CGEQPF", 6L, 6L);
	infoc_1.infot = 1;
	cgeqpf_(&c_n1, &c__0, a, &c__1, ip, tau, w, rw, &info);
	chkxer_("CGEQPF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 2;
	cgeqpf_(&c__0, &c_n1, a, &c__1, ip, tau, w, rw, &info);
	chkxer_("CGEQPF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
	infoc_1.infot = 4;
	cgeqpf_(&c__2, &c__0, a, &c__1, ip, tau, w, rw, &info);
	chkxer_("CGEQPF", &infoc_1.infot, &infoc_1.nout, &infoc_1.lerr, &
		infoc_1.ok);
    }

/*     Print a summary line. */

    alaesm_(path, &infoc_1.ok, &infoc_1.nout);

    return 0;

/*     End of CERRQP */

} /* cerrqp_ */

