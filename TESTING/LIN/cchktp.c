/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, iounit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c_n1 = -1;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static real c_b75 = 1.f;

/* Subroutine */ int cchktp_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, real *thresh, logical *tsterr, integer *nmax, complex *
	ap, complex *ainvp, complex *b, complex *x, complex *xact, complex *
	work, real *rwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO='\002,a1,\002', TRANS='\002,a1,\002"
	    "', N=\002,i5,\002, type \002,i2,\002, test(\002,i2,\002)= \002,g"
	    "12.5)";
    static char fmt_9998[] = "(1x,a6,\002( '\002,a1,\002', '\002,a1,\002', "
	    "'\002,a1,\002', '\002,a1,\002',\002,i5,\002, ... ), type \002,i2,"
	    "\002, ratio =\002,g12.5)";

    /* System generated locals */
    address a__1[2], a__2[3], a__3[4];
    integer i__1, i__2[2], i__3[3], i__4, i__5[4];
    char ch__1[2], ch__2[3], ch__3[4];

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static char diag[1];
    static integer imat, info;
    static char path[3], norm[1], uplo[1];
    static integer nrun, i;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer k, idiag, n;
    extern /* Subroutine */ int cget04_(integer *, integer *, complex *, 
	    integer *, complex *, integer *, real *, real *);
    static real scale;
    static integer nfail, iseed[4];
    extern logical lsame_(char *, char *);
    static real rcond;
    extern /* Subroutine */ int ctpt01_(char *, char *, integer *, complex *, 
	    complex *, real *, real *, real *);
    static real anorm;
    static integer itran;
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *), ctpt02_(char *, char *, char *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, real *, real *), ctpt03_(char *
	    , char *, char *, integer *, integer *, complex *, real *, real *,
	     real *, complex *, integer *, complex *, integer *, complex *, 
	    real *), ctpt05_(char *, char *, char *, 
	    integer *, integer *, complex *, complex *, integer *, complex *, 
	    integer *, complex *, integer *, real *, real *, real *), ctpt06_(real *, real *, char *, char *, integer *
	    , complex *, real *, real *);
    static char trans[1];
    static integer iuplo, nerrs, k1;
    static char xtype[1];
    static integer in;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer nt;
    static real rcondc;
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), clarhs_(char *, char 
	    *, char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, integer *, integer *);
    static real rcondi;
    extern doublereal clantp_(char *, char *, char *, integer *, complex *, 
	    real *);
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static real rcondo;
    extern /* Subroutine */ int clatps_(char *, char *, char *, char *, 
	    integer *, complex *, complex *, real *, real *, integer *), clattp_(integer *, char *, char *
	    , char *, integer *, integer *, complex *, complex *, complex *, 
	    real *, integer *);
    static real ainvnm;
    extern /* Subroutine */ int ctpcon_(char *, char *, char *, integer *, 
	    complex *, real *, complex *, real *, integer *), cerrtr_(char *, integer *), ctprfs_(char *, char 
	    *, char *, integer *, integer *, complex *, complex *, integer *, 
	    complex *, integer *, real *, real *, complex *, real *, integer *
	    ), ctptri_(char *, char *, integer *, 
	    complex *, integer *);
    static real result[9];
    extern /* Subroutine */ int ctptrs_(char *, char *, char *, integer *, 
	    integer *, complex *, complex *, integer *, integer *);
    static integer lda, lap;

    /* Fortran I/O blocks */
    static cilist io___34 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___37 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CCHKTP tests CTPTRI, -TRS, -RFS, and -CON, and CLATPS   

    Arguments   
    =========   

    DOTYPE  (input) LOGICAL array, dimension (NTYPES)   
            The matrix types to be used for testing.  Matrices of type j 
  
            (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =   
            .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

    NRHS    (input) INTEGER   
            The number of right hand side vectors to be generated for   
            each linear system.   

    THRESH  (input) REAL   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    NMAX    (input) INTEGER   
            The leading dimension of the work arrays.  NMAX >= the   
            maximumm value of N in NVAL.   

    AP      (workspace) COMPLEX array, dimension (NMAX*(NMAX+1)/2)   

    AINVP   (workspace) COMPLEX array, dimension (NMAX*(NMAX+1)/2)   

    B       (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) REAL array, dimension   
                        (max(NMAX,2*NRHS))   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --rwork;
    --work;
    --xact;
    --x;
    --b;
    --ainvp;
    --ap;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'C';
    s_copy(path + 1, "TP", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	cerrtr_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {

/*        Do for each value of N in NVAL */

	n = nval[in];
	lda = max(1,n);
	lap = lda * (lda + 1) / 2;
	*(unsigned char *)xtype = 'N';

	for (imat = 1; imat <= 10; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L50;
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*              Do first for UPLO = 'U', then for UPLO = 'L' 
*/

		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*              Call CLATTP to generate a triangular test matr
ix. */

		s_copy(srnamc_1.srnamt, "CLATTP", 6L, 6L);
		clattp_(&imat, uplo, "No transpose", diag, iseed, &n, &ap[1], 
			&x[1], &work[1], &rwork[1], &info);

/*              Set IDIAG = 1 for non-unit matrices, 2 for uni
t. */

		if (lsame_(diag, "N")) {
		    idiag = 1;
		} else {
		    idiag = 2;
		}

/* +    TEST 1   
                Form the inverse of A. */

		if (n > 0) {
		    ccopy_(&lap, &ap[1], &c__1, &ainvp[1], &c__1);
		}
		s_copy(srnamc_1.srnamt, "CTPTRI", 6L, 6L);
		ctptri_(uplo, diag, &n, &ainvp[1], &info);

/*              Check error code from CTPTRI. */

		if (info != 0) {
/* Writing concatenation */
		    i__2[0] = 1, a__1[0] = uplo;
		    i__2[1] = 1, a__1[1] = diag;
		    s_cat(ch__1, a__1, i__2, &c__2, 2L);
		    alaerh_(path, "CTPTRI", &info, &c__0, ch__1, &n, &n, &
			    c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		}

/*              Compute the infinity-norm condition number of 
A. */

		anorm = clantp_("I", uplo, diag, &n, &ap[1], &rwork[1]);
		ainvnm = clantp_("I", uplo, diag, &n, &ainvp[1], &rwork[1]);
		if (anorm <= 0.f || ainvnm <= 0.f) {
		    rcondi = 1.f;
		} else {
		    rcondi = 1.f / anorm / ainvnm;
		}

/*              Compute the residual for the triangular matrix
 times its   
                inverse.  Also compute the 1-norm condition nu
mber of A. */

		ctpt01_(uplo, diag, &n, &ap[1], &ainvp[1], &rcondo, &rwork[1],
			 result);
		k1 = 1;

		for (itran = 1; itran <= 3; ++itran) {

/*                 Do for op(A) = A, A**T, or A**H. */

		    *(unsigned char *)trans = *(unsigned char *)&transs[itran 
			    - 1];
		    if (itran == 1) {
			*(unsigned char *)norm = 'O';
			rcondc = rcondo;
		    } else {
			*(unsigned char *)norm = 'I';
			rcondc = rcondi;
			k1 = 2;
		    }

/* +    TEST 2   
                   Solve and compute residual for op(A)*x 
= b. */

		    s_copy(srnamc_1.srnamt, "CLARHS", 6L, 6L);
		    clarhs_(path, xtype, uplo, trans, &n, &n, &c__0, &idiag, 
			    nrhs, &ap[1], &lap, &xact[1], &lda, &b[1], &lda, 
			    iseed, &info);
		    *(unsigned char *)xtype = 'C';
		    clacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

		    s_copy(srnamc_1.srnamt, "CTPTRS", 6L, 6L);
		    ctptrs_(uplo, trans, diag, &n, nrhs, &ap[1], &x[1], &lda, 
			    &info);

/*                 Check error code from CTPTRS. */

		    if (info != 0) {
/* Writing concatenation */
			i__3[0] = 1, a__2[0] = uplo;
			i__3[1] = 1, a__2[1] = trans;
			i__3[2] = 1, a__2[2] = diag;
			s_cat(ch__2, a__2, i__3, &c__3, 3L);
			alaerh_(path, "CTPTRS", &info, &c__0, ch__2, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ctpt02_(uplo, trans, diag, &n, nrhs, &ap[1], &x[1], &lda, 
			    &b[1], &lda, &work[1], &rwork[1], &result[1]);

/* +    TEST 3   
                   Check solution from generated exact sol
ution. */

		    cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[2]);

/* +    TESTS 4, 5, and 6   
                   Use iterative refinement to improve the
 solution and   
                   compute error bounds. */

		    s_copy(srnamc_1.srnamt, "CTPRFS", 6L, 6L);
		    ctprfs_(uplo, trans, diag, &n, nrhs, &ap[1], &b[1], &lda, 
			    &x[1], &lda, &rwork[1], &rwork[*nrhs + 1], &work[
			    1], &rwork[(*nrhs << 1) + 1], &info);

/*                 Check error code from CTPRFS. */

		    if (info != 0) {
/* Writing concatenation */
			i__3[0] = 1, a__2[0] = uplo;
			i__3[1] = 1, a__2[1] = trans;
			i__3[2] = 1, a__2[2] = diag;
			s_cat(ch__2, a__2, i__3, &c__3, 3L);
			alaerh_(path, "CTPRFS", &info, &c__0, ch__2, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[3]);
		    ctpt05_(uplo, trans, diag, &n, nrhs, &ap[1], &b[1], &lda, 
			    &x[1], &lda, &xact[1], &lda, &rwork[1], &rwork[*
			    nrhs + 1], &result[4]);

/* +    TEST 7   
                   Get an estimate of RCOND = 1/CNDNUM. */

		    s_copy(srnamc_1.srnamt, "CTPCON", 6L, 6L);
		    ctpcon_(norm, uplo, diag, &n, &ap[1], &rcond, &work[1], &
			    rwork[1], &info);

/*                 Check error code from CTPCON. */

		    if (info != 0) {
/* Writing concatenation */
			i__3[0] = 1, a__2[0] = norm;
			i__3[1] = 1, a__2[1] = uplo;
			i__3[2] = 1, a__2[2] = diag;
			s_cat(ch__2, a__2, i__3, &c__3, 3L);
			alaerh_(path, "CTPCON", &info, &c__0, ch__2, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ctpt06_(&rcond, &rcondc, uplo, diag, &n, &ap[1], &rwork[1]
			    , &result[6]);
		    nt = 7;

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

		    i__4 = nt;
		    for (k = k1; k <= i__4; ++k) {
			if (result[k - 1] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___34.ciunit = *nout;
			    s_wsfe(&io___34);
			    do_fio(&c__1, uplo, 1L);
			    do_fio(&c__1, trans, 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(real));
			    e_wsfe();
			    ++nfail;
			}
/* L20: */
		    }
		    nrun = nrun + nt - k1 + 1;
/* L30: */
		}
/* L40: */
	    }
L50:
	    ;
	}

/*        Use pathological test matrices to test CLATPS. */

	for (imat = 11; imat <= 18; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L80;
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*              Do first for UPLO = 'U', then for UPLO = 'L' 
*/

		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		for (itran = 1; itran <= 3; ++itran) {

/*                 Do for op(A) = A, A**T, or A**H. */

		    *(unsigned char *)trans = *(unsigned char *)&transs[itran 
			    - 1];

/*                 Call CLATTP to generate a triangular te
st matrix. */

		    s_copy(srnamc_1.srnamt, "CLATTP", 6L, 6L);
		    clattp_(&imat, uplo, trans, diag, iseed, &n, &ap[1], &x[1]
			    , &work[1], &rwork[1], &info);

/* +    TEST 8   
                   Solve the system op(A)*x = b. */

		    s_copy(srnamc_1.srnamt, "CLATPS", 6L, 6L);
		    ccopy_(&n, &x[1], &c__1, &b[1], &c__1);
		    clatps_(uplo, trans, diag, "N", &n, &ap[1], &b[1], &scale,
			     &rwork[1], &info);

/*                 Check error code from CLATPS. */

		    if (info != 0) {
/* Writing concatenation */
			i__5[0] = 1, a__3[0] = uplo;
			i__5[1] = 1, a__3[1] = trans;
			i__5[2] = 1, a__3[2] = diag;
			i__5[3] = 1, a__3[3] = "N";
			s_cat(ch__3, a__3, i__5, &c__4, 4L);
			alaerh_(path, "CLATPS", &info, &c__0, ch__3, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ctpt03_(uplo, trans, diag, &n, &c__1, &ap[1], &scale, &
			    rwork[1], &c_b75, &b[1], &lda, &x[1], &lda, &work[
			    1], &result[7]);

/* +    TEST 9   
                   Solve op(A)*x = b again with NORMIN = '
Y'. */

		    ccopy_(&n, &x[1], &c__1, &b[n + 1], &c__1);
		    clatps_(uplo, trans, diag, "Y", &n, &ap[1], &b[n + 1], &
			    scale, &rwork[1], &info);

/*                 Check error code from CLATPS. */

		    if (info != 0) {
/* Writing concatenation */
			i__5[0] = 1, a__3[0] = uplo;
			i__5[1] = 1, a__3[1] = trans;
			i__5[2] = 1, a__3[2] = diag;
			i__5[3] = 1, a__3[3] = "Y";
			s_cat(ch__3, a__3, i__5, &c__4, 4L);
			alaerh_(path, "CLATPS", &info, &c__0, ch__3, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ctpt03_(uplo, trans, diag, &n, &c__1, &ap[1], &scale, &
			    rwork[1], &c_b75, &b[n + 1], &lda, &x[1], &lda, &
			    work[1], &result[8]);

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

		    if (result[7] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			io___36.ciunit = *nout;
			s_wsfe(&io___36);
			do_fio(&c__1, "CLATPS", 6L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, trans, 1L);
			do_fio(&c__1, diag, 1L);
			do_fio(&c__1, "N", 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[7], (ftnlen)sizeof(real)
				);
			e_wsfe();
			++nfail;
		    }
		    if (result[8] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			io___37.ciunit = *nout;
			s_wsfe(&io___37);
			do_fio(&c__1, "CLATPS", 6L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, trans, 1L);
			do_fio(&c__1, diag, 1L);
			do_fio(&c__1, "Y", 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[8], (ftnlen)sizeof(real)
				);
			e_wsfe();
			++nfail;
		    }
		    nrun += 2;
/* L60: */
		}
/* L70: */
	    }
L80:
	    ;
	}
/* L90: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of CCHKTP */

} /* cchktp_ */

