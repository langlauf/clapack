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

static doublereal c_b14 = 0.;
static doublereal c_b15 = 1.;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__3 = 3;
static integer c_n1 = -1;
static integer c__4 = 4;

/* Subroutine */ int dchktb_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, integer *nmax, 
	doublereal *ab, doublereal *ainv, doublereal *b, doublereal *x, 
	doublereal *xact, doublereal *work, doublereal *rwork, integer *iwork,
	 integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO='\002,a1,\002', TRANS='\002,a1,\002"
	    "', N=\002,i5,\002, KD=\002,i5,\002, type \002,i2,\002, test(\002"
	    ",i2,\002)=\002,g12.5)";
    static char fmt_9998[] = "(1x,a6,\002( '\002,a1,\002', '\002,a1,\002', "
	    "'\002,a1,\002', '\002,a1,\002',\002,i5,\002,\002,i5,\002,  ... )"
	    ", type \002,i2,\002, ratio =\002,g12.5)";

    /* System generated locals */
    address a__1[3], a__2[4];
    integer i__1, i__2, i__3, i__4, i__5, i__6[3], i__7[4];
    char ch__1[3], ch__2[4];

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ldab;
    static char diag[1];
    static integer imat, info;
    static char path[3], norm[1], uplo[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer idiag, n;
    static doublereal scale;
    static integer nfail, iseed[4];
    extern /* Subroutine */ int dget04_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *), 
	    dtbt02_(char *, char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *), 
	    dtbt03_(char *, char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *);
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int dtbt05_(char *, char *, char *, integer *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *),
	     dtbt06_(doublereal *, doublereal *, char *, char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    static integer itran;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dtbsv_(char *, char *, char *, integer *
	    , integer *, doublereal *, integer *, doublereal *, integer *);
    static char trans[1];
    static integer iuplo, nerrs;
    static char xtype[1];
    static integer nimat2, kd, ik, in, nk;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer nt;
    extern doublereal dlantb_(char *, char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *);
    static doublereal rcondc;
    extern /* Subroutine */ int dlatbs_(char *, char *, char *, char *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *), dlattb_(integer *, char *, char *, char *, integer *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *), dtbcon_(char *, 
	    char *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *), dlacpy_(char *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *), dlarhs_(char *, char 
	    *, char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *);
    static doublereal rcondi;
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    alasum_(char *, integer *, integer *, integer *, integer *);
    static doublereal rcondo;
    extern doublereal dlantr_(char *, char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int dtbrfs_(char *, char *, char *, integer *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *);
    static doublereal ainvnm;
    extern /* Subroutine */ int derrtr_(char *, integer *), dtbtrs_(
	    char *, char *, char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *);
    static doublereal result[8];
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___39 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DCHKTB tests DTBTRI, -TRS, -RFS, and -CON, and DLATBS.   

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

    THRESH  (input) DOUBLE PRECISION   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    NMAX    (input) INTEGER   
            The leading dimension of the work arrays.   
            NMAX >= the maximum value of N in NVAL.   

    AB      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    AINV    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    X       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    XACT    (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    WORK    (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max(NMAX,2*NRHS))   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --iwork;
    --rwork;
    --work;
    --xact;
    --x;
    --b;
    --ainv;
    --ab;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'D';
    s_copy(path + 1, "TB", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	derrtr_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {

/*        Do for each value of N in NVAL */

	n = nval[in];
	lda = max(1,n);
	*(unsigned char *)xtype = 'N';
	nimat = 9;
	nimat2 = 17;
	if (n <= 0) {
	    nimat = 1;
	    nimat2 = 10;
	}

/* Computing MIN */
	i__2 = n + 1;
	nk = min(i__2,4);
	i__2 = nk;
	for (ik = 1; ik <= i__2; ++ik) {

/*           Do for KD = 0, N, (3N-1)/4, and (N+1)/4. This order m
akes   
             it easier to skip redundant values for small values o
f N. */

	    if (ik == 1) {
		kd = 0;
	    } else if (ik == 2) {
		kd = max(n,0);
	    } else if (ik == 3) {
		kd = (n * 3 - 1) / 4;
	    } else if (ik == 4) {
		kd = (n + 1) / 4;
	    }
	    ldab = kd + 1;

	    i__3 = nimat;
	    for (imat = 1; imat <= i__3; ++imat) {

/*              Do the tests only if DOTYPE( IMAT ) is true. 
*/

		if (! dotype[imat]) {
		    goto L70;
		}

		for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*                 Do first for UPLO = 'U', then for UPLO 
= 'L' */

		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];

/*                 Call DLATTB to generate a triangular te
st matrix. */

		    s_copy(srnamc_1.srnamt, "DLATTB", 6L, 6L);
		    dlattb_(&imat, uplo, "No transpose", diag, iseed, &n, &kd,
			     &ab[1], &ldab, &x[1], &work[1], &info);

/*                 Set IDIAG = 1 for non-unit matrices, 2 
for unit. */

		    if (lsame_(diag, "N")) {
			idiag = 1;
		    } else {
			idiag = 2;
		    }

/*                 Form the inverse of A so we can get a g
ood estimate   
                   of RCONDC = 1/(norm(A) * norm(inv(A))).
 */

		    dlaset_("Full", &n, &n, &c_b14, &c_b15, &ainv[1], &lda);
		    if (lsame_(uplo, "U")) {
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    dtbsv_(uplo, "No transpose", diag, &j, &kd, &ab[1]
				    , &ldab, &ainv[(j - 1) * lda + 1], &c__1);
/* L20: */
			}
		    } else {
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    i__5 = n - j + 1;
			    dtbsv_(uplo, "No transpose", diag, &i__5, &kd, &
				    ab[(j - 1) * ldab + 1], &ldab, &ainv[(j - 
				    1) * lda + j], &c__1);
/* L30: */
			}
		    }

/*                 Compute the 1-norm condition number of 
A. */

		    anorm = dlantb_("1", uplo, diag, &n, &kd, &ab[1], &ldab, &
			    rwork[1]);
		    ainvnm = dlantr_("1", uplo, diag, &n, &n, &ainv[1], &lda, 
			    &rwork[1]);
		    if (anorm <= 0. || ainvnm <= 0.) {
			rcondo = 1.;
		    } else {
			rcondo = 1. / anorm / ainvnm;
		    }

/*                 Compute the infinity-norm condition num
ber of A. */

		    anorm = dlantb_("I", uplo, diag, &n, &kd, &ab[1], &ldab, &
			    rwork[1]);
		    ainvnm = dlantr_("I", uplo, diag, &n, &n, &ainv[1], &lda, 
			    &rwork[1]);
		    if (anorm <= 0. || ainvnm <= 0.) {
			rcondi = 1.;
		    } else {
			rcondi = 1. / anorm / ainvnm;
		    }

		    for (itran = 1; itran <= 3; ++itran) {

/*                    Do for op(A) = A, A**T, or A**H.
 */

			*(unsigned char *)trans = *(unsigned char *)&transs[
				itran - 1];
			if (itran == 1) {
			    *(unsigned char *)norm = 'O';
			    rcondc = rcondo;
			} else {
			    *(unsigned char *)norm = 'I';
			    rcondc = rcondi;
			}

/* +    TEST 1   
                      Solve and compute residual for o
p(A)*x = b. */

			s_copy(srnamc_1.srnamt, "DLARHS", 6L, 6L);
			dlarhs_(path, xtype, uplo, trans, &n, &n, &kd, &idiag,
				 nrhs, &ab[1], &ldab, &xact[1], &lda, &b[1], &
				lda, iseed, &info);
			*(unsigned char *)xtype = 'C';
			dlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

			s_copy(srnamc_1.srnamt, "DTBTRS", 6L, 6L);
			dtbtrs_(uplo, trans, diag, &n, &kd, nrhs, &ab[1], &
				ldab, &x[1], &lda, &info);

/*                    Check error code from DTBTRS. */

			if (info != 0) {
/* Writing concatenation */
			    i__6[0] = 1, a__1[0] = uplo;
			    i__6[1] = 1, a__1[1] = trans;
			    i__6[2] = 1, a__1[2] = diag;
			    s_cat(ch__1, a__1, i__6, &c__3, 3L);
			    alaerh_(path, "DTBTRS", &info, &c__0, ch__1, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			dtbt02_(uplo, trans, diag, &n, &kd, nrhs, &ab[1], &
				ldab, &x[1], &lda, &b[1], &lda, &work[1], 
				result);

/* +    TEST 2   
                      Check solution from generated ex
act solution. */

			dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[1]);

/* +    TESTS 3, 4, and 5   
                      Use iterative refinement to impr
ove the solution   
                      and compute error bounds. */

			s_copy(srnamc_1.srnamt, "DTBRFS", 6L, 6L);
			dtbrfs_(uplo, trans, diag, &n, &kd, nrhs, &ab[1], &
				ldab, &b[1], &lda, &x[1], &lda, &rwork[1], &
				rwork[*nrhs + 1], &work[1], &iwork[1], &info);

/*                    Check error code from DTBRFS. */

			if (info != 0) {
/* Writing concatenation */
			    i__6[0] = 1, a__1[0] = uplo;
			    i__6[1] = 1, a__1[1] = trans;
			    i__6[2] = 1, a__1[2] = diag;
			    s_cat(ch__1, a__1, i__6, &c__3, 3L);
			    alaerh_(path, "DTBRFS", &info, &c__0, ch__1, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[2]);
			dtbt05_(uplo, trans, diag, &n, &kd, nrhs, &ab[1], &
				ldab, &b[1], &lda, &x[1], &lda, &xact[1], &
				lda, &rwork[1], &rwork[*nrhs + 1], &result[3]);

/* +    TEST 6   
                      Get an estimate of RCOND = 1/CND
NUM. */

			s_copy(srnamc_1.srnamt, "DTBCON", 6L, 6L);
			dtbcon_(norm, uplo, diag, &n, &kd, &ab[1], &ldab, &
				rcond, &work[1], &iwork[1], &info)
				;

/*                    Check error code from DTBCON. */

			if (info != 0) {
/* Writing concatenation */
			    i__6[0] = 1, a__1[0] = norm;
			    i__6[1] = 1, a__1[1] = uplo;
			    i__6[2] = 1, a__1[2] = diag;
			    s_cat(ch__1, a__1, i__6, &c__3, 3L);
			    alaerh_(path, "DTBCON", &info, &c__0, ch__1, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			}

			dtbt06_(&rcond, &rcondc, uplo, diag, &n, &kd, &ab[1], 
				&ldab, &rwork[1], &result[5]);
			nt = 6;

/*                    Print information about the test
s that did not pass   
                      the threshold. */

			i__4 = nt;
			for (k = 1; k <= i__4; ++k) {
			    if (result[k - 1] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    alahd_(nout, path);
				}
				io___39.ciunit = *nout;
				s_wsfe(&io___39);
				do_fio(&c__1, uplo, 1L);
				do_fio(&c__1, trans, 1L);
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&kd, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
					sizeof(doublereal));
				e_wsfe();
				++nfail;
			    }
/* L40: */
			}
			nrun += nt;
/* L50: */
		    }
/* L60: */
		}
L70:
		;
	    }

/*           Use pathological test matrices to test DLATBS. */

	    i__3 = nimat2;
	    for (imat = 10; imat <= i__3; ++imat) {

/*              Do the tests only if DOTYPE( IMAT ) is true. 
*/

		if (! dotype[imat]) {
		    goto L100;
		}

		for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*                 Do first for UPLO = 'U', then for UPLO 
= 'L' */

		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    for (itran = 1; itran <= 3; ++itran) {

/*                    Do for op(A) = A, A**T, and A**H
. */

			*(unsigned char *)trans = *(unsigned char *)&transs[
				itran - 1];

/*                    Call DLATTB to generate a triang
ular test matrix. */

			s_copy(srnamc_1.srnamt, "DLATTB", 6L, 6L);
			dlattb_(&imat, uplo, trans, diag, iseed, &n, &kd, &ab[
				1], &ldab, &x[1], &work[1], &info)
				;

/* +    TEST 7   
                      Solve the system op(A)*x = b */

			s_copy(srnamc_1.srnamt, "DLATBS", 6L, 6L);
			dcopy_(&n, &x[1], &c__1, &b[1], &c__1);
			dlatbs_(uplo, trans, diag, "N", &n, &kd, &ab[1], &
				ldab, &b[1], &scale, &rwork[1], &info);

/*                    Check error code from DLATBS. */

			if (info != 0) {
/* Writing concatenation */
			    i__7[0] = 1, a__2[0] = uplo;
			    i__7[1] = 1, a__2[1] = trans;
			    i__7[2] = 1, a__2[2] = diag;
			    i__7[3] = 1, a__2[3] = "N";
			    s_cat(ch__2, a__2, i__7, &c__4, 4L);
			    alaerh_(path, "DLATBS", &info, &c__0, ch__2, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			}

			dtbt03_(uplo, trans, diag, &n, &kd, &c__1, &ab[1], &
				ldab, &scale, &rwork[1], &c_b15, &b[1], &lda, 
				&x[1], &lda, &work[1], &result[6])
				;

/* +    TEST 8   
                      Solve op(A)*x = b again with NOR
MIN = 'Y'. */

			dcopy_(&n, &x[1], &c__1, &b[1], &c__1);
			dlatbs_(uplo, trans, diag, "Y", &n, &kd, &ab[1], &
				ldab, &b[1], &scale, &rwork[1], &info);

/*                    Check error code from DLATBS. */

			if (info != 0) {
/* Writing concatenation */
			    i__7[0] = 1, a__2[0] = uplo;
			    i__7[1] = 1, a__2[1] = trans;
			    i__7[2] = 1, a__2[2] = diag;
			    i__7[3] = 1, a__2[3] = "Y";
			    s_cat(ch__2, a__2, i__7, &c__4, 4L);
			    alaerh_(path, "DLATBS", &info, &c__0, ch__2, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			}

			dtbt03_(uplo, trans, diag, &n, &kd, &c__1, &ab[1], &
				ldab, &scale, &rwork[1], &c_b15, &b[1], &lda, 
				&x[1], &lda, &work[1], &result[7])
				;

/*                    Print information about the test
s that did not pass   
                      the threshold. */

			if (result[6] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___41.ciunit = *nout;
			    s_wsfe(&io___41);
			    do_fio(&c__1, "DLATBS", 6L);
			    do_fio(&c__1, uplo, 1L);
			    do_fio(&c__1, trans, 1L);
			    do_fio(&c__1, diag, 1L);
			    do_fio(&c__1, "N", 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&kd, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&result[6], (ftnlen)sizeof(
				    doublereal));
			    e_wsfe();
			    ++nfail;
			}
			if (result[7] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___42.ciunit = *nout;
			    s_wsfe(&io___42);
			    do_fio(&c__1, "DLATBS", 6L);
			    do_fio(&c__1, uplo, 1L);
			    do_fio(&c__1, trans, 1L);
			    do_fio(&c__1, diag, 1L);
			    do_fio(&c__1, "Y", 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&kd, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&result[7], (ftnlen)sizeof(
				    doublereal));
			    e_wsfe();
			    ++nfail;
			}
			nrun += 2;
/* L80: */
		    }
/* L90: */
		}
L100:
		;
	    }
/* L110: */
	}
/* L120: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of DCHKTB */

} /* dchktb_ */

