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
static doublereal c_b74 = 1.;

/* Subroutine */ int zchktr_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, doublereal *thresh, 
	logical *tsterr, integer *nmax, doublecomplex *a, doublecomplex *ainv,
	 doublecomplex *b, doublecomplex *x, doublecomplex *xact, 
	doublecomplex *work, doublereal *rwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO='\002,a1,\002', TRANS='\002,a1,\002"
	    "', N=\002,i5,\002, NB=\002,i4,\002, type \002,i2,\002, test(\002"
	    ",i2,\002)= \002,g12.5)";
    static char fmt_9998[] = "(1x,a6,\002( '\002,a1,\002', '\002,a1,\002', "
	    "'\002,a1,\002', '\002,a1,\002',\002,i5,\002, ... ), type \002,i2,"
	    "\002, ratio =\002,g12.5)";

    /* System generated locals */
    address a__1[2], a__2[3], a__3[4];
    integer i__1, i__2, i__3[2], i__4[3], i__5, i__6[4];
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
    static doublereal scale;
    static integer nfail, iseed[4];
    extern logical lsame_(char *, char *);
    static doublereal rcond, anorm;
    static integer itran;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    );
    static char trans[1];
    static integer iuplo, nerrs;
    static doublereal dummy;
    static integer k1;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), ztrt01_(char *, char *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *), 
	    ztrt02_(char *, char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublereal *, 
	    doublereal *), ztrt03_(char *, char *, 
	    char *, integer *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublereal *);
    static char xtype[1];
    extern /* Subroutine */ int ztrt05_(char *, char *, char *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *),
	     ztrt06_(doublereal *, doublereal *, char *, char *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *);
    static integer nb, in;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer nt;
    static doublereal rcondc, rcondi;
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal rcondo, ainvnm;
    extern /* Subroutine */ int xlaenv_(integer *, integer *), zlacpy_(char *,
	     integer *, integer *, doublecomplex *, integer *, doublecomplex *
	    , integer *), zlarhs_(char *, char *, char *, char *, 
	    integer *, integer *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *, integer *);
    extern doublereal zlantr_(char *, char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *);
    static doublereal result[9];
    extern /* Subroutine */ int zlatrs_(char *, char *, char *, char *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublereal *, doublereal *, integer *), zlattr_(integer *, char *, char *, char *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublereal *, integer *),
	     ztrcon_(char *, char *, char *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublecomplex *, doublereal *, integer *), zerrtr_(char *, integer *), 
	    ztrrfs_(char *, char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, 
	    doublecomplex *, doublereal *, integer *),
	     ztrtri_(char *, char *, integer *, doublecomplex *, integer *, 
	    integer *), ztrtrs_(char *, char *, char *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, integer *);
    static integer lda, inb;

    /* Fortran I/O blocks */
    static cilist io___36 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___39 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKTR tests ZTRTRI, -TRS, -RFS, and -CON, and ZLATRS   

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

    NNB     (input) INTEGER   
            The number of values of NB contained in the vector NBVAL.   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

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

    A       (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)   

    AINV    (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)   

    B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
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
    --ainv;
    --a;
    --nbval;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "TR", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	zerrtr_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {

/*        Do for each value of N in NVAL */

	n = nval[in];
	lda = max(1,n);
	*(unsigned char *)xtype = 'N';

	for (imat = 1; imat <= 10; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L60;
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*              Do first for UPLO = 'U', then for UPLO = 'L' 
*/

		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*              Call ZLATTR to generate a triangular test matr
ix. */

		s_copy(srnamc_1.srnamt, "ZLATTR", 6L, 6L);
		zlattr_(&imat, uplo, "No transpose", diag, iseed, &n, &a[1], &
			lda, &x[1], &work[1], &rwork[1], &info);

/*              Set IDIAG = 1 for non-unit matrices, 2 for uni
t. */

		if (lsame_(diag, "N")) {
		    idiag = 1;
		} else {
		    idiag = 2;
		}

		i__2 = *nnb;
		for (inb = 1; inb <= i__2; ++inb) {

/*                 Do for each blocksize in NBVAL */

		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/* +    TEST 1   
                   Form the inverse of A. */

		    zlacpy_(uplo, &n, &n, &a[1], &lda, &ainv[1], &lda);
		    s_copy(srnamc_1.srnamt, "ZTRTRI", 6L, 6L);
		    ztrtri_(uplo, diag, &n, &ainv[1], &lda, &info);

/*                 Check error code from ZTRTRI. */

		    if (info != 0) {
/* Writing concatenation */
			i__3[0] = 1, a__1[0] = uplo;
			i__3[1] = 1, a__1[1] = diag;
			s_cat(ch__1, a__1, i__3, &c__2, 2L);
			alaerh_(path, "ZTRTRI", &info, &c__0, ch__1, &n, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
		    }

/*                 Compute the infinity-norm condition num
ber of A. */

		    anorm = zlantr_("I", uplo, diag, &n, &n, &a[1], &lda, &
			    rwork[1]);
		    ainvnm = zlantr_("I", uplo, diag, &n, &n, &ainv[1], &lda, 
			    &rwork[1]);
		    if (anorm <= 0. || ainvnm <= 0.) {
			rcondi = 1.;
		    } else {
			rcondi = 1. / anorm / ainvnm;
		    }

/*                 Compute the residual for the triangular
 matrix times   
                   its inverse.  Also compute the 1-norm c
ondition number   
                   of A. */

		    ztrt01_(uplo, diag, &n, &a[1], &lda, &ainv[1], &lda, &
			    rcondo, &rwork[1], result);
		    k1 = 1;
		    nt = 1;

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
			    k1 = 2;
			}

/*                    Do the rest of the tests only if
 this is the first   
                      blocksize. */

			if (inb == 1) {

/* +    TEST 2   
                         Solve and compute residua
l for op(A)*x = b. */

			    s_copy(srnamc_1.srnamt, "ZLARHS", 6L, 6L);
			    zlarhs_(path, xtype, uplo, trans, &n, &n, &c__0, &
				    idiag, nrhs, &a[1], &lda, &xact[1], &lda, 
				    &b[1], &lda, iseed, &info)
				    ;
			    *(unsigned char *)xtype = 'C';
			    zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &
				    lda);

			    s_copy(srnamc_1.srnamt, "ZTRTRS", 6L, 6L);
			    ztrtrs_(uplo, trans, diag, &n, nrhs, &a[1], &lda, 
				    &x[1], &lda, &info);

/*                       Check error code from ZTR
TRS. */

			    if (info != 0) {
/* Writing concatenation */
				i__4[0] = 1, a__2[0] = uplo;
				i__4[1] = 1, a__2[1] = trans;
				i__4[2] = 1, a__2[2] = diag;
				s_cat(ch__2, a__2, i__4, &c__3, 3L);
				alaerh_(path, "ZTRTRS", &info, &c__0, ch__2, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

/*                       This line is needed on a 
Sun SPARCstation. */

			    if (n > 0) {
				dummy = a[1].r;
			    }

			    ztrt02_(uplo, trans, diag, &n, nrhs, &a[1], &lda, 
				    &x[1], &lda, &b[1], &lda, &work[1], &
				    rwork[1], &result[1]);

/* +    TEST 3   
                         Check solution from gener
ated exact solution. */

			    zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[2]);

/* +    TESTS 4, 5, and 6   
                         Use iterative refinement 
to improve the solution   
                         and compute error bounds.
 */

			    s_copy(srnamc_1.srnamt, "ZTRRFS", 6L, 6L);
			    ztrrfs_(uplo, trans, diag, &n, nrhs, &a[1], &lda, 
				    &b[1], &lda, &x[1], &lda, &rwork[1], &
				    rwork[*nrhs + 1], &work[1], &rwork[(*nrhs 
				    << 1) + 1], &info);

/*                       Check error code from ZTR
RFS. */

			    if (info != 0) {
/* Writing concatenation */
				i__4[0] = 1, a__2[0] = uplo;
				i__4[1] = 1, a__2[1] = trans;
				i__4[2] = 1, a__2[2] = diag;
				s_cat(ch__2, a__2, i__4, &c__3, 3L);
				alaerh_(path, "ZTRRFS", &info, &c__0, ch__2, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

			    zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[3]);
			    ztrt05_(uplo, trans, diag, &n, nrhs, &a[1], &lda, 
				    &b[1], &lda, &x[1], &lda, &xact[1], &lda, 
				    &rwork[1], &rwork[*nrhs + 1], &result[4]);

/* +    TEST 7   
                         Get an estimate of RCOND 
= 1/CNDNUM. */

			    s_copy(srnamc_1.srnamt, "ZTRCON", 6L, 6L);
			    ztrcon_(norm, uplo, diag, &n, &a[1], &lda, &rcond,
				     &work[1], &rwork[1], &info);

/*                       Check error code from ZTR
CON. */

			    if (info != 0) {
/* Writing concatenation */
				i__4[0] = 1, a__2[0] = norm;
				i__4[1] = 1, a__2[1] = uplo;
				i__4[2] = 1, a__2[2] = diag;
				s_cat(ch__2, a__2, i__4, &c__3, 3L);
				alaerh_(path, "ZTRCON", &info, &c__0, ch__2, &
					n, &n, &c_n1, &c_n1, &c_n1, &imat, &
					nfail, &nerrs, nout);
			    }

			    ztrt06_(&rcond, &rcondc, uplo, diag, &n, &a[1], &
				    lda, &rwork[1], &result[6]);
			    nt = 7;
			}

/*                    Print information about the test
s that did not pass   
                      the threshold. */

			i__5 = nt;
			for (k = k1; k <= i__5; ++k) {
			    if (result[k - 1] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    alahd_(nout, path);
				}
				io___36.ciunit = *nout;
				s_wsfe(&io___36);
				do_fio(&c__1, uplo, 1L);
				do_fio(&c__1, trans, 1L);
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(
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
/* L20: */
			}
			nrun = nrun + nt - k1 + 1;
/* L30: */
		    }
/* L40: */
		}
/* L50: */
	    }
L60:
	    ;
	}

/*        Use pathological test matrices to test ZLATRS. */

	for (imat = 11; imat <= 18; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L90;
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*              Do first for UPLO = 'U', then for UPLO = 'L' 
*/

		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		for (itran = 1; itran <= 3; ++itran) {

/*                 Do for op(A) = A, A**T, and A**H. */

		    *(unsigned char *)trans = *(unsigned char *)&transs[itran 
			    - 1];

/*                 Call ZLATTR to generate a triangular te
st matrix. */

		    s_copy(srnamc_1.srnamt, "ZLATTR", 6L, 6L);
		    zlattr_(&imat, uplo, trans, diag, iseed, &n, &a[1], &lda, 
			    &x[1], &work[1], &rwork[1], &info);

/* +    TEST 8   
                   Solve the system op(A)*x = b. */

		    s_copy(srnamc_1.srnamt, "ZLATRS", 6L, 6L);
		    zcopy_(&n, &x[1], &c__1, &b[1], &c__1);
		    zlatrs_(uplo, trans, diag, "N", &n, &a[1], &lda, &b[1], &
			    scale, &rwork[1], &info);

/*                 Check error code from ZLATRS. */

		    if (info != 0) {
/* Writing concatenation */
			i__6[0] = 1, a__3[0] = uplo;
			i__6[1] = 1, a__3[1] = trans;
			i__6[2] = 1, a__3[2] = diag;
			i__6[3] = 1, a__3[3] = "N";
			s_cat(ch__3, a__3, i__6, &c__4, 4L);
			alaerh_(path, "ZLATRS", &info, &c__0, ch__3, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ztrt03_(uplo, trans, diag, &n, &c__1, &a[1], &lda, &scale,
			     &rwork[1], &c_b74, &b[1], &lda, &x[1], &lda, &
			    work[1], &result[7]);

/* +    TEST 9   
                   Solve op(A)*X = b again with NORMIN = '
Y'. */

		    zcopy_(&n, &x[1], &c__1, &b[n + 1], &c__1);
		    zlatrs_(uplo, trans, diag, "Y", &n, &a[1], &lda, &b[n + 1]
			    , &scale, &rwork[1], &info);

/*                 Check error code from ZLATRS. */

		    if (info != 0) {
/* Writing concatenation */
			i__6[0] = 1, a__3[0] = uplo;
			i__6[1] = 1, a__3[1] = trans;
			i__6[2] = 1, a__3[2] = diag;
			i__6[3] = 1, a__3[3] = "Y";
			s_cat(ch__3, a__3, i__6, &c__4, 4L);
			alaerh_(path, "ZLATRS", &info, &c__0, ch__3, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    ztrt03_(uplo, trans, diag, &n, &c__1, &a[1], &lda, &scale,
			     &rwork[1], &c_b74, &b[n + 1], &lda, &x[1], &lda, 
			    &work[1], &result[8]);

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

		    if (result[7] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			io___38.ciunit = *nout;
			s_wsfe(&io___38);
			do_fio(&c__1, "ZLATRS", 6L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, trans, 1L);
			do_fio(&c__1, diag, 1L);
			do_fio(&c__1, "N", 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[7], (ftnlen)sizeof(
				doublereal));
			e_wsfe();
			++nfail;
		    }
		    if (result[8] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			io___39.ciunit = *nout;
			s_wsfe(&io___39);
			do_fio(&c__1, "ZLATRS", 6L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, trans, 1L);
			do_fio(&c__1, diag, 1L);
			do_fio(&c__1, "Y", 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[8], (ftnlen)sizeof(
				doublereal));
			e_wsfe();
			++nfail;
		    }
		    nrun += 2;
/* L70: */
		}
/* L80: */
	    }
L90:
	    ;
	}
/* L100: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of ZCHKTR */

} /* zchktr_ */

