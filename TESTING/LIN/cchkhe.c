/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, nunit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static integer c__0 = 0;
static integer c_n1 = -1;
static integer c__1 = 1;

/* Subroutine */ int cchkhe_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, real *thresh, logical *
	tsterr, integer *nmax, complex *a, complex *afac, complex *ainv, 
	complex *b, complex *x, complex *xact, complex *work, real *rwork, 
	integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO = '\002,a1,\002', N =\002,i5,\002, "
	    "NB =\002,i4,\002, type \002,i2,\002, test \002,i2,\002, ratio "
	    "=\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ioff, mode, imat, info;
    static char path[3], dist[1], uplo[1], type[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer n;
    extern /* Subroutine */ int chet01_(char *, integer *, complex *, integer 
	    *, complex *, integer *, integer *, complex *, integer *, real *, 
	    real *), cget04_(integer *, integer *, complex *, integer 
	    *, complex *, integer *, real *, real *);
    static integer nfail, iseed[4];
    static real rcond;
    extern /* Subroutine */ int cpot02_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *, complex *, integer *, real *, 
	    real *);
    static integer nimat;
    extern doublereal sget06_(real *, real *);
    extern /* Subroutine */ int cpot03_(char *, integer *, complex *, integer 
	    *, complex *, integer *, complex *, integer *, real *, real *, 
	    real *), cpot05_(char *, integer *, integer *, complex *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, real *, real *, real *);
    static real anorm;
    static integer iuplo, izero, i1, i2, ktest, lwork, nerrs;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int clatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, real *, integer *, real *, char *
	    );
    static integer nb, in, kl;
    extern doublereal clanhe_(char *, char *, integer *, complex *, integer *,
	     real *);
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), claipd_(integer *, complex *, integer *, integer *), 
	    checon_(char *, integer *, complex *, integer *, integer *, real *
	    , real *, complex *, integer *);
    static integer ku, nt;
    static real rcondc;
    extern /* Subroutine */ int cerrhe_(char *, integer *), cherfs_(
	    char *, integer *, integer *, complex *, integer *, complex *, 
	    integer *, integer *, complex *, integer *, complex *, integer *, 
	    real *, real *, complex *, real *, integer *), chetrf_(
	    char *, integer *, complex *, integer *, integer *, complex *, 
	    integer *, integer *), clacpy_(char *, integer *, integer 
	    *, complex *, integer *, complex *, integer *), clarhs_(
	    char *, char *, char *, char *, integer *, integer *, integer *, 
	    integer *, integer *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, integer *, integer *), chetri_(char *, integer *, complex *, integer *, 
	    integer *, complex *, integer *), alasum_(char *, integer 
	    *, integer *, integer *, integer *);
    static real cndnum;
    extern /* Subroutine */ int clatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, complex *, integer *, complex *, integer *), chetrs_(char *, integer *, integer *, complex *, 
	    integer *, integer *, complex *, integer *, integer *);
    static logical trfcon;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static real result[8];
    static integer lda, inb;

    /* Fortran I/O blocks */
    static cilist io___41 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CCHKHE tests CHETRF, -TRI, -TRS, -RFS, and -CON.   

    Arguments   
    =========   

    DOTYPE  (input) LOGICAL array, dimension (NTYPES)   
            The matrix types to be used for testing.  Matrices of type j 
  
            (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =   
            .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix dimension N.   

    NNB     (input) INTEGER   
            The number of values of NB contained in the vector NBVAL.   

    NBVAL   (input) INTEGER array, dimension (NBVAL)   
            The values of the blocksize NB.   

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
            The maximum value permitted for N, used in dimensioning the   
            work arrays.   

    A       (workspace) COMPLEX array, dimension (NMAX*NMAX)   

    AFAC    (workspace) COMPLEX array, dimension (NMAX*NMAX)   

    AINV    (workspace) COMPLEX array, dimension (NMAX*NMAX)   

    B       (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) REAL array, dimension   
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
    --afac;
    --a;
    --nbval;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'C';
    s_copy(path + 1, "HE", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	cerrhe_(path, nout);
    }
    infoc_1.infot = 0;

/*     Do for each value of N in NVAL */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];
	lda = max(n,1);
	*(unsigned char *)xtype = 'N';
	nimat = 10;
	if (n <= 0) {
	    nimat = 1;
	}

	izero = 0;
	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L160;
	    }

/*           Skip types 3, 4, 5, or 6 if the matrix size is too sm
all. */

	    zerot = imat >= 3 && imat <= 6;
	    if (zerot && n < imat - 2) {
		goto L160;
	    }

/*           Do first for UPLO = 'U', then for UPLO = 'L' */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*              Set up parameters with CLATB4 and generate a t
est matrix   
                with CLATMS. */

		clatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "CLATMS", 6L, 6L);
		clatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, uplo, &a[1], &lda, &work[1], &info);

/*              Check error code from CLATMS. */

		if (info != 0) {
		    alaerh_(path, "CLATMS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L150;
		}

/*              For types 3-6, zero one or more rows and colum
ns of   
                the matrix to test that INFO is returned corre
ctly. */

		if (zerot) {
		    if (imat == 3) {
			izero = 1;
		    } else if (imat == 4) {
			izero = n;
		    } else {
			izero = n / 2 + 1;
		    }

		    if (imat < 6) {

/*                    Set row and column IZERO to zero
. */

			if (iuplo == 1) {
			    ioff = (izero - 1) * lda;
			    i__3 = izero - 1;
			    for (i = 1; i <= i__3; ++i) {
				i__4 = ioff + i;
				a[i__4].r = 0.f, a[i__4].i = 0.f;
/* L20: */
			    }
			    ioff += izero;
			    i__3 = n;
			    for (i = izero; i <= i__3; ++i) {
				i__4 = ioff;
				a[i__4].r = 0.f, a[i__4].i = 0.f;
				ioff += lda;
/* L30: */
			    }
			} else {
			    ioff = izero;
			    i__3 = izero - 1;
			    for (i = 1; i <= i__3; ++i) {
				i__4 = ioff;
				a[i__4].r = 0.f, a[i__4].i = 0.f;
				ioff += lda;
/* L40: */
			    }
			    ioff -= izero;
			    i__3 = n;
			    for (i = izero; i <= i__3; ++i) {
				i__4 = ioff + i;
				a[i__4].r = 0.f, a[i__4].i = 0.f;
/* L50: */
			    }
			}
		    } else {
			ioff = 0;
			if (iuplo == 1) {

/*                       Set the first IZERO rows 
and columns to zero. */

			    i__3 = n;
			    for (j = 1; j <= i__3; ++j) {
				i2 = min(j,izero);
				i__4 = i2;
				for (i = 1; i <= i__4; ++i) {
				    i__5 = ioff + i;
				    a[i__5].r = 0.f, a[i__5].i = 0.f;
/* L60: */
				}
				ioff += lda;
/* L70: */
			    }
			} else {

/*                       Set the last IZERO rows a
nd columns to zero. */

			    i__3 = n;
			    for (j = 1; j <= i__3; ++j) {
				i1 = max(j,izero);
				i__4 = n;
				for (i = i1; i <= i__4; ++i) {
				    i__5 = ioff + i;
				    a[i__5].r = 0.f, a[i__5].i = 0.f;
/* L80: */
				}
				ioff += lda;
/* L90: */
			    }
			}
		    }
		} else {
		    izero = 0;
		}

/*              Set the imaginary part of the diagonals. */

		i__3 = lda + 1;
		claipd_(&n, &a[1], &i__3, &c__0);

/*              Do for each value of NB in NBVAL */

		i__3 = *nnb;
		for (inb = 1; inb <= i__3; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/*                 Compute the L*D*L' or U*D*U' factorizat
ion of the   
                   matrix. */

		    clacpy_(uplo, &n, &n, &a[1], &lda, &afac[1], &lda);
		    lwork = max(2,nb) * lda;
		    s_copy(srnamc_1.srnamt, "CHETRF", 6L, 6L);
		    chetrf_(uplo, &n, &afac[1], &lda, &iwork[1], &ainv[1], &
			    lwork, &info);

/*                 Adjust the expected value of INFO to ac
count for   
                   pivoting. */

		    k = izero;
		    if (k > 0) {
L100:
			if (iwork[k] < 0) {
			    if (iwork[k] != -k) {
				k = -iwork[k];
				goto L100;
			    }
			} else if (iwork[k] != k) {
			    k = iwork[k];
			    goto L100;
			}
		    }

/*                 Check error code from CHETRF. */

		    if (info != k) {
			alaerh_(path, "CHETRF", &info, &k, uplo, &n, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
		    }
		    trfcon = FALSE_;

/* +    TEST 1   
                   Reconstruct matrix from factors and com
pute residual. */

		    chet01_(uplo, &n, &a[1], &lda, &afac[1], &lda, &iwork[1], 
			    &ainv[1], &lda, &rwork[1], result);
		    nt = 1;

/*                 Skip the other tests if this is not the
 first block   
                   size. */

		    if (inb > 1) {
			goto L120;
		    }

/*                 Do only the condition estimate if INFO 
is not 0. */

		    if (info != 0) {
			trfcon = TRUE_;
			rcondc = 0.f;
			goto L110;
		    }

/* +    TEST 2   
                   Form the inverse and compute the residu
al. */

		    clacpy_(uplo, &n, &n, &afac[1], &lda, &ainv[1], &lda);
		    s_copy(srnamc_1.srnamt, "CHETRI", 6L, 6L);
		    chetri_(uplo, &n, &ainv[1], &lda, &iwork[1], &work[1], &
			    info);

/*                 Check error code from CHETRI. */

		    if (info != 0) {
			alaerh_(path, "CHETRI", &info, &c_n1, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    cpot03_(uplo, &n, &a[1], &lda, &ainv[1], &lda, &work[1], &
			    lda, &rwork[1], &rcondc, &result[1]);

/* +    TEST 3   
                   Solve and compute residual for  A * X =
 B. */

		    s_copy(srnamc_1.srnamt, "CLARHS", 6L, 6L);
		    clarhs_(path, xtype, uplo, " ", &n, &n, &kl, &ku, nrhs, &
			    a[1], &lda, &xact[1], &lda, &b[1], &lda, iseed, &
			    info);
		    *(unsigned char *)xtype = 'C';
		    clacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

		    s_copy(srnamc_1.srnamt, "CHETRS", 6L, 6L);
		    chetrs_(uplo, &n, nrhs, &afac[1], &lda, &iwork[1], &x[1], 
			    &lda, &info);

/*                 Check error code from CHETRS. */

		    if (info != 0) {
			alaerh_(path, "CHETRS", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    clacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda)
			    ;
		    cpot02_(uplo, &n, nrhs, &a[1], &lda, &x[1], &lda, &work[1]
			    , &lda, &rwork[1], &result[2]);

/* +    TEST 4   
                   Check solution from generated exact sol
ution. */

		    cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[3]);

/* +    TESTS 5, 6, and 7   
                   Use iterative refinement to improve the
 solution. */

		    s_copy(srnamc_1.srnamt, "CHERFS", 6L, 6L);
		    cherfs_(uplo, &n, nrhs, &a[1], &lda, &afac[1], &lda, &
			    iwork[1], &b[1], &lda, &x[1], &lda, &rwork[1], &
			    rwork[*nrhs + 1], &work[1], &rwork[(*nrhs << 1) + 
			    1], &info);

/*                 Check error code from CHERFS. */

		    if (info != 0) {
			alaerh_(path, "CHERFS", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[4]);
		    cpot05_(uplo, &n, nrhs, &a[1], &lda, &b[1], &lda, &x[1], &
			    lda, &xact[1], &lda, &rwork[1], &rwork[*nrhs + 1],
			     &result[5]);
		    nt = 7;

/* +    TEST 8   
                   Get an estimate of RCOND = 1/CNDNUM. */

L110:
		    anorm = clanhe_("1", uplo, &n, &a[1], &lda, &rwork[1]);
		    s_copy(srnamc_1.srnamt, "CHECON", 6L, 6L);
		    checon_(uplo, &n, &afac[1], &lda, &iwork[1], &anorm, &
			    rcond, &work[1], &info);

/*                 Check error code from CHECON. */

		    if (info != 0) {
			alaerh_(path, "CHECON", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    result[7] = sget06_(&rcond, &rcondc);
		    ++nt;

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

L120:
		    i__4 = nt;
		    for (k = 1; k <= i__4; ++k) {
			ktest = k;
			if (trfcon && k == 2) {
			    ktest = 8;
			}
			if (result[ktest - 1] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___41.ciunit = *nout;
			    s_wsfe(&io___41);
			    do_fio(&c__1, uplo, 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ktest, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&result[ktest - 1], (ftnlen)
				    sizeof(real));
			    e_wsfe();
			    ++nfail;
			}
/* L130: */
		    }
		    nrun += nt;
/* L140: */
		}

L150:
		;
	    }
L160:
	    ;
	}
/* L170: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of CCHKHE */

} /* cchkhe_ */

