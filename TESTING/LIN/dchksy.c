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

static integer c__2 = 2;
static integer c__0 = 0;
static integer c_n1 = -1;
static integer c__1 = 1;

/* Subroutine */ int dchksy_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, doublereal *thresh, 
	logical *tsterr, integer *nmax, doublereal *a, doublereal *afac, 
	doublereal *ainv, doublereal *b, doublereal *x, doublereal *xact, 
	doublereal *work, doublereal *rwork, integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO = '\002,a1,\002', N =\002,i5,\002, "
	    "NB =\002,i4,\002, type \002,i2,\002, test \002,i2,\002, ratio "
	    "=\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ioff, mode, imat, info;
    static char path[3], dist[1], uplo[1], type[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer n;
    extern /* Subroutine */ int dget04_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *);
    static integer nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    extern /* Subroutine */ int dpot02_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *), dpot03_(char *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *), dpot05_(char *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *);
    static doublereal anorm;
    extern /* Subroutine */ int dsyt01_(char *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *);
    static integer iuplo, izero, i1, i2, ktest, lwork, nerrs;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int dlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer nb, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, nt;
    static doublereal rcondc;
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    dlarhs_(char *, char *, char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *), alasum_(char *, 
	    integer *, integer *, integer *, integer *);
    static doublereal cndnum;
    extern /* Subroutine */ int dlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublereal *, integer *, doublereal 
	    *, integer *);
    extern doublereal dlansy_(char *, char *, integer *, doublereal *, 
	    integer *, doublereal *);
    static logical trfcon;
    extern /* Subroutine */ int xlaenv_(integer *, integer *), dsycon_(char *,
	     integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *), 
	    derrsy_(char *, integer *), dsyrfs_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *), dsytrf_(char *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, integer *);
    static doublereal result[8];
    extern /* Subroutine */ int dsytri_(char *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, integer *), dsytrs_(
	    char *, integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, integer *);
    static integer lda, inb;

    /* Fortran I/O blocks */
    static cilist io___41 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DCHKSY tests DSYTRF, -TRI, -TRS, -RFS, and -CON.   

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

    THRESH  (input) DOUBLE PRECISION   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    NMAX    (input) INTEGER   
            The maximum value permitted for N, used in dimensioning the   
            work arrays.   

    A       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    AFAC    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    AINV    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    X       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    XACT    (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    WORK    (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max(NMAX,2*NRHS))   

    IWORK   (workspace) INTEGER array, dimension (2*NMAX)   

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

    *(unsigned char *)path = 'D';
    s_copy(path + 1, "SY", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	derrsy_(path, nout);
    }
    infoc_1.infot = 0;
    xlaenv_(&c__2, &c__2);

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

/*              Set up parameters with DLATB4 and generate a t
est matrix   
                with DLATMS. */

		dlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "DLATMS", 6L, 6L);
		dlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, uplo, &a[1], &lda, &work[1], &info);

/*              Check error code from DLATMS. */

		if (info != 0) {
		    alaerh_(path, "DLATMS", &info, &c__0, uplo, &n, &n, &c_n1,
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
				a[ioff + i] = 0.;
/* L20: */
			    }
			    ioff += izero;
			    i__3 = n;
			    for (i = izero; i <= i__3; ++i) {
				a[ioff] = 0.;
				ioff += lda;
/* L30: */
			    }
			} else {
			    ioff = izero;
			    i__3 = izero - 1;
			    for (i = 1; i <= i__3; ++i) {
				a[ioff] = 0.;
				ioff += lda;
/* L40: */
			    }
			    ioff -= izero;
			    i__3 = n;
			    for (i = izero; i <= i__3; ++i) {
				a[ioff + i] = 0.;
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
				    a[ioff + i] = 0.;
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
				    a[ioff + i] = 0.;
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

/*              Do for each value of NB in NBVAL */

		i__3 = *nnb;
		for (inb = 1; inb <= i__3; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/*                 Compute the L*D*L' or U*D*U' factorizat
ion of the   
                   matrix. */

		    dlacpy_(uplo, &n, &n, &a[1], &lda, &afac[1], &lda);
		    lwork = max(2,nb) * lda;
		    s_copy(srnamc_1.srnamt, "DSYTRF", 6L, 6L);
		    dsytrf_(uplo, &n, &afac[1], &lda, &iwork[1], &ainv[1], &
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

/*                 Check error code from DSYTRF. */

		    if (info != k) {
			alaerh_(path, "DSYTRF", &info, &k, uplo, &n, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
		    }
		    trfcon = FALSE_;

/* +    TEST 1   
                   Reconstruct matrix from factors and com
pute residual. */

		    dsyt01_(uplo, &n, &a[1], &lda, &afac[1], &lda, &iwork[1], 
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
			rcondc = 0.;
			goto L110;
		    }

/* +    TEST 2   
                   Form the inverse and compute the residu
al. */

		    dlacpy_(uplo, &n, &n, &afac[1], &lda, &ainv[1], &lda);
		    s_copy(srnamc_1.srnamt, "DSYTRI", 6L, 6L);
		    dsytri_(uplo, &n, &ainv[1], &lda, &iwork[1], &work[1], &
			    info);

/*                 Check error code from DSYTRI. */

		    if (info != 0) {
			alaerh_(path, "DSYTRI", &info, &c_n1, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    dpot03_(uplo, &n, &a[1], &lda, &ainv[1], &lda, &work[1], &
			    lda, &rwork[1], &rcondc, &result[1]);

/* +    TEST 3   
                   Solve and compute residual for  A * X =
 B. */

		    s_copy(srnamc_1.srnamt, "DLARHS", 6L, 6L);
		    dlarhs_(path, xtype, uplo, " ", &n, &n, &kl, &ku, nrhs, &
			    a[1], &lda, &xact[1], &lda, &b[1], &lda, iseed, &
			    info);
		    *(unsigned char *)xtype = 'C';
		    dlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

		    s_copy(srnamc_1.srnamt, "DSYTRS", 6L, 6L);
		    dsytrs_(uplo, &n, nrhs, &afac[1], &lda, &iwork[1], &x[1], 
			    &lda, &info);

/*                 Check error code from DSYTRS. */

		    if (info != 0) {
			alaerh_(path, "DSYTRS", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    dlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda)
			    ;
		    dpot02_(uplo, &n, nrhs, &a[1], &lda, &x[1], &lda, &work[1]
			    , &lda, &rwork[1], &result[2]);

/* +    TEST 4   
                   Check solution from generated exact sol
ution. */

		    dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[3]);

/* +    TESTS 5, 6, and 7   
                   Use iterative refinement to improve the
 solution. */

		    s_copy(srnamc_1.srnamt, "DSYRFS", 6L, 6L);
		    dsyrfs_(uplo, &n, nrhs, &a[1], &lda, &afac[1], &lda, &
			    iwork[1], &b[1], &lda, &x[1], &lda, &rwork[1], &
			    rwork[*nrhs + 1], &work[1], &iwork[n + 1], &info);

/*                 Check error code from DSYRFS. */

		    if (info != 0) {
			alaerh_(path, "DSYRFS", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[4]);
		    dpot05_(uplo, &n, nrhs, &a[1], &lda, &b[1], &lda, &x[1], &
			    lda, &xact[1], &lda, &rwork[1], &rwork[*nrhs + 1],
			     &result[5]);
		    nt = 7;

/* +    TEST 8   
                   Get an estimate of RCOND = 1/CNDNUM. */

L110:
		    anorm = dlansy_("1", uplo, &n, &a[1], &lda, &rwork[1]);
		    s_copy(srnamc_1.srnamt, "DSYCON", 6L, 6L);
		    dsycon_(uplo, &n, &afac[1], &lda, &iwork[1], &anorm, &
			    rcond, &work[1], &iwork[n + 1], &info);

/*                 Check error code from DSYCON. */

		    if (info != 0) {
			alaerh_(path, "DSYCON", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    result[7] = dget06_(&rcond, &rcondc);
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
				    sizeof(doublereal));
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

/*     End of DCHKSY */

} /* dchksy_ */

