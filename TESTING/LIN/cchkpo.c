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

/* Subroutine */ int cchkpo_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, real *thresh, logical *
	tsterr, integer *nmax, complex *a, complex *afac, complex *ainv, 
	complex *b, complex *x, complex *xact, complex *work, real *rwork, 
	integer *nout)
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
    static integer nrun, i;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer k, n;
    extern /* Subroutine */ int cget04_(integer *, integer *, complex *, 
	    integer *, complex *, integer *, real *, real *);
    static integer nfail, iseed[4];
    static real rcond;
    extern /* Subroutine */ int cpot01_(char *, integer *, complex *, integer 
	    *, complex *, integer *, real *, real *), cpot02_(char *, 
	    integer *, integer *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, real *, real *);
    static integer nimat;
    extern doublereal sget06_(real *, real *);
    extern /* Subroutine */ int cpot03_(char *, integer *, complex *, integer 
	    *, complex *, integer *, complex *, integer *, real *, real *, 
	    real *), cpot05_(char *, integer *, integer *, complex *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, real *, real *, real *);
    static real anorm;
    static integer iuplo, izero, nerrs;
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
	    integer *, integer *, integer *, integer *), claipd_(integer *, complex *, integer *, integer *);
    static integer ku, nt;
    static real rcondc;
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), clarhs_(char *, char 
	    *, char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, integer *, integer *), 
	    cpocon_(char *, integer *, complex *, integer *, real *, real *, 
	    complex *, real *, integer *), alasum_(char *, integer *, 
	    integer *, integer *, integer *);
    static real cndnum;
    extern /* Subroutine */ int clatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, complex *, integer *, complex *, integer *), cerrpo_(char *, integer *), cporfs_(char 
	    *, integer *, integer *, complex *, integer *, complex *, integer 
	    *, complex *, integer *, complex *, integer *, real *, real *, 
	    complex *, real *, integer *), cpotrf_(char *, integer *, 
	    complex *, integer *, integer *), xlaenv_(integer *, 
	    integer *), cpotri_(char *, integer *, complex *, integer *, 
	    integer *), cpotrs_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *, integer *);
    static real result[8];
    static integer lda, inb;

    /* Fortran I/O blocks */
    static cilist io___35 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CCHKPO tests CPOTRF, -TRI, -TRS, -RFS, and -CON   

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
    --afac;
    --a;
    --nbval;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'C';
    s_copy(path + 1, "PO", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	cerrpo_(path, nout);
    }
    infoc_1.infot = 0;

/*     Do for each value of N in NVAL */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];
	lda = max(n,1);
	*(unsigned char *)xtype = 'N';
	nimat = 9;
	if (n <= 0) {
	    nimat = 1;
	}

	izero = 0;
	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L100;
	    }

/*           Skip types 3, 4, or 5 if the matrix size is too small
. */

	    zerot = imat >= 3 && imat <= 5;
	    if (zerot && n < imat - 2) {
		goto L100;
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
		    goto L90;
		}

/*              For types 3-5, zero one row and column of the 
matrix to   
                test that INFO is returned correctly. */

		if (zerot) {
		    if (imat == 3) {
			izero = 1;
		    } else if (imat == 4) {
			izero = n;
		    } else {
			izero = n / 2 + 1;
		    }
		    ioff = (izero - 1) * lda;

/*                 Set row and column IZERO of A to 0. */

		    if (iuplo == 1) {
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

/*                 Compute the L*L' or U'*U factorization 
of the matrix. */

		    clacpy_(uplo, &n, &n, &a[1], &lda, &afac[1], &lda);
		    s_copy(srnamc_1.srnamt, "CPOTRF", 6L, 6L);
		    cpotrf_(uplo, &n, &afac[1], &lda, &info);

/*                 Check error code from CPOTRF. */

		    if (info != izero) {
			alaerh_(path, "CPOTRF", &info, &izero, uplo, &n, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
			goto L80;
		    }

/*                 Skip the tests if INFO is not 0. */

		    if (info != 0) {
			goto L80;
		    }

/* +    TEST 1   
                   Reconstruct matrix from factors and com
pute residual. */

		    clacpy_(uplo, &n, &n, &afac[1], &lda, &ainv[1], &lda);
		    cpot01_(uplo, &n, &a[1], &lda, &ainv[1], &lda, &rwork[1], 
			    result);

/* +    TEST 2   
                   Form the inverse and compute the residu
al. */

		    clacpy_(uplo, &n, &n, &afac[1], &lda, &ainv[1], &lda);
		    s_copy(srnamc_1.srnamt, "CPOTRI", 6L, 6L);
		    cpotri_(uplo, &n, &ainv[1], &lda, &info);

/*                 Check error code from CPOTRI. */

		    if (info != 0) {
			alaerh_(path, "CPOTRI", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    cpot03_(uplo, &n, &a[1], &lda, &ainv[1], &lda, &work[1], &
			    lda, &rwork[1], &rcondc, &result[1]);

/*                 Skip the rest of the tests unless this 
is the first   
                   blocksize. */

		    if (inb != 1) {
			nt = 2;
			goto L60;
		    }

/* +    TEST 3   
                   Solve and compute residual for A * X = 
B . */

		    s_copy(srnamc_1.srnamt, "CLARHS", 6L, 6L);
		    clarhs_(path, xtype, uplo, " ", &n, &n, &kl, &ku, nrhs, &
			    a[1], &lda, &xact[1], &lda, &b[1], &lda, iseed, &
			    info);
		    *(unsigned char *)xtype = 'C';
		    clacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

		    s_copy(srnamc_1.srnamt, "CPOTRS", 6L, 6L);
		    cpotrs_(uplo, &n, nrhs, &afac[1], &lda, &x[1], &lda, &
			    info);

/*                 Check error code from CPOTRS. */

		    if (info != 0) {
			alaerh_(path, "CPOTRS", &info, &c__0, uplo, &n, &n, &
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

		    s_copy(srnamc_1.srnamt, "CPORFS", 6L, 6L);
		    cporfs_(uplo, &n, nrhs, &a[1], &lda, &afac[1], &lda, &b[1]
			    , &lda, &x[1], &lda, &rwork[1], &rwork[*nrhs + 1],
			     &work[1], &rwork[(*nrhs << 1) + 1], &info);

/*                 Check error code from CPORFS. */

		    if (info != 0) {
			alaerh_(path, "CPORFS", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }

		    cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[4]);
		    cpot05_(uplo, &n, nrhs, &a[1], &lda, &b[1], &lda, &x[1], &
			    lda, &xact[1], &lda, &rwork[1], &rwork[*nrhs + 1],
			     &result[5]);

/* +    TEST 8   
                   Get an estimate of RCOND = 1/CNDNUM. */

		    anorm = clanhe_("1", uplo, &n, &a[1], &lda, &rwork[1]);
		    s_copy(srnamc_1.srnamt, "CPOCON", 6L, 6L);
		    cpocon_(uplo, &n, &afac[1], &lda, &anorm, &rcond, &work[1]
			    , &rwork[1], &info);

/*                 Check error code from CPOCON. */

		    if (info != 0) {
			alaerh_(path, "CPOCON", &info, &c__0, uplo, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    result[7] = sget06_(&rcond, &rcondc);
		    nt = 8;

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

L60:
		    i__4 = nt;
		    for (k = 1; k <= i__4; ++k) {
			if (result[k - 1] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___35.ciunit = *nout;
			    s_wsfe(&io___35);
			    do_fio(&c__1, uplo, 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(real));
			    e_wsfe();
			    ++nfail;
			}
/* L70: */
		    }
		    nrun += nt;

L80:
		    ;
		}

L90:
		;
	    }
L100:
	    ;
	}
/* L110: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of CCHKPO */

} /* cchkpo_ */

