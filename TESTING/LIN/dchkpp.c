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

/* Subroutine */ int dchkpp_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, integer *nmax, 
	doublereal *a, doublereal *afac, doublereal *ainv, doublereal *b, 
	doublereal *x, doublereal *xact, doublereal *work, doublereal *rwork, 
	integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";
    static char packs[1*2] = "C" "R";

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO = '\002,a1,\002', N =\002,i5,\002, "
	    "type \002,i2,\002, test \002,i2,\002, ratio =\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ioff, mode, imat, info;
    static char path[3], dist[1], uplo[1], type[1];
    static integer nrun, i;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer k, n;
    extern /* Subroutine */ int dget04_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *);
    static integer nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    extern /* Subroutine */ int dppt01_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), dppt02_(char *,
	     integer *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *), 
	    dppt03_(char *, integer *, doublereal *, doublereal *, doublereal 
	    *, integer *, doublereal *, doublereal *, doublereal *);
    static doublereal anorm;
    extern /* Subroutine */ int dppt05_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *), dcopy_(integer *, doublereal *, integer *, doublereal *,
	     integer *);
    static integer iuplo, izero, nerrs;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int dlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, nt;
    static doublereal rcondc;
    static char packit[1];
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    dlarhs_(char *, char *, char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *);
    extern doublereal dlansp_(char *, char *, integer *, doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal cndnum;
    extern /* Subroutine */ int dlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublereal *, integer *, doublereal 
	    *, integer *), dppcon_(char *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *), derrpo_(char *, integer *), dpprfs_(
	    char *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *), 
	    dpptrf_(char *, integer *, doublereal *, integer *), 
	    dpptri_(char *, integer *, doublereal *, integer *), 
	    dpptrs_(char *, integer *, integer *, doublereal *, doublereal *, 
	    integer *, integer *);
    static doublereal result[8];
    static integer lda, npp;

    /* Fortran I/O blocks */
    static cilist io___36 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DCHKPP tests DPPTRF, -TRI, -TRS, -RFS, and -CON   

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

    A       (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*(NMAX+1)/2)   

    AFAC    (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*(NMAX+1)/2)   

    AINV    (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*(NMAX+1)/2)   

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
    --afac;
    --a;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'D';
    s_copy(path + 1, "PP", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	derrpo_(path, nout);
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

	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L80;
	    }

/*           Skip types 3, 4, or 5 if the matrix size is too small
. */

	    zerot = imat >= 3 && imat <= 5;
	    if (zerot && n < imat - 2) {
		goto L80;
	    }

/*           Do first for UPLO = 'U', then for UPLO = 'L' */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		*(unsigned char *)packit = *(unsigned char *)&packs[iuplo - 1]
			;

/*              Set up parameters with DLATB4 and generate a t
est matrix   
                with DLATMS. */

		dlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "DLATMS", 6L, 6L);
		dlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, packit, &a[1], &lda, &work[1], &
			info);

/*              Check error code from DLATMS. */

		if (info != 0) {
		    alaerh_(path, "DLATMS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L70;
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

/*                 Set row and column IZERO of A to 0. */

		    if (iuplo == 1) {
			ioff = (izero - 1) * izero / 2;
			i__3 = izero - 1;
			for (i = 1; i <= i__3; ++i) {
			    a[ioff + i] = 0.;
/* L20: */
			}
			ioff += izero;
			i__3 = n;
			for (i = izero; i <= i__3; ++i) {
			    a[ioff] = 0.;
			    ioff += i;
/* L30: */
			}
		    } else {
			ioff = izero;
			i__3 = izero - 1;
			for (i = 1; i <= i__3; ++i) {
			    a[ioff] = 0.;
			    ioff = ioff + n - i;
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
		    izero = 0;
		}

/*              Compute the L*L' or U'*U factorization of the 
matrix. */

		npp = n * (n + 1) / 2;
		dcopy_(&npp, &a[1], &c__1, &afac[1], &c__1);
		s_copy(srnamc_1.srnamt, "DPPTRF", 6L, 6L);
		dpptrf_(uplo, &n, &afac[1], &info);

/*              Check error code from DPPTRF. */

		if (info != izero) {
		    alaerh_(path, "DPPTRF", &info, &izero, uplo, &n, &n, &
			    c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L70;
		}

/*              Skip the tests if INFO is not 0. */

		if (info != 0) {
		    goto L70;
		}

/* +    TEST 1   
                Reconstruct matrix from factors and compute re
sidual. */

		dcopy_(&npp, &afac[1], &c__1, &ainv[1], &c__1);
		dppt01_(uplo, &n, &a[1], &ainv[1], &rwork[1], result);

/* +    TEST 2   
                Form the inverse and compute the residual. */

		dcopy_(&npp, &afac[1], &c__1, &ainv[1], &c__1);
		s_copy(srnamc_1.srnamt, "DPPTRI", 6L, 6L);
		dpptri_(uplo, &n, &ainv[1], &info);

/*              Check error code from DPPTRI. */

		if (info != 0) {
		    alaerh_(path, "DPPTRI", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		}

		dppt03_(uplo, &n, &a[1], &ainv[1], &work[1], &lda, &rwork[1], 
			&rcondc, &result[1]);

/* +    TEST 3   
                Solve and compute residual for  A * X = B. */

		s_copy(srnamc_1.srnamt, "DLARHS", 6L, 6L);
		dlarhs_(path, xtype, uplo, " ", &n, &n, &kl, &ku, nrhs, &a[1],
			 &lda, &xact[1], &lda, &b[1], &lda, iseed, &info);
		*(unsigned char *)xtype = 'C';
		dlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

		s_copy(srnamc_1.srnamt, "DPPTRS", 6L, 6L);
		dpptrs_(uplo, &n, nrhs, &afac[1], &x[1], &lda, &info);

/*              Check error code from DPPTRS. */

		if (info != 0) {
		    alaerh_(path, "DPPTRS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		dlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
		dppt02_(uplo, &n, nrhs, &a[1], &x[1], &lda, &work[1], &lda, &
			rwork[1], &result[2]);

/* +    TEST 4   
                Check solution from generated exact solution. 
*/

		dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[3]);

/* +    TESTS 5, 6, and 7   
                Use iterative refinement to improve the soluti
on. */

		s_copy(srnamc_1.srnamt, "DPPRFS", 6L, 6L);
		dpprfs_(uplo, &n, nrhs, &a[1], &afac[1], &b[1], &lda, &x[1], &
			lda, &rwork[1], &rwork[*nrhs + 1], &work[1], &iwork[1]
			, &info);

/*              Check error code from DPPRFS. */

		if (info != 0) {
		    alaerh_(path, "DPPRFS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[4]);
		dppt05_(uplo, &n, nrhs, &a[1], &b[1], &lda, &x[1], &lda, &
			xact[1], &lda, &rwork[1], &rwork[*nrhs + 1], &result[
			5]);

/* +    TEST 8   
                Get an estimate of RCOND = 1/CNDNUM. */

		anorm = dlansp_("1", uplo, &n, &a[1], &rwork[1]);
		s_copy(srnamc_1.srnamt, "DPPCON", 6L, 6L);
		dppcon_(uplo, &n, &afac[1], &anorm, &rcond, &work[1], &iwork[
			1], &info);

/*              Check error code from DPPCON. */

		if (info != 0) {
		    alaerh_(path, "DPPCON", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		}

		result[7] = dget06_(&rcond, &rcondc);
		nt = 8;

/*              Print information about the tests that did not
 pass   
                the threshold. */

		i__3 = nt;
		for (k = 1; k <= i__3; ++k) {
		    if (result[k - 1] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			io___36.ciunit = *nout;
			s_wsfe(&io___36);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[k - 1], (ftnlen)sizeof(
				doublereal));
			e_wsfe();
			++nfail;
		    }
/* L60: */
		}
		nrun += nt;

L70:
		;
	    }
L80:
	    ;
	}
/* L90: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of DCHKPP */

} /* dchkpp_ */

