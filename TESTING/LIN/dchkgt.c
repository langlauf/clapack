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

static integer c__3 = 3;
static integer c__0 = 0;
static integer c_n1 = -1;
static integer c__1 = 1;
static integer c__2 = 2;
static doublereal c_b48 = 1.;
static doublereal c_b49 = 0.;

/* Subroutine */ int dchkgt_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, doublereal *a, 
	doublereal *af, doublereal *b, doublereal *x, doublereal *xact, 
	doublereal *work, doublereal *rwork, integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 0,0,0,1 };
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 N =\002,i5,\002, type \002,i2,\002, te"
	    "st \002,i2,\002, ratio = \002,g12.5)";
    static char fmt_9998[] = "(\002 TRANS='\002,a1,\002', N =\002,i5,\002, t"
	    "ype \002,i2,\002, test \002,i2,\002, ratio = \002,g12.5)";
    static char fmt_9997[] = "(\002 NORM ='\002,a1,\002', N =\002,i5,\002, t"
	    "ype \002,i2,\002, test \002,i2,\002, ratio = \002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static doublereal cond;
    static integer mode, koff, imat, info;
    static char path[3], dist[1], norm[1], type[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer m, n;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dget04_(integer *, integer *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *, doublereal *);
    static integer nfail, iseed[4];
    static doublereal z[3];
    extern doublereal dget06_(doublereal *, doublereal *);
    extern /* Subroutine */ int dgtt01_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *), dgtt02_(char *, integer *, integer *, doublereal *,
	     doublereal *, doublereal *, doublereal *, integer *, doublereal *
	    , integer *, doublereal *, doublereal *);
    static doublereal rcond;
    extern /* Subroutine */ int dgtt05_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    static integer nimat;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static doublereal anorm;
    static integer itran;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static char trans[1];
    static integer izero, nerrs, k1;
    static logical zerot;
    extern /* Subroutine */ int dlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, ix, nt;
    static doublereal rcondc;
    extern doublereal dlangt_(char *, integer *, doublereal *, doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int derrge_(char *, integer *), dlagtm_(
	    char *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *, integer *), dlacpy_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *), dgtcon_(char *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, integer *, integer *), alasum_(char *, 
	    integer *, integer *, integer *, integer *), dlatms_(
	    integer *, integer *, char *, integer *, char *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, char 
	    *, doublereal *, integer *, doublereal *, integer *), dlarnv_(integer *, integer *, integer *, 
	    doublereal *);
    static doublereal ainvnm;
    extern /* Subroutine */ int dgtrfs_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *), dgttrf_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *);
    static logical trfcon;
    extern /* Subroutine */ int dgttrs_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, integer *);
    static doublereal result[7];
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___29 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9997, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DCHKGT tests DGTTRF, -TRS, -RFS, and -CON   

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

    THRESH  (input) DOUBLE PRECISION   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    A       (workspace) DOUBLE PRECISION array, dimension (NMAX*4)   

    AF      (workspace) DOUBLE PRECISION array, dimension (NMAX*4)   

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
    --af;
    --a;
    --nval;
    --dotype;

    /* Function Body */

    *(unsigned char *)path = 'D';
    s_copy(path + 1, "GT", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	derrge_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {

/*        Do for each value of N in NVAL. */

	n = nval[in];
/* Computing MAX */
	i__2 = n - 1;
	m = max(i__2,0);
	lda = max(1,n);
	nimat = 12;
	if (n <= 0) {
	    nimat = 1;
	}

	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L90;
	    }

/*           Set up parameters with DLATB4. */

	    dlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &cond,
		     dist);

	    zerot = imat >= 8 && imat <= 10;
	    if (imat <= 6) {

/*              Types 1-6:  generate matrices of known conditi
on number.   

   Computing MAX */
		i__3 = 2 - ku, i__4 = 3 - max(1,n);
		koff = max(i__3,i__4);
		s_copy(srnamc_1.srnamt, "DLATMS", 6L, 6L);
		dlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cond, &
			anorm, &kl, &ku, "Z", &af[koff], &c__3, &work[1], &
			info);

/*              Check the error code from DLATMS. */

		if (info != 0) {
		    alaerh_(path, "DLATMS", &info, &c__0, " ", &n, &n, &kl, &
			    ku, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L90;
		}
		izero = 0;

		if (n > 1) {
		    i__3 = n - 1;
		    dcopy_(&i__3, &af[4], &c__3, &a[1], &c__1);
		    i__3 = n - 1;
		    dcopy_(&i__3, &af[3], &c__3, &a[n + m + 1], &c__1);
		}
		dcopy_(&n, &af[2], &c__3, &a[m + 1], &c__1);
	    } else {

/*              Types 7-12:  generate tridiagonal matrices wit
h   
                unknown condition numbers. */

		if (! zerot || ! dotype[7]) {

/*                 Generate a matrix with elements from [-
1,1]. */

		    i__3 = n + (m << 1);
		    dlarnv_(&c__2, iseed, &i__3, &a[1]);
		    if (anorm != 1.) {
			i__3 = n + (m << 1);
			dscal_(&i__3, &anorm, &a[1], &c__1);
		    }
		} else if (izero > 0) {

/*                 Reuse the last matrix by copying back t
he zeroed out   
                   elements. */

		    if (izero == 1) {
			a[n] = z[1];
			if (n > 1) {
			    a[1] = z[2];
			}
		    } else if (izero == n) {
			a[n * 3 - 2] = z[0];
			a[(n << 1) - 1] = z[1];
		    } else {
			a[(n << 1) - 2 + izero] = z[0];
			a[n - 1 + izero] = z[1];
			a[izero] = z[2];
		    }
		}

/*              If IMAT > 7, set one column of the matrix to 0
. */

		if (! zerot) {
		    izero = 0;
		} else if (imat == 8) {
		    izero = 1;
		    z[1] = a[n];
		    a[n] = 0.;
		    if (n > 1) {
			z[2] = a[1];
			a[1] = 0.;
		    }
		} else if (imat == 9) {
		    izero = n;
		    z[0] = a[n * 3 - 2];
		    z[1] = a[(n << 1) - 1];
		    a[n * 3 - 2] = 0.;
		    a[(n << 1) - 1] = 0.;
		} else {
		    izero = (n + 1) / 2;
		    i__3 = n - 1;
		    for (i = izero; i <= i__3; ++i) {
			a[(n << 1) - 2 + i] = 0.;
			a[n - 1 + i] = 0.;
			a[i] = 0.;
/* L20: */
		    }
		    a[n * 3 - 2] = 0.;
		    a[(n << 1) - 1] = 0.;
		}
	    }

/* +    TEST 1   
             Factor A as L*U and compute the ratio   
                norm(L*U - A) / (n * norm(A) * EPS ) */

	    i__3 = n + (m << 1);
	    dcopy_(&i__3, &a[1], &c__1, &af[1], &c__1);
	    s_copy(srnamc_1.srnamt, "DGTTRF", 6L, 6L);
	    dgttrf_(&n, &af[1], &af[m + 1], &af[n + m + 1], &af[n + (m << 1) 
		    + 1], &iwork[1], &info);

/*           Check error code from DGTTRF. */

	    if (info != izero) {
		alaerh_(path, "DGTTRF", &info, &izero, " ", &n, &n, &c__1, &
			c__1, &c_n1, &imat, &nfail, &nerrs, nout);
	    }
	    trfcon = info != 0;

	    dgtt01_(&n, &a[1], &a[m + 1], &a[n + m + 1], &af[1], &af[m + 1], &
		    af[n + m + 1], &af[n + (m << 1) + 1], &iwork[1], &work[1],
		     &lda, &rwork[1], result);

/*           Print information about the tests so far that did not
   
             pass the threshold. */

	    if (result[0] >= *thresh) {
		if (nfail == 0 && nerrs == 0) {
		    alahd_(nout, path);
		}
		io___29.ciunit = *nout;
		s_wsfe(&io___29);
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&result[0], (ftnlen)sizeof(doublereal));
		e_wsfe();
		++nfail;
	    }
	    ++nrun;

/*           Generate NRHS random solution vectors. */

	    if (! trfcon) {
		ix = 1;
		i__3 = *nrhs;
		for (j = 1; j <= i__3; ++j) {
		    dlarnv_(&c__2, iseed, &n, &xact[ix]);
		    ix += lda;
/* L30: */
		}
	    }

	    for (itran = 1; itran <= 3; ++itran) {
		*(unsigned char *)trans = *(unsigned char *)&transs[itran - 1]
			;
		if (itran == 1) {
		    *(unsigned char *)norm = 'O';
		} else {
		    *(unsigned char *)norm = 'I';
		}

/*              Skip all but the condition estimate if TRFCON 
is true. */

		if (trfcon) {
		    k1 = 7;
		    rcondc = 0.;
		    goto L60;
		}

		k1 = 2;
		if (itran <= 2) {

/*                 Compute RCONDC = 1 / (norm(A) * norm(in
v(A)) */

		    anorm = dlangt_(norm, &n, &a[1], &a[m + 1], &a[n + m + 1]);

/*                 Use DGTTRS to solve for one column at a
 time of   
                   inv(A), computing the maximum column su
m as we go. */

		    ainvnm = 0.;
		    i__3 = n;
		    for (i = 1; i <= i__3; ++i) {
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    x[j] = 0.;
/* L40: */
			}
			x[i] = 1.;
			dgttrs_(trans, &n, &c__1, &af[1], &af[m + 1], &af[n + 
				m + 1], &af[n + (m << 1) + 1], &iwork[1], &x[
				1], &lda, &info);
/* Computing MAX */
			d__1 = ainvnm, d__2 = dasum_(&n, &x[1], &c__1);
			ainvnm = max(d__1,d__2);
/* L50: */
		    }
		    if (anorm <= 0. || ainvnm <= 0.) {
			rcondc = 1.;
		    } else {
			rcondc = 1. / anorm / ainvnm;
		    }
		}

/*              Set the right hand side. */

		dlagtm_(trans, &n, nrhs, &c_b48, &a[1], &a[m + 1], &a[n + m + 
			1], &xact[1], &lda, &c_b49, &b[1], &lda);

/* +    TEST 2   
                Solve op(A) * X = B and compute the residual. 
*/

		dlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);
		s_copy(srnamc_1.srnamt, "DGTTRS", 6L, 6L);
		dgttrs_(trans, &n, nrhs, &af[1], &af[m + 1], &af[n + m + 1], &
			af[n + (m << 1) + 1], &iwork[1], &x[1], &lda, &info);

/*              Check error code from DGTTRS. */

		if (info != 0) {
		    alaerh_(path, "DGTTRS", &info, &c__0, trans, &n, &n, &
			    c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		dlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
		dgtt02_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &x[
			1], &lda, &work[1], &lda, &rwork[1], &result[1]);

/* +    TEST 3   
                Check solution from generated exact solution. 
*/

		dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[2]);

/* +    TESTS 4, 5, and 6   
                Use iterative refinement to improve the soluti
on. */

		s_copy(srnamc_1.srnamt, "DGTRFS", 6L, 6L);
		dgtrfs_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &af[
			1], &af[m + 1], &af[n + m + 1], &af[n + (m << 1) + 1],
			 &iwork[1], &b[1], &lda, &x[1], &lda, &rwork[1], &
			rwork[*nrhs + 1], &work[1], &iwork[n + 1], &info);

/*              Check error code from DGTRFS. */

		if (info != 0) {
		    alaerh_(path, "DGTRFS", &info, &c__0, trans, &n, &n, &
			    c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[3]);
		dgtt05_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &b[
			1], &lda, &x[1], &lda, &xact[1], &lda, &rwork[1], &
			rwork[*nrhs + 1], &result[4]);
		nt = 6;
L60:

/* +    TEST 7   
                Estimate the reciprocal of the condition numbe
r of the   
                matrix. */

		if (itran <= 2) {
		    s_copy(srnamc_1.srnamt, "DGTCON", 6L, 6L);
		    dgtcon_(norm, &n, &af[1], &af[m + 1], &af[n + m + 1], &af[
			    n + (m << 1) + 1], &iwork[1], &anorm, &rcond, &
			    work[1], &iwork[n + 1], &info);

/*                 Check error code from DGTCON. */

		    if (info != 0) {
			alaerh_(path, "DGTCON", &info, &c__0, norm, &n, &n, &
				c_n1, &c_n1, &c_n1, &imat, &nfail, &nerrs, 
				nout);
		    }

		    result[6] = dget06_(&rcond, &rcondc);
		    nt = 7;
		}

/*              Print information about the tests that did not
 pass the   
                threshold. */

		i__3 = nt;
		for (k = k1; k <= i__3; ++k) {
		    if (result[k - 1] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			if (k < 7) {
			    io___41.ciunit = *nout;
			    s_wsfe(&io___41);
			    do_fio(&c__1, trans, 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(doublereal));
			    e_wsfe();
			} else {
			    io___42.ciunit = *nout;
			    s_wsfe(&io___42);
			    do_fio(&c__1, norm, 1L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(doublereal));
			    e_wsfe();
			}
			++nfail;
		    }
/* L70: */
		}
		nrun = nrun + nt - k1 + 1;
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

/*     End of DCHKGT */

} /* dchkgt_ */

