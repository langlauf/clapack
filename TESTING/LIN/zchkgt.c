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

/* Subroutine */ int zchkgt_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, doublecomplex *a, 
	doublecomplex *af, doublecomplex *b, doublecomplex *x, doublecomplex *
	xact, doublecomplex *work, doublereal *rwork, integer *iwork, integer 
	*nout)
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
    integer i__1, i__2, i__3, i__4, i__5;
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
    static integer m, n, nfail, iseed[4];
    static doublecomplex z[3];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    static integer itran;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    );
    static char trans[1];
    static integer izero, nerrs;
    extern /* Subroutine */ int zgtt01_(integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublereal *);
    static integer k1;
    extern /* Subroutine */ int zgtt02_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , integer *, doublecomplex *, integer *, doublereal *, doublereal 
	    *), zgtt05_(char *, integer *, integer *, doublecomplex *,
	     doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *);
    static logical zerot;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zlatb4_(char *, integer *, integer *,
	     integer *, char *, integer *, integer *, doublereal *, integer *,
	     doublereal *, char *);
    static integer in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, ix, nt;
    static doublereal rcondc;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *), alasum_(char *, integer *, integer *,
	     integer *, integer *);
    static doublereal ainvnm;
    static logical trfcon;
    extern /* Subroutine */ int zerrge_(char *, integer *);
    extern doublereal zlangt_(char *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *);
    extern /* Subroutine */ int zlagtm_(char *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublereal *, doublecomplex *, 
	    integer *), zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern doublereal dzasum_(integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zgtcon_(char *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, integer *), 
	    zlatms_(integer *, integer *, char *, integer *, char *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, char *, doublecomplex *, integer *, doublecomplex *, 
	    integer *), zlarnv_(integer *, integer *, 
	    integer *, doublecomplex *);
    static doublereal result[7];
    extern /* Subroutine */ int zgtrfs_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, doublereal *, 
	    integer *), zgttrf_(integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    integer *), zgttrs_(char *, integer *, integer *, doublecomplex *,
	     doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *);
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

    ZCHKGT tests ZGTTRF, -TRS, -RFS, and -CON   

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

    A       (workspace) COMPLEX*16 array, dimension (NMAX*4)   

    AF      (workspace) COMPLEX*16 array, dimension (NMAX*4)   

    B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max(NMAX)+2*NRHS)   

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
    --af;
    --a;
    --nval;
    --dotype;

    /* Function Body */

    *(unsigned char *)path = 'Z';
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
	zerrge_(path, nout);
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

/*           Set up parameters with ZLATB4. */

	    zlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &cond,
		     dist);

	    zerot = imat >= 8 && imat <= 10;
	    if (imat <= 6) {

/*              Types 1-6:  generate matrices of known conditi
on number.   

   Computing MAX */
		i__3 = 2 - ku, i__4 = 3 - max(1,n);
		koff = max(i__3,i__4);
		s_copy(srnamc_1.srnamt, "ZLATMS", 6L, 6L);
		zlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cond, &
			anorm, &kl, &ku, "Z", &af[koff], &c__3, &work[1], &
			info);

/*              Check the error code from ZLATMS. */

		if (info != 0) {
		    alaerh_(path, "ZLATMS", &info, &c__0, " ", &n, &n, &kl, &
			    ku, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L90;
		}
		izero = 0;

		if (n > 1) {
		    i__3 = n - 1;
		    zcopy_(&i__3, &af[4], &c__3, &a[1], &c__1);
		    i__3 = n - 1;
		    zcopy_(&i__3, &af[3], &c__3, &a[n + m + 1], &c__1);
		}
		zcopy_(&n, &af[2], &c__3, &a[m + 1], &c__1);
	    } else {

/*              Types 7-12:  generate tridiagonal matrices wit
h   
                unknown condition numbers. */

		if (! zerot || ! dotype[7]) {

/*                 Generate a matrix with elements whose r
eal and   
                   imaginary parts are from [-1,1]. */

		    i__3 = n + (m << 1);
		    zlarnv_(&c__2, iseed, &i__3, &a[1]);
		    if (anorm != 1.) {
			i__3 = n + (m << 1);
			zdscal_(&i__3, &anorm, &a[1], &c__1);
		    }
		} else if (izero > 0) {

/*                 Reuse the last matrix by copying back t
he zeroed out   
                   elements. */

		    if (izero == 1) {
			i__3 = n;
			a[i__3].r = z[1].r, a[i__3].i = z[1].i;
			if (n > 1) {
			    a[1].r = z[2].r, a[1].i = z[2].i;
			}
		    } else if (izero == n) {
			i__3 = n * 3 - 2;
			a[i__3].r = z[0].r, a[i__3].i = z[0].i;
			i__3 = (n << 1) - 1;
			a[i__3].r = z[1].r, a[i__3].i = z[1].i;
		    } else {
			i__3 = (n << 1) - 2 + izero;
			a[i__3].r = z[0].r, a[i__3].i = z[0].i;
			i__3 = n - 1 + izero;
			a[i__3].r = z[1].r, a[i__3].i = z[1].i;
			i__3 = izero;
			a[i__3].r = z[2].r, a[i__3].i = z[2].i;
		    }
		}

/*              If IMAT > 7, set one column of the matrix to 0
. */

		if (! zerot) {
		    izero = 0;
		} else if (imat == 8) {
		    izero = 1;
		    i__3 = n;
		    z[1].r = a[i__3].r, z[1].i = a[i__3].i;
		    i__3 = n;
		    a[i__3].r = 0., a[i__3].i = 0.;
		    if (n > 1) {
			z[2].r = a[1].r, z[2].i = a[1].i;
			a[1].r = 0., a[1].i = 0.;
		    }
		} else if (imat == 9) {
		    izero = n;
		    i__3 = n * 3 - 2;
		    z[0].r = a[i__3].r, z[0].i = a[i__3].i;
		    i__3 = (n << 1) - 1;
		    z[1].r = a[i__3].r, z[1].i = a[i__3].i;
		    i__3 = n * 3 - 2;
		    a[i__3].r = 0., a[i__3].i = 0.;
		    i__3 = (n << 1) - 1;
		    a[i__3].r = 0., a[i__3].i = 0.;
		} else {
		    izero = (n + 1) / 2;
		    i__3 = n - 1;
		    for (i = izero; i <= i__3; ++i) {
			i__4 = (n << 1) - 2 + i;
			a[i__4].r = 0., a[i__4].i = 0.;
			i__4 = n - 1 + i;
			a[i__4].r = 0., a[i__4].i = 0.;
			i__4 = i;
			a[i__4].r = 0., a[i__4].i = 0.;
/* L20: */
		    }
		    i__3 = n * 3 - 2;
		    a[i__3].r = 0., a[i__3].i = 0.;
		    i__3 = (n << 1) - 1;
		    a[i__3].r = 0., a[i__3].i = 0.;
		}
	    }

/* +    TEST 1   
             Factor A as L*U and compute the ratio   
                norm(L*U - A) / (n * norm(A) * EPS ) */

	    i__3 = n + (m << 1);
	    zcopy_(&i__3, &a[1], &c__1, &af[1], &c__1);
	    s_copy(srnamc_1.srnamt, "ZGTTRF", 6L, 6L);
	    zgttrf_(&n, &af[1], &af[m + 1], &af[n + m + 1], &af[n + (m << 1) 
		    + 1], &iwork[1], &info);

/*           Check error code from ZGTTRF. */

	    if (info != izero) {
		alaerh_(path, "ZGTTRF", &info, &izero, " ", &n, &n, &c__1, &
			c__1, &c_n1, &imat, &nfail, &nerrs, nout);
	    }
	    trfcon = info != 0;

	    zgtt01_(&n, &a[1], &a[m + 1], &a[n + m + 1], &af[1], &af[m + 1], &
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
		    zlarnv_(&c__2, iseed, &n, &xact[ix]);
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

		    anorm = zlangt_(norm, &n, &a[1], &a[m + 1], &a[n + m + 1]);

/*                 Use ZGTTRS to solve for one column at a
 time of   
                   inv(A), computing the maximum column su
m as we go. */

		    ainvnm = 0.;
		    i__3 = n;
		    for (i = 1; i <= i__3; ++i) {
			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    i__5 = j;
			    x[i__5].r = 0., x[i__5].i = 0.;
/* L40: */
			}
			i__4 = i;
			x[i__4].r = 1., x[i__4].i = 0.;
			zgttrs_(trans, &n, &c__1, &af[1], &af[m + 1], &af[n + 
				m + 1], &af[n + (m << 1) + 1], &iwork[1], &x[
				1], &lda, &info);
/* Computing MAX */
			d__1 = ainvnm, d__2 = dzasum_(&n, &x[1], &c__1);
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

		zlagtm_(trans, &n, nrhs, &c_b48, &a[1], &a[m + 1], &a[n + m + 
			1], &xact[1], &lda, &c_b49, &b[1], &lda);

/* +    TEST 2   
                Solve op(A) * X = B and compute the residual. 
*/

		zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);
		s_copy(srnamc_1.srnamt, "ZGTTRS", 6L, 6L);
		zgttrs_(trans, &n, nrhs, &af[1], &af[m + 1], &af[n + m + 1], &
			af[n + (m << 1) + 1], &iwork[1], &x[1], &lda, &info);

/*              Check error code from ZGTTRS. */

		if (info != 0) {
		    alaerh_(path, "ZGTTRS", &info, &c__0, trans, &n, &n, &
			    c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
		zgtt02_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &x[
			1], &lda, &work[1], &lda, &rwork[1], &result[1]);

/* +    TEST 3   
                Check solution from generated exact solution. 
*/

		zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[2]);

/* +    TESTS 4, 5, and 6   
                Use iterative refinement to improve the soluti
on. */

		s_copy(srnamc_1.srnamt, "ZGTRFS", 6L, 6L);
		zgtrfs_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &af[
			1], &af[m + 1], &af[n + m + 1], &af[n + (m << 1) + 1],
			 &iwork[1], &b[1], &lda, &x[1], &lda, &rwork[1], &
			rwork[*nrhs + 1], &work[1], &rwork[(*nrhs << 1) + 1], 
			&info);

/*              Check error code from ZGTRFS. */

		if (info != 0) {
		    alaerh_(path, "ZGTRFS", &info, &c__0, trans, &n, &n, &
			    c_n1, &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[3]);
		zgtt05_(trans, &n, nrhs, &a[1], &a[m + 1], &a[n + m + 1], &b[
			1], &lda, &x[1], &lda, &xact[1], &lda, &rwork[1], &
			rwork[*nrhs + 1], &result[4]);
		nt = 6;
L60:

/* +    TEST 7   
                Estimate the reciprocal of the condition numbe
r of the   
                matrix. */

		if (itran <= 2) {
		    s_copy(srnamc_1.srnamt, "ZGTCON", 6L, 6L);
		    zgtcon_(norm, &n, &af[1], &af[m + 1], &af[n + m + 1], &af[
			    n + (m << 1)], &iwork[1], &anorm, &rcond, &work[1]
			    , &info);

/*                 Check error code from ZGTCON. */

		    if (info != 0) {
			alaerh_(path, "ZGTCON", &info, &c__0, norm, &n, &n, &
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

/*     End of ZCHKGT */

} /* zchkgt_ */

