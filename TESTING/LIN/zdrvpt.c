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
static doublereal c_b24 = 1.;
static doublereal c_b25 = 0.;
static doublecomplex c_b62 = {0.,0.};

/* Subroutine */ int zdrvpt_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, doublecomplex *a, 
	doublereal *d, doublecomplex *e, doublecomplex *b, doublecomplex *x, 
	doublecomplex *xact, doublecomplex *work, doublereal *rwork, integer *
	nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002, N =\002,i5,\002, type \002,i2,"
	    "\002, test \002,i2,\002, ratio = \002,g12.5)";
    static char fmt_9998[] = "(1x,a6,\002, FACT='\002,a1,\002', N =\002,i5"
	    ",\002, type \002,i2,\002, test \002,i2,\002, ratio = \002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double z_abs(doublecomplex *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static char fact[1];
    static doublereal cond;
    static integer mode;
    static doublereal dmax__;
    static integer imat, info;
    static char path[3], dist[1], type[1];
    static integer nrun, i, j, k, n, ifact;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    static integer nfail, iseed[4];
    static doublereal z[3];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    ), dcopy_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer izero, nerrs, k1;
    extern /* Subroutine */ int zptt01_(integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublereal *);
    static logical zerot;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zptt02_(char *, integer *, integer *,
	     doublereal *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *), zptt05_(
	    integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, 
	    doublereal *), zptsv_(integer *, integer *, doublereal *, 
	    doublecomplex *, doublecomplex *, integer *, integer *), zlatb4_(
	    char *, integer *, integer *, integer *, char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, char *);
    static integer ia;
    extern /* Subroutine */ int aladhd_(integer *, char *);
    static integer in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, ix, nt;
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal rcondc;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *), alasvm_(char *, integer *, integer *,
	     integer *, integer *), dlarnv_(integer *, integer *, 
	    integer *, doublereal *);
    static doublereal ainvnm;
    extern doublereal zlanht_(char *, integer *, doublereal *, doublecomplex *
	    );
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern doublereal dzasum_(integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *), zlaptm_(char *, integer *, integer *, doublereal *, 
	    doublereal *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, doublecomplex *, integer *), zlatms_(
	    integer *, integer *, char *, integer *, char *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, char 
	    *, doublecomplex *, integer *, doublecomplex *, integer *), zlarnv_(integer *, integer *, integer *, 
	    doublecomplex *);
    static doublereal result[6];
    extern /* Subroutine */ int zpttrf_(integer *, doublereal *, 
	    doublecomplex *, integer *), zerrvx_(char *, integer *), 
	    zpttrs_(char *, integer *, integer *, doublereal *, doublecomplex 
	    *, doublecomplex *, integer *, integer *), zptsvx_(char *,
	     integer *, integer *, doublereal *, doublecomplex *, doublereal *
	    , doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublereal *, doublereal *, 
	    doublecomplex *, doublereal *, integer *);
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___35 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZDRVPT tests ZPTSV and -SVX.   

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

    A       (workspace) COMPLEX*16 array, dimension (NMAX*2)   

    D       (workspace) DOUBLE PRECISION array, dimension (NMAX*2)   

    E       (workspace) COMPLEX*16 array, dimension (NMAX*2)   

    B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (NMAX+2*NRHS)   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --rwork;
    --work;
    --xact;
    --x;
    --b;
    --e;
    --d;
    --a;
    --nval;
    --dotype;

    /* Function Body */

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "PT", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	zerrvx_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {

/*        Do for each value of N in NVAL. */

	n = nval[in];
	lda = max(1,n);
	nimat = 12;
	if (n <= 0) {
	    nimat = 1;
	}

	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (n > 0 && ! dotype[imat]) {
		goto L110;
	    }

/*           Set up parameters with ZLATB4. */

	    zlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &cond,
		     dist);

	    zerot = imat >= 8 && imat <= 10;
	    if (imat <= 6) {

/*              Type 1-6:  generate a symmetric tridiagonal ma
trix of   
                known condition number in lower triangular ban
d storage. */

		s_copy(srnamc_1.srnamt, "ZLATMS", 6L, 6L);
		zlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cond, &
			anorm, &kl, &ku, "B", &a[1], &c__2, &work[1], &info);

/*              Check the error code from ZLATMS. */

		if (info != 0) {
		    alaerh_(path, "ZLATMS", &info, &c__0, " ", &n, &n, &kl, &
			    ku, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L110;
		}
		izero = 0;

/*              Copy the matrix to D and E. */

		ia = 1;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    i__4 = i;
		    i__5 = ia;
		    d[i__4] = a[i__5].r;
		    i__4 = i;
		    i__5 = ia + 1;
		    e[i__4].r = a[i__5].r, e[i__4].i = a[i__5].i;
		    ia += 2;
/* L20: */
		}
		if (n > 0) {
		    i__3 = n;
		    i__4 = ia;
		    d[i__3] = a[i__4].r;
		}
	    } else {

/*              Type 7-12:  generate a diagonally dominant mat
rix with   
                unknown condition number in the vectors D and 
E. */

		if (! zerot || ! dotype[7]) {

/*                 Let D and E have values from [-1,1]. */

		    dlarnv_(&c__2, iseed, &n, &d[1]);
		    i__3 = n - 1;
		    zlarnv_(&c__2, iseed, &i__3, &e[1]);

/*                 Make the tridiagonal matrix diagonally 
dominant. */

		    if (n == 1) {
			d[1] = abs(d[1]);
		    } else {
			d[1] = abs(d[1]) + z_abs(&e[1]);
			d[n] = (d__1 = d[n], abs(d__1)) + z_abs(&e[n - 1]);
			i__3 = n - 1;
			for (i = 2; i <= i__3; ++i) {
			    d[i] = (d__1 = d[i], abs(d__1)) + z_abs(&e[i]) + 
				    z_abs(&e[i - 1]);
/* L30: */
			}
		    }

/*                 Scale D and E so the maximum element is
 ANORM. */

		    ix = idamax_(&n, &d[1], &c__1);
		    dmax__ = d[ix];
		    d__1 = anorm / dmax__;
		    dscal_(&n, &d__1, &d[1], &c__1);
		    if (n > 1) {
			i__3 = n - 1;
			d__1 = anorm / dmax__;
			zdscal_(&i__3, &d__1, &e[1], &c__1);
		    }

		} else if (izero > 0) {

/*                 Reuse the last matrix by copying back t
he zeroed out   
                   elements. */

		    if (izero == 1) {
			d[1] = z[1];
			if (n > 1) {
			    e[1].r = z[2], e[1].i = 0.;
			}
		    } else if (izero == n) {
			i__3 = n - 1;
			e[i__3].r = z[0], e[i__3].i = 0.;
			d[n] = z[1];
		    } else {
			i__3 = izero - 1;
			e[i__3].r = z[0], e[i__3].i = 0.;
			d[izero] = z[1];
			i__3 = izero;
			e[i__3].r = z[2], e[i__3].i = 0.;
		    }
		}

/*              For types 8-10, set one row and column of the 
matrix to   
                zero. */

		izero = 0;
		if (imat == 8) {
		    izero = 1;
		    z[1] = d[1];
		    d[1] = 0.;
		    if (n > 1) {
			z[2] = e[1].r;
			e[1].r = 0., e[1].i = 0.;
		    }
		} else if (imat == 9) {
		    izero = n;
		    if (n > 1) {
			i__3 = n - 1;
			z[0] = e[i__3].r;
			i__3 = n - 1;
			e[i__3].r = 0., e[i__3].i = 0.;
		    }
		    z[1] = d[n];
		    d[n] = 0.;
		} else if (imat == 10) {
		    izero = (n + 1) / 2;
		    if (izero > 1) {
			i__3 = izero - 1;
			z[0] = e[i__3].r;
			i__3 = izero - 1;
			e[i__3].r = 0., e[i__3].i = 0.;
			i__3 = izero;
			z[2] = e[i__3].r;
			i__3 = izero;
			e[i__3].r = 0., e[i__3].i = 0.;
		    }
		    z[1] = d[izero];
		    d[izero] = 0.;
		}
	    }

/*           Generate NRHS random solution vectors. */

	    ix = 1;
	    i__3 = *nrhs;
	    for (j = 1; j <= i__3; ++j) {
		zlarnv_(&c__2, iseed, &n, &xact[ix]);
		ix += lda;
/* L40: */
	    }

/*           Set the right hand side. */

	    zlaptm_("Lower", &n, nrhs, &c_b24, &d[1], &e[1], &xact[1], &lda, &
		    c_b25, &b[1], &lda);

	    for (ifact = 1; ifact <= 2; ++ifact) {
		if (ifact == 1) {
		    *(unsigned char *)fact = 'F';
		} else {
		    *(unsigned char *)fact = 'N';
		}

/*              Compute the condition number for comparison wi
th   
                the value returned by ZPTSVX. */

		if (zerot) {
		    if (ifact == 1) {
			goto L100;
		    }
		    rcondc = 0.;

		} else if (ifact == 1) {

/*                 Compute the 1-norm of A. */

		    anorm = zlanht_("1", &n, &d[1], &e[1]);

		    dcopy_(&n, &d[1], &c__1, &d[n + 1], &c__1);
		    if (n > 1) {
			i__3 = n - 1;
			zcopy_(&i__3, &e[1], &c__1, &e[n + 1], &c__1);
		    }

/*                 Factor the matrix A. */

		    zpttrf_(&n, &d[n + 1], &e[n + 1], &info);

/*                 Use ZPTTRS to solve for one column at a
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
/* L50: */
			}
			i__4 = i;
			x[i__4].r = 1., x[i__4].i = 0.;
			zpttrs_("Lower", &n, &c__1, &d[n + 1], &e[n + 1], &x[
				1], &lda, &info);
/* Computing MAX */
			d__1 = ainvnm, d__2 = dzasum_(&n, &x[1], &c__1);
			ainvnm = max(d__1,d__2);
/* L60: */
		    }

/*                 Compute the 1-norm condition number of 
A. */

		    if (anorm <= 0. || ainvnm <= 0.) {
			rcondc = 1.;
		    } else {
			rcondc = 1. / anorm / ainvnm;
		    }
		}

		if (ifact == 2) {

/*                 --- Test ZPTSV -- */

		    dcopy_(&n, &d[1], &c__1, &d[n + 1], &c__1);
		    if (n > 1) {
			i__3 = n - 1;
			zcopy_(&i__3, &e[1], &c__1, &e[n + 1], &c__1);
		    }
		    zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

/*                 Factor A as L*D*L' and solve the system
 A*X = B. */

		    s_copy(srnamc_1.srnamt, "ZPTSV ", 6L, 6L);
		    zptsv_(&n, nrhs, &d[n + 1], &e[n + 1], &x[1], &lda, &info)
			    ;

/*                 Check error code from ZPTSV . */

		    if (info != izero) {
			alaerh_(path, "ZPTSV ", &info, &izero, " ", &n, &n, &
				c__1, &c__1, nrhs, &imat, &nfail, &nerrs, 
				nout);
		    }
		    nt = 0;
		    if (izero == 0) {

/*                    Check the factorization by compu
ting the ratio   
                         norm(L*D*L' - A) / (n * norm(
A) * EPS ) */

			zptt01_(&n, &d[1], &e[1], &d[n + 1], &e[n + 1], &work[
				1], result);

/*                    Compute the residual in the solu
tion. */

			zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
			zptt02_("Lower", &n, nrhs, &d[1], &e[1], &x[1], &lda, 
				&work[1], &lda, &result[1]);

/*                    Check solution from generated ex
act solution. */

			zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[2]);
			nt = 3;
		    }

/*                 Print information about the tests that 
did not pass   
                   the threshold. */

		    i__3 = nt;
		    for (k = 1; k <= i__3; ++k) {
			if (result[k - 1] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				aladhd_(nout, path);
			    }
			    io___35.ciunit = *nout;
			    s_wsfe(&io___35);
			    do_fio(&c__1, "ZPTSV ", 6L);
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(doublereal));
			    e_wsfe();
			    ++nfail;
			}
/* L70: */
		    }
		    nrun += nt;
		}

/*              --- Test ZPTSVX --- */

		if (ifact > 1) {

/*                 Initialize D( N+1:2*N ) and E( N+1:2*N 
) to zero. */

		    i__3 = n - 1;
		    for (i = 1; i <= i__3; ++i) {
			d[n + i] = 0.;
			i__4 = n + i;
			e[i__4].r = 0., e[i__4].i = 0.;
/* L80: */
		    }
		    if (n > 0) {
			d[n + n] = 0.;
		    }
		}

		zlaset_("Full", &n, nrhs, &c_b62, &c_b62, &x[1], &lda);

/*              Solve the system and compute the condition num
ber and   
                error bounds using ZPTSVX. */

		s_copy(srnamc_1.srnamt, "ZPTSVX", 6L, 6L);
		zptsvx_(fact, &n, nrhs, &d[1], &e[1], &d[n + 1], &e[n + 1], &
			b[1], &lda, &x[1], &lda, &rcond, &rwork[1], &rwork[*
			nrhs + 1], &work[1], &rwork[(*nrhs << 1) + 1], &info);

/*              Check the error code from ZPTSVX. */

		if (info != izero) {
		    alaerh_(path, "ZPTSVX", &info, &izero, fact, &n, &n, &
			    c__1, &c__1, nrhs, &imat, &nfail, &nerrs, nout);
		}
		if (izero == 0) {
		    if (ifact == 2) {

/*                    Check the factorization by compu
ting the ratio   
                         norm(L*D*L' - A) / (n * norm(
A) * EPS ) */

			k1 = 1;
			zptt01_(&n, &d[1], &e[1], &d[n + 1], &e[n + 1], &work[
				1], result);
		    } else {
			k1 = 2;
		    }

/*                 Compute the residual in the solution. 
*/

		    zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda)
			    ;
		    zptt02_("Lower", &n, nrhs, &d[1], &e[1], &x[1], &lda, &
			    work[1], &lda, &result[1]);

/*                 Check solution from generated exact sol
ution. */

		    zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			    result[2]);

/*                 Check error bounds from iterative refin
ement. */

		    zptt05_(&n, nrhs, &d[1], &e[1], &b[1], &lda, &x[1], &lda, 
			    &xact[1], &lda, &rwork[1], &rwork[*nrhs + 1], &
			    result[3]);
		} else {
		    k1 = 6;
		}

/*              Check the reciprocal of the condition number. 
*/

		result[5] = dget06_(&rcond, &rcondc);

/*              Print information about the tests that did not
 pass   
                the threshold. */

		for (k = k1; k <= 6; ++k) {
		    if (result[k - 1] >= *thresh) {
			if (nfail == 0 && nerrs == 0) {
			    aladhd_(nout, path);
			}
			io___38.ciunit = *nout;
			s_wsfe(&io___38);
			do_fio(&c__1, "ZPTSVX", 6L);
			do_fio(&c__1, fact, 1L);
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&result[k - 1], (ftnlen)sizeof(
				doublereal));
			e_wsfe();
			++nfail;
		    }
/* L90: */
		}
		nrun = nrun + 7 - k1;
L100:
		;
	    }
L110:
	    ;
	}
/* L120: */
    }

/*     Print a summary of the results. */

    alasvm_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of ZDRVPT */

} /* zdrvpt_ */

