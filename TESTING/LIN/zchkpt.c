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
static doublereal c_b47 = 1.;
static doublereal c_b48 = 0.;
static integer c__7 = 7;

/* Subroutine */ int zchkpt_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, doublereal *thresh, logical *tsterr, doublecomplex *a, 
	doublereal *d, doublecomplex *e, doublecomplex *b, doublecomplex *x, 
	doublecomplex *xact, doublecomplex *work, doublereal *rwork, integer *
	nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 0,0,0,1 };
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(\002 N =\002,i5,\002, type \002,i2,\002, te"
	    "st \002,i2,\002, ratio = \002,g12.5)";
    static char fmt_9998[] = "(\002 UPLO = '\002,a1,\002', N =\002,i5,\002, "
	    "type \002,i2,\002, test \002,i2,\002, ratio = \002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double z_abs(doublecomplex *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static doublereal cond;
    static integer mode;
    static doublereal dmax__;
    static integer imat, info;
    static char path[3], dist[1], uplo[1], type[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer n;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    static integer nfail, iseed[4];
    static doublecomplex z[3];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    ), dcopy_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer iuplo, izero, nerrs;
    static logical zerot;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zptt01_(integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublereal *), zptt02_(char *, integer *, integer *, doublereal *,
	     doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublereal *), zptt05_(integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *), zlatb4_(char *, 
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, char *);
    static integer ia, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, ix;
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal rcondc;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *), alasum_(char *, integer *, integer *,
	     integer *, integer *), dlarnv_(integer *, integer *, 
	    integer *, doublereal *);
    static doublereal ainvnm;
    extern doublereal zlanht_(char *, integer *, doublereal *, doublecomplex *
	    );
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern doublereal dzasum_(integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zlaptm_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, doublecomplex *, 
	    integer *, doublereal *, doublecomplex *, integer *), 
	    zlatms_(integer *, integer *, char *, integer *, char *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, char *, doublecomplex *, integer *, doublecomplex *, 
	    integer *), zlarnv_(integer *, integer *, 
	    integer *, doublecomplex *), zerrgt_(char *, integer *);
    static doublereal result[7];
    extern /* Subroutine */ int zptcon_(integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublereal *, doublereal *, 
	    integer *), zptrfs_(char *, integer *, integer *, doublereal *, 
	    doublecomplex *, doublereal *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, doublereal *,
	     doublecomplex *, doublereal *, integer *), zpttrf_(
	    integer *, doublereal *, doublecomplex *, integer *), zpttrs_(
	    char *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, integer *, integer *);
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___30 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKPT tests ZPTTRF, -TRS, -RFS, and -CON   

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
	zerrgt_(path, nout);
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
		goto L100;
	    }

/*           Set up parameters with ZLATB4. */

	    zlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &cond,
		     dist);

	    zerot = imat >= 8 && imat <= 10;
	    if (imat <= 6) {

/*              Type 1-6:  generate a Hermitian tridiagonal ma
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
		    goto L100;
		}
		izero = 0;

/*              Copy the matrix to D and E. */

		ia = 1;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    i__4 = ia;
		    d[i] = a[i__4].r;
		    i__4 = i;
		    i__5 = ia + 1;
		    e[i__4].r = a[i__5].r, e[i__4].i = a[i__5].i;
		    ia += 2;
/* L20: */
		}
		if (n > 0) {
		    i__3 = ia;
		    d[n] = a[i__3].r;
		}
	    } else {

/*              Type 7-12:  generate a diagonally dominant mat
rix with   
                unknown condition number in the vectors D and 
E. */

		if (! zerot || ! dotype[7]) {

/*                 Let E be complex, D real, with values f
rom [-1,1]. */

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
		    i__3 = n - 1;
		    d__1 = anorm / dmax__;
		    zdscal_(&i__3, &d__1, &e[1], &c__1);

		} else if (izero > 0) {

/*                 Reuse the last matrix by copying back t
he zeroed out   
                   elements. */

		    if (izero == 1) {
			d[1] = z[1].r;
			if (n > 1) {
			    e[1].r = z[2].r, e[1].i = z[2].i;
			}
		    } else if (izero == n) {
			i__3 = n - 1;
			e[i__3].r = z[0].r, e[i__3].i = z[0].i;
			i__3 = n;
			d[i__3] = z[1].r;
		    } else {
			i__3 = izero - 1;
			e[i__3].r = z[0].r, e[i__3].i = z[0].i;
			i__3 = izero;
			d[i__3] = z[1].r;
			i__3 = izero;
			e[i__3].r = z[2].r, e[i__3].i = z[2].i;
		    }
		}

/*              For types 8-10, set one row and column of the 
matrix to   
                zero. */

		izero = 0;
		if (imat == 8) {
		    izero = 1;
		    z[1].r = d[1], z[1].i = 0.;
		    d[1] = 0.;
		    if (n > 1) {
			z[2].r = e[1].r, z[2].i = e[1].i;
			e[1].r = 0., e[1].i = 0.;
		    }
		} else if (imat == 9) {
		    izero = n;
		    if (n > 1) {
			i__3 = n - 1;
			z[0].r = e[i__3].r, z[0].i = e[i__3].i;
			i__3 = n - 1;
			e[i__3].r = 0., e[i__3].i = 0.;
		    }
		    i__3 = n;
		    z[1].r = d[i__3], z[1].i = 0.;
		    d[n] = 0.;
		} else if (imat == 10) {
		    izero = (n + 1) / 2;
		    if (izero > 1) {
			i__3 = izero - 1;
			z[0].r = e[i__3].r, z[0].i = e[i__3].i;
			i__3 = izero;
			z[2].r = e[i__3].r, z[2].i = e[i__3].i;
			i__3 = izero - 1;
			e[i__3].r = 0., e[i__3].i = 0.;
			i__3 = izero;
			e[i__3].r = 0., e[i__3].i = 0.;
		    }
		    i__3 = izero;
		    z[1].r = d[i__3], z[1].i = 0.;
		    d[izero] = 0.;
		}
	    }

	    dcopy_(&n, &d[1], &c__1, &d[n + 1], &c__1);
	    if (n > 1) {
		i__3 = n - 1;
		zcopy_(&i__3, &e[1], &c__1, &e[n + 1], &c__1);
	    }

/* +    TEST 1   
             Factor A as L*D*L' and compute the ratio   
                norm(L*D*L' - A) / (n * norm(A) * EPS ) */

	    zpttrf_(&n, &d[n + 1], &e[n + 1], &info);

/*           Check error code from ZPTTRF. */

	    if (info != izero) {
		alaerh_(path, "ZPTTRF", &info, &izero, " ", &n, &n, &c_n1, &
			c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		goto L100;
	    }

	    if (info > 0) {
		rcondc = 0.;
		goto L90;
	    }

	    zptt01_(&n, &d[1], &e[1], &d[n + 1], &e[n + 1], &work[1], result);

/*           Print the test ratio if greater than or equal to THRE
SH. */

	    if (result[0] >= *thresh) {
		if (nfail == 0 && nerrs == 0) {
		    alahd_(nout, path);
		}
		io___30.ciunit = *nout;
		s_wsfe(&io___30);
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&result[0], (ftnlen)sizeof(doublereal));
		e_wsfe();
		++nfail;
	    }
	    ++nrun;

/*           Compute RCONDC = 1 / (norm(A) * norm(inv(A))   

             Compute norm(A). */

	    anorm = zlanht_("1", &n, &d[1], &e[1]);

/*           Use ZPTTRS to solve for one column at a time of inv(A
),   
             computing the maximum column sum as we go. */

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
		zpttrs_("Lower", &n, &c__1, &d[n + 1], &e[n + 1], &x[1], &lda,
			 &info);
/* Computing MAX */
		d__1 = ainvnm, d__2 = dzasum_(&n, &x[1], &c__1);
		ainvnm = max(d__1,d__2);
/* L50: */
	    }
/* Computing MAX */
	    d__1 = 1., d__2 = anorm * ainvnm;
	    rcondc = 1. / max(d__1,d__2);

/*           Generate NRHS random solution vectors. */

	    ix = 1;
	    i__3 = *nrhs;
	    for (j = 1; j <= i__3; ++j) {
		zlarnv_(&c__2, iseed, &n, &xact[ix]);
		ix += lda;
/* L60: */
	    }

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {

/*              Do first for UPLO = 'U', then for UPLO = 'L'. 
*/

		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*              Set the right hand side. */

		zlaptm_(uplo, &n, nrhs, &c_b47, &d[1], &e[1], &xact[1], &lda, 
			&c_b48, &b[1], &lda);

/* +    TEST 2   
                Solve A*x = b and compute the residual. */

		zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);
		zpttrs_(uplo, &n, nrhs, &d[n + 1], &e[n + 1], &x[1], &lda, &
			info);

/*              Check error code from ZPTTRS. */

		if (info != 0) {
		    alaerh_(path, "ZPTTRS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
		zptt02_(uplo, &n, nrhs, &d[1], &e[1], &x[1], &lda, &work[1], &
			lda, &result[1]);

/* +    TEST 3   
                Check solution from generated exact solution. 
*/

		zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[2]);

/* +    TESTS 4, 5, and 6   
                Use iterative refinement to improve the soluti
on. */

		s_copy(srnamc_1.srnamt, "ZPTRFS", 6L, 6L);
		zptrfs_(uplo, &n, nrhs, &d[1], &e[1], &d[n + 1], &e[n + 1], &
			b[1], &lda, &x[1], &lda, &rwork[1], &rwork[*nrhs + 1],
			 &work[1], &rwork[(*nrhs << 1) + 1], &info);

/*              Check error code from ZPTRFS. */

		if (info != 0) {
		    alaerh_(path, "ZPTRFS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, nrhs, &imat, &nfail, &nerrs, nout);
		}

		zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &rcondc, &
			result[3]);
		zptt05_(&n, nrhs, &d[1], &e[1], &b[1], &lda, &x[1], &lda, &
			xact[1], &lda, &rwork[1], &rwork[*nrhs + 1], &result[
			4]);

/*              Print information about the tests that did not
 pass the   
                threshold. */

		for (k = 2; k <= 6; ++k) {
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
/* L70: */
		}
		nrun += 5;

/* L80: */
	    }

/* +    TEST 7   
             Estimate the reciprocal of the condition number of th
e   
             matrix. */

L90:
	    s_copy(srnamc_1.srnamt, "ZPTCON", 6L, 6L);
	    zptcon_(&n, &d[n + 1], &e[n + 1], &anorm, &rcond, &rwork[1], &
		    info);

/*           Check error code from ZPTCON. */

	    if (info != 0) {
		alaerh_(path, "ZPTCON", &info, &c__0, " ", &n, &n, &c_n1, &
			c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
	    }

	    result[6] = dget06_(&rcond, &rcondc);

/*           Print the test ratio if greater than or equal to THRE
SH. */

	    if (result[6] >= *thresh) {
		if (nfail == 0 && nerrs == 0) {
		    alahd_(nout, path);
		}
		io___38.ciunit = *nout;
		s_wsfe(&io___38);
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__7, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&result[6], (ftnlen)sizeof(doublereal));
		e_wsfe();
		++nfail;
	    }
	    ++nrun;
L100:
	    ;
	}
/* L110: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of ZCHKPT */

} /* zchkpt_ */

