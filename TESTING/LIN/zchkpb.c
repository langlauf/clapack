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
static doublecomplex c_b41 = {0.,0.};
static doublecomplex c_b42 = {1.,0.};

/* Subroutine */ int zchkpb_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, doublereal *thresh, 
	logical *tsterr, integer *nmax, doublecomplex *a, doublecomplex *afac,
	 doublecomplex *ainv, doublecomplex *b, doublecomplex *x, 
	doublecomplex *xact, doublecomplex *work, doublereal *rwork, integer *
	nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };

    /* Format strings */
    static char fmt_9999[] = "(\002 UPLO='\002,a1,\002', N=\002,i5,\002, KD"
	    "=\002,i5,\002, NB=\002,i4,\002, type \002,i2,\002, test \002,i2"
	    ",\002, ratio= \002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ldab, ioff, mode, koff, imat, info;
    static char path[3], dist[1], uplo[1], type[1];
    static integer nrun, i;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer k, n, nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    static integer kdval[4];
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    ), zpbt01_(char *, integer *, integer *, doublecomplex *, integer 
	    *, doublecomplex *, integer *, doublereal *, doublereal *)
	    , zpbt02_(char *, integer *, integer *, integer *, doublecomplex *
	    , integer *, doublecomplex *, integer *, doublecomplex *, integer 
	    *, doublereal *, doublereal *), zpbt05_(char *, integer *,
	     integer *, integer *, doublecomplex *, integer *, doublecomplex *
	    , integer *, doublecomplex *, integer *, doublecomplex *, integer 
	    *, doublereal *, doublereal *, doublereal *);
    static integer iuplo, izero, i1, i2, nerrs;
    static logical zerot;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zswap_(integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *);
    static char xtype[1];
    extern /* Subroutine */ int zlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer kd, nb, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer iw, ku, nt;
    static doublereal rcondc;
    static char packit[1];
    extern doublereal zlanhb_(char *, char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *), 
	    zlange_(char *, integer *, integer *, doublecomplex *, integer *, 
	    doublereal *);
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal cndnum;
    extern /* Subroutine */ int zlaipd_(integer *, doublecomplex *, integer *,
	     integer *);
    static doublereal ainvnm;
    extern /* Subroutine */ int zpbcon_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *, 
	    doublecomplex *, doublereal *, integer *), xlaenv_(
	    integer *, integer *), zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), 
	    zlarhs_(char *, char *, char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *,
	     integer *), zlaset_(char *, 
	    integer *, integer *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *), zpbrfs_(char *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublecomplex *, integer *
	    , doublereal *, doublereal *, doublecomplex *, doublereal *, 
	    integer *), zpbtrf_(char *, integer *, integer *, 
	    doublecomplex *, integer *, integer *), zlatms_(integer *,
	     integer *, char *, integer *, char *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, char *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static doublereal result[7];
    extern /* Subroutine */ int zerrpo_(char *, integer *), zpbtrs_(
	    char *, integer *, integer *, integer *, doublecomplex *, integer 
	    *, doublecomplex *, integer *, integer *);
    static integer lda, ikd, inb, nkd;

    /* Fortran I/O blocks */
    static cilist io___45 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKPB tests ZPBTRF, -TRS, -RFS, and -CON.   

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

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "PB", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	zerrpo_(path, nout);
    }
    infoc_1.infot = 0;
    kdval[0] = 0;

/*     Do for each value of N in NVAL */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];
	lda = max(n,1);
	*(unsigned char *)xtype = 'N';

/*        Set limits on the number of loop iterations.   

   Computing MAX */
	i__2 = 1, i__3 = min(n,4);
	nkd = max(i__2,i__3);
	nimat = 8;
	if (n == 0) {
	    nimat = 1;
	}

	kdval[1] = n + (n + 1) / 4;
	kdval[2] = (n * 3 - 1) / 4;
	kdval[3] = (n + 1) / 4;

	i__2 = nkd;
	for (ikd = 1; ikd <= i__2; ++ikd) {

/*           Do for KD = 0, (5*N+1)/4, (3N-1)/4, and (N+1)/4. This
 order   
             makes it easier to skip redundant values for small va
lues   
             of N. */

	    kd = kdval[ikd - 1];
	    ldab = kd + 1;

/*           Do first for UPLO = 'U', then for UPLO = 'L' */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		koff = 1;
		if (iuplo == 1) {
		    *(unsigned char *)uplo = 'U';
/* Computing MAX */
		    i__3 = 1, i__4 = kd + 2 - n;
		    koff = max(i__3,i__4);
		    *(unsigned char *)packit = 'Q';
		} else {
		    *(unsigned char *)uplo = 'L';
		    *(unsigned char *)packit = 'B';
		}

		i__3 = nimat;
		for (imat = 1; imat <= i__3; ++imat) {

/*                 Do the tests only if DOTYPE( IMAT ) is 
true. */

		    if (! dotype[imat]) {
			goto L60;
		    }

/*                 Skip types 2, 3, or 4 if the matrix siz
e is too small. */

		    zerot = imat >= 2 && imat <= 4;
		    if (zerot && n < imat - 1) {
			goto L60;
		    }

		    if (! zerot || ! dotype[1]) {

/*                    Set up parameters with ZLATB4 an
d generate a test   
                      matrix with ZLATMS. */

			zlatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &
				mode, &cndnum, dist);

			s_copy(srnamc_1.srnamt, "ZLATMS", 6L, 6L);
			zlatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &
				cndnum, &anorm, &kd, &kd, packit, &a[koff], &
				ldab, &work[1], &info);

/*                    Check error code from ZLATMS. */

			if (info != 0) {
			    alaerh_(path, "ZLATMS", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			    goto L60;
			}
		    } else if (izero > 0) {

/*                    Use the same matrix for types 3 
and 4 as for type   
                      2 by copying back the zeroed out
 column, */

			iw = (lda << 1) + 1;
			if (iuplo == 1) {
			    ioff = (izero - 1) * ldab + kd + 1;
			    i__4 = izero - i1;
			    zcopy_(&i__4, &work[iw], &c__1, &a[ioff - izero + 
				    i1], &c__1);
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    zcopy_(&i__4, &work[iw], &c__1, &a[ioff], &i__5);
			} else {
			    ioff = (i1 - 1) * ldab + 1;
			    i__4 = izero - i1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    zcopy_(&i__4, &work[iw], &c__1, &a[ioff + izero - 
				    i1], &i__5);
			    ioff = (izero - 1) * ldab + 1;
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
			    zcopy_(&i__4, &work[iw], &c__1, &a[ioff], &c__1);
			}
		    }

/*                 For types 2-4, zero one row and column 
of the matrix   
                   to test that INFO is returned correctly
. */

		    izero = 0;
		    if (zerot) {
			if (imat == 2) {
			    izero = 1;
			} else if (imat == 3) {
			    izero = n;
			} else {
			    izero = n / 2 + 1;
			}

/*                    Save the zeroed out row and colu
mn in WORK(*,3) */

			iw = lda << 1;
/* Computing MIN */
			i__5 = (kd << 1) + 1;
			i__4 = min(i__5,n);
			for (i = 1; i <= i__4; ++i) {
			    i__5 = iw + i;
			    work[i__5].r = 0., work[i__5].i = 0.;
/* L20: */
			}
			++iw;
/* Computing MAX */
			i__4 = izero - kd;
			i1 = max(i__4,1);
/* Computing MIN */
			i__4 = izero + kd;
			i2 = min(i__4,n);

			if (iuplo == 1) {
			    ioff = (izero - 1) * ldab + kd + 1;
			    i__4 = izero - i1;
			    zswap_(&i__4, &a[ioff - izero + i1], &c__1, &work[
				    iw], &c__1);
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    zswap_(&i__4, &a[ioff], &i__5, &work[iw], &c__1);
			} else {
			    ioff = (i1 - 1) * ldab + 1;
			    i__4 = izero - i1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    zswap_(&i__4, &a[ioff + izero - i1], &i__5, &work[
				    iw], &c__1);
			    ioff = (izero - 1) * ldab + 1;
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
			    zswap_(&i__4, &a[ioff], &c__1, &work[iw], &c__1);
			}
		    }

/*                 Set the imaginary part of the diagonals
. */

		    if (iuplo == 1) {
			zlaipd_(&n, &a[kd + 1], &ldab, &c__0);
		    } else {
			zlaipd_(&n, &a[1], &ldab, &c__0);
		    }

/*                 Do for each value of NB in NBVAL */

		    i__4 = *nnb;
		    for (inb = 1; inb <= i__4; ++inb) {
			nb = nbval[inb];
			xlaenv_(&c__1, &nb);

/*                    Compute the L*L' or U'*U factori
zation of the band   
                      matrix. */

			i__5 = kd + 1;
			zlacpy_("Full", &i__5, &n, &a[1], &ldab, &afac[1], &
				ldab);
			s_copy(srnamc_1.srnamt, "ZPBTRF", 6L, 6L);
			zpbtrf_(uplo, &n, &kd, &afac[1], &ldab, &info);

/*                    Check error code from ZPBTRF. */

			if (info != izero) {
			    alaerh_(path, "ZPBTRF", &info, &izero, uplo, &n, &
				    n, &kd, &kd, &nb, &imat, &nfail, &nerrs, 
				    nout);
			    goto L50;
			}

/*                    Skip the tests if INFO is not 0.
 */

			if (info != 0) {
			    goto L50;
			}

/* +    TEST 1   
                      Reconstruct matrix from factors 
and compute   
                      residual. */

			i__5 = kd + 1;
			zlacpy_("Full", &i__5, &n, &afac[1], &ldab, &ainv[1], 
				&ldab);
			zpbt01_(uplo, &n, &kd, &a[1], &ldab, &ainv[1], &ldab, 
				&rwork[1], result);
			nt = 1;

/*                    Only do other tests if this is t
he first blocksize. */

			if (inb > 1) {
			    goto L30;
			}

/*                    Form the inverse of A so we can 
get a good estimate   
                      of RCONDC = 1/(norm(A) * norm(in
v(A))). */

			zlaset_("Full", &n, &n, &c_b41, &c_b42, &ainv[1], &
				lda);
			s_copy(srnamc_1.srnamt, "ZPBTRS", 6L, 6L);
			zpbtrs_(uplo, &n, &kd, &n, &afac[1], &ldab, &ainv[1], 
				&lda, &info);

/*                    Compute RCONDC = 1/(norm(A) * no
rm(inv(A))). */

			anorm = zlanhb_("1", uplo, &n, &kd, &a[1], &ldab, &
				rwork[1]);
			ainvnm = zlange_("1", &n, &n, &ainv[1], &lda, &rwork[
				1]);
			if (anorm <= 0. || ainvnm <= 0.) {
			    rcondc = 1.;
			} else {
			    rcondc = 1. / anorm / ainvnm;
			}

/* +    TEST 2   
                      Solve and compute residual for A
 * X = B. */

			s_copy(srnamc_1.srnamt, "ZLARHS", 6L, 6L);
			zlarhs_(path, xtype, uplo, " ", &n, &n, &kd, &kd, 
				nrhs, &a[1], &ldab, &xact[1], &lda, &b[1], &
				lda, iseed, &info);
			*(unsigned char *)xtype = 'C';
			zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

			s_copy(srnamc_1.srnamt, "ZPBTRS", 6L, 6L);
			zpbtrs_(uplo, &n, &kd, nrhs, &afac[1], &ldab, &x[1], &
				lda, &info);

/*                    Check error code from ZPBTRS. */

			if (info != 0) {
			    alaerh_(path, "ZPBTRS", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
			zpbt02_(uplo, &n, &kd, nrhs, &a[1], &ldab, &x[1], &
				lda, &work[1], &lda, &rwork[1], &result[1]);

/* +    TEST 3   
                      Check solution from generated ex
act solution. */

			zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[2]);

/* +    TESTS 4, 5, and 6   
                      Use iterative refinement to impr
ove the solution. */

			s_copy(srnamc_1.srnamt, "ZPBRFS", 6L, 6L);
			zpbrfs_(uplo, &n, &kd, nrhs, &a[1], &ldab, &afac[1], &
				ldab, &b[1], &lda, &x[1], &lda, &rwork[1], &
				rwork[*nrhs + 1], &work[1], &rwork[(*nrhs << 
				1) + 1], &info);

/*                    Check error code from ZPBRFS. */

			if (info != 0) {
			    alaerh_(path, "ZPBRFS", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[3]);
			zpbt05_(uplo, &n, &kd, nrhs, &a[1], &ldab, &b[1], &
				lda, &x[1], &lda, &xact[1], &lda, &rwork[1], &
				rwork[*nrhs + 1], &result[4]);

/* +    TEST 7   
                      Get an estimate of RCOND = 1/CND
NUM. */

			s_copy(srnamc_1.srnamt, "ZPBCON", 6L, 6L);
			zpbcon_(uplo, &n, &kd, &afac[1], &ldab, &anorm, &
				rcond, &work[1], &rwork[1], &info);

/*                    Check error code from ZPBCON. */

			if (info != 0) {
			    alaerh_(path, "ZPBCON", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			}

			result[6] = dget06_(&rcond, &rcondc);
			nt = 7;

/*                    Print information about the test
s that did not pass   
                      the threshold. */

L30:
			i__5 = nt;
			for (k = 1; k <= i__5; ++k) {
			    if (result[k - 1] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    alahd_(nout, path);
				}
				io___45.ciunit = *nout;
				s_wsfe(&io___45);
				do_fio(&c__1, uplo, 1L);
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&kd, (ftnlen)sizeof(
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
/* L40: */
			}
			nrun += nt;
L50:
			;
		    }

L60:
		    ;
		}
/* L70: */
	    }
/* L80: */
	}
/* L90: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of ZCHKPB */

} /* zchkpb_ */

