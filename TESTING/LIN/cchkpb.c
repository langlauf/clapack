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
static complex c_b41 = {0.f,0.f};
static complex c_b42 = {1.f,0.f};

/* Subroutine */ int cchkpb_(logical *dotype, integer *nn, integer *nval, 
	integer *nnb, integer *nbval, integer *nrhs, real *thresh, logical *
	tsterr, integer *nmax, complex *a, complex *afac, complex *ainv, 
	complex *b, complex *x, complex *xact, complex *work, real *rwork, 
	integer *nout)
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
    static integer k, n;
    extern /* Subroutine */ int cget04_(integer *, integer *, complex *, 
	    integer *, complex *, integer *, real *, real *);
    static integer nfail, iseed[4];
    extern /* Subroutine */ int cpbt01_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *, real *, real *), 
	    cpbt02_(char *, integer *, integer *, integer *, complex *, 
	    integer *, complex *, integer *, complex *, integer *, real *, 
	    real *), cpbt05_(char *, integer *, integer *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, integer *, 
	    complex *, integer *, real *, real *, real *);
    static integer kdval[4];
    static real rcond;
    static integer nimat;
    extern doublereal sget06_(real *, real *);
    static real anorm;
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *), cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer iuplo, izero, i1, i2, nerrs;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int clatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, real *, integer *, real *, char *
	    );
    static integer kd, nb, in, kl;
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *), clanhb_(char *, char *, integer *, 
	    integer *, complex *, integer *, real *);
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), claipd_(integer *, complex *, integer *, integer *);
    static integer iw, ku, nt;
    extern /* Subroutine */ int cpbcon_(char *, integer *, integer *, complex 
	    *, integer *, real *, real *, complex *, real *, integer *);
    static real rcondc;
    static char packit[1];
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), clarhs_(char *, char 
	    *, char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, complex *, integer *, complex *, integer *, complex *, 
	    integer *, integer *, integer *), 
	    claset_(char *, integer *, integer *, complex *, complex *, 
	    complex *, integer *), cpbrfs_(char *, integer *, integer 
	    *, integer *, complex *, integer *, complex *, integer *, complex 
	    *, integer *, complex *, integer *, real *, real *, complex *, 
	    real *, integer *), cpbtrf_(char *, integer *, integer *, 
	    complex *, integer *, integer *), alasum_(char *, integer 
	    *, integer *, integer *, integer *);
    static real cndnum;
    extern /* Subroutine */ int clatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, complex *, integer *, complex *, integer *);
    static real ainvnm;
    extern /* Subroutine */ int cerrpo_(char *, integer *), cpbtrs_(
	    char *, integer *, integer *, integer *, complex *, integer *, 
	    complex *, integer *, integer *), xlaenv_(integer *, 
	    integer *);
    static real result[7];
    static integer lda, ikd, inb, nkd;

    /* Fortran I/O blocks */
    static cilist io___45 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CCHKPB tests CPBTRF, -TRS, -RFS, and -CON.   

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

    A       (workspace) REAL array, dimension (NMAX*NMAX)   

    AFAC    (workspace) REAL array, dimension (NMAX*NMAX)   

    AINV    (workspace) REAL array, dimension (NMAX*NMAX)   

    B       (workspace) REAL array, dimension (NMAX*NRHS)   

    X       (workspace) REAL array, dimension (NMAX*NRHS)   

    XACT    (workspace) REAL array, dimension (NMAX*NRHS)   

    WORK    (workspace) REAL array, dimension   
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
	cerrpo_(path, nout);
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

/*                    Set up parameters with CLATB4 an
d generate a test   
                      matrix with CLATMS. */

			clatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &
				mode, &cndnum, dist);

			s_copy(srnamc_1.srnamt, "CLATMS", 6L, 6L);
			clatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &
				cndnum, &anorm, &kd, &kd, packit, &a[koff], &
				ldab, &work[1], &info);

/*                    Check error code from CLATMS. */

			if (info != 0) {
			    alaerh_(path, "CLATMS", &info, &c__0, uplo, &n, &
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
			    ccopy_(&i__4, &work[iw], &c__1, &a[ioff - izero + 
				    i1], &c__1);
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    ccopy_(&i__4, &work[iw], &c__1, &a[ioff], &i__5);
			} else {
			    ioff = (i1 - 1) * ldab + 1;
			    i__4 = izero - i1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    ccopy_(&i__4, &work[iw], &c__1, &a[ioff + izero - 
				    i1], &i__5);
			    ioff = (izero - 1) * ldab + 1;
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
			    ccopy_(&i__4, &work[iw], &c__1, &a[ioff], &c__1);
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
			    work[i__5].r = 0.f, work[i__5].i = 0.f;
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
			    cswap_(&i__4, &a[ioff - izero + i1], &c__1, &work[
				    iw], &c__1);
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    cswap_(&i__4, &a[ioff], &i__5, &work[iw], &c__1);
			} else {
			    ioff = (i1 - 1) * ldab + 1;
			    i__4 = izero - i1;
/* Computing MAX */
			    i__6 = ldab - 1;
			    i__5 = max(i__6,1);
			    cswap_(&i__4, &a[ioff + izero - i1], &i__5, &work[
				    iw], &c__1);
			    ioff = (izero - 1) * ldab + 1;
			    iw = iw + izero - i1;
			    i__4 = i2 - izero + 1;
			    cswap_(&i__4, &a[ioff], &c__1, &work[iw], &c__1);
			}
		    }

/*                 Set the imaginary part of the diagonals
. */

		    if (iuplo == 1) {
			claipd_(&n, &a[kd + 1], &ldab, &c__0);
		    } else {
			claipd_(&n, &a[1], &ldab, &c__0);
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
			clacpy_("Full", &i__5, &n, &a[1], &ldab, &afac[1], &
				ldab);
			s_copy(srnamc_1.srnamt, "CPBTRF", 6L, 6L);
			cpbtrf_(uplo, &n, &kd, &afac[1], &ldab, &info);

/*                    Check error code from CPBTRF. */

			if (info != izero) {
			    alaerh_(path, "CPBTRF", &info, &izero, uplo, &n, &
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
			clacpy_("Full", &i__5, &n, &afac[1], &ldab, &ainv[1], 
				&ldab);
			cpbt01_(uplo, &n, &kd, &a[1], &ldab, &ainv[1], &ldab, 
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

			claset_("Full", &n, &n, &c_b41, &c_b42, &ainv[1], &
				lda);
			s_copy(srnamc_1.srnamt, "CPBTRS", 6L, 6L);
			cpbtrs_(uplo, &n, &kd, &n, &afac[1], &ldab, &ainv[1], 
				&lda, &info);

/*                    Compute RCONDC = 1/(norm(A) * no
rm(inv(A))). */

			anorm = clanhb_("1", uplo, &n, &kd, &a[1], &ldab, &
				rwork[1]);
			ainvnm = clange_("1", &n, &n, &ainv[1], &lda, &rwork[
				1]);
			if (anorm <= 0.f || ainvnm <= 0.f) {
			    rcondc = 1.f;
			} else {
			    rcondc = 1.f / anorm / ainvnm;
			}

/* +    TEST 2   
                      Solve and compute residual for A
 * X = B. */

			s_copy(srnamc_1.srnamt, "CLARHS", 6L, 6L);
			clarhs_(path, xtype, uplo, " ", &n, &n, &kd, &kd, 
				nrhs, &a[1], &ldab, &xact[1], &lda, &b[1], &
				lda, iseed, &info);
			*(unsigned char *)xtype = 'C';
			clacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &lda);

			s_copy(srnamc_1.srnamt, "CPBTRS", 6L, 6L);
			cpbtrs_(uplo, &n, &kd, nrhs, &afac[1], &ldab, &x[1], &
				lda, &info);

/*                    Check error code from CPBTRS. */

			if (info != 0) {
			    alaerh_(path, "CPBTRS", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			clacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &lda);
			cpbt02_(uplo, &n, &kd, nrhs, &a[1], &ldab, &x[1], &
				lda, &work[1], &lda, &rwork[1], &result[1]);

/* +    TEST 3   
                      Check solution from generated ex
act solution. */

			cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[2]);

/* +    TESTS 4, 5, and 6   
                      Use iterative refinement to impr
ove the solution. */

			s_copy(srnamc_1.srnamt, "CPBRFS", 6L, 6L);
			cpbrfs_(uplo, &n, &kd, nrhs, &a[1], &ldab, &afac[1], &
				ldab, &b[1], &lda, &x[1], &lda, &rwork[1], &
				rwork[*nrhs + 1], &work[1], &rwork[(*nrhs << 
				1) + 1], &info);

/*                    Check error code from CPBRFS. */

			if (info != 0) {
			    alaerh_(path, "CPBRFS", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, nrhs, &imat, &nfail, &nerrs, 
				    nout);
			}

			cget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				rcondc, &result[3]);
			cpbt05_(uplo, &n, &kd, nrhs, &a[1], &ldab, &b[1], &
				lda, &x[1], &lda, &xact[1], &lda, &rwork[1], &
				rwork[*nrhs + 1], &result[4]);

/* +    TEST 7   
                      Get an estimate of RCOND = 1/CND
NUM. */

			s_copy(srnamc_1.srnamt, "CPBCON", 6L, 6L);
			cpbcon_(uplo, &n, &kd, &afac[1], &ldab, &anorm, &
				rcond, &work[1], &rwork[1], &info);

/*                    Check error code from CPBCON. */

			if (info != 0) {
			    alaerh_(path, "CPBCON", &info, &c__0, uplo, &n, &
				    n, &kd, &kd, &c_n1, &imat, &nfail, &nerrs,
				     nout);
			}

			result[6] = sget06_(&rcond, &rcondc);
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
					sizeof(real));
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

/*     End of CCHKPB */

} /* cchkpb_ */

