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
static doublecomplex c_b21 = {0.,0.};
static integer c__1 = 1;

/* Subroutine */ int zchkge_(logical *dotype, integer *nm, integer *mval, 
	integer *nn, integer *nval, integer *nnb, integer *nbval, integer *
	nrhs, doublereal *thresh, logical *tsterr, integer *nmax, 
	doublecomplex *a, doublecomplex *afac, doublecomplex *ainv, 
	doublecomplex *b, doublecomplex *x, doublecomplex *xact, 
	doublecomplex *work, doublereal *rwork, integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 M = \002,i5,\002, N =\002,i5,\002, NB "
	    "=\002,i4,\002, type \002,i2,\002, test \002,i2,\002, ratio =\002"
	    ",g12.5)";
    static char fmt_9998[] = "(\002 TRANS='\002,a1,\002', N =\002,i5,\002, N"
	    "B =\002,i4,\002, type \002,i2,\002, test \002,i2,\002, ratio "
	    "=\002,g12.5)";
    static char fmt_9997[] = "(\002 NORM ='\002,a1,\002', N =\002,i5,\002, N"
	    "B =\002,i4,\002, type \002,i2,\002, test \002,i2,\002, ratio "
	    "=\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ioff, mode, imat, info;
    static char path[3], dist[1], norm[1], type[1];
    static integer nrun, i;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer k, m, n, nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    static integer nimat;
    extern /* Subroutine */ int zget01_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, integer *, doublereal *, 
	    doublereal *), zget02_(char *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *);
    static doublereal anorm;
    static integer itran;
    extern /* Subroutine */ int zget03_(integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *), zget04_(integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublereal *, doublereal *), zget07_(char *, integer *, integer *
	    , doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *);
    static char trans[1];
    static integer izero, nerrs, k1;
    static doublereal dummy;
    static integer lwork;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int zlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer nb, im, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, nt;
    static doublereal rcondc, rcondi;
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal cndnum, anormi, rcondo;
    extern /* Subroutine */ int zgecon_(char *, integer *, doublecomplex *, 
	    integer *, doublereal *, doublereal *, doublecomplex *, 
	    doublereal *, integer *);
    static doublereal ainvnm;
    static logical trfcon;
    static doublereal anormo;
    extern /* Subroutine */ int xlaenv_(integer *, integer *), zerrge_(char *,
	     integer *), zgerfs_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, doublereal *, 
	    integer *), zgetrf_(integer *, integer *, doublecomplex *,
	     integer *, integer *, integer *), zlacpy_(char *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *), zlarhs_(char *, char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     integer *, integer *), zgetri_(
	    integer *, doublecomplex *, integer *, integer *, doublecomplex *,
	     integer *, integer *), zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *), zlatms_(integer *, integer *, char *, integer *, char *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, char *, doublecomplex *, integer *, doublecomplex *, 
	    integer *);
    static doublereal result[8];
    extern /* Subroutine */ int zgetrs_(char *, integer *, integer *, 
	    doublecomplex *, integer *, integer *, doublecomplex *, integer *,
	     integer *);
    static integer lda, inb;

    /* Fortran I/O blocks */
    static cilist io___40 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9997, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKGE tests ZGETRF, -TRI, -TRS, -RFS, and -CON.   

    Arguments   
    =========   

    DOTYPE  (input) LOGICAL array, dimension (NTYPES)   
            The matrix types to be used for testing.  Matrices of type j 
  
            (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =   
            .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.   

    NM      (input) INTEGER   
            The number of values of M contained in the vector MVAL.   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix row dimension M.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

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
            The maximum value permitted for M or N, used in dimensioning 
  
            the work arrays.   

    A       (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)   

    AFAC    (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)   

    AINV    (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)   

    B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max(2*NMAX,2*NRHS+NWORK))   

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
    --mval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "GE", 2L, 2L);
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
    xlaenv_(&c__2, &c__2);

/*     Do for each value of M in MVAL */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	lda = max(1,m);

/*        Do for each value of N in NVAL */

	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];
	    *(unsigned char *)xtype = 'N';
	    nimat = 11;
	    if (m <= 0 || n <= 0) {
		nimat = 1;
	    }

	    i__3 = nimat;
	    for (imat = 1; imat <= i__3; ++imat) {

/*              Do the tests only if DOTYPE( IMAT ) is true. 
*/

		if (! dotype[imat]) {
		    goto L70;
		}

/*              Skip types 5, 6, or 7 if the matrix size is to
o small. */

		zerot = imat >= 5 && imat <= 7;
		if (zerot && n < imat - 4) {
		    goto L70;
		}

/*              Set up parameters with ZLATB4 and generate a t
est matrix   
                with ZLATMS. */

		zlatb4_(path, &imat, &m, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "ZLATMS", 6L, 6L);
		zlatms_(&m, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, "No packing", &a[1], &lda, &work[1],
			 &info);

/*              Check error code from ZLATMS. */

		if (info != 0) {
		    alaerh_(path, "ZLATMS", &info, &c__0, " ", &m, &n, &c_n1, 
			    &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L70;
		}

/*              For types 5-7, zero one or more columns of the
 matrix to   
                test that INFO is returned correctly. */

		if (zerot) {
		    if (imat == 5) {
			izero = 1;
		    } else if (imat == 6) {
			izero = min(m,n);
		    } else {
			izero = min(m,n) / 2 + 1;
		    }
		    ioff = (izero - 1) * lda;
		    if (imat < 7) {
			i__4 = m;
			for (i = 1; i <= i__4; ++i) {
			    i__5 = ioff + i;
			    a[i__5].r = 0., a[i__5].i = 0.;
/* L20: */
			}
		    } else {
			i__4 = n - izero + 1;
			zlaset_("Full", &m, &i__4, &c_b21, &c_b21, &a[ioff + 
				1], &lda);
		    }
		} else {
		    izero = 0;
		}

/*              These lines, if used in place of the calls in 
the DO 60   
                loop, cause the code to bomb on a Sun SPARCsta
tion.   

                 ANORMO = ZLANGE( 'O', M, N, A, LDA, RWORK ) 
  
                 ANORMI = ZLANGE( 'I', M, N, A, LDA, RWORK ) 
  

                Do for each blocksize in NBVAL */

		i__4 = *nnb;
		for (inb = 1; inb <= i__4; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/*                 Compute the LU factorization of the mat
rix. */

		    zlacpy_("Full", &m, &n, &a[1], &lda, &afac[1], &lda);
		    s_copy(srnamc_1.srnamt, "ZGETRF", 6L, 6L);
		    zgetrf_(&m, &n, &afac[1], &lda, &iwork[1], &info);

/*                 Check error code from ZGETRF. */

		    if (info != izero) {
			alaerh_(path, "ZGETRF", &info, &izero, " ", &m, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
		    }
		    trfcon = FALSE_;

/* +    TEST 1   
                   Reconstruct matrix from factors and com
pute residual. */

		    zlacpy_("Full", &m, &n, &afac[1], &lda, &ainv[1], &lda);
		    zget01_(&m, &n, &a[1], &lda, &ainv[1], &lda, &iwork[1], &
			    rwork[1], result);
		    nt = 1;

/* +    TEST 2   
                   Form the inverse if the factorization w
as successful   
                   and compute the residual. */

		    if (m == n && info == 0) {
			zlacpy_("Full", &n, &n, &afac[1], &lda, &ainv[1], &
				lda);
			s_copy(srnamc_1.srnamt, "ZGETRI", 6L, 6L);
			lwork = *nmax * max(3,*nrhs);
			zgetri_(&n, &ainv[1], &lda, &iwork[1], &work[1], &
				lwork, &info);

/*                    Check error code from ZGETRI. */

			if (info != 0) {
			    alaerh_(path, "ZGETRI", &info, &c__0, " ", &n, &n,
				     &c_n1, &c_n1, &nb, &imat, &nfail, &nerrs,
				     nout);
			}

/*                    Compute the residual for the mat
rix times its   
                      inverse.  Also compute the 1-nor
m condition number   
                      of A. */

			zget03_(&n, &a[1], &lda, &ainv[1], &lda, &work[1], &
				lda, &rwork[1], &rcondo, &result[1]);
			anormo = zlange_("O", &m, &n, &a[1], &lda, &rwork[1]);

/*                    Compute the infinity-norm condit
ion number of A. */

			anormi = zlange_("I", &m, &n, &a[1], &lda, &rwork[1]);
			ainvnm = zlange_("I", &n, &n, &ainv[1], &lda, &rwork[
				1]);
			if (anormi <= 0. || ainvnm <= 0.) {
			    rcondi = 1.;
			} else {
			    rcondi = 1. / anormi / ainvnm;
			}
			nt = 2;
		    } else {

/*                    Do only the condition estimate i
f INFO > 0. */

			trfcon = TRUE_;
			anormo = zlange_("O", &m, &n, &a[1], &lda, &rwork[1]);
			anormi = zlange_("I", &m, &n, &a[1], &lda, &rwork[1]);
			rcondo = 0.;
			rcondi = 0.;
		    }

/*                 Print information about the tests so fa
r that did not   
                   pass the threshold. */

		    i__5 = nt;
		    for (k = 1; k <= i__5; ++k) {
			if (result[k - 1] >= *thresh) {
			    if (nfail == 0 && nerrs == 0) {
				alahd_(nout, path);
			    }
			    io___40.ciunit = *nout;
			    s_wsfe(&io___40);
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&result[k - 1], (ftnlen)
				    sizeof(doublereal));
			    e_wsfe();
			    ++nfail;
			}
/* L30: */
		    }
		    nrun += nt;

/*                 Skip the remaining tests if this is not
 the first   
                   block size or if M .ne. N. */

		    if (inb > 1 || m != n) {
			goto L60;
		    }

		    for (itran = 1; itran <= 3; ++itran) {
			*(unsigned char *)trans = *(unsigned char *)&transs[
				itran - 1];
			if (itran == 1) {
			    anorm = anormo;
			    rcondc = rcondo;
			    *(unsigned char *)norm = 'O';
			} else {
			    anorm = anormi;
			    rcondc = rcondi;
			    *(unsigned char *)norm = 'I';
			}
			if (trfcon) {
			    k1 = 8;
			} else {
			    k1 = 3;

/* +    TEST 3   
                         Solve and compute residua
l for A * X = B. */

			    s_copy(srnamc_1.srnamt, "ZLARHS", 6L, 6L);
			    zlarhs_(path, xtype, " ", trans, &n, &n, &kl, &ku,
				     nrhs, &a[1], &lda, &xact[1], &lda, &b[1],
				     &lda, iseed, &info);
			    *(unsigned char *)xtype = 'C';

			    zlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &
				    lda);
			    s_copy(srnamc_1.srnamt, "ZGETRS", 6L, 6L);
			    zgetrs_(trans, &n, nrhs, &afac[1], &lda, &iwork[1]
				    , &x[1], &lda, &info);

/*                       Check error code from ZGE
TRS. */

			    if (info != 0) {
				alaerh_(path, "ZGETRS", &info, &c__0, trans, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

			    zlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &
				    lda);
			    zget02_(trans, &n, &n, nrhs, &a[1], &lda, &x[1], &
				    lda, &work[1], &lda, &rwork[1], &result[2]
				    );

/* +    TEST 4   
                         Check solution from gener
ated exact solution. */

			    zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[3]);

/* +    TESTS 5, 6, and 7   
                         Use iterative refinement 
to improve the   
                         solution. */

			    s_copy(srnamc_1.srnamt, "ZGERFS", 6L, 6L);
			    zgerfs_(trans, &n, nrhs, &a[1], &lda, &afac[1], &
				    lda, &iwork[1], &b[1], &lda, &x[1], &lda, 
				    &rwork[1], &rwork[*nrhs + 1], &work[1], &
				    rwork[(*nrhs << 1) + 1], &info);

/*                       Check error code from ZGE
RFS. */

			    if (info != 0) {
				alaerh_(path, "ZGERFS", &info, &c__0, trans, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

			    zget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[4]);
			    zget07_(trans, &n, nrhs, &a[1], &lda, &b[1], &lda,
				     &x[1], &lda, &xact[1], &lda, &rwork[1], &
				    rwork[*nrhs + 1], &result[5]);
			    nt = 7;
			}

/* +    TEST 8   
                      Get an estimate of RCOND = 1/CND
NUM. */

			if (itran <= 2) {
			    s_copy(srnamc_1.srnamt, "ZGECON", 6L, 6L);
			    zgecon_(norm, &n, &afac[1], &lda, &anorm, &rcond, 
				    &work[1], &rwork[1], &info);

/*                       Check error code from ZGE
CON. */

			    if (info != 0) {
				alaerh_(path, "ZGECON", &info, &c__0, norm, &
					n, &n, &c_n1, &c_n1, &c_n1, &imat, &
					nfail, &nerrs, nout);
			    }

/*                       This line is needed on a 
Sun SPARCstation. */

			    dummy = rcond;

			    result[7] = dget06_(&rcond, &rcondc);
			    nt = 8;
			}

/*                    Print information about the test
s that did not pass   
                      the threshold. */

			i__5 = nt;
			for (k = k1; k <= i__5; ++k) {
			    if (result[k - 1] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    alahd_(nout, path);
				}
				if (k < 8) {
				    io___48.ciunit = *nout;
				    s_wsfe(&io___48);
				    do_fio(&c__1, trans, 1L);
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&imat, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(doublereal));
				    e_wsfe();
				} else {
				    io___49.ciunit = *nout;
				    s_wsfe(&io___49);
				    do_fio(&c__1, norm, 1L);
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&imat, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(doublereal));
				    e_wsfe();
				}
				++nfail;
			    }
/* L40: */
			}
			nrun = nrun + nt - k1 + 1;
/* L50: */
		    }
L60:
		    ;
		}
L70:
		;
	    }

/* L80: */
	}
/* L90: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of ZCHKGE */

} /* zchkge_ */

