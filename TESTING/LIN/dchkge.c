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
static doublereal c_b21 = 0.;
static integer c__1 = 1;

/* Subroutine */ int dchkge_(logical *dotype, integer *nm, integer *mval, 
	integer *nn, integer *nval, integer *nnb, integer *nbval, integer *
	nrhs, doublereal *thresh, logical *tsterr, integer *nmax, doublereal *
	a, doublereal *afac, doublereal *ainv, doublereal *b, doublereal *x, 
	doublereal *xact, doublereal *work, doublereal *rwork, integer *iwork,
	 integer *nout)
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
    static integer k, m, n;
    extern /* Subroutine */ int dget01_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *), dget02_(char *, integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *), dget03_(integer *,
	     doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *), dget04_(
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *);
    static integer nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    extern /* Subroutine */ int dget07_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *);
    static doublereal rcond;
    static integer nimat;
    static doublereal anorm;
    static integer itran;
    static char trans[1];
    static integer izero, nerrs, k1;
    static doublereal dummy;
    static integer lwork;
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int dlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer nb, im, in, kl;
    extern doublereal dlange_(char *, integer *, integer *, doublereal *, 
	    integer *, doublereal *);
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), dgecon_(char *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *);
    static integer ku, nt;
    static doublereal rcondc;
    extern /* Subroutine */ int derrge_(char *, integer *), dgerfs_(
	    char *, integer *, integer *, doublereal *, integer *, doublereal 
	    *, integer *, integer *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    integer *), dgetrf_(integer *, integer *, doublereal *, 
	    integer *, integer *, integer *), dlacpy_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *), dlarhs_(char *, char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, integer 
	    *, doublereal *, integer *, doublereal *, integer *, integer *, 
	    integer *);
    static doublereal rcondi;
    extern /* Subroutine */ int dgetri_(integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, integer *), dlaset_(char *, 
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    integer *), alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal cndnum, anormi, rcondo;
    extern /* Subroutine */ int dlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublereal *, integer *, doublereal 
	    *, integer *);
    static doublereal ainvnm;
    extern /* Subroutine */ int dgetrs_(char *, integer *, integer *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *);
    static logical trfcon;
    static doublereal anormo;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static doublereal result[8];
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

    DCHKGE tests DGETRF, -TRI, -TRS, -RFS, and -CON.   

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

    A       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    AFAC    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    AINV    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)   

    B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    X       (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    XACT    (workspace) DOUBLE PRECISION array, dimension (NMAX*NRHS)   

    WORK    (workspace) DOUBLE PRECISION array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
                        (max(2*NMAX,2*NRHS+NWORK))   

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
    --mval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'D';
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
	derrge_(path, nout);
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

/*              Set up parameters with DLATB4 and generate a t
est matrix   
                with DLATMS. */

		dlatb4_(path, &imat, &m, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "DLATMS", 6L, 6L);
		dlatms_(&m, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, "No packing", &a[1], &lda, &work[1],
			 &info);

/*              Check error code from DLATMS. */

		if (info != 0) {
		    alaerh_(path, "DLATMS", &info, &c__0, " ", &m, &n, &c_n1, 
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
			    a[ioff + i] = 0.;
/* L20: */
			}
		    } else {
			i__4 = n - izero + 1;
			dlaset_("Full", &m, &i__4, &c_b21, &c_b21, &a[ioff + 
				1], &lda);
		    }
		} else {
		    izero = 0;
		}

/*              These lines, if used in place of the calls in 
the DO 60   
                loop, cause the code to bomb on a Sun SPARCsta
tion.   

                 ANORMO = DLANGE( 'O', M, N, A, LDA, RWORK ) 
  
                 ANORMI = DLANGE( 'I', M, N, A, LDA, RWORK ) 
  

                Do for each blocksize in NBVAL */

		i__4 = *nnb;
		for (inb = 1; inb <= i__4; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/*                 Compute the LU factorization of the mat
rix. */

		    dlacpy_("Full", &m, &n, &a[1], &lda, &afac[1], &lda);
		    s_copy(srnamc_1.srnamt, "DGETRF", 6L, 6L);
		    dgetrf_(&m, &n, &afac[1], &lda, &iwork[1], &info);

/*                 Check error code from DGETRF. */

		    if (info != izero) {
			alaerh_(path, "DGETRF", &info, &izero, " ", &m, &n, &
				c_n1, &c_n1, &nb, &imat, &nfail, &nerrs, nout);
		    }
		    trfcon = FALSE_;

/* +    TEST 1   
                   Reconstruct matrix from factors and com
pute residual. */

		    dlacpy_("Full", &m, &n, &afac[1], &lda, &ainv[1], &lda);
		    dget01_(&m, &n, &a[1], &lda, &ainv[1], &lda, &iwork[1], &
			    rwork[1], result);
		    nt = 1;

/* +    TEST 2   
                   Form the inverse if the factorization w
as successful   
                   and compute the residual. */

		    if (m == n && info == 0) {
			dlacpy_("Full", &n, &n, &afac[1], &lda, &ainv[1], &
				lda);
			s_copy(srnamc_1.srnamt, "DGETRI", 6L, 6L);
			lwork = *nmax * max(3,*nrhs);
			dgetri_(&n, &ainv[1], &lda, &iwork[1], &work[1], &
				lwork, &info);

/*                    Check error code from DGETRI. */

			if (info != 0) {
			    alaerh_(path, "DGETRI", &info, &c__0, " ", &n, &n,
				     &c_n1, &c_n1, &nb, &imat, &nfail, &nerrs,
				     nout);
			}

/*                    Compute the residual for the mat
rix times its   
                      inverse.  Also compute the 1-nor
m condition number   
                      of A. */

			dget03_(&n, &a[1], &lda, &ainv[1], &lda, &work[1], &
				lda, &rwork[1], &rcondo, &result[1]);
			anormo = dlange_("O", &m, &n, &a[1], &lda, &rwork[1]);

/*                    Compute the infinity-norm condit
ion number of A. */

			anormi = dlange_("I", &m, &n, &a[1], &lda, &rwork[1]);
			ainvnm = dlange_("I", &n, &n, &ainv[1], &lda, &rwork[
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
			anormo = dlange_("O", &m, &n, &a[1], &lda, &rwork[1]);
			anormi = dlange_("I", &m, &n, &a[1], &lda, &rwork[1]);
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

			    s_copy(srnamc_1.srnamt, "DLARHS", 6L, 6L);
			    dlarhs_(path, xtype, " ", trans, &n, &n, &kl, &ku,
				     nrhs, &a[1], &lda, &xact[1], &lda, &b[1],
				     &lda, iseed, &info);
			    *(unsigned char *)xtype = 'C';

			    dlacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &
				    lda);
			    s_copy(srnamc_1.srnamt, "DGETRS", 6L, 6L);
			    dgetrs_(trans, &n, nrhs, &afac[1], &lda, &iwork[1]
				    , &x[1], &lda, &info);

/*                       Check error code from DGE
TRS. */

			    if (info != 0) {
				alaerh_(path, "DGETRS", &info, &c__0, trans, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

			    dlacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &
				    lda);
			    dget02_(trans, &n, &n, nrhs, &a[1], &lda, &x[1], &
				    lda, &work[1], &lda, &rwork[1], &result[2]
				    );

/* +    TEST 4   
                         Check solution from gener
ated exact solution. */

			    dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[3]);

/* +    TESTS 5, 6, and 7   
                         Use iterative refinement 
to improve the   
                         solution. */

			    s_copy(srnamc_1.srnamt, "DGERFS", 6L, 6L);
			    dgerfs_(trans, &n, nrhs, &a[1], &lda, &afac[1], &
				    lda, &iwork[1], &b[1], &lda, &x[1], &lda, 
				    &rwork[1], &rwork[*nrhs + 1], &work[1], &
				    iwork[n + 1], &info);

/*                       Check error code from DGE
RFS. */

			    if (info != 0) {
				alaerh_(path, "DGERFS", &info, &c__0, trans, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
			    }

			    dget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[4]);
			    dget07_(trans, &n, nrhs, &a[1], &lda, &b[1], &lda,
				     &x[1], &lda, &xact[1], &lda, &rwork[1], &
				    rwork[*nrhs + 1], &result[5]);
			    nt = 7;
			}

/* +    TEST 8   
                      Get an estimate of RCOND = 1/CND
NUM. */

			if (itran <= 2) {
			    s_copy(srnamc_1.srnamt, "DGECON", 6L, 6L);
			    dgecon_(norm, &n, &afac[1], &lda, &anorm, &rcond, 
				    &work[1], &iwork[n + 1], &info);

/*                       Check error code from DGE
CON. */

			    if (info != 0) {
				alaerh_(path, "DGECON", &info, &c__0, norm, &
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

/*     End of DCHKGE */

} /* dchkge_ */

