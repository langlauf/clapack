/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, iounit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static doublecomplex c_b1 = {1.,0.};
static doublecomplex c_b2 = {0.,0.};
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__2 = 2;
static integer c__0 = 0;
static integer c_n1 = -1;
static doublereal c_b67 = -1.;

/* Subroutine */ int zdrvls_(logical *dotype, integer *nm, integer *mval, 
	integer *nn, integer *nval, integer *nns, integer *nsval, integer *
	nnb, integer *nbval, integer *nxval, doublereal *thresh, logical *
	tsterr, doublecomplex *a, doublecomplex *copya, doublecomplex *b, 
	doublecomplex *copyb, doublereal *s, doublereal *copys, doublecomplex 
	*tau, doublecomplex *work, doublereal *rwork, integer *iwork, integer 
	*nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };

    /* Format strings */
    static char fmt_9999[] = "(\002 TRANS='\002,a1,\002', M=\002,i5,\002, N"
	    "=\002,i5,\002, NRHS=\002,i4,\002, NB=\002,i4,\002, type\002,i2"
	    ",\002, test(\002,i2,\002)=\002,g12.5)";
    static char fmt_9998[] = "(\002 M=\002,i5,\002, N=\002,i5,\002, NRHS="
	    "\002,i4,\002, NB=\002,i4,\002, type\002,i2,\002, test(\002,i2"
	    ",\002)=\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    doublereal d__1;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static char path[3];
    static integer rank, nrhs, nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer m, n, nfail, iseed[4], crank, irank;
    static doublereal rcond;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static integer itran, mnmin, ncols;
    static doublereal norma, normb;
    extern /* Subroutine */ int zgels_(char *, integer *, integer *, integer *
	    , doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *), daxpy_(integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *), 
	    zgemm_(char *, char *, integer *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *);
    static char trans[1];
    static integer nerrs, itype, lwork;
    extern doublereal zqrt12_(integer *, integer *, doublecomplex *, integer *
	    , doublereal *, doublecomplex *, integer *, doublereal *), 
	    zqrt14_(char *, integer *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zqrt13_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *, integer *), zqrt15_(
	    integer *, integer *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublecomplex *, integer *);
    static integer nrows;
    extern doublereal zqrt17_(char *, integer *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zqrt16_(char *, integer *, integer *, integer 
	    *, doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublereal *, doublereal *);
    extern doublereal dznrm2_(integer *, doublecomplex *, integer *);
    static integer nb, im, in;
    extern doublereal dlamch_(char *);
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer iscale, lw;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *), alasvm_(char *, integer *, integer *,
	     integer *, integer *), xlaenv_(integer *, integer *);
    static integer ldwork;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), 
	    zgelss_(integer *, integer *, integer *, doublecomplex *, integer 
	    *, doublecomplex *, integer *, doublereal *, doublereal *, 
	    integer *, doublecomplex *, integer *, doublereal *, integer *), 
	    zgelsx_(integer *, integer *, integer *, doublecomplex *, integer 
	    *, doublecomplex *, integer *, integer *, doublereal *, integer *,
	     doublecomplex *, doublereal *, integer *), zlarnv_(integer *, 
	    integer *, integer *, doublecomplex *);
    static doublereal result[10];
    extern /* Subroutine */ int zerrls_(char *, integer *);
    static integer lda, ldb, inb;
    static doublereal eps;
    static integer ins;

    /* Fortran I/O blocks */
    static cilist io___36 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZDRVLS tests the least squares driver routines ZGELS, CGELSX, and   
    ZGELSS.   

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
            The number of values of NB and NX contained in the   
            vectors NBVAL and NXVAL.  The blocking parameters are used   
            in pairs (NB,NX).   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

    NXVAL   (input) INTEGER array, dimension (NNB)   
            The values of the crossover point NX.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    THRESH  (input) DOUBLE PRECISION   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    A       (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   
            where MMAX is the maximum value of M in MVAL and NMAX is the 
  
            maximum value of N in NVAL.   

    COPYA   (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   

    B       (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   
            where MMAX is the maximum value of M in MVAL and NMAX is the 
  
            maximum value of N in NVAL.   

    COPYB   (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)   

    S       (workspace) DOUBLE PRECISION array, dimension   
                        (min(MMAX,NMAX))   

    COPYS   (workspace) DOUBLE PRECISION array, dimension   
                        (min(MMAX,NMAX))   

    TAU     (workspace) COMPLEX*16 array, dimension (MMAX)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (MMAX*NMAX + 4*NMAX + MMAX)   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (5*NMAX-1)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --iwork;
    --rwork;
    --work;
    --tau;
    --copys;
    --s;
    --copyb;
    --b;
    --copya;
    --a;
    --nxval;
    --nbval;
    --nsval;
    --nval;
    --mval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "LS", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }
    eps = dlamch_("Epsilon");

/*     Threshold for rank estimation */

    rcond = sqrt(eps) - (sqrt(eps) - eps) / 2;

/*     Test the error exits */

    if (*tsterr) {
	zerrls_(path, nout);
    }
    infoc_1.infot = 0;

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	lda = max(1,m);

	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];
	    mnmin = min(m,n);
/* Computing MAX */
	    i__3 = max(1,m);
	    ldb = max(i__3,n);

	    i__3 = *nns;
	    for (ins = 1; ins <= i__3; ++ins) {
		nrhs = nsval[ins];
/* Computing MAX */
		i__4 = 1, i__5 = (m + nrhs) * (n + 2), i__4 = max(i__4,i__5), 
			i__5 = m * n + (mnmin << 2) + max(m,n), i__4 = max(
			i__4,i__5), i__5 = (n << 1) + m;
		lwork = max(i__4,i__5);

		for (irank = 1; irank <= 2; ++irank) {
		    for (iscale = 1; iscale <= 3; ++iscale) {
			itype = (irank - 1) * 3 + iscale;

			if (irank == 1) {

/*                       Test ZGELS   

                         Generate a matrix of scal
ing type ISCALE */

			    zqrt13_(&iscale, &m, &n, &copya[1], &lda, &norma, 
				    iseed);
			    i__4 = *nnb;
			    for (inb = 1; inb <= i__4; ++inb) {
				nb = nbval[inb];
				xlaenv_(&c__1, &nb);
				xlaenv_(&c__3, &nxval[inb]);

				for (itran = 1; itran <= 2; ++itran) {
				    if (itran == 1) {
					*(unsigned char *)trans = 'N';
					nrows = m;
					ncols = n;
				    } else {
					*(unsigned char *)trans = 'C';
					nrows = n;
					ncols = m;
				    }
/* Computing MAX   
   Computing MAX */
				    i__7 = max(m,n);
				    i__5 = 1, i__6 = (nrows + nrhs) * (ncols 
					    + 2), i__5 = max(i__5,i__6), i__6 
					    = mnmin + max(i__7,nrhs) * max(1,
					    nb);
				    lw = max(i__5,i__6);
				    ldwork = max(1,ncols);

/*                             Set up a co
nsistent rhs */

				    if (ncols > 0) {
					i__5 = ncols * nrhs;
					zlarnv_(&c__2, iseed, &i__5, &work[1])
						;
					i__5 = nrhs;
					for (j = 1; j <= i__5; ++j) {
					    d__1 = 1. / dznrm2_(&ncols, &work[
						    (j - 1) * ncols + 1], &
						    c__1);
					    zdscal_(&ncols, &d__1, &work[(j - 
						    1) * ncols + 1], &c__1);
/* L20: */
					}
				    }
				    zgemm_(trans, "No transpose", &nrows, &
					    nrhs, &ncols, &c_b1, &copya[1], &
					    lda, &work[1], &ldwork, &c_b2, &b[
					    1], &ldb);
				    zlacpy_("Full", &nrows, &nrhs, &b[1], &
					    ldb, &copyb[1], &ldb);

/*                             Solve LS or
 overdetermined system */

				    if (m > 0 && n > 0) {
					zlacpy_("Full", &m, &n, &copya[1], &
						lda, &a[1], &lda);
					zlacpy_("Full", &nrows, &nrhs, &copyb[
						1], &ldb, &b[1], &ldb);
				    }
				    s_copy(srnamc_1.srnamt, "ZGELS ", 6L, 6L);
				    zgels_(trans, &m, &n, &nrhs, &a[1], &lda, 
					    &b[1], &ldb, &work[1], &lw, &info);

				    if (info != 0) {
					alaerh_(path, "ZGELS ", &info, &c__0, 
						trans, &m, &n, &nrhs, &c_n1, &
						nb, &itype, &nfail, &nerrs, 
						nout);
				    }

/*                             Check corre
ctness of results */

				    ldwork = max(1,nrows);
				    if (nrows > 0 && nrhs > 0) {
					zlacpy_("Full", &nrows, &nrhs, &copyb[
						1], &ldb, &work[1], &ldwork);
				    }
				    zqrt16_(trans, &m, &n, &nrhs, &copya[1], &
					    lda, &b[1], &ldb, &work[1], &
					    ldwork, &rwork[1], result);

				    if (itran == 1 && m >= n || itran == 2 && 
					    m < n) {

/*                                Solv
ing LS system */

					result[1] = zqrt17_(trans, &c__1, &m, 
						&n, &nrhs, &copya[1], &lda, &
						b[1], &ldb, &copyb[1], &ldb, &
						work[1], &lw);
				    } else {

/*                                Solv
ing overdetermined system */

					result[1] = zqrt14_(trans, &m, &n, &
						nrhs, &copya[1], &lda, &b[1], 
						&ldb, &work[1], &lw);
				    }

/*                             Print infor
mation about the tests that   
                               did not pas
s the threshold. */

				    for (k = 1; k <= 2; ++k) {
					if (result[k - 1] >= *thresh) {
					    if (nfail == 0 && nerrs == 0) {
			  alahd_(nout, path);
					    }
					    io___36.ciunit = *nout;
					    s_wsfe(&io___36);
					    do_fio(&c__1, trans, 1L);
					    do_fio(&c__1, (char *)&m, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&n, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&nrhs, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&nb, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&itype, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&k, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&result[k - 
						    1], (ftnlen)sizeof(
						    doublereal));
					    e_wsfe();
					    ++nfail;
					}
/* L30: */
				    }
				    nrun += 2;
/* L40: */
				}
/* L50: */
			    }
			}

/*                    Generate a matrix of scaling typ
e ISCALE and rank   
                      type IRANK. */

			zqrt15_(&iscale, &irank, &m, &n, &nrhs, &copya[1], &
				lda, &copyb[1], &ldb, &copys[1], &rank, &
				norma, &normb, iseed, &work[1], &lwork);

/*                    workspace used: MAX(M+MIN(M,N),N
RHS*MIN(M,N),2*N+M) */

			i__4 = n;
			for (j = 1; j <= i__4; ++j) {
			    iwork[j] = 0;
/* L60: */
			}
			ldwork = max(1,m);

			i__4 = *nnb;
			for (inb = 1; inb <= i__4; ++inb) {
			    nb = nbval[inb];
			    xlaenv_(&c__1, &nb);
			    xlaenv_(&c__3, &nxval[inb]);

			    zlacpy_("Full", &m, &n, &copya[1], &lda, &a[1], &
				    lda);
			    zlacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &b[1],
				     &ldb);

/*                       ZGELSX:  Compute the mini
mum-norm solution X to   
                            min( norm( A * X - B )
 )   
                         using a complete orthogon
al factorization. */

			    s_copy(srnamc_1.srnamt, "ZGELSX", 6L, 6L);
			    zgelsx_(&m, &n, &nrhs, &a[1], &lda, &b[1], &ldb, &
				    iwork[1], &rcond, &crank, &work[1], &
				    rwork[1], &info);

			    if (info != 0) {
				alaerh_(path, "ZGELSX", &info, &c__0, " ", &m,
					 &n, &nrhs, &c_n1, &nb, &itype, &
					nfail, &nerrs, nout);
			    }

/*                       workspace used: MAX( MNMI
N+3*N, 2*MNMIN+NRHS )   

                         Test 3:  Compute relative
 error in svd   
                                  workspace: M*N +
 4*MIN(M,N) + MAX(M,N) */

			    result[2] = zqrt12_(&crank, &crank, &a[1], &lda, &
				    copys[1], &work[1], &lwork, &rwork[1]);

/*                       Test 4:  Compute error in
 solution   
                                  workspace:  M*NR
HS + M */

			    zlacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &work[
				    1], &ldwork);
			    zqrt16_("No transpose", &m, &n, &nrhs, &copya[1], 
				    &lda, &b[1], &ldb, &work[1], &ldwork, &
				    rwork[1], &result[3]);

/*                       Test 5:  Check norm of r'
*A   
                                  workspace: NRHS*
(M+N) */

			    result[4] = 0.;
			    if (m > crank) {
				result[4] = zqrt17_("No transpose", &c__1, &m,
					 &n, &nrhs, &copya[1], &lda, &b[1], &
					ldb, &copyb[1], &ldb, &work[1], &
					lwork);
			    }

/*                       Test 6:  Check if x is in
 the rowspace of A   
                                  workspace: (M+NR
HS)*(N+2) */

			    result[5] = 0.;

			    if (n > crank) {
				result[5] = zqrt14_("No transpose", &m, &n, &
					nrhs, &copya[1], &lda, &b[1], &ldb, &
					work[1], &lwork);
			    }

/*                       ZGELSS:  Compute the mini
mum-norm solution X to   
                            min( norm( A * X - B )
 )   
                         using the SVD. */

			    zlacpy_("Full", &m, &n, &copya[1], &lda, &a[1], &
				    lda);
			    zlacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &b[1],
				     &ldb);
			    s_copy(srnamc_1.srnamt, "ZGELSS", 6L, 6L);
			    zgelss_(&m, &n, &nrhs, &a[1], &lda, &b[1], &ldb, &
				    s[1], &rcond, &crank, &work[1], &lwork, &
				    rwork[1], &info);

			    if (info != 0) {
				alaerh_(path, "ZGELSS", &info, &c__0, " ", &m,
					 &n, &nrhs, &c_n1, &nb, &itype, &
					nfail, &nerrs, nout);
			    }

/*                       workspace used: 3*min(m,n
) +   
                                         max(2*min
(m,n),nrhs,max(m,n))   

                         Test 7:  Compute relative
 error in svd */

			    if (rank > 0) {
				daxpy_(&mnmin, &c_b67, &copys[1], &c__1, &s[1]
					, &c__1);
				result[6] = dasum_(&mnmin, &s[1], &c__1) / 
					dasum_(&mnmin, &copys[1], &c__1) / (
					eps * (doublereal) mnmin);
			    } else {
				result[6] = 0.;
			    }

/*                       Test 8:  Compute error in
 solution */

			    zlacpy_("Full", &m, &nrhs, &copyb[1], &ldb, &work[
				    1], &ldwork);
			    zqrt16_("No transpose", &m, &n, &nrhs, &copya[1], 
				    &lda, &b[1], &ldb, &work[1], &ldwork, &
				    rwork[1], &result[7]);

/*                       Test 9:  Check norm of r'
*A */

			    result[8] = 0.;
			    if (m > crank) {
				result[8] = zqrt17_("No transpose", &c__1, &m,
					 &n, &nrhs, &copya[1], &lda, &b[1], &
					ldb, &copyb[1], &ldb, &work[1], &
					lwork);
			    }

/*                       Test 10:  Check if x is i
n the rowspace of A */

			    result[9] = 0.;
			    if (n > crank) {
				result[9] = zqrt14_("No transpose", &m, &n, &
					nrhs, &copya[1], &lda, &b[1], &ldb, &
					work[1], &lwork);
			    }

/*                       Print information about t
he tests that did not   
                         pass the threshold. */

			    for (k = 3; k <= 10; ++k) {
				if (result[k - 1] >= *thresh) {
				    if (nfail == 0 && nerrs == 0) {
					alahd_(nout, path);
				    }
				    io___40.ciunit = *nout;
				    s_wsfe(&io___40);
				    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&nrhs, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&itype, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(doublereal));
				    e_wsfe();
				    ++nfail;
				}
/* L70: */
			    }
			    nrun += 8;

/* L80: */
			}
/* L90: */
		    }
/* L100: */
		}
/* L110: */
	    }
/* L120: */
	}
/* L130: */
    }

/*     Print a summary of the results. */

    alasvm_(path, nout, &nfail, &nrun, &nerrs);


/*     End of ZDRVLS */

    return 0;
} /* zdrvls_ */

