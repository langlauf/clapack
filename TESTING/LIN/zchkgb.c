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

static integer c__1 = 1;
static integer c__0 = 0;
static integer c_n1 = -1;
static doublecomplex c_b61 = {0.,0.};
static doublecomplex c_b62 = {1.,0.};

/* Subroutine */ int zchkgb_(logical *dotype, integer *nm, integer *mval, 
	integer *nn, integer *nval, integer *nnb, integer *nbval, integer *
	nrhs, doublereal *thresh, logical *tsterr, doublecomplex *a, integer *
	la, doublecomplex *afac, integer *lafac, doublecomplex *b, 
	doublecomplex *x, doublecomplex *xact, doublecomplex *work, 
	doublereal *rwork, integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char transs[1*3] = "N" "T" "C";

    /* Format strings */
    static char fmt_9999[] = "(\002 *** In ZCHKGB, LA=\002,i5,\002 is too sm"
	    "all for M=\002,i5,\002, N=\002,i5,\002, KL=\002,i4,\002, KU=\002"
	    ",i4,/\002 ==> Increase LA to at least \002,i5)";
    static char fmt_9998[] = "(\002 *** In ZCHKGB, LAFAC=\002,i5,\002 is too"
	    " small for M=\002,i5,\002, N=\002,i5,\002, KL=\002,i4,\002, KU"
	    "=\002,i4,/\002 ==> Increase LAFAC to at least \002,i5)";
    static char fmt_9997[] = "(\002 M=\002,i5,\002,N=\002,i5,\002,KL=\002,"
	    "i5,\002,KU=\002,i5,\002, NB=\002,i4,\002, type \002,i1,\002, tes"
	    "t \002,i1,\002, ratio=\002,g12.5)";
    static char fmt_9996[] = "(\002 TRANS= '\002,a1,\002', N =\002,i5,\002, "
	    "KL =\002,i5,\002, KU =\002,i5,\002, type \002,i1,\002, test \002"
	    ",i1,\002, ratio=\002,g12.5)";
    static char fmt_9995[] = "(\002 NORM = '\002,a1,\002', N =\002,i5,\002, "
	    "KL =\002,i5,\002, KU =\002,i5,\002, type \002,i1,\002, test \002"
	    ",i1,\002, ratio=\002,g12.5)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10, 
	    i__11;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer ioff, mode, koff, imat, info;
    static char path[3], dist[1], norm[1], type[1];
    static integer nrun, i, j, k;
    extern /* Subroutine */ int alahd_(integer *, char *);
    static integer m, n, nfail, iseed[4];
    extern doublereal dget06_(doublereal *, doublereal *);
    static doublereal rcond;
    extern /* Subroutine */ int zgbt01_(integer *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     integer *, doublecomplex *, doublereal *);
    static integer nimat, klval[4];
    extern /* Subroutine */ int zgbt02_(char *, integer *, integer *, integer 
	    *, integer *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *), zgbt05_(char *, integer *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublecomplex *, integer *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal anorm;
    static integer itran;
    extern /* Subroutine */ int zget04_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *, doublereal *, doublereal *
	    );
    static integer kuval[4];
    static char trans[1];
    static integer izero, nerrs, i1, i2, k1;
    static logical zerot;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *);
    static char xtype[1];
    extern /* Subroutine */ int zlatb4_(char *, integer *, integer *, integer 
	    *, char *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, char *);
    static integer ldafac, nb, im, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer ku, nt;
    static doublereal rcondc;
    extern doublereal zlangb_(char *, integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublereal *);
    static doublereal rcondi;
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    extern /* Subroutine */ int alasum_(char *, integer *, integer *, integer 
	    *, integer *);
    static doublereal cndnum, anormi, rcondo;
    extern /* Subroutine */ int zgbcon_(char *, integer *, integer *, integer 
	    *, doublecomplex *, integer *, integer *, doublereal *, 
	    doublereal *, doublecomplex *, doublereal *, integer *);
    static doublereal ainvnm;
    static logical trfcon;
    static doublereal anormo;
    extern /* Subroutine */ int xlaenv_(integer *, integer *), zerrge_(char *,
	     integer *), zgbrfs_(char *, integer *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     integer *, integer *, doublecomplex *, integer *, doublecomplex *
	    , integer *, doublereal *, doublereal *, doublecomplex *, 
	    doublereal *, integer *), zgbtrf_(integer *, integer *, 
	    integer *, integer *, doublecomplex *, integer *, integer *, 
	    integer *), zlacpy_(char *, integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, integer *), zlarhs_(char *, 
	    char *, char *, char *, integer *, integer *, integer *, integer *
	    , integer *, doublecomplex *, integer *, doublecomplex *, integer 
	    *, doublecomplex *, integer *, integer *, integer *), zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *), zgbtrs_(char *, integer *, integer *, integer *, integer 
	    *, doublecomplex *, integer *, integer *, doublecomplex *, 
	    integer *, integer *), zlatms_(integer *, integer *, char 
	    *, integer *, char *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, char *, doublecomplex *, 
	    integer *, doublecomplex *, integer *);
    static doublereal result[7];
    static integer lda, ldb, inb, ikl, nkl, iku, nku;

    /* Fortran I/O blocks */
    static cilist io___25 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9995, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZCHKGB tests ZGBTRF, -TRS, -RFS, and -CON   

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

    A       (workspace) COMPLEX*16 array, dimension (LA)   

    LA      (input) INTEGER   
            The length of the array A.  LA >= (KLMAX+KUMAX+1)*NMAX   
            where KLMAX is the largest entry in local array KLVAL,   
                  KUMAX is the largest entry in local array KUVAL, and   
                  NMAX is the largest entry in input array NVAL.   

    AFAC    (workspace) COMPLEX*16 array, dimension (LAFAC)   

    LAFAC   (input) INTEGER   
            The length of the array AFAC. LAFAC >= (2*KLMAX+KUMAX+1)*NMAX 
  
            where KLMAX is the largest entry in local array KLVAL,   
                  KUMAX is the largest entry in local array KUVAL, and   
                  NMAX is the largest entry in input array NVAL.   

    B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)   

    WORK    (workspace) COMPLEX*16 array, dimension   
                        (NMAX*max(3,NRHS,NMAX))   

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
    --afac;
    --a;
    --nbval;
    --nval;
    --mval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'Z';
    s_copy(path + 1, "GB", 2L, 2L);
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

/*     Initialize the first value for the lower and upper bandwidths. */

    klval[0] = 0;
    kuval[0] = 0;

/*     Do for each value of M in MVAL */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];

/*        Set values to use for the lower bandwidth. */

	klval[1] = m + (m + 1) / 4;

/*        KLVAL( 2 ) = MAX( M-1, 0 ) */

	klval[2] = (m * 3 - 1) / 4;
	klval[3] = (m + 1) / 4;

/*        Do for each value of N in NVAL */

	i__2 = *nn;
	for (in = 1; in <= i__2; ++in) {
	    n = nval[in];
	    *(unsigned char *)xtype = 'N';

/*           Set values to use for the upper bandwidth. */

	    kuval[1] = n + (n + 1) / 4;

/*           KUVAL( 2 ) = MAX( N-1, 0 ) */

	    kuval[2] = (n * 3 - 1) / 4;
	    kuval[3] = (n + 1) / 4;

/*           Set limits on the number of loop iterations.   

   Computing MIN */
	    i__3 = m + 1;
	    nkl = min(i__3,4);
	    if (n == 0) {
		nkl = 2;
	    }
/* Computing MIN */
	    i__3 = n + 1;
	    nku = min(i__3,4);
	    if (m == 0) {
		nku = 2;
	    }
	    nimat = 8;
	    if (m <= 0 || n <= 0) {
		nimat = 1;
	    }

	    i__3 = nkl;
	    for (ikl = 1; ikl <= i__3; ++ikl) {

/*              Do for KL = 0, (5*M+1)/4, (3M-1)/4, and (M+1)/
4. This   
                order makes it easier to skip redundant values
 for small   
                values of M. */

		kl = klval[ikl - 1];
		i__4 = nku;
		for (iku = 1; iku <= i__4; ++iku) {

/*                 Do for KU = 0, (5*N+1)/4, (3N-1)/4, and
 (N+1)/4. This   
                   order makes it easier to skip redundant
 values for   
                   small values of N. */

		    ku = kuval[iku - 1];

/*                 Check that A and AFAC are big enough to
 generate this   
                   matrix. */

		    lda = kl + ku + 1;
		    ldafac = (kl << 1) + ku + 1;
		    if (lda * n > *la || ldafac * n > *lafac) {
			if (nfail == 0 && nerrs == 0) {
			    alahd_(nout, path);
			}
			if (n * (kl + ku + 1) > *la) {
			    io___25.ciunit = *nout;
			    s_wsfe(&io___25);
			    do_fio(&c__1, (char *)&(*la), (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&kl, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&ku, (ftnlen)sizeof(integer)
				    );
			    i__5 = n * (kl + ku + 1);
			    do_fio(&c__1, (char *)&i__5, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    ++nerrs;
			}
			if (n * ((kl << 1) + ku + 1) > *lafac) {
			    io___26.ciunit = *nout;
			    s_wsfe(&io___26);
			    do_fio(&c__1, (char *)&(*lafac), (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&kl, (ftnlen)sizeof(integer)
				    );
			    do_fio(&c__1, (char *)&ku, (ftnlen)sizeof(integer)
				    );
			    i__5 = n * ((kl << 1) + ku + 1);
			    do_fio(&c__1, (char *)&i__5, (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    ++nerrs;
			}
			goto L100;
		    }

		    i__5 = nimat;
		    for (imat = 1; imat <= i__5; ++imat) {

/*                    Do the tests only if DOTYPE( IMA
T ) is true. */

			if (! dotype[imat]) {
			    goto L90;
			}

/*                    Skip types 2, 3, or 4 if the mat
rix size is too   
                      small. */

			zerot = imat >= 2 && imat <= 4;
			if (zerot && n < imat - 1) {
			    goto L90;
			}

			if (! zerot || ! dotype[1]) {

/*                       Set up parameters with ZL
ATB4 and generate a   
                         test matrix with ZLATMS. 
*/

			    zlatb4_(path, &imat, &m, &n, type, &kl, &ku, &
				    anorm, &mode, &cndnum, dist);

/* Computing MAX */
			    i__6 = 1, i__7 = ku + 2 - n;
			    koff = max(i__6,i__7);
			    i__6 = koff - 1;
			    for (i = 1; i <= i__6; ++i) {
				i__7 = i;
				a[i__7].r = 0., a[i__7].i = 0.;
/* L20: */
			    }
			    s_copy(srnamc_1.srnamt, "ZLATMS", 6L, 6L);
			    zlatms_(&m, &n, dist, iseed, type, &rwork[1], &
				    mode, &cndnum, &anorm, &kl, &ku, "Z", &a[
				    koff], &lda, &work[1], &info);

/*                       Check the error code from
 ZLATMS. */

			    if (info != 0) {
				alaerh_(path, "ZLATMS", &info, &c__0, " ", &m,
					 &n, &kl, &ku, &c_n1, &imat, &nfail, &
					nerrs, nout);
				goto L90;
			    }
			} else if (izero > 0) {

/*                       Use the same matrix for t
ypes 3 and 4 as for   
                         type 2 by copying back th
e zeroed out column. */

			    i__6 = i2 - i1 + 1;
			    zcopy_(&i__6, &b[1], &c__1, &a[ioff + i1], &c__1);
			}

/*                    For types 2, 3, and 4, zero one 
or more columns of   
                      the matrix to test that INFO is 
returned correctly. */

			izero = 0;
			if (zerot) {
			    if (imat == 2) {
				izero = 1;
			    } else if (imat == 3) {
				izero = min(m,n);
			    } else {
				izero = min(m,n) / 2 + 1;
			    }
			    ioff = (izero - 1) * lda;
			    if (imat < 4) {

/*                          Store the column t
o be zeroed out in B.   

   Computing MAX */
				i__6 = 1, i__7 = ku + 2 - izero;
				i1 = max(i__6,i__7);
/* Computing MIN */
				i__6 = kl + ku + 1, i__7 = ku + 1 + (m - 
					izero);
				i2 = min(i__6,i__7);
				i__6 = i2 - i1 + 1;
				zcopy_(&i__6, &a[ioff + i1], &c__1, &b[1], &
					c__1);

				i__6 = i2;
				for (i = i1; i <= i__6; ++i) {
				    i__7 = ioff + i;
				    a[i__7].r = 0., a[i__7].i = 0.;
/* L30: */
				}
			    } else {
				i__6 = n;
				for (j = izero; j <= i__6; ++j) {
/* Computing MAX */
				    i__7 = 1, i__8 = ku + 2 - j;
/* Computing MIN */
				    i__10 = kl + ku + 1, i__11 = ku + 1 + (m 
					    - j);
				    i__9 = min(i__10,i__11);
				    for (i = max(i__7,i__8); i <= i__9; ++i) {
					i__7 = ioff + i;
					a[i__7].r = 0., a[i__7].i = 0.;
/* L40: */
				    }
				    ioff += lda;
/* L50: */
				}
			    }
			}

/*                    These lines, if used in place of
 the calls in the   
                      loop over INB, cause the code to
 bomb on a Sun   
                      SPARCstation.   

                       ANORMO = ZLANGB( 'O', N, KL, KU
, A, LDA, RWORK )   
                       ANORMI = ZLANGB( 'I', N, KL, KU
, A, LDA, RWORK )   

                      Do for each blocksize in NBVAL 
*/

			i__6 = *nnb;
			for (inb = 1; inb <= i__6; ++inb) {
			    nb = nbval[inb];
			    xlaenv_(&c__1, &nb);

/*                       Compute the LU factorizat
ion of the band matrix. */

			    if (m > 0 && n > 0) {
				i__9 = kl + ku + 1;
				zlacpy_("Full", &i__9, &n, &a[1], &lda, &afac[
					kl + 1], &ldafac);
			    }
			    s_copy(srnamc_1.srnamt, "ZGBTRF", 6L, 6L);
			    zgbtrf_(&m, &n, &kl, &ku, &afac[1], &ldafac, &
				    iwork[1], &info);

/*                       Check error code from ZGB
TRF. */

			    if (info != izero) {
				alaerh_(path, "ZGBTRF", &info, &izero, " ", &
					m, &n, &kl, &ku, &nb, &imat, &nfail, &
					nerrs, nout);
			    }
			    trfcon = FALSE_;

/* +    TEST 1   
                         Reconstruct matrix from f
actors and compute   
                         residual. */

			    zgbt01_(&m, &n, &kl, &ku, &a[1], &lda, &afac[1], &
				    ldafac, &iwork[1], &work[1], result);
			    nt = 1;

/*                       Print information about t
he tests so far that   
                         did not pass the threshol
d. */

			    if (result[0] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    alahd_(nout, path);
				}
				io___46.ciunit = *nout;
				s_wsfe(&io___46);
				do_fio(&c__1, (char *)&m, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&kl, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ku, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&imat, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&result[0], (ftnlen)
					sizeof(doublereal));
				e_wsfe();
				++nfail;
			    }
			    ++nrun;

/*                       Skip the remaining tests 
if this is not the   
                         first block size or if M 
.ne. N. */

			    if (inb > 1 || m != n) {
				goto L80;
			    }

			    anormo = zlangb_("O", &n, &kl, &ku, &a[1], &lda, &
				    rwork[1]);
			    anormi = zlangb_("I", &n, &kl, &ku, &a[1], &lda, &
				    rwork[1]);

			    if (info == 0) {

/*                          Form the inverse o
f A so we can get a good   
                            estimate of CNDNUM
 = norm(A) * norm(inv(A)). */

				ldb = max(1,n);
				zlaset_("Full", &n, &n, &c_b61, &c_b62, &work[
					1], &ldb);
				s_copy(srnamc_1.srnamt, "ZGBTRS", 6L, 6L);
				zgbtrs_("No transpose", &n, &kl, &ku, &n, &
					afac[1], &ldafac, &iwork[1], &work[1],
					 &ldb, &info);

/*                          Compute the 1-norm
 condition number of A. */

				ainvnm = zlange_("O", &n, &n, &work[1], &ldb, 
					&rwork[1]);
				if (anormo <= 0. || ainvnm <= 0.) {
				    rcondo = 1.;
				} else {
				    rcondo = 1. / anormo / ainvnm;
				}

/*                          Compute the infini
ty-norm condition number of   
                            A. */

				ainvnm = zlange_("I", &n, &n, &work[1], &ldb, 
					&rwork[1]);
				if (anormi <= 0. || ainvnm <= 0.) {
				    rcondi = 1.;
				} else {
				    rcondi = 1. / anormi / ainvnm;
				}
			    } else {

/*                          Do only the condit
ion estimate if INFO.NE.0. */

				trfcon = TRUE_;
				rcondo = 0.;
				rcondi = 0.;
			    }

			    for (itran = 1; itran <= 3; ++itran) {
				*(unsigned char *)trans = *(unsigned char *)&
					transs[itran - 1];
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
				    k1 = 7;
				} else {
				    k1 = 2;

/* +    TEST 2:   
                               Solve and c
ompute residual for A * X = B. */

				    s_copy(srnamc_1.srnamt, "ZLARHS", 6L, 6L);
				    zlarhs_(path, xtype, " ", trans, &n, &n, &
					    kl, &ku, nrhs, &a[1], &lda, &xact[
					    1], &ldb, &b[1], &ldb, iseed, &
					    info);
				    *(unsigned char *)xtype = 'C';
				    zlacpy_("Full", &n, nrhs, &b[1], &ldb, &x[
					    1], &ldb);

				    s_copy(srnamc_1.srnamt, "ZGBTRS", 6L, 6L);
				    zgbtrs_(trans, &n, &kl, &ku, nrhs, &afac[
					    1], &ldafac, &iwork[1], &x[1], &
					    ldb, &info);

/*                             Check error
 code from ZGBTRS. */

				    if (info != 0) {
					alaerh_(path, "ZGBTRS", &info, &c__0, 
						trans, &n, &n, &kl, &ku, &
						c_n1, &imat, &nfail, &nerrs, 
						nout);
				    }

				    zlacpy_("Full", &n, nrhs, &b[1], &ldb, &
					    work[1], &ldb);
				    zgbt02_(trans, &m, &n, &kl, &ku, nrhs, &a[
					    1], &lda, &x[1], &ldb, &work[1], &
					    ldb, &result[1]);

/* +    TEST 3:   
                               Check solut
ion from generated exact   
                               solution. 
*/

				    zget04_(&n, nrhs, &x[1], &ldb, &xact[1], &
					    ldb, &rcondc, &result[2]);

/* +    TESTS 4, 5, 6:   
                               Use iterati
ve refinement to improve the   
                               solution. 
*/

				    s_copy(srnamc_1.srnamt, "ZGBRFS", 6L, 6L);
				    zgbrfs_(trans, &n, &kl, &ku, nrhs, &a[1], 
					    &lda, &afac[1], &ldafac, &iwork[1]
					    , &b[1], &ldb, &x[1], &ldb, &
					    rwork[1], &rwork[*nrhs + 1], &
					    work[1], &rwork[(*nrhs << 1) + 1],
					     &info);

/*                             Check error
 code from ZGBRFS. */

				    if (info != 0) {
					alaerh_(path, "ZGBRFS", &info, &c__0, 
						trans, &n, &n, &kl, &ku, nrhs,
						 &imat, &nfail, &nerrs, nout);
				    }

				    zget04_(&n, nrhs, &x[1], &ldb, &xact[1], &
					    ldb, &rcondc, &result[3]);
				    zgbt05_(trans, &n, &kl, &ku, nrhs, &a[1], 
					    &lda, &b[1], &ldb, &x[1], &ldb, &
					    xact[1], &ldb, &rwork[1], &rwork[*
					    nrhs + 1], &result[4]);
				    nt = 6;
				}

/* +    TEST 7:   
                            Get an estimate of
 RCOND = 1/CNDNUM. */

				if (itran <= 2) {
				    s_copy(srnamc_1.srnamt, "ZGBCON", 6L, 6L);
				    zgbcon_(norm, &n, &kl, &ku, &afac[1], &
					    ldafac, &iwork[1], &anorm, &rcond,
					     &work[1], &rwork[1], &info);

/*                             Check error
 code from ZGBCON. */

				    if (info != 0) {
					alaerh_(path, "ZGBCON", &info, &c__0, 
						norm, &n, &n, &kl, &ku, &c_n1,
						 &imat, &nfail, &nerrs, nout);
				    }

				    result[6] = dget06_(&rcond, &rcondc);
				    nt = 7;
				}

/*                          Print information 
about the tests that did   
                            not pass the thres
hold. */

				i__9 = nt;
				for (k = k1; k <= i__9; ++k) {
				    if (result[k - 1] >= *thresh) {
					if (nfail == 0 && nerrs == 0) {
					    alahd_(nout, path);
					}
					if (k < 7) {
					    io___60.ciunit = *nout;
					    s_wsfe(&io___60);
					    do_fio(&c__1, trans, 1L);
					    do_fio(&c__1, (char *)&n, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&kl, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&ku, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&imat, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&k, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&result[k - 
						    1], (ftnlen)sizeof(
						    doublereal));
					    e_wsfe();
					} else {
					    io___61.ciunit = *nout;
					    s_wsfe(&io___61);
					    do_fio(&c__1, norm, 1L);
					    do_fio(&c__1, (char *)&n, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&kl, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&ku, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&imat, (
						    ftnlen)sizeof(integer));
					    do_fio(&c__1, (char *)&k, (ftnlen)
						    sizeof(integer));
					    do_fio(&c__1, (char *)&result[k - 
						    1], (ftnlen)sizeof(
						    doublereal));
					    e_wsfe();
					}
					++nfail;
				    }
/* L60: */
				}
				nrun = nrun + nt - k1 + 1;
/* L70: */
			    }
L80:
			    ;
			}

L90:
			;
		    }
L100:
		    ;
		}
/* L110: */
	    }
/* L120: */
	}
/* L130: */
    }

/*     Print a summary of the results. */

    alasum_(path, nout, &nfail, &nrun, &nerrs);


    return 0;

/*     End of ZCHKGB */

} /* zchkgb_ */

