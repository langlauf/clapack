/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;

/* Subroutine */ int stimqp_(char *line, integer *nm, integer *mval, integer *
	nval, integer *nlda, integer *ldaval, real *timmin, real *a, real *
	copya, real *tau, real *work, integer *iwork, real *reslts, integer *
	ldr1, integer *ldr2, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*1] = "SGEQPF";
    static integer modes[2] = { 2,3 };
    static integer iseed[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda;
    static real cond;
    static integer mode;
    static real dmax__;
    static integer info;
    static char path[3];
    static real time;
    static integer i, m, n;
    static char cname[6];
    static integer imode, minmn;
    extern doublereal sopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int icopy_(integer *, integer *, integer *, 
	    integer *, integer *);
    static real s1, s2;
    static integer ic;
    extern /* Subroutine */ int sprtb5_(char *, char *, char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, real *, 
	    integer *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    static integer nb, im;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), sgeqpf_(
	    integer *, integer *, real *, integer *, integer *, real *, real *
	    , integer *), slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *);
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    static logical timsub[1];
    extern /* Subroutine */ int slatms_(integer *, integer *, char *, integer 
	    *, char *, real *, integer *, real *, real *, integer *, integer *
	    , char *, real *, integer *, real *, integer *);
    static integer lda, icl;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___30 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STIMQP times the LAPACK routines to perform the QR factorization with 
  
    column pivoting of a REAL general matrix.   

    Two matrix types may be used for timing.  The number of types is   
    set in the parameter NMODE and the matrix types are set in the vector 
  
    MODES, using the following key:   
       2.  BREAK1    D(1:N-1)=1 and D(N)=1.0/COND in SLATMS   
       3.  GEOM      D(I)=COND**(-(I-1)/(N-1)) in SLATMS   
    These numbers are chosen to correspond with the matrix types in the   
    test code.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for 
  
            a full description of the format of the input line.   

    NM      (input) INTEGER   
            The number of values of M and N contained in the vectors   
            MVAL and NVAL.  The matrix sizes are used in pairs (M,N).   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix row dimension M.   

    NVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix column dimension N.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) REAL array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values of LDA and N.   

    COPYA   (workspace) REAL array, dimension (LDAMAX*NMAX)   

    TAU     (workspace) REAL array, dimension (min(M,N))   

    WORK    (workspace) REAL array, dimension (3*NMAX)   

    IWORK   (workspace) INTEGER array, dimension (2*NMAX)   

    RESLTS  (workspace) REAL array, dimension   
                        (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of MODE, (M,N), and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NM).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --mval;
    --nval;
    --ldaval;
    --a;
    --copya;
    --tau;
    --work;
    --iwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = reslts_dim1 * (reslts_dim2 + 1) + 1;
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", 1L, 16L);
    s_copy(path + 1, "QP", 2L, 2L);
    atimin_(path, line, &c__1, subnam, timsub, nout, &info, 3L, 80L, 6L);
    if (! timsub[0] || info != 0) {
	goto L90;
    }

/*     Check that M <= LDA for the input values. */

    s_copy(cname, line, 6L, 6L);
    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, 6L);
    if (info > 0) {
	io___8.ciunit = *nout;
	s_wsfe(&io___8);
	do_fio(&c__1, cname, 6L);
	e_wsfe();
	goto L90;
    }

/*     Set the condition number and scaling factor for the matrices   
       to be generated. */

    dmax__ = 1.f;
    cond = 1.f / slamch_("Precision");

/*     Do for each pair of values (M,N): */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {
	m = mval[im];
	n = nval[im];
	minmn = min(m,n);

/*        Do for each value of LDA: */

	i__2 = *nlda;
	for (ilda = 1; ilda <= i__2; ++ilda) {
	    lda = ldaval[ilda];
	    for (imode = 1; imode <= 2; ++imode) {
		mode = modes[imode - 1];

/*              Generate a test matrix of size m by n using th
e   
                singular value distribution indicated by MODE.
 */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    iwork[n + i] = 0;
/* L10: */
		}
		slatms_(&m, &n, "Uniform", iseed, "Nonsymm", &tau[1], &mode, &
			cond, &dmax__, &m, &n, "No packing", &copya[1], &lda, 
			&work[1], &info);

/*              SGEQPF:  QR factorization with column pivoting
 */

		slacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
		icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
		ic = 0;
		s1 = second_();
L20:
		sgeqpf_(&m, &n, &a[1], &lda, &iwork[1], &tau[1], &work[1], &
			info);
		s2 = second_();
		time = s2 - s1;
		++ic;
		if (time < *timmin) {
		    slacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
		    icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
		    goto L20;
		}

/*              Subtract the time used in SLACPY and ICOPY. */

		icl = 1;
		s1 = second_();
L30:
		s2 = second_();
		untime = s2 - s1;
		++icl;
		if (icl <= ic) {
		    slacpy_("All", &m, &n, &copya[1], &lda, &a[1], &lda);
		    icopy_(&n, &iwork[n + 1], &c__1, &iwork[1], &c__1);
		    goto L30;
		}

		time = (time - untime) / (real) ic;
		ops = sopla_("SGEQPF", &m, &n, &c__0, &c__0, &nb);
		reslts[imode + (im + ilda * reslts_dim2) * reslts_dim1] = 
			smflop_(&ops, &time, &info);

/* L40: */
	    }
/* L50: */
	}
/* L60: */
    }

/*     Print tables of results */

    io___28.ciunit = *nout;
    s_wsfe(&io___28);
    do_fio(&c__1, subnam, 6L);
    e_wsfe();
    if (*nlda > 1) {
	i__1 = *nlda;
	for (i = 1; i <= i__1; ++i) {
	    io___29.ciunit = *nout;
	    s_wsfe(&io___29);
	    do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&ldaval[i], (ftnlen)sizeof(integer));
	    e_wsfe();
/* L70: */
	}
    }
    io___30.ciunit = *nout;
    s_wsle(&io___30);
    e_wsle();
    sprtb5_("Type", "M", "N", &c__2, modes, nm, &mval[1], &nval[1], nlda, &
	    reslts[reslts_offset], ldr1, ldr2, nout, 4L, 1L, 1L);
/* L80: */
L90:
    return 0;

/*     End of STIMQP */

} /* stimqp_ */

