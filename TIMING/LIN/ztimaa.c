/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer iparms[100];
} claenv_;

#define claenv_1 claenv_

/* Table of constant values */

static integer c__1 = 1;
static integer c__4 = 4;
static integer c__3 = 3;
static integer c__12 = 12;
static integer c__5000 = 5000;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c_b170 = 362400;
static integer c__2 = 2;

/* Main program */ MAIN__(void)
{
    /* Format strings */
    static char fmt_9983[] = "(\002 LAPACK VERSION 2.0, released September 3"
	    "0, 1994 \002,/)";
    static char fmt_9999[] = "(\002 Too many values of \002,a,\002 using "
	    "\002,a,\002 = \002,i2)";
    static char fmt_9991[] = "(4x,a7,1x,10i6,/12x,10i6)";
    static char fmt_9997[] = "(\002 *** \002,a1,\002 = \002,i7,\002 is too b"
	    "ig:  \002,\002maximum allowed is\002,i7)";
    static char fmt_9998[] = "(\002 *** LDA = \002,i7,\002 is too small, mus"
	    "t have \002,\002LDA > 0.\002)";
    static char fmt_9995[] = "(\002 *** LDA*N is too big for the dense routi"
	    "nes \002,\002(LDA =\002,i6,\002, N =\002,i6,\002)\002,/\002 --> "
	    "Increase LA to at least \002,i8)";
    static char fmt_9994[] = "(\002 *** (LDA+K)*M is too big for the band ro"
	    "utines \002,\002(LDA=\002,i6,\002, M=\002,i6,\002, K=\002,i6,"
	    "\002)\002,/\002 --> Increase LA to at least \002,i8)";
    static char fmt_9996[] = "(\002 *** N*NB is too big for N =\002,i6,\002,"
	    " NB =\002,i6,/\002 --> Increase LA to at least \002,i8)";
    static char fmt_9984[] = "(/\002 Tests not done due to input errors\002)";
    static char fmt_9993[] = "(\002 The minimum time a subroutine will be ti"
	    "med = \002,f6.3,\002 seconds\002)";
    static char fmt_9990[] = "(/\002 ------------------------------\002,/"
	    "\002 >>>>>    Sample BLAS     <<<<<\002,/\002 ------------------"
	    "------------\002)";
    static char fmt_9989[] = "(1x,a6,\002 not timed due to input errors\002,"
	    "/)";
    static char fmt_9988[] = "(/\002 ------------------------------\002,/"
	    "\002 >>>>>    Timing data     <<<<<\002,/\002 ------------------"
	    "------------\002)";
    static char fmt_9987[] = "(1x,a6,\002:  Unrecognized path or subroutine "
	    "name\002,/)";
    static char fmt_9986[] = "(\002 End of tests\002)";
    static char fmt_9985[] = "(\002 Total time used = \002,f12.2,\002 seco"
	    "nds\002)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void), s_rsfe(cilist *), do_fio(integer *
	    , char *, ftnlen), e_rsfe(void), s_rsle(cilist *), do_lio(integer 
	    *, integer *, char *, ftnlen), e_rsle(void), s_wsle(cilist *), 
	    e_wsle(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer need, nlda;
    static logical blas;
    static char line[80];
    static integer kval[12], mval[12], maxk, nval[12], maxm, maxn;
    static doublecomplex a[1087200]	/* was [362400][3] */;
    static doublereal d[1200];
    static doublecomplex e[1200];
    static integer i, l;
    static logical ldaok;
    extern logical lsame_(char *, char *);
    static integer nbval[12], maxnb, mkmax;
    static char c1[1], c2[2], c3[3];
    static integer nxval[12], i2, j2, iwork[10000];
    static doublereal s1, s2;
    extern /* Subroutine */ int ztimb2_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublereal *, integer *, 
	    integer *, integer *, ftnlen), ztimb3_(char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, doublereal *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublereal *, integer *, integer *, integer *, ftnlen);
    static integer nk, nm, nn;
    extern doublereal dsecnd_(void);
    static integer ldaval[4];
    static logical ldamok, ldanok;
    static integer maxlda;
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int ztimgb_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublecomplex *, doublecomplex *, 
	    integer *, doublereal *, integer *, integer *, integer *, integer 
	    *, ftnlen);
    static doublereal timmin;
    static logical nxnbok;
    extern /* Subroutine */ int ztimge_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *, doublereal *, integer *, integer *, integer *, integer 
	    *, ftnlen), ztimhe_(char *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublecomplex *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     ztimpb_(char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     ztimbr_(char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, doublereal *, 
	    doublecomplex *, doublecomplex *, doublereal *, integer *, 
	    integer *, integer *, integer *, ftnlen), ztimtb_(char *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     ztimhp_(char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *, ftnlen);
    static doublereal reslts[55296]	/* was [12][12][8][48] */;
    extern /* Subroutine */ int ztimhr_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, doublereal *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublecomplex *, doublereal *, doublereal *, integer *, integer 
	    *, integer *, integer *, ftnlen), ztimgt_(char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublecomplex *, doublecomplex *, integer *, doublereal *, 
	    integer *, integer *, integer *, integer *, ftnlen), ztimmm_(char 
	    *, char *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublecomplex *, doublecomplex *, doublecomplex *, doublereal *
	    , integer *, integer *, integer *, ftnlen, ftnlen), ztimlq_(char *
	    , integer *, integer *, integer *, integer *, integer *, integer *
	    , integer *, integer *, integer *, integer *, doublereal *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublecomplex *
	    , doublereal *, doublereal *, integer *, integer *, integer *, 
	    integer *, ftnlen), ztimql_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublereal *, doublereal *, 
	    integer *, integer *, integer *, integer *, ftnlen), ztimpo_(char 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *, ftnlen), ztimpp_(char *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, doublecomplex *,
	     doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *, ftnlen), ztimqp_(char *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, doublecomplex *,
	     doublecomplex *, doublecomplex *, doublecomplex *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, integer *, ftnlen),
	     ztimtd_(char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, doublereal 
	    *, doublecomplex *, doublecomplex *, doublereal *, doublecomplex *
	    , doublecomplex *, doublereal *, integer *, integer *, integer *, 
	    integer *, ftnlen), ztimmv_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, doublecomplex *,
	     integer *, doublecomplex *, doublecomplex *, doublereal *, 
	    integer *, integer *, integer *, ftnlen), ztimpt_(char *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     ztimqr_(char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, doublereal *, doublereal *, integer *, integer *,
	     integer *, integer *, ftnlen), ztimrq_(char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *, ftnlen),
	     ztimsp_(char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublecomplex *, doublecomplex *, 
	    doublecomplex *, integer *, doublereal *, integer *, integer *, 
	    integer *, integer *, ftnlen), ztimtp_(char *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, doublecomplex *,
	     doublecomplex *, doublereal *, integer *, integer *, integer *, 
	    integer *, ftnlen), ztimtr_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublecomplex *, doublecomplex *, doublereal *, 
	    integer *, integer *, integer *, integer *, ftnlen), ztimsy_(char 
	    *, integer *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, doublereal *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublereal *, 
	    integer *, integer *, integer *, integer *, ftnlen);
    static integer nnb;
    static logical mok, nok;
    static integer ldr1, ldr2, ldr3;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___6 = { 0, 5, 0, "( A80 )", 0 };
    static cilist io___10 = { 0, 6, 0, "( 1X, A, / )", 0 };
    static cilist io___11 = { 0, 5, 0, 0, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___14 = { 0, 5, 0, 0, 0 };
    static cilist io___17 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___20 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___21 = { 0, 6, 0, 0, 0 };
    static cilist io___22 = { 0, 5, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___25 = { 0, 5, 0, 0, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___31 = { 0, 6, 0, 0, 0 };
    static cilist io___32 = { 0, 5, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___35 = { 0, 5, 0, 0, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___40 = { 0, 5, 0, 0, 0 };
    static cilist io___42 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___43 = { 0, 5, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___48 = { 0, 5, 0, 0, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___50 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___51 = { 0, 5, 0, 0, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___54 = { 0, 5, 0, 0, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___59 = { 0, 6, 0, fmt_9998, 0 };
    static cilist io___60 = { 0, 6, 0, 0, 0 };
    static cilist io___63 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___65 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___67 = { 0, 6, 0, fmt_9996, 0 };
    static cilist io___68 = { 0, 6, 0, fmt_9984, 0 };
    static cilist io___69 = { 0, 6, 0, 0, 0 };
    static cilist io___70 = { 0, 6, 0, 0, 0 };
    static cilist io___71 = { 0, 5, 0, 0, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_9993, 0 };
    static cilist io___74 = { 0, 6, 0, 0, 0 };
    static cilist io___75 = { 0, 5, 1, "(A)", 0 };
    static cilist io___76 = { 0, 5, 1, "(A)", 0 };
    static cilist io___77 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___82 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___83 = { 0, 5, 1, "(A)", 0 };
    static cilist io___84 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_9987, 0 };
    static cilist io___92 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___93 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___94 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___95 = { 0, 6, 0, fmt_9987, 0 };
    static cilist io___96 = { 0, 5, 1, "(A)", 0 };
    static cilist io___98 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___99 = { 0, 6, 0, fmt_9985, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

    Purpose   
    =======   

    ZTIMAA is the timing program for the COMPLEX*16 LAPACK   
    routines.  This program collects performance data for the factor,   
    solve, and inverse routines used in solving systems of linear   
    equations, and also for the orthogonal factorization and reduction   
    routines used in solving least squares problems and matrix eigenvalue 
  
    problems.   

    The subprograms call a DOUBLE PRECISION function DSECND with no   
    arguments which is assumed to return the central-processor time in   
    seconds from some fixed starting time.   

    The program is driven by a short data file, which specifies values   
    for the matrix dimensions M, N and K, for the blocking parameters   
    NB and NX, and for the leading array dimension LDA.  A minimum time   
    for each subroutine is included for timing small problems or for   
    obtaining results on a machine with an inaccurate DSECND function.   

    The matrix dimensions M, N, and K correspond to the three dimensions 
  
    m, n, and k in the Level 3 BLAS.  When timing the LAPACK routines for 
  
    square matrices, M and N correspond to the matrix dimensions m and n, 
  
    and K is the number of right-hand sides (nrhs) for the solves.  When 
  
    timing the LAPACK routines for band matrices, M is the matrix order   
    m, N is the half-bandwidth (kl, ku, or kd in the LAPACK notation),   
    and K is again the number of right-hand sides.   

    The first 13 records of the data file are read using list-directed   
    input.  The first line of input is printed as the first line of   
    output and can be used to identify different sets of results.  To   
    assist with debugging an input file, the values are printed out as   
    they are read in.   

    The following records are read using the format (A).  For these   
    records, the first 6 characters are reserved for the path or   
    subroutine name.  If a path name is used, the characters after the   
    path name indicate the routines in the path to be timed, where   
    'T' or 't' means 'Time this routine'.  If the line is blank after the 
  
    path name, all routines in the path are timed.  If fewer characters   
    appear than routines in a path, the remaining characters are assumed 
  
    to be 'F'.  For example, the following 3 lines are equivalent ways of 
  
    requesting timing of ZGETRF:   
    ZGE    T F F   
    ZGE    T   
    ZGETRF   

    An annotated example of a data file can be obtained by deleting the   
    first 3 characters from the following 32 lines:   
    LAPACK timing, COMPLEX*16 square matrices   
    5                                Number of values of M   
    100 200 300 400 500              Values of M (row dimension)   
    5                                Number of values of N   
    100 200 300 400 500              Values of N (column dimension)   
    2                                Number of values of K   
    100 400                          Values of K   
    5                                Number of values of NB   
    1 16  32  48  64                 Values of NB (blocksize)   
    0 48 128 128 128                 Values of NX (crossover point)   
    2                                Number of values of LDA   
    512 513                          Values of LDA (leading dimension)   
    0.0                              Minimum time in seconds   
    ZGE    T T T   
    ZPO    T T T   
    ZPP    T T T   
    ZHE    T T T   
    ZHP    T T T   
    ZSY    T T T   
    ZSP    T T T   
    ZTR    T T   
    ZTP    T T   
    CQR    T T F   
    CLQ    T T F   
    CQL    T T F   
    CRQ    T T F   
    CQP    T   
    CHR    T T F F   
    CTD    T T F F   
    CBR    T F F   

    The routines are timed for all combinations of applicable values of   
    M, N, K, NB, NX, and LDA, and for all combinations of options such as 
  
    UPLO and TRANS.  For Level 2 BLAS timings, values of NB are used for 
  
    INCX.  Certain subroutines, such as the QR factorization, treat the   
    values of M and N as ordered pairs and operate on M x N matrices.   

    Internal Parameters   
    ===================   

    NMAX    INTEGER   
            The maximum value of M or N for square matrices.   

    LDAMAX  INTEGER   
            The maximum value of LDA.   

    NMAXB   INTEGER   
            The maximum value of N for band matrices.   

    MAXVAL  INTEGER   
            The maximum number of values that can be read in for M, N,   
            K, NB, or NX.   

    MXNLDA  INTEGER   
            The maximum number of values that can be read in for LDA.   

    NIN     INTEGER   
            The unit number for input.  Currently set to 5 (std input).   

    NOUT    INTEGER   
            The unit number for output.  Currently set to 6 (std output). 
  

    ===================================================================== 
*/


    s1 = dsecnd_();
    ldr1 = 12;
    ldr2 = 12;
    ldr3 = 8;
    s_wsfe(&io___5);
    e_wsfe();

/*     Read the first line.  The first four characters must be 'BLAS'   
       for the BLAS data file format to be used.  Otherwise, the LAPACK   
       data file format is assumed. */

    s_rsfe(&io___6);
    do_fio(&c__1, line, 80L);
    e_rsfe();
    blas = lsamen_(&c__4, line, "BLAS");

/*     Find the last non-blank and print the first line of input as the   
       first line of output. */

    for (l = 80; l >= 1; --l) {
	if (*(unsigned char *)&line[l - 1] != ' ') {
	    goto L20;
	}
/* L10: */
    }
    l = 1;
L20:
    s_wsfe(&io___10);
    do_fio(&c__1, line, l);
    e_wsfe();

/*     Read in NM and the values for M. */

    s_rsle(&io___11);
    do_lio(&c__3, &c__1, (char *)&nm, (ftnlen)sizeof(integer));
    e_rsle();
    if (nm > 12) {
	s_wsfe(&io___13);
	do_fio(&c__1, "M", 1L);
	do_fio(&c__1, "NM", 2L);
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nm = 12;
    }
    s_rsle(&io___14);
    i__1 = nm;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    s_wsfe(&io___17);
    do_fio(&c__1, "M:     ", 7L);
    i__1 = nm;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Check that  M <= NMAXB for all values of M. */

    mok = TRUE_;
    maxm = 0;
    i__1 = nm;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	i__2 = mval[i - 1];
	maxm = max(i__2,maxm);
	if (mval[i - 1] > 5000) {
	    s_wsfe(&io___20);
	    do_fio(&c__1, "M", 1L);
	    do_fio(&c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__5000, (ftnlen)sizeof(integer));
	    e_wsfe();
	    mok = FALSE_;
	}
/* L30: */
    }
    if (! mok) {
	s_wsle(&io___21);
	e_wsle();
    }

/*     Read in NN and the values for N. */

    s_rsle(&io___22);
    do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
    e_rsle();
    if (nn > 12) {
	s_wsfe(&io___24);
	do_fio(&c__1, "N", 1L);
	do_fio(&c__1, "NN", 2L);
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 12;
    }
    s_rsle(&io___25);
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    s_wsfe(&io___27);
    do_fio(&c__1, "N:     ", 7L);
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Check that  N <= NMAXB for all values of N. */

    nok = TRUE_;
    maxn = 0;
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	i__2 = nval[i - 1];
	maxn = max(i__2,maxn);
	if (nval[i - 1] > 5000) {
	    s_wsfe(&io___30);
	    do_fio(&c__1, "N", 1L);
	    do_fio(&c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__5000, (ftnlen)sizeof(integer));
	    e_wsfe();
	    nok = FALSE_;
	}
/* L40: */
    }
    if (! nok) {
	s_wsle(&io___31);
	e_wsle();
    }

/*     Read in NK and the values for K. */

    s_rsle(&io___32);
    do_lio(&c__3, &c__1, (char *)&nk, (ftnlen)sizeof(integer));
    e_rsle();
    if (nk > 12) {
	s_wsfe(&io___34);
	do_fio(&c__1, "K", 1L);
	do_fio(&c__1, "NK", 2L);
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nk = 12;
    }
    s_rsle(&io___35);
    i__1 = nk;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    s_wsfe(&io___37);
    do_fio(&c__1, "K:     ", 7L);
    i__1 = nk;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Find the maximum value of K (= NRHS). */

    maxk = 0;
    i__1 = nk;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	i__2 = kval[i - 1];
	maxk = max(i__2,maxk);
/* L50: */
    }
    mkmax = maxm * max(2,maxk);

/*     Read in NNB and the values for NB.  For the BLAS input files,   
       NBVAL is used to store values for INCX and INCY. */

    s_rsle(&io___40);
    do_lio(&c__3, &c__1, (char *)&nnb, (ftnlen)sizeof(integer));
    e_rsle();
    if (nnb > 12) {
	s_wsfe(&io___42);
	do_fio(&c__1, "NB", 2L);
	do_fio(&c__1, "NNB", 3L);
	do_fio(&c__1, (char *)&c__12, (ftnlen)sizeof(integer));
	e_wsfe();
	nnb = 12;
    }
    s_rsle(&io___43);
    i__1 = nnb;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();

/*     Find the maximum value of NB. */

    maxnb = 0;
    i__1 = nnb;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	i__2 = nbval[i - 1];
	maxnb = max(i__2,maxnb);
/* L60: */
    }

    if (blas) {
	s_wsfe(&io___46);
	do_fio(&c__1, "INCX:  ", 7L);
	i__1 = nnb;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
	i__1 = nnb;
	for (i = 1; i <= i__1; ++i) {
	    nxval[i - 1] = 0;
/* L70: */
	}
    } else {

/*        LAPACK data files:  Read in the values for NX. */

	s_rsle(&io___48);
	i__1 = nnb;
	for (i = 1; i <= i__1; ++i) {
	    do_lio(&c__3, &c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();

	s_wsfe(&io___49);
	do_fio(&c__1, "NB:    ", 7L);
	i__1 = nnb;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
	s_wsfe(&io___50);
	do_fio(&c__1, "NX:    ", 7L);
	i__1 = nnb;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

/*     Read in NLDA and the values for LDA. */

    s_rsle(&io___51);
    do_lio(&c__3, &c__1, (char *)&nlda, (ftnlen)sizeof(integer));
    e_rsle();
    if (nlda > 4) {
	s_wsfe(&io___53);
	do_fio(&c__1, "LDA", 3L);
	do_fio(&c__1, "NLDA", 4L);
	do_fio(&c__1, (char *)&c__4, (ftnlen)sizeof(integer));
	e_wsfe();
	nlda = 4;
    }
    s_rsle(&io___54);
    i__1 = nlda;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&ldaval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    s_wsfe(&io___56);
    do_fio(&c__1, "LDA:   ", 7L);
    i__1 = nlda;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&ldaval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Check that LDA >= 1 for all values of LDA. */

    ldaok = TRUE_;
    maxlda = 0;
    i__1 = nlda;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	i__2 = ldaval[i - 1];
	maxlda = max(i__2,maxlda);
	if (ldaval[i - 1] <= 0) {
	    s_wsfe(&io___59);
	    do_fio(&c__1, (char *)&ldaval[i - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    ldaok = FALSE_;
	}
/* L80: */
    }
    if (! ldaok) {
	s_wsle(&io___60);
	e_wsle();
    }

/*     Check that MAXLDA*MAXN <= LA (for the dense routines). */

    ldanok = TRUE_;
    need = maxlda * maxn;
    if (need > 362400) {
	s_wsfe(&io___63);
	do_fio(&c__1, (char *)&maxlda, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&maxn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&need, (ftnlen)sizeof(integer));
	e_wsfe();
	ldanok = FALSE_;
    }

/*     Check that MAXLDA*MAXM + MAXM*MAXK <= 3*LA (for band routines). */

    ldamok = TRUE_;
    need = maxlda * maxm + maxm * maxk;
    if (need > 1087200) {
	need = (need + 2) / 3;
	s_wsfe(&io___65);
	do_fio(&c__1, (char *)&maxlda, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&maxm, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&maxk, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&need, (ftnlen)sizeof(integer));
	e_wsfe();
	ldamok = FALSE_;
    }

/*     Check that MAXN*MAXNB (or MAXN*INCX) <= LA. */

    nxnbok = TRUE_;
    need = maxn * maxnb;
    if (need > 362400) {
	s_wsfe(&io___67);
	do_fio(&c__1, (char *)&maxn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&maxnb, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&need, (ftnlen)sizeof(integer));
	e_wsfe();
	nxnbok = FALSE_;
    }

    if (! (mok && nok && ldaok && ldanok && nxnbok)) {
	s_wsfe(&io___68);
	e_wsfe();
	goto L110;
    }
    if (! ldamok) {
	s_wsle(&io___69);
	e_wsle();
    }

/*     Read the minimum time to time a subroutine. */

    s_wsle(&io___70);
    e_wsle();
    s_rsle(&io___71);
    do_lio(&c__5, &c__1, (char *)&timmin, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsfe(&io___73);
    do_fio(&c__1, (char *)&timmin, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsle(&io___74);
    e_wsle();

/*     Read the first input line. */

    i__1 = s_rsfe(&io___75);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_fio(&c__1, line, 80L);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L100;
    }

/*     If the first record is the special signal 'NONE', then get the   
       next line but don't time ZGEMV and CGEMM. */

    if (lsamen_(&c__4, line, "NONE")) {
	i__1 = s_rsfe(&io___76);
	if (i__1 != 0) {
	    goto L100;
	}
	i__1 = do_fio(&c__1, line, 80L);
	if (i__1 != 0) {
	    goto L100;
	}
	i__1 = e_rsfe();
	if (i__1 != 0) {
	    goto L100;
	}
    } else {
	s_wsfe(&io___77);
	e_wsfe();

/*        If the first record is the special signal 'BAND', then time 
  
          the band routine ZGBMV and ZGEMM with N = K. */

	if (lsamen_(&c__4, line, "BAND")) {
	    if (ldamok) {
		if (mkmax > 362400) {
		    i2 = 724800 - mkmax + 1;
		    j2 = 2;
		} else {
		    i2 = 362400 - mkmax + 1;
		    j2 = 3;
		}
		i__1 = mkmax / 2;
		ztimmv_("ZGBMV ", &nm, mval, &nn, nval, &nlda, ldaval, &
			timmin, a, &i__1, &a[i2 + j2 * 362400 - 362401], &a[
			362400 - mkmax / 2 + 724800], reslts, &ldr1, &ldr2, &
			c__6, 6L);
	    } else {
		s_wsfe(&io___82);
		do_fio(&c__1, "ZGBMV ", 6L);
		e_wsfe();
	    }
	    ztimmm_("ZGEMM ", "K", &nn, nval, &nlda, ldaval, &timmin, a, &a[
		    362400], &a[724800], reslts, &ldr1, &ldr2, &c__6, 6L, 1L);
	    i__1 = s_rsfe(&io___83);
	    if (i__1 != 0) {
		goto L100;
	    }
	    i__1 = do_fio(&c__1, line, 80L);
	    if (i__1 != 0) {
		goto L100;
	    }
	    i__1 = e_rsfe();
	    if (i__1 != 0) {
		goto L100;
	    }

	} else {

/*           Otherwise time ZGEMV and CGEMM. */

	    ztimmv_("ZGEMV ", &nn, nval, &nnb, nbval, &nlda, ldaval, &timmin, 
		    a, &c_b170, &a[362400], &a[724800], reslts, &ldr1, &ldr2, 
		    &c__6, 6L);
	    ztimmm_("ZGEMM ", "N", &nn, nval, &nlda, ldaval, &timmin, a, &a[
		    362400], &a[724800], reslts, &ldr1, &ldr2, &c__6, 6L, 1L);
	}
    }

/*     Call the appropriate timing routine for each input line. */

    s_wsfe(&io___84);
    e_wsfe();
L90:
    *(unsigned char *)c1 = *(unsigned char *)line;
    s_copy(c2, line + 1, 2L, 2L);
    s_copy(c3, line + 3, 3L, 3L);

/*     Check first character for correct precision. */

    if (! lsame_(c1, "Zomplex precision")) {
	s_wsfe(&io___88);
	do_fio(&c__1, line, 6L);
	e_wsfe();

    } else if (lsamen_(&c__2, c2, "B2") || lsamen_(&c__3, c3, "MV ") || lsamen_(&c__3, c3, "SV ") || lsamen_(&c__3, c3,
	     "R  ") || lsamen_(&c__3, c3, "RC ") || lsamen_(&
	    c__3, c3, "RU ") || lsamen_(&c__3, c3, "R2 ")) {

/*        Level 2 BLAS */

	ztimb2_(line, &nm, mval, &nn, nval, &nk, kval, &nnb, nbval, &nlda, 
		ldaval, &c_b170, &timmin, a, &a[362400], &a[724800], reslts, &
		ldr1, &ldr2, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "B3") || lsamen_(&c__3, c3, "MM ") || lsamen_(&c__3, c3, "SM ") || lsamen_(&c__3, c3,
	     "RK ") || lsamen_(&c__3, c3, "R2K")) {

/*        Level 3 BLAS */

	ztimb3_(line, &nm, mval, &nn, nval, &nk, kval, &nlda, ldaval, &timmin,
		 a, &a[362400], &a[724800], reslts, &ldr1, &ldr2, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "QR") || lsamen_(&c__2, c3, "QR") || lsamen_(&c__2, c3 + 1, "QR")) {

/*        QR routines */

	ztimqr_(line, &nn, mval, nval, &nk, kval, &nnb, nbval, nxval, &nlda, 
		ldaval, &timmin, a, e, &a[362400], &a[724800], d, reslts, &
		ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "LQ") || lsamen_(&c__2, c3, "LQ") || lsamen_(&c__2, c3 + 1, "LQ")) {

/*        LQ routines */

	ztimlq_(line, &nn, mval, nval, &nk, kval, &nnb, nbval, nxval, &nlda, 
		ldaval, &timmin, a, e, &a[362400], &a[724800], d, reslts, &
		ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "QL") || lsamen_(&c__2, c3, "QL") || lsamen_(&c__2, c3 + 1, "QL")) {

/*        QL routines */

	ztimql_(line, &nn, mval, nval, &nk, kval, &nnb, nbval, nxval, &nlda, 
		ldaval, &timmin, a, e, &a[362400], &a[724800], d, reslts, &
		ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "RQ") || lsamen_(&c__2, c3, "RQ") || lsamen_(&c__2, c3 + 1, "RQ")) {

/*        RQ routines */

	ztimrq_(line, &nn, mval, nval, &nk, kval, &nnb, nbval, nxval, &nlda, 
		ldaval, &timmin, a, e, &a[362400], &a[724800], d, reslts, &
		ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "QP") || lsamen_(&c__3, c3, "QPF")) {

/*        QR with column pivoting */

	ztimqp_(line, &nm, mval, nval, &nlda, ldaval, &timmin, a, &a[362400], 
		e, &a[724800], d, iwork, reslts, &ldr1, &ldr2, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "HR") || lsamen_(&c__3, c3, "HRD") || lsamen_(&c__2, c3 + 1, "HR")) {

/*        Reduction to Hessenberg form */

	ztimhr_(line, &nn, nval, &nk, kval, &nnb, nbval, nxval, &nlda, ldaval,
		 &timmin, a, e, &a[362400], &a[724800], d, reslts, &ldr1, &
		ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "TD") || lsamen_(&c__3, c3, "TRD") || lsamen_(&c__2, c3 + 1, "TR")) {

/*        Reduction to tridiagonal form */

	ztimtd_(line, &nn, nval, &nk, kval, &nnb, nbval, nxval, &nlda, ldaval,
		 &timmin, a, &a[362400], d, e, &a[724800], reslts, &ldr1, &
		ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "BR") || lsamen_(&c__3, c3, "BRD") || lsamen_(&c__2, c3 + 1, "BR")) {

/*        Reduction to bidiagonal form */

	ztimbr_(line, &nn, mval, nval, &nk, kval, &nnb, nbval, nxval, &nlda, 
		ldaval, &timmin, a, &a[362400], d, e, &a[724800], reslts, &
		ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "GE")) {

/*        Routines for general matrices */

	ztimge_(line, &nn, nval, &nk, kval, &nnb, nbval, &nlda, ldaval, &
		timmin, a, &a[362400], &a[724800], iwork, reslts, &ldr1, &
		ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "GB")) {

/*        General band matrices */

	if (ldamok) {
	    ztimgb_(line, &nm, mval, &nn, nval, &nk, kval, &nnb, nbval, &nlda,
		     ldaval, &timmin, a, &a[362400 - mkmax + 724800], iwork, 
		    reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);
	} else {
	    s_wsfe(&io___92);
	    do_fio(&c__1, line, 6L);
	    e_wsfe();
	}

    } else if (lsamen_(&c__2, c2, "GT")) {

/*        Routines for general tridiagonal matrices */

	ztimgt_(line, &nn, nval, &nk, kval, &nlda, ldaval, &timmin, a, &a[
		362400], iwork, reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "PO")) {

/*        Positive definite matrices */

	ztimpo_(line, &nn, nval, &nk, kval, &nnb, nbval, &nlda, ldaval, &
		timmin, a, &a[362400], iwork, reslts, &ldr1, &ldr2, &ldr3, &
		c__6, 80L);

    } else if (lsamen_(&c__2, c2, "PP")) {

/*        Positive definite packed matrices */

	ztimpp_(line, &nn, nval, &nk, kval, &c_b170, &timmin, a, &a[362400], 
		iwork, reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "PB")) {

/*        Positive definite banded matrices */

	if (ldamok) {
	    if (mkmax > 362400) {
		j2 = 2;
		i2 = 724800 - mkmax + 1;
	    } else {
		j2 = 3;
		i2 = 362400 - mkmax + 1;
	    }
	    ztimpb_(line, &nm, mval, &nn, nval, &nk, kval, &nnb, nbval, &nlda,
		     ldaval, &timmin, a, &a[i2 + j2 * 362400 - 362401], iwork,
		     reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);
	} else {
	    s_wsfe(&io___93);
	    do_fio(&c__1, line, 6L);
	    e_wsfe();
	}

    } else if (lsamen_(&c__2, c2, "PT")) {

/*        Routines for positive definite tridiagonal matrices */

	ztimpt_(line, &nn, nval, &nk, kval, &nlda, ldaval, &timmin, d, a, &a[
		362400], reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "HE")) {

/*        Hermitian indefinite matrices */

	ztimhe_(line, &nn, nval, &nk, kval, &nnb, nbval, &nlda, ldaval, &
		timmin, a, &a[362400], &a[724800], iwork, reslts, &ldr1, &
		ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "HP")) {

/*        Hermitian indefinite packed matrices */

	ztimhp_(line, &nn, nval, &nk, kval, &c_b170, &timmin, a, &a[362400], &
		a[724800], iwork, reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "SY")) {

/*        Symmetric indefinite matrices */

	ztimsy_(line, &nn, nval, &nk, kval, &nnb, nbval, &nlda, ldaval, &
		timmin, a, &a[362400], &a[724800], iwork, reslts, &ldr1, &
		ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "SP")) {

/*        Symmetric indefinite packed matrices */

	ztimsp_(line, &nn, nval, &nk, kval, &c_b170, &timmin, a, &a[362400], &
		a[724800], iwork, reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "TR")) {

/*        Triangular matrices */

	ztimtr_(line, &nn, nval, &nk, kval, &nnb, nbval, &nlda, ldaval, &
		timmin, a, &a[362400], reslts, &ldr1, &ldr2, &ldr3, &c__6, 
		80L);

    } else if (lsamen_(&c__2, c2, "TP")) {

/*        Triangular packed matrices */

	ztimtp_(line, &nn, nval, &nk, kval, &c_b170, &timmin, a, &a[362400], 
		reslts, &ldr1, &ldr2, &ldr3, &c__6, 80L);

    } else if (lsamen_(&c__2, c2, "TB")) {

/*        Triangular band matrices */

	if (ldamok) {
	    if (mkmax > 362400) {
		j2 = 2;
		i2 = 724800 - mkmax + 1;
	    } else {
		j2 = 3;
		i2 = 362400 - mkmax + 1;
	    }
	    ztimtb_(line, &nm, mval, &nn, nval, &nk, kval, &nlda, ldaval, &
		    timmin, a, &a[i2 + j2 * 362400 - 362401], reslts, &ldr1, &
		    ldr2, &ldr3, &c__6, 80L);
	} else {
	    s_wsfe(&io___94);
	    do_fio(&c__1, line, 6L);
	    e_wsfe();
	}

    } else {

	s_wsfe(&io___95);
	do_fio(&c__1, line, 6L);
	e_wsfe();
    }

/*     Read the next line of the input file. */

    i__1 = s_rsfe(&io___96);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_fio(&c__1, line, 80L);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L100;
    }
    goto L90;

/*     Branch to this line when the last record is read. */

L100:
    s2 = dsecnd_();
    s_wsfe(&io___98);
    e_wsfe();
    s_wsfe(&io___99);
    d__1 = s2 - s1;
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
L110:

/* L9992:   

       End of ZTIMAA */

    return 0;
} /* MAIN__   

   Main program alias */ int ztimaa_ () { MAIN__ (); return 0; }
