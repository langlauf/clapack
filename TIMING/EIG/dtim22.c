/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal ops, itcnt;
} latime_;

#define latime_1 latime_

/* Table of constant values */

static integer c__19 = 19;
static integer c__1 = 1;
static integer c__2 = 2;
static doublereal c_b27 = 1.;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__0 = 0;
static doublereal c_b82 = 2.;

/* Subroutine */ int dtim22_(char *line, integer *nsizes, integer *nn, 
	integer *ntypes, logical *dotype, integer *nparms, integer *nnb, 
	integer *ldas, doublereal *timmin, integer *nout, integer *iseed, 
	doublereal *a, doublereal *d, doublereal *e, doublereal *e2, 
	doublereal *z, doublereal *work, integer *lwork, logical *llwork, 
	integer *iwork, doublereal *times, integer *ldt1, integer *ldt2, 
	integer *ldt3, doublereal *opcnts, integer *ldo1, integer *ldo2, 
	integer *ldo3, integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[9*19] = "DSYTRD   " "DSTEQR(N)" "DSTEQR(V)" "DSTERF   "
	     "DPTEQR(N)" "DPTEQR(V)" "DSTEBZ(I)" "DSTEBZ(V)" "DSTEIN   " 
	    "DSTEDC(N)" "DSTEDC(I)" "DSTEDC(V)" "TRED1    " "IMTQL1   " "IMT"
	    "QL2   " "TQLRAT   " "TRIDIB   " "BISECT   " "TINVIT   ";
    static integer inparm[19] = { 2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
    static char pnames[4*4] = "LDA " "NB  " "bad1" "bad2";
    static integer kmode[4] = { 4,3,1,5 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted -- N > LD"
	    "A\002,/)";
    static char fmt_9998[] = "(1x,a,\002 timing run not attempted -- LWORK t"
	    "oo small.\002,/)";
    static char fmt_9997[] = "(\002 DTIM22: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, ITYPE=\002,i6,\002, IPAR="
	    "\002,i6,\002, ISEED=(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double log(doublereal);
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer ipar;
    static doublereal time;
    static integer isub;
    static char uplo[1];
    extern /* Subroutine */ int tred1_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static integer i, j, n, imode, lwedc;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iinfo;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer lastl, itype, j1, j2, j3, j4;
    static doublereal s1, s2;
    extern /* Subroutine */ int imtql1_(integer *, doublereal *, doublereal *,
	     integer *), imtql2_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *);
    static integer ic, m11, nb, il, in;
    extern doublereal dlamch_(char *);
    static integer mm;
    static logical runtr1;
    static integer iu;
    extern doublereal dsecnd_(void);
    static doublereal vl;
    extern /* Subroutine */ int dstedc_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, integer *, integer *);
    static integer liwedc;
    extern /* Subroutine */ int bisect_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, integer *
	    , integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *);
    static doublereal vu;
    static integer idumma[1];
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *);
    static integer ioldsd[4];
    extern /* Subroutine */ int dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    dprtbe_(char *, integer *, logical *, integer *, integer *, 
	    integer *, char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, logical *, integer *, ftnlen, 
	    ftnlen), atimin_(char *, char *, integer *, char *, logical *, 
	    integer *, integer *, ftnlen, ftnlen, ftnlen), tridib_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, doublereal *);
    static doublereal abstol;
    extern /* Subroutine */ int dlatms_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, doublereal *, integer *, doublereal 
	    *, integer *), dstein_(integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *, integer *), dsterf_(integer *, doublereal *, 
	    doublereal *, integer *), dstebz_(char *, char *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, integer *, integer *), xlaenv_(integer *, integer *), dpteqr_(char *, integer *,
	     doublereal *, doublereal *, doublereal *, integer *, doublereal *
	    , integer *);
    static doublereal untime;
    extern /* Subroutine */ int dsteqr_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *);
    static logical timsub[19];
    static integer ilwork;
    extern /* Subroutine */ int tqlrat_(integer *, doublereal *, doublereal *,
	     integer *);
    static integer nsplit;
    extern /* Subroutine */ int dsytrd_(char *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     integer *, integer *);
    static doublereal ulpinv;
    static logical runtrd;
    extern /* Subroutine */ int tinvit_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    static integer mtypes, lda;
    static doublereal rlb;
    static integer lgn, mmm;
    static doublereal rub, ulp, eps1;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___38 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___40 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9997, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

       DTIM22 times the LAPACK routines for the real symmetric   
       eigenvalue problem.   

       For each N value in NN(1:NSIZES) and .TRUE. value in   
       DOTYPE(1:NTYPES), a matrix will be generated and used to test the 
  
       selected routines.  Thus, NSIZES*(number of .TRUE. values in   
       DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            On entry, LINE contains the input line which requested   
            this routine.  This line may contain a subroutine name,   
            such as DSYTRD, indicating that only routine SSYTRD will   
            be timed, or it may contain a generic name, such as DST.   
            In this case, the rest of the line is scanned for the   
            first 19 non-blank characters, corresponding to the eight   
            combinations of subroutine and options:   
            LAPACK:   
            1: DSYTRD   
            2: DSTEQR(VECT='N')   
            3: DSTEQR(VECT='V')   
            4: DSTERF   
            5: DPTEQR(VECT='N')   
            6: DPTEQR(VECT='V')   
            7: DSTEBZ(RANGE='I')   
            8: DSTEBZ(RANGE='V')   
            9: DSTEIN   
            10: DSTEDC(COMPQ='N')   
            11: DSTEDC(COMPQ='I')   
            12: DSTEDC(COMPQ='V')   
            EISPACK:   
            13: TRED1  (compare with DSYTRD)   
            14: IMTQL1 (compare w/ DSTEQR -- VECT='N')   
            15: IMTQL2 (compare w/ DSTEQR -- VECT='V')   
            16: TQLRAT (compare with DSTERF)   
            17: TRIDIB (compare with DSTEBZ -- RANGE='I')   
            18: BISECT (compare with DSTEBZ -- RANGE='V')   
            19: TINVIT (compare with DSTEIN)   
            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

    NSIZES  (input) INTEGER   
            The number of values of N contained in the vector NN.   

    NN      (input) INTEGER array, dimension( NSIZES )   
            The values of the matrix size N to be tested.  For each   
            N value in the array NN, and each .TRUE. value in DOTYPE,   
            a matrix A will be generated and used to test the routines.   

    NTYPES  (input) INTEGER   
            The number of types in DOTYPE.  Only the first MAXTYP   
            elements will be examined.  Exception: if NSIZES=1 and   
            NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input   
            value of A will be used.   

    DOTYPE  (input) LOGICAL   
            If DOTYPE(j) is .TRUE., then a matrix of type j will be   
            generated.  The matrix A has the form X**(-1) D X, where   
            X is orthogonal and D is diagonal with:   
            (j=1)  evenly spaced entries 1, ..., ULP with random signs.   
            (j=2)  geometrically spaced entries 1, ..., ULP with random   
                   signs.   
            (j=3)  "clustered" entries 1, ULP,..., ULP with random   
                   signs.   
            (j=4)  entries randomly chosen from ( ULP, 1 ).   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB and LDAS.   
            For each matrix A generated according to NN and DOTYPE,   
            tests will be run with (NB,LDA)=   
            (NNB(1),LDAS(1)),...,(NNB(NPARMS), LDAS(NPARMS))   

    NNB     (input) INTEGER array, dimension( NPARMS )   
            The values of the blocksize ("NB") to be tested.   

    LDAS    (input) INTEGER array, dimension( NPARMS )   
            The values of LDA, the leading dimension of all matrices,   
            to be tested.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    NOUT    (input) INTEGER   
            If NOUT > 0 then NOUT specifies the unit number   
            on which the output will be printed.  If NOUT <= 0, no   
            output is printed.   

    ISEED   (input/output) INTEGER array, dimension( 4 )   
            The random seed used by the random number generator, used   
            by the test matrix generator.  It is used and updated on   
            each call to DTIM22   

    A       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            The original matrix to be tested.   

    D       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN) )   
            The diagonal of the tridiagonal generated by DSYTRD/TRED1.   

    E       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN) )   
            The off-diagonal of the tridiagonal generated by   
            DSYTRD/TRED1.   

    E2      (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN) )   
            The square of the off-diagonal of the tridiagonal generated   
            by TRED1.  (Used by TQLRAT.)   

    Z       (workspace) DOUBLE PRECISION array,   
                        dimension( max(NN)*max(LDAS) )   
            Various output arrays.   

    WORK    (workspace) DOUBLE PRECISION array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK.  It must be at least   
            (a)  max( (NNB + 2 )*LDAS )   
            (b)  max( 5*LDAS )   
            (c)  NSIZES*NTYPES*NPARMS   
            (d)  2*LDAS + 1 + 3*maxNN + 2*maxNN*log2(maxNN) + 3*maxNN**2 
  
                 where maxNN = maximum matrix dimension in NN   
                       log2(x) = smallest integer power of 2 .ge. x   

    LLWORK  (workspace) LOGICAL array of dimension( NPARMS ),   

    IWORK   (workspace) INTEGER array of dimension   
            6 + 6*maxNN + 5*maxNN*log2(maxNN)   

    TIMES   (output) DOUBLE PRECISION array,   
                     dimension (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine l, with N=NN(k), matrix type j, and LDA=LDAS(i),   
            NBLOCK=NNB(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) DOUBLE PRECISION array,   
                     dimension (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine l, with N=NN(k), matrix   
            type j, and LDA=LDAS(i), NBLOCK=NNB(i).   

    LDO1    (input) INTEGER   
            The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).   

    LDO2    (input) INTEGER   
            The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).   

    LDO3    (input) INTEGER   
            The third dimension of OPCNTS.  LDO3 >= min( 1, NSIZES ).   

    INFO    (output) INTEGER   
            Error flag.  It will be set to zero if no error occurred.   

    ===================================================================== 
  

       Parameter adjustments */
    --nn;
    --dotype;
    --nnb;
    --ldas;
    --iseed;
    --a;
    --d;
    --e;
    --e2;
    --z;
    --work;
    --llwork;
    --iwork;
    times_dim1 = *ldt1;
    times_dim2 = *ldt2;
    times_dim3 = *ldt3;
    times_offset = times_dim1 * (times_dim2 * (times_dim3 + 1) + 1) + 1;
    times -= times_offset;
    opcnts_dim1 = *ldo1;
    opcnts_dim2 = *ldo2;
    opcnts_dim3 = *ldo3;
    opcnts_offset = opcnts_dim1 * (opcnts_dim2 * (opcnts_dim3 + 1) + 1) + 1;
    opcnts -= opcnts_offset;

    /* Function Body   


       Extract the timing request from the input line. */

    atimin_("DST", line, &c__19, subnam, timsub, nout, info, 3L, 80L, 9L);
    if (*info != 0) {
	return 0;
    }

/*     Check that N <= LDA for the input values. */

    i__1 = *nsizes;
    for (j2 = 1; j2 <= i__1; ++j2) {
	i__2 = *nparms;
	for (j1 = 1; j1 <= i__2; ++j1) {
	    if (nn[j2] > ldas[j1]) {
		*info = -8;
		io___8.ciunit = *nout;
		s_wsfe(&io___8);
		do_fio(&c__1, line, 6L);
		e_wsfe();
		return 0;
	    }
/* L10: */
	}
/* L20: */
    }

/*     Check LWORK */

    ilwork = *nsizes * *nparms * *ntypes;
    i__1 = *nparms;
    for (j1 = 1; j1 <= i__1; ++j1) {
/* Computing MAX */
	i__2 = ilwork, i__3 = ldas[j1] * 5, i__2 = max(i__2,i__3), i__3 = (
		nnb[j1] + 2) * ldas[j1];
	ilwork = max(i__2,i__3);
/* L30: */
    }
    if (ilwork > *lwork) {
	*info = -18;
	io___10.ciunit = *nout;
	s_wsfe(&io___10);
	do_fio(&c__1, line, 6L);
	e_wsfe();
	return 0;
    }

/*     Check to see whether DSYTRD must be run.   

       RUNTRD -- if DSYTRD must be run. */

    runtrd = FALSE_;
    if (timsub[1] || timsub[2] || timsub[3] || timsub[4] || timsub[5] || 
	    timsub[6] || timsub[7] || timsub[8] || timsub[9] || timsub[10] || 
	    timsub[11]) {
	runtrd = TRUE_;
    }

/*     Check to see whether TRED1 must be run.   

       RUNTR1 -- if TRED1 must be run. */

    runtr1 = FALSE_;
    if (timsub[12] || timsub[13] || timsub[14] || timsub[15] || timsub[16] || 
	    timsub[17]) {
	runtr1 = TRUE_;
    }

/*     Various Constants */

    ulp = dlamch_("Epsilon") * dlamch_("Base");
    ulpinv = 1. / ulp;

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 19; ++j4) {
	i__1 = *nsizes;
	for (j3 = 1; j3 <= i__1; ++j3) {
	    i__2 = *ntypes;
	    for (j2 = 1; j2 <= i__2; ++j2) {
		i__3 = *nparms;
		for (j1 = 1; j1 <= i__3; ++j1) {
		    opcnts[j1 + (j2 + (j3 + j4 * opcnts_dim3) * opcnts_dim2) *
			     opcnts_dim1] = 0.;
		    times[j1 + (j2 + (j3 + j4 * times_dim3) * times_dim2) * 
			    times_dim1] = 0.;
/* L40: */
		}
/* L50: */
	    }
/* L60: */
	}
/* L70: */
    }

/*     Do for each value of N: */

    i__1 = *nsizes;
    for (in = 1; in <= i__1; ++in) {

	n = nn[in];
	if (n > 0) {
	    lgn = (integer) (log((doublereal) n) / log(2.));
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
	    if (pow_ii(&c__2, &lgn) < n) {
		++lgn;
	    }
/* Computing 2nd power */
	    i__2 = n;
	    lwedc = (n << 2) + 1 + (n << 1) * lgn + i__2 * i__2 * 3;
	    liwedc = n * 6 + 6 + n * 5 * lgn;
	} else {
	    lwedc = 8;
	    liwedc = 12;
	}

/*        Do for each .TRUE. value in DOTYPE: */

	mtypes = min(4,*ntypes);
	if (*ntypes == 5 && *nsizes == 1) {
	    mtypes = *ntypes;
	}
	i__2 = mtypes;
	for (itype = 1; itype <= i__2; ++itype) {
	    if (! dotype[itype]) {
		goto L810;
	    }

/*           Save random number seed for error messages */

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L80: */
	    }

/* --------------------------------------------------------------
---------   

             Time the LAPACK Routines   

             Generate A */

	    *(unsigned char *)uplo = 'L';
	    if (itype <= 4) {
		imode = kmode[itype - 1];
		dlatms_(&n, &n, "S", &iseed[1], "S", &work[1], &imode, &
			ulpinv, &c_b27, &n, &n, uplo, &a[1], &n, &work[n + 1],
			 &iinfo);
	    }

/*           Time DSYTRD for each pair NNB(j), LDAS(j) */

	    if (timsub[0]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__4 = n, i__5 = nnb[ipar];
		    nb = min(i__4,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time DSYTRD */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L90:
		    dlacpy_(uplo, &n, &n, &a[1], &n, &z[1], &lda);
		    i__4 = *lwork - n;
		    dsytrd_(uplo, &n, &z[1], &lda, &d[1], &e[1], &work[1], &
			    work[n + 1], &i__4, &iinfo);
		    if (iinfo != 0) {
			io___34.ciunit = *nout;
			s_wsfe(&io___34);
			do_fio(&c__1, subnam, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L470;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L90;
		    }

/*                 Subtract the time used in DLACPY. */

		    s1 = dsecnd_();
		    i__4 = ic;
		    for (j = 1; j <= i__4; ++j) {
			dlacpy_(uplo, &n, &n, &a[1], &n, &z[1], &lda);
/* L100: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + times_dim3) * times_dim2) * 
			    times_dim1] = max(d__1,0.) / (doublereal) ic;
		    opcnts[ipar + (itype + (in + opcnts_dim3) * opcnts_dim2) *
			     opcnts_dim1] = dopla_("DSYTRD", &n, &c__0, &c__0,
			     &c__0, &nb);
/* L110: */
		}
	    } else {
		if (runtrd) {
		    dlacpy_(uplo, &n, &n, &a[1], &n, &z[1], &n);
		    i__3 = *lwork - n;
		    dsytrd_(uplo, &n, &z[1], &n, &d[1], &e[1], &work[1], &
			    work[n + 1], &i__3, &iinfo);
		    if (iinfo != 0) {
			io___38.ciunit = *nout;
			s_wsfe(&io___38);
			do_fio(&c__1, subnam, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L470;
		    }
		}
	    }

/*           Time DSTEQR, SSTERF, DPTEQR, SSTEBZ, SSTEIN, SSTEDC f
or each   
             distinct LDA=LDAS(j) */

	    if (timsub[1] || timsub[2] || timsub[3] || timsub[4] || timsub[5] 
		    || timsub[6] || timsub[6] || timsub[7] || timsub[8] || 
		    timsub[9] || timsub[10] || timsub[11]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L120: */
		    }
		    if (lastl == 0) {

/*                    Time DSTEQR with VECT='N' */

			if (timsub[1]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L130:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dsteqr_("N", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___40.ciunit = *nout;
				s_wsfe(&io___40);
				do_fio(&c__1, subnam + 9, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L150;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L130;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L140: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + (times_dim3 << 1)) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 1)) *
				     opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEQR with VECT='V' */

L150:
			if (timsub[2]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L160:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[1], &
				    lda);
			    dsteqr_("V", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___41.ciunit = *nout;
				s_wsfe(&io___41);
				do_fio(&c__1, subnam + 18, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L180;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L160;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[
					1], &lda);
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L170: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 3) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 3) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTERF */

L180:
			if (timsub[3]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L190:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dsterf_(&n, &work[1], &work[lda + 1], &iinfo);
			    if (iinfo != 0) {
				io___42.ciunit = *nout;
				s_wsfe(&io___42);
				do_fio(&c__1, subnam + 27, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L210;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L190;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L200: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + (times_dim3 << 2)) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 2)) *
				     opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DPTEQR with VECT='N' */

L210:
			if (timsub[4]) {

/*                       Modify the tridiagonal ma
trix to make it   
                         positive definite. */
			    e2[1] = abs(d[1]) + abs(e[1]);
			    i__4 = n - 1;
			    for (i = 2; i <= i__4; ++i) {
				e2[i] = (d__1 = d[i], abs(d__1)) + (d__2 = e[
					i], abs(d__2)) + (d__3 = e[i - 1], 
					abs(d__3));
/* L220: */
			    }
			    e2[n] = (d__1 = d[n], abs(d__1)) + (d__2 = e[n - 
				    1], abs(d__2));
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L230:
			    dcopy_(&n, &e2[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dpteqr_("N", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___44.ciunit = *nout;
				s_wsfe(&io___44);
				do_fio(&c__1, subnam + 36, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L250;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L230;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L240: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 5) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 5) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DPTEQR with VECT='V' */

L250:
			if (timsub[5]) {

/*                       Modify the tridiagonal ma
trix to make it   
                         positive definite. */
			    e2[1] = abs(d[1]) + abs(e[1]);
			    i__4 = n - 1;
			    for (i = 2; i <= i__4; ++i) {
				e2[i] = (d__1 = d[i], abs(d__1)) + (d__2 = e[
					i], abs(d__2)) + (d__3 = e[i - 1], 
					abs(d__3));
/* L260: */
			    }
			    e2[n] = (d__1 = d[n], abs(d__1)) + (d__2 = e[n - 
				    1], abs(d__2));
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L270:
			    dcopy_(&n, &e2[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dpteqr_("V", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___45.ciunit = *nout;
				s_wsfe(&io___45);
				do_fio(&c__1, subnam + 45, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L290;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L270;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L280: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 6) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 6) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEBZ(I) */

L290:
			if (timsub[6]) {
			    il = 1;
			    iu = n;
			    abstol = 0.;
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L300:
			    dstebz_("I", "B", &n, &vl, &vu, &il, &iu, &abstol,
				     &d[1], &e[1], &mm, &nsplit, &work[1], &
				    iwork[1], &iwork[lda + 1], &work[(lda << 
				    1) + 1], &iwork[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___53.ciunit = *nout;
				s_wsfe(&io___53);
				do_fio(&c__1, subnam + 54, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L310;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L300;
			    }
			    untime = 0.;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 7) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 7) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEBZ(V) */

L310:
			if (timsub[7]) {
			    if (n == 1) {
				vl = d[1] - abs(d[1]);
				vu = d[1] + abs(d[1]);
			    } else {
				vl = d[1] - abs(e[1]);
				vu = d[1] + abs(e[1]);
				i__4 = n - 1;
				for (i = 2; i <= i__4; ++i) {
/* Computing MIN */
				    d__3 = vl, d__4 = d[i] - (d__1 = e[i], 
					    abs(d__1)) - (d__2 = e[i - 1], 
					    abs(d__2));
				    vl = min(d__3,d__4);
/* Computing MAX */
				    d__3 = vu, d__4 = d[i] + (d__1 = e[i], 
					    abs(d__1)) + (d__2 = e[i - 1], 
					    abs(d__2));
				    vu = max(d__3,d__4);
/* L320: */
				}
/* Computing MIN */
				d__2 = vl, d__3 = d[n] - (d__1 = e[n - 1], 
					abs(d__1));
				vl = min(d__2,d__3);
/* Computing MAX */
				d__2 = vu, d__3 = d[n] + (d__1 = e[n - 1], 
					abs(d__1));
				vu = max(d__2,d__3);
			    }
			    abstol = 0.;
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L330:
			    dstebz_("V", "B", &n, &vl, &vu, &il, &iu, &abstol,
				     &d[1], &e[1], &mm, &nsplit, &work[1], &
				    iwork[1], &iwork[lda + 1], &work[(lda << 
				    1) + 1], &iwork[(lda << 1) + 1], &iinfo);
			    if (iinfo != 0) {
				io___54.ciunit = *nout;
				s_wsfe(&io___54);
				do_fio(&c__1, subnam + 63, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L340;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L330;
			    }
			    untime = 0.;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + (times_dim3 << 3)) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 3)) *
				     opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEIN */

L340:
			if (timsub[8]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L350:
			    dstein_(&n, &d[1], &e[1], &mm, &work[1], &iwork[1]
				    , &iwork[lda + 1], &z[1], &lda, &work[lda 
				    + 1], &iwork[(lda << 1) + 1], &iwork[lda *
				     3 + 1], &iinfo);
			    if (iinfo != 0) {
				io___55.ciunit = *nout;
				s_wsfe(&io___55);
				do_fio(&c__1, subnam + 72, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L360;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L350;
			    }
			    untime = 0.;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 9) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 9) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEDC with COMPQ='N' */

L360:
			if (timsub[9]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L370:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dstedc_("N", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___56.ciunit = *nout;
				s_wsfe(&io___56);
				do_fio(&c__1, subnam + 81, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L390;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L370;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L380: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 10) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 10) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}

/*                    Time DSTEDC with COMPQ='I' */

L390:
			if (timsub[10]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L400:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[1], &
				    lda);
			    dstedc_("I", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___57.ciunit = *nout;
				s_wsfe(&io___57);
				do_fio(&c__1, subnam + 90, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L420;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L400;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[
					1], &lda);
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L410: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 11) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 11) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}
L420:

/*                    Time DSTEDC with COMPQ='V' */

			if (timsub[11]) {
			    ic = 0;
			    latime_1.ops = 0.;
			    s1 = dsecnd_();
L430:
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__4 = n - 1;
			    dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[1], &
				    lda);
			    dstedc_("V", &n, &work[1], &work[lda + 1], &z[1], 
				    &lda, &work[(lda << 1) + 1], &lwedc, &
				    iwork[1], &liwedc, &iinfo);
			    if (iinfo != 0) {
				io___58.ciunit = *nout;
				s_wsfe(&io___58);
				do_fio(&c__1, subnam + 99, 9L);
				do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
					integer));
				do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
					integer));
				do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)
					sizeof(integer));
				e_wsfe();
				*info = abs(iinfo);
				goto L450;
			    }
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				goto L430;
			    }

/*                       Subtract the time used in
 DCOPY. */

			    s1 = dsecnd_();
			    i__4 = ic;
			    for (j = 1; j <= i__4; ++j) {
				dlaset_("Full", &lda, &n, &c_b27, &c_b82, &z[
					1], &lda);
				dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
				i__5 = n - 1;
				dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &
					c__1);
/* L440: */
			    }
			    s2 = dsecnd_();
			    untime = s2 - s1;

/* Computing MAX */
			    d__1 = time - untime;
			    times[ipar + (itype + (in + times_dim3 * 12) * 
				    times_dim2) * times_dim1] = max(d__1,0.) /
				     (doublereal) ic;
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 12) * 
				    opcnts_dim2) * opcnts_dim1] = 
				    latime_1.ops / (doublereal) ic;
			}
L450:


			;
		    } else {
			if (timsub[1]) {
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 1)) *
				     opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + (opcnts_dim3 << 1))
				     * opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + (times_dim3 << 1)) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + (times_dim3 << 1)) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[2]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 3) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 3) * 
				    opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 3) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 3) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[3]) {
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 2)) *
				     opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + (opcnts_dim3 << 2))
				     * opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + (times_dim3 << 2)) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + (times_dim3 << 2)) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[4]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 5) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 5) * 
				    opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 5) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 5) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[5]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 6) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 6) * 
				    opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 6) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 6) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[6]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 7) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 7) * 
				    opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 7) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 7) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[7]) {
			    opcnts[ipar + (itype + (in + (opcnts_dim3 << 3)) *
				     opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + (opcnts_dim3 << 3))
				     * opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + (times_dim3 << 3)) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + (times_dim3 << 3)) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[8]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 9) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 9) * 
				    opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 9) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 9) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[9]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 10) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 10) *
				     opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 10) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 10) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[10]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 11) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 11) *
				     opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 11) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 11) * 
				    times_dim2) * times_dim1];
			}
			if (timsub[11]) {
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 12) * 
				    opcnts_dim2) * opcnts_dim1] = opcnts[
				    lastl + (itype + (in + opcnts_dim3 * 12) *
				     opcnts_dim2) * opcnts_dim1];
			    times[ipar + (itype + (in + times_dim3 * 12) * 
				    times_dim2) * times_dim1] = times[lastl + 
				    (itype + (in + times_dim3 * 12) * 
				    times_dim2) * times_dim1];
			}
		    }
/* L460: */
		}
	    }
L470:

/* --------------------------------------------------------------
---------   

             Time the EISPACK Routines   

             Skip routines if N <= 0 (EISPACK requirement) */

	    if (n <= 0) {
		goto L810;
	    }

/*           Time TRED1 for each LDAS(j) */

	    if (timsub[12]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L480: */
		    }

		    if (lastl == 0) {

/*                    Time TRED1 */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L490:
			dlacpy_("L", &n, &n, &a[1], &n, &z[1], &lda);
			tred1_(&lda, &n, &z[1], &d[1], &e[1], &e2[1]);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L490;
			}

/*                    Subtract the time used in DLACPY
. */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dlacpy_("L", &n, &n, &a[1], &n, &z[1], &lda);
/* L500: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 13) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 13) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 13) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 13) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 13) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 13) * times_dim2) *
				 times_dim1];
		    }
/* L510: */
		}
	    } else {
		if (runtr1) {
		    dlacpy_("L", &n, &n, &a[1], &n, &z[1], &lda);
		    tred1_(&lda, &n, &z[1], &d[1], &e[1], &e2[1]);
		}
	    }

/*           Time IMTQL1 for each LDAS(j) */

	    if (timsub[13]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L520: */
		    }

		    if (lastl == 0) {

/*                    Time IMTQL1 */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L530:
			dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			imtql1_(&n, &work[1], &work[lda + 1], &iinfo);
			if (iinfo != 0) {
			    io___59.ciunit = *nout;
			    s_wsfe(&io___59);
			    do_fio(&c__1, subnam + 117, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L560;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L530;
			}

/*                    Subtract the time used in DCOPY.
 */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
/* L540: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 14) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 14) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 14) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 14) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 14) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 14) * times_dim2) *
				 times_dim1];
		    }
/* L550: */
		}
	    }

/*           Time IMTQL2 for each LDAS(j) */

L560:
	    if (timsub[14]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L570: */
		    }

		    if (lastl == 0) {

/*                    Time IMTQL2 */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L580:
			dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			dlaset_("Full", &n, &n, &c_b27, &c_b82, &z[1], &lda);
			imtql2_(&lda, &n, &work[1], &work[lda + 1], &z[1], &
				iinfo);
			if (iinfo != 0) {
			    io___60.ciunit = *nout;
			    s_wsfe(&io___60);
			    do_fio(&c__1, subnam + 126, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L610;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L580;
			}

/*                    Subtract the time used in DCOPY.
 */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    dlaset_("Full", &n, &n, &c_b27, &c_b82, &z[1], &
				    lda);
/* L590: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 15) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 15) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 15) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 15) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 15) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 15) * times_dim2) *
				 times_dim1];
		    }
/* L600: */
		}
	    }

/*           Time TQLRAT for each LDAS(j) */

L610:
	    if (timsub[15]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L620: */
		    }

		    if (lastl == 0) {

/*                    Time TQLRAT */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L630:
			dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e2[1], &c__1, &work[lda + 1], &c__1);
			tqlrat_(&n, &work[1], &work[lda + 1], &iinfo);
			if (iinfo != 0) {
			    io___61.ciunit = *nout;
			    s_wsfe(&io___61);
			    do_fio(&c__1, subnam + 135, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L660;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L630;
			}

/*                    Subtract the time used in DCOPY.
 */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e2[1], &c__1, &work[lda + 1], &
				    c__1);
/* L640: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + (times_dim3 << 4)) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 4)) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 4)) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + (opcnts_dim3 << 4)) * 
				opcnts_dim2) * opcnts_dim1];
			times[ipar + (itype + (in + (times_dim3 << 4)) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + (times_dim3 << 4)) * times_dim2)
				 * times_dim1];
		    }
/* L650: */
		}
	    }

/*           Time TRIDIB for each LDAS(j) */

L660:
	    if (timsub[16]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L670: */
		    }

		    if (lastl == 0) {

/*                    Time TRIDIB */

			ic = 0;
			latime_1.ops = 0.;
			eps1 = 0.;
			rlb = 0.;
			rub = 0.;
			m11 = 1;
			mm = n;
			s1 = dsecnd_();
L680:
			dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e[1], &c__1, &work[lda + 1], &c__1);
			i__4 = n - 1;
			dcopy_(&i__4, &e2[1], &c__1, &work[(lda << 1) + 1], &
				c__1);
			tridib_(&n, &eps1, &work[1], &work[lda + 1], &work[(
				lda << 1) + 1], &rlb, &rub, &m11, &mm, &work[
				lda * 3 + 1], &iwork[1], &iinfo, &work[(lda <<
				 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___66.ciunit = *nout;
			    s_wsfe(&io___66);
			    do_fio(&c__1, subnam + 144, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L710;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L680;
			}

/*                    Subtract the time used in DCOPY.
 */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    i__5 = n - 1;
			    dcopy_(&i__5, &e[1], &c__1, &work[lda + 1], &c__1)
				    ;
			    i__5 = n - 1;
			    dcopy_(&i__5, &e2[1], &c__1, &work[(lda << 1) + 1]
				    , &c__1);
/* L690: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 17) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 17) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 17) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 17) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 17) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 17) * times_dim2) *
				 times_dim1];
		    }
/* L700: */
		}
	    }

/*           Time BISECT for each LDAS(j) */

L710:
	    if (timsub[17]) {
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L720: */
		    }

		    if (lastl == 0) {

/*                    Time BISECT */

			vl = d[1] - abs(e[2]);
			vu = d[1] + abs(e[2]);
			i__4 = n - 1;
			for (i = 2; i <= i__4; ++i) {
/* Computing MIN */
			    d__3 = vl, d__4 = d[i] - (d__1 = e[i + 1], abs(
				    d__1)) - (d__2 = e[i], abs(d__2));
			    vl = min(d__3,d__4);
/* Computing MAX */
			    d__3 = vu, d__4 = d[i] + (d__1 = e[i + 1], abs(
				    d__1)) + (d__2 = e[i], abs(d__2));
			    vu = max(d__3,d__4);
/* L730: */
			}
/* Computing MIN */
			d__2 = vl, d__3 = d[n] - (d__1 = e[n], abs(d__1));
			vl = min(d__2,d__3);
/* Computing MAX */
			d__2 = vu, d__3 = d[n] + (d__1 = e[n], abs(d__1));
			vu = max(d__2,d__3);
			ic = 0;
			latime_1.ops = 0.;
			eps1 = 0.;
			mm = n;
			mmm = 0;
			s1 = dsecnd_();
L740:
			dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			dcopy_(&n, &e[1], &c__1, &work[lda + 1], &c__1);
			dcopy_(&n, &e2[1], &c__1, &work[(lda << 1) + 1], &
				c__1);
			bisect_(&n, &eps1, &work[1], &work[lda + 1], &work[(
				lda << 1) + 1], &vl, &vu, &mm, &mmm, &work[
				lda * 3 + 1], &iwork[1], &iinfo, &work[(lda <<
				 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___68.ciunit = *nout;
			    s_wsfe(&io___68);
			    do_fio(&c__1, subnam + 153, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L770;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L740;
			}

/*                    Subtract the time used in DCOPY.
 */

			s1 = dsecnd_();
			i__4 = ic;
			for (j = 1; j <= i__4; ++j) {
			    dcopy_(&n, &d[1], &c__1, &work[1], &c__1);
			    dcopy_(&n, &e[1], &c__1, &work[lda + 1], &c__1);
			    dcopy_(&n, &e2[1], &c__1, &work[(lda << 1) + 1], &
				    c__1);
/* L750: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 18) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 18) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 18) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 18) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 18) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 18) * times_dim2) *
				 times_dim1];
		    }
/* L760: */
		}
	    }

/*           Time TINVIT for each LDAS(j) */

L770:
	    if (timsub[18]) {
		dcopy_(&n, &work[lda * 3 + 1], &c__1, &work[1], &c__1);
		i__3 = *nparms;
		for (ipar = 1; ipar <= i__3; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has come up before
, just use   
                   the value previously computed. */

		    lastl = 0;
		    i__4 = ipar - 1;
		    for (j = 1; j <= i__4; ++j) {
			if (lda == ldas[j]) {
			    lastl = j;
			}
/* L780: */
		    }

		    if (lastl == 0) {

/*                    Time TINVIT */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L790:
			tinvit_(&lda, &n, &d[1], &e[1], &e2[1], &mmm, &work[1]
				, &iwork[1], &z[1], &iinfo, &work[lda + 1], &
				work[(lda << 1) + 1], &work[lda * 3 + 1], &
				work[(lda << 2) + 1], &work[lda * 5 + 1]);
			if (iinfo != 0) {
			    io___69.ciunit = *nout;
			    s_wsfe(&io___69);
			    do_fio(&c__1, subnam + 162, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer))
				    ;
			    do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				    integer));
			    e_wsfe();
			    *info = abs(iinfo);
			    goto L810;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L790;
			}
			untime = 0.;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 19) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 19) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;
		    } else {
			opcnts[ipar + (itype + (in + opcnts_dim3 * 19) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastl + (
				itype + (in + opcnts_dim3 * 19) * opcnts_dim2)
				 * opcnts_dim1];
			times[ipar + (itype + (in + times_dim3 * 19) * 
				times_dim2) * times_dim1] = times[lastl + (
				itype + (in + times_dim3 * 19) * times_dim2) *
				 times_dim1];
		    }
/* L800: */
		}
	    }

L810:
	    ;
	}
/* L820: */
    }

/* -----------------------------------------------------------------------
   

       Print a table of results for each timed routine. */

    for (isub = 1; isub <= 19; ++isub) {
	if (timsub[isub - 1]) {
	    dprtbe_(subnam + (isub - 1) * 9, &mtypes, &dotype[1], nsizes, &nn[
		    1], &inparm[isub - 1], pnames, nparms, &ldas[1], &nnb[1], 
		    idumma, idumma, &opcnts[((isub * opcnts_dim3 + 1) * 
		    opcnts_dim2 + 1) * opcnts_dim1 + 1], ldo1, ldo2, &times[((
		    isub * times_dim3 + 1) * times_dim2 + 1) * times_dim1 + 1]
		    , ldt1, ldt2, &work[1], &llwork[1], nout, 9L, 4L);
	}
/* L830: */
    }


    return 0;

/*     End of DTIM22 */

} /* dtim22_ */

