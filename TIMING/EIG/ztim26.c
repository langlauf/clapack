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

static doublecomplex c_b1 = {1.,0.};
static doublecomplex c_b2 = {2.,0.};
static integer c__16 = 16;
static integer c__1 = 1;
static doublereal c_b25 = 1.;
static integer c__6 = 6;
static doublereal c_b30 = 0.;
static integer c__0 = 0;
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__20 = 20;
static integer c__10 = 10;
static integer c__21 = 21;

/* Subroutine */ int ztim26_(char *line, integer *nsizes, integer *nn, 
	integer *mm, integer *ntypes, logical *dotype, integer *nparms, 
	integer *nnb, integer *ldas, doublereal *timmin, integer *nout, 
	integer *iseed, doublecomplex *a, doublecomplex *h, doublecomplex *u, 
	doublecomplex *vt, doublereal *d, doublecomplex *dc, doublereal *e, 
	doublecomplex *ec, doublecomplex *taup, doublecomplex *tauq, 
	doublecomplex *work, integer *lwork, doublereal *rwork, logical *
	llwork, doublereal *times, integer *ldt1, integer *ldt2, integer *
	ldt3, doublereal *opcnts, integer *ldo1, integer *ldo2, integer *ldo3,
	 integer *info, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[9*16] = "ZGEBRD   " "ZBDSQR   " "ZBDSQR(L)" "ZBDSQR(R)"
	     "ZBDSQR(B)" "ZBDSQR(V)" "LAPSVD   " "LAPSVD(l)" "LAPSVD(L)" 
	    "LAPSVD(R)" "LAPSVD(B)" "LINSVD   " "LINSVD(l)" "LINSVD(L)" "LIN"
	    "SVD(R)" "LINSVD(B)";
    static integer inparm[16] = { 2,1,1,1,1,1,2,2,2,2,2,1,1,1,1,1 };
    static char pnames[4*2] = "LDA " "NB  ";
    static integer kmode[3] = { 4,3,1 };

    /* Format strings */
    static char fmt_9999[] = "(1x,a,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(\002 ZTIM26: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002M=\002,i6,\002, N=\002,i6,\002, ITYPE=\002,i"
	    "6,\002, IPAR=\002,i6,\002,         \002,\002        ISEED=(\002,"
	    "4(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    integer opcnts_dim1, opcnts_dim2, opcnts_dim3, opcnts_offset, times_dim1, 
	    times_dim2, times_dim3, times_offset, i__1, i__2, i__3, i__4, 
	    i__5, i__6, i__7;
    doublereal d__1;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double log(doublereal), exp(doublereal);

    /* Local variables */
    static integer ipar;
    static doublereal time;
    static integer jdum[1], isub;
    static char uplo[1];
    static integer iqvt, j, m, n, imode;
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    static integer iinfo;
    static doublereal conds;
    static integer minmn;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer maxmn;
    extern /* Subroutine */ int zsvdc_(doublecomplex *, integer *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     integer *);
    static integer itype, j1, j2, j3, j4;
    static doublereal s1, s2;
    extern doublereal dopla2_(char *, char *, integer *, integer *, integer *,
	     integer *, integer *);
    static integer ic, nb, in;
    extern doublereal dlamch_(char *);
    static integer ku;
    extern doublereal dsecnd_(void), dlarnd_(integer *, integer *);
    extern /* Subroutine */ int zgebrd_(integer *, integer *, doublecomplex *,
	     integer *, doublereal *, doublereal *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, integer *);
    static integer ioldsd[4];
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    extern /* Double Complex */ VOID zlarnd_(doublecomplex *, integer *, 
	    integer *);
    static logical trnbrd, runbrd;
    static integer lastnl;
    extern /* Subroutine */ int xlaenv_(integer *, integer *);
    static doublereal untime;
    extern /* Subroutine */ int dprtbv_(char *, integer *, logical *, integer 
	    *, integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *, doublereal *, logical *, integer *, ftnlen, 
	    ftnlen);
    static logical timsub[16];
    extern /* Subroutine */ int zbdsqr_(char *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublereal *, integer *), zlacpy_(char *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *), zlaset_(char *, integer *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *), zlatmr_(
	    integer *, integer *, char *, integer *, char *, doublecomplex *, 
	    integer *, doublereal *, doublecomplex *, char *, char *, 
	    doublecomplex *, integer *, doublereal *, doublecomplex *, 
	    integer *, doublereal *, char *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, char *, doublecomplex *, integer *, 
	    integer *, integer *), zlatms_(integer *, integer *, char *, integer *, char *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, char *, doublecomplex *, integer *, doublecomplex *, 
	    integer *);
    static doublereal ulpinv;
    extern /* Subroutine */ int zungbr_(char *, integer *, integer *, integer 
	    *, doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *);
    static integer mtypes, lda, ldh, iqu;
    static doublereal ulp;
    static integer kvt;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___39 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___58 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

       ZTIM26 times the LAPACK routines for the COMPLEX*16 singular value 
  
       decomposition.   

       For each N value in NN(1:NSIZES), M value in MM(1:NSIZES),   
       and .TRUE. value in DOTYPE(1:NTYPES), a matrix will be generated   
       and used to test the selected routines.  Thus, NSIZES*(number of   
       .TRUE. values in DOTYPE) matrices will be generated.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            On entry, LINE contains the input line which requested   
            this routine.  This line may contain a subroutine name,   
            such as ZGEBRD, indicating that only routine CGEBRD will   
            be timed, or it may contain a generic name, such as ZBD.   
            In this case, the rest of the line is scanned for the   
            first 11 non-blank characters, corresponding to the eleven   
            combinations of subroutine and options:   
            LAPACK:   
             1: ZGEBRD   
                (labeled ZGEBRD in the output)   
             2: ZBDSQR (singular values only)   
                (labeled ZBDSQR in the output)   
             3: ZBDSQR (singular values and left singular vectors;   
                        assume original matrix M by N)   
                (labeled ZBDSQR(L) in the output)   
             4: ZBDSQR (singular values and right singular vectors;   
                        assume original matrix M by N)   
                (labeled ZBDSQR(R) in the output)   
             5: ZBDSQR (singular values and left and right singular   
                        vectors; assume original matrix M by N)   
                (labeled ZBDSQR(B) in the output)   
             6: ZBDSQR (singular value and multiply square MIN(M,N)   
                        matrix by transpose of left singular vectors)   
                (labeled ZBDSQR(V) in the output)   
             7: ZGEBRD+ZBDSQR (singular values only)   
                (labeled LAPSVD in the output)   
             8: ZGEBRD+ZUNGBR+ZBDSQR(L) (singular values and min(M,N)   
                                         left singular vectors)   
                (labeled LAPSVD(l) in the output)   
             9: ZGEBRD+ZUNGBR+ZBDSQR(L) (singular values and M left   
                                         singular vectors)   
                (labeled LAPSVD(L) in the output)   
            10: ZGEBRD+ZUNGBR+ZBDSQR(R) (singular values and N right   
                                         singular vectors)   
                (labeled LAPSVD(R) in the output)   
            11: ZGEBRD+ZUNGBR+ZBDSQR(B) (singular values and min(M,N)   
                                         left singular vectors and N   
                                         right singular vectors)   
                (labeled LAPSVD(B) in the output)   
            LINPACK:   
            12: ZSVDC (singular values only) (comparable to 7 above)   
                (labeled LINSVD in the output)   
            13: ZSVDC (singular values and min(M,N) left singular   
                       vectors) (comparable to 8 above)   
                (labeled LINSVD(l) in the output)   
            14: ZSVDC (singular values and M left singular vectors)   
                       (comparable to 9 above)   
                (labeled LINSVD(L) in the output)   
            15: ZSVDC (singular values and N right singular vectors)   
                       (comparable to 10 above)   
                (labeled LINSVD(R) in the output)   
            16: ZSVDC (singular values and min(M,N) left singular   
                       vectors and N right singular vectors)   
                       (comparable to 11 above)   
                (labeled LINSVD(B) in the output)   

            If a character is 'T' or 't', the corresponding routine in   
            this path is timed.  If the entire line is blank, all the   
            routines in the path are timed.   

    NSIZES  (input) INTEGER   
            The number of values of N contained in the vector NN.   

    NN      (input) INTEGER array, dimension( NSIZES )   
            The numbers of columns of the matrices to be tested.  For   
            each N value in the array NN, and each .TRUE. value in   
            DOTYPE, a matrix A will be generated and used to test the   
            routines.   

    MM      (input) INTEGER array, dimension( NSIZES )   
            The numbers of rows of the matrices to be tested.  For   
            each M value in the array MM, and each .TRUE. value in   
            DOTYPE, a matrix A will be generated and used to test the   
            routines.   

    NTYPES  (input) INTEGER   
            The number of types in DOTYPE.  Only the first MAXTYP   
            elements will be examined.  Exception: if NSIZES=1 and   
            NTYPES=MAXTYP+1, and DOTYPE=MAXTYP*f,t, then the input   
            value of A will be used.   

    DOTYPE  (input) LOGICAL   
            If DOTYPE(j) is .TRUE., then a matrix of type j will be   
            generated as follows:   
             j=1: A = U*D*V where U and V are random unitary   
                  matrices and D has evenly spaced entries 1,...,ULP   
                  with random signs on the diagonal   
             j=2: A = U*D*V where U and V are random unitary   
                  matrices and D has geometrically spaced entries   
                  1,...,ULP with random signs on the diagonal   
             j=3: A = U*D*V where U and V are random unitary   
                  matrices and D has "clustered" entries   
                  1,ULP,...,ULP with random signs on the diagonal   
             j=4: A contains uniform complex random numbers with   
                  components from [-1,1]   
             j=5: A is a special nearly bidiagonal matrix, where the   
                  upper bidiagonal entries are exp(-2*r*log(ULP))   
                  where r is a uniform random number from [0,1],   
                  and the nonbidiagonal entries are r*ULP, where   
                  r is a uniform complex random number with components   
                  from [0,1]   

    NPARMS  (input) INTEGER   
            The number of values in each of the arrays NNB and LDAS.   
            For each matrix A generated according to NN, MM and DOTYPE,   
            tests will be run with (NB,,LDA)= (NNB(1), LDAS(1)),...,   
            (NNB(NPARMS), LDAS(NPARMS)).   

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
            each call to ZTIM26.   

    A       (workspace) COMPLEX*16 array,   
                        dimension( max(NN)*max(LDAS) )   
            During the testing of ZGEBRD, the original dense matrix.   

    H       (workspace) COMPLEX*16 array,   
                        dimension( max(NN)*max(LDAS) )   
            The packed unitary matrices reducing A to bidiagonal   
            form.   

    U       (workspace) COMPLEX*16 array,   
                        dimension( max(NN,MM)*max(LDAS) )   
            The left singular vectors of the original matrix.   

    VT      (workspace) COMPLEX*16 array,   
                        dimension( max(NN,MM)*max(LDAS) )   
            The right singular vectors of the original matrix.   

    D       (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )   
            Diagonal entries of bidiagonal matrix to which A   
            is reduced.   

    DC      (workspace) COMPLEX*16 array, dimension( max(NN,MM) )   
            Diagonal entries of bidiagonal matrix to which A   
            is reduced. May be equivalence with D in calling routine.   

    E       (workspace) DOUBLE PRECISION array, dimension( max(NN,MM) )   
            Offdiagonal entries of bidiagonal matrix to which A   
            is reduced.   

    EC      (workspace) COMPLEX*16 array, dimension( max(NN,MM) )   
            Offdiagonal entries of bidiagonal matrix to which A   
            is reduced. May be equivalence with E in calling routine.   

    TAUP    (workspace) COMPLEX*16 array, dimension( max(NN,MM) )   
            More information used with H.   

    TAUQ    (workspace) COMPLEX*16 array, dimension( max(NN,MM) )   
            More information used with H.   

    WORK    (workspace) COMPLEX*16 array, dimension( LWORK )   

    LWORK   (input) INTEGER   
            Number of elements in WORK and RWORK. Must be at least   
            MAX(6*MIN(M,N),3*MAX(M,N),NSIZES*NPARMS*NTYPES)   

    RWORK   (workspace) DOUBLE PRECISION array, dimension( LWORK )   
            May be equivalenced to WORK in calling routine.   

    LLWORK  (workspace) LOGICAL array, dimension( NPARMS ),   

    TIMES   (output) DOUBLE PRECISION array,   
                     dimension (LDT1,LDT2,LDT3,NSUBS)   
            TIMES(i,j,k,l) will be set to the run time (in seconds) for   
            subroutine/path l, with N=NN(k), M=MM(k), matrix type j,   
            LDA=LDAS(i), and NBLOCK=NNB(i).   

    LDT1    (input) INTEGER   
            The first dimension of TIMES.  LDT1 >= min( 1, NPARMS ).   

    LDT2    (input) INTEGER   
            The second dimension of TIMES.  LDT2 >= min( 1, NTYPES ).   

    LDT3    (input) INTEGER   
            The third dimension of TIMES.  LDT3 >= min( 1, NSIZES ).   

    OPCNTS  (output) DOUBLE PRECISION array,   
                     dimension (LDO1,LDO2,LDO3,NSUBS)   
            OPCNTS(i,j,k,l) will be set to the number of floating-point   
            operations executed by subroutine/path l, with N=NN(k),   
            M=MM(k), matrix type j, LDA=LDAS(i), and NBLOCK=NNB(i).   

    LDO1    (input) INTEGER   
            The first dimension of OPCNTS.  LDO1 >= min( 1, NPARMS ).   

    LDO2    (input) INTEGER   
            The second dimension of OPCNTS.  LDO2 >= min( 1, NTYPES ).   

    LDO3    (input) INTEGER   
            The third dimension of OPCNTS.   LDO3 >= min( 1, NSIZES ).   

    INFO    (output) INTEGER   
            Error flag.  It will be set to zero if no error occurred.   

    ===================================================================== 
  

       Parameter adjustments */
    --nn;
    --mm;
    --dotype;
    --nnb;
    --ldas;
    --iseed;
    --a;
    --h;
    --u;
    --vt;
    --d;
    --dc;
    --e;
    --ec;
    --taup;
    --tauq;
    --work;
    --rwork;
    --llwork;
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

    atimin_("ZBD", line, &c__16, subnam, timsub, nout, info, 3L, 80L, 9L);
    if (*info != 0) {
	return 0;
    }

/*     Check LWORK and   
       Check that N <= LDA and M <= LDA for the input values. */

    i__1 = *nsizes;
    for (j2 = 1; j2 <= i__1; ++j2) {
/* Computing MAX   
   Computing MIN */
	i__4 = mm[j2], i__5 = nn[j2];
/* Computing MAX */
	i__6 = mm[j2], i__7 = nn[j2];
	i__2 = min(i__4,i__5) * 6, i__3 = max(i__6,i__7) * 3, i__2 = max(i__2,
		i__3), i__3 = *nsizes * *nparms * *ntypes;
	if (*lwork < max(i__2,i__3)) {
	    *info = -24;
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
	    do_fio(&c__1, line, 6L);
	    e_wsfe();
	    return 0;
	}
	i__2 = *nparms;
	for (j1 = 1; j1 <= i__2; ++j1) {
/* Computing MAX */
	    i__3 = nn[j2], i__4 = mm[j2];
	    if (max(i__3,i__4) > ldas[j1]) {
		*info = -9;
		io___9.ciunit = *nout;
		s_wsfe(&io___9);
		do_fio(&c__1, line, 6L);
		e_wsfe();
		return 0;
	    }
/* L10: */
	}
/* L20: */
    }

/*     Check to see whether ZGEBRD must be run.   

       RUNBRD -- if ZGEBRD must be run without timing.   
       TRNBRD -- if ZGEBRD must be run with timing. */

    runbrd = FALSE_;
    trnbrd = FALSE_;
    if (timsub[1] || timsub[2] || timsub[3] || timsub[4] || timsub[5]) {
	runbrd = TRUE_;
    }
    if (timsub[0]) {
	runbrd = FALSE_;
    }
    if (timsub[6] || timsub[7] || timsub[8] || timsub[9] || timsub[10]) {
	trnbrd = TRUE_;
    }

/*     Various Constants */

    ulp = dlamch_("Epsilon") * dlamch_("Base");
    ulpinv = 1. / ulp;

/*     Zero out OPCNTS, TIMES */

    for (j4 = 1; j4 <= 16; ++j4) {
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
/* L30: */
		}
/* L40: */
	    }
/* L50: */
	}
/* L60: */
    }

/*     Do for each value of N: */

    i__1 = *nsizes;
    for (in = 1; in <= i__1; ++in) {

	n = nn[in];
	m = mm[in];
	minmn = min(m,n);
	maxmn = max(m,n);
	if (m >= n) {
	    *(unsigned char *)uplo = 'U';
	    ku = minmn;
/* Computing MAX */
	    i__2 = minmn - 1;
	    kvt = max(i__2,0);
	    iqu = 0;
	    iqvt = 1;
	    if (n == 0) {
		iqvt = 0;
	    }
	} else {
	    *(unsigned char *)uplo = 'L';
/* Computing MAX */
	    i__2 = minmn - 1;
	    ku = max(i__2,0);
	    kvt = minmn;
	    iqu = 1;
	    iqvt = 0;
	    if (m == 0) {
		iqu = 0;
	    }
	}

/*        Do for each .TRUE. value in DOTYPE: */

	mtypes = min(5,*ntypes);
	if (*ntypes == 6 && *nsizes == 1) {
	    mtypes = *ntypes;
	}
	i__2 = mtypes;
	for (itype = 1; itype <= i__2; ++itype) {
	    if (! dotype[itype]) {
		goto L670;
	    }

/*           Save random number seed for error messages */

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L70: */
	    }

/* --------------------------------------------------------------
---------   

             Time the LAPACK Routines   

             Generate A */

	    if (itype <= 5) {
		if (itype >= 1 && itype <= 3) {
		    imode = kmode[itype - 1];
		    zlatms_(&m, &n, "U", &iseed[1], "N", &d[1], &imode, &
			    ulpinv, &c_b25, &m, &n, "N", &a[1], &m, &work[1], 
			    info);
		} else if (itype >= 4 && itype <= 5) {
		    if (itype == 4) {
			conds = -1.;
		    }
		    if (itype == 5) {
			conds = ulp;
		    }
		    zlatmr_(&m, &n, "S", &iseed[1], "N", &dc[1], &c__6, &
			    c_b30, &c_b1, "T", "N", &dc[1], &c__0, &c_b25, &
			    dc[1], &c__0, &c_b25, "N", jdum, &m, &n, &c_b30, &
			    conds, "N", &a[1], &m, jdum, info);
		    if (itype == 5) {
			conds = log(ulp) * -2.;
			i__3 = (minmn - 1) * m + minmn;
			i__4 = m + 1;
			for (j = 1; i__4 < 0 ? j >= i__3 : j <= i__3; j += 
				i__4) {
			    i__5 = j;
			    zlarnd_(&z__2, &c__5, &iseed[1]);
			    d__1 = exp(conds * dlarnd_(&c__1, &iseed[1]));
			    z__1.r = d__1 * z__2.r, z__1.i = d__1 * z__2.i;
			    a[i__5].r = z__1.r, a[i__5].i = z__1.i;
/* L80: */
			}
			if (m >= n) {
			    i__4 = (minmn - 1) * m + minmn - 1;
			    i__3 = m + 1;
			    for (j = m + 1; i__3 < 0 ? j >= i__4 : j <= i__4; 
				    j += i__3) {
				i__5 = j;
				zlarnd_(&z__2, &c__5, &iseed[1]);
				d__1 = exp(conds * dlarnd_(&c__1, &iseed[1]));
				z__1.r = d__1 * z__2.r, z__1.i = d__1 * 
					z__2.i;
				a[i__5].r = z__1.r, a[i__5].i = z__1.i;
/* L90: */
			    }
			} else {
			    i__3 = (minmn - 2) * m + minmn;
			    i__4 = m + 1;
			    for (j = 2; i__4 < 0 ? j >= i__3 : j <= i__3; j +=
				     i__4) {
				i__5 = j;
				zlarnd_(&z__2, &c__5, &iseed[1]);
				d__1 = exp(conds * dlarnd_(&c__1, &iseed[1]));
				z__1.r = d__1 * z__2.r, z__1.i = d__1 * 
					z__2.i;
				a[i__5].r = z__1.r, a[i__5].i = z__1.i;
/* L100: */
			    }
			}
		    }
		}
	    }

/*           Time ZGEBRD for each pair NNB(j), LDAS(j) */

	    if (timsub[0] || trnbrd) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZGEBRD */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L110:
		    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
		    zgebrd_(&m, &n, &h[1], &lda, &d[1], &e[1], &tauq[1], &
			    taup[1], &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___39.ciunit = *nout;
			s_wsfe(&io___39);
			do_fio(&c__1, subnam, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }

		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L110;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			zlacpy_("Full", &m, &n, &a[1], &m, &u[1], &lda);
/* L120: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + times_dim3) * times_dim2) * 
			    times_dim1] = max(d__1,0.) / (doublereal) ic;
		    opcnts[ipar + (itype + (in + opcnts_dim3) * opcnts_dim2) *
			     opcnts_dim1] = dopla_("ZGEBRD", &m, &n, &c__0, &
			    c__0, &nb);
/* L130: */
		}
		ldh = lda;
	    } else {
		if (runbrd) {
		    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &m);
		    zgebrd_(&m, &n, &h[1], &m, &d[1], &e[1], &tauq[1], &taup[
			    1], &work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___44.ciunit = *nout;
			s_wsfe(&io___44);
			do_fio(&c__1, subnam, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    ldh = m;
		}
	    }

/*           Time ZBDSQR (singular values only) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[1] || timsub[6]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L140: */
		    }

		    if (lastnl == 0) {

/*                    Time ZBDSQR (singular values onl
y) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L150:
			dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			dcopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			zbdsqr_(uplo, &minmn, &c__0, &c__0, &c__0, &rwork[1], 
				&rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, 
				&u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___46.ciunit = *nout;
			    s_wsfe(&io___46);
			    do_fio(&c__1, subnam + 9, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L150;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L160: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + (times_dim3 << 1)) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 1)) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + (times_dim3 << 1)) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + (times_dim3 << 1)) * times_dim2)
				 * times_dim1];
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 1)) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + (opcnts_dim3 << 1)) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L170: */
		}
	    }

/*           Time ZBDSQR (singular values and left singular vector
s,   
             assume original matrix square) for each pair NNB(j), 
LDAS(j) */

	    if (timsub[2] || timsub[7] || timsub[8]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L180: */
		    }

		    if (lastnl == 0) {

/*                    Time ZBDSQR (singular values and
 left singular   
                      vectors, assume original matrix 
square) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L190:
			zlaset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &lda);
			dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			dcopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			zbdsqr_(uplo, &minmn, &c__0, &m, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___47.ciunit = *nout;
			    s_wsfe(&io___47);
			    do_fio(&c__1, subnam + 18, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L190;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlaset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &
				    lda);
			    dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L200: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 3) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 3) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + times_dim3 * 3) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 3) * times_dim2) * 
				times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 3) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 3) * opcnts_dim2)
				 * opcnts_dim1];
		    }
/* L210: */
		}
	    }

/*           Time ZBDSQR (singular values and right singular vecto
rs,   
             assume original matrix square) for each pair NNB(j), 
LDAS(j) */

	    if (timsub[3] || timsub[9]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L220: */
		    }

		    if (lastnl == 0) {

/*                    Time ZBDSQR (singular values and
 right singular   
                      vectors, assume original matrix 
square) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L230:
			zlaset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], &
				lda);
			dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			dcopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			zbdsqr_(uplo, &minmn, &n, &c__0, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___48.ciunit = *nout;
			    s_wsfe(&io___48);
			    do_fio(&c__1, subnam + 27, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L230;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlaset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], 
				    &lda);
			    dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L240: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + (times_dim3 << 2)) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 2)) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + (times_dim3 << 2)) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + (times_dim3 << 2)) * times_dim2)
				 * times_dim1];
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 2)) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + (opcnts_dim3 << 2)) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L250: */
		}
	    }

/*           Time ZBDSQR (singular values and left and right singu
lar   
             vectors,assume original matrix square) for each pair 
  
             NNB(j), LDAS(j) */

	    if (timsub[4] || timsub[10]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L260: */
		    }

		    if (lastnl == 0) {

/*                    Time ZBDSQR (singular values and
 left and right   
                      singular vectors, assume origina
l matrix square) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L270:
			zlaset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], &
				lda);
			zlaset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &lda);
			dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			dcopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			zbdsqr_(uplo, &minmn, &n, &m, &c__0, &rwork[1], &
				rwork[minmn + 1], &vt[1], &lda, &u[1], &lda, &
				u[1], &lda, &rwork[(minmn << 1) + 1], &iinfo);
			if (iinfo != 0) {
			    io___49.ciunit = *nout;
			    s_wsfe(&io___49);
			    do_fio(&c__1, subnam + 36, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L270;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlaset_("Full", &minmn, &n, &c_b1, &c_b2, &vt[1], 
				    &lda);
			    zlaset_("Full", &m, &minmn, &c_b1, &c_b2, &u[1], &
				    lda);
			    dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L280: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 5) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 5) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + times_dim3 * 5) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 5) * times_dim2) * 
				times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 5) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 5) * opcnts_dim2)
				 * opcnts_dim1];
		    }
/* L290: */
		}
	    }

/*           Time ZBDSQR (singular values and multiply square matr
ix   
             by transpose of left singular vectors) for each pair 
  
             NNB(j), LDAS(j) */

	    if (timsub[5]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L300: */
		    }

		    if (lastnl == 0) {

/*                    Time ZBDSQR (singular values and
 multiply square   
                      matrix by transpose of left sing
ular vectors) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L310:
			zlaset_("Full", &minmn, &minmn, &c_b1, &c_b2, &u[1], &
				lda);
			dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			i__3 = minmn - 1;
			dcopy_(&i__3, &e[1], &c__1, &rwork[minmn + 1], &c__1);
			zbdsqr_(uplo, &minmn, &c__0, &c__0, &minmn, &rwork[1],
				 &rwork[minmn + 1], &vt[1], &lda, &u[1], &lda,
				 &u[1], &lda, &rwork[(minmn << 1) + 1], &
				iinfo);
			if (iinfo != 0) {
			    io___50.ciunit = *nout;
			    s_wsfe(&io___50);
			    do_fio(&c__1, subnam + 45, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L310;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlaset_("Full", &minmn, &minmn, &c_b1, &c_b2, &u[
				    1], &lda);
			    dcopy_(&minmn, &d[1], &c__1, &rwork[1], &c__1);
			    i__5 = minmn - 1;
			    dcopy_(&i__5, &e[1], &c__1, &rwork[minmn + 1], &
				    c__1);
/* L320: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 6) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 6) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + times_dim3 * 6) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 6) * times_dim2) * 
				times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 6) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 6) * opcnts_dim2)
				 * opcnts_dim1];
		    }
/* L330: */
		}
	    }

/*           Time ZGEBRD+ZBDSQR (singular values only) for each pa
ir   
             NNB(j), LDAS(j) */

	    if (timsub[6]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    times[ipar + (itype + (in + times_dim3 * 7) * times_dim2) 
			    * times_dim1] = times[ipar + (itype + (in + 
			    times_dim3) * times_dim2) * times_dim1] + times[
			    ipar + (itype + (in + (times_dim3 << 1)) * 
			    times_dim2) * times_dim1];
		    opcnts[ipar + (itype + (in + opcnts_dim3 * 7) * 
			    opcnts_dim2) * opcnts_dim1] = opcnts[ipar + (
			    itype + (in + opcnts_dim3) * opcnts_dim2) * 
			    opcnts_dim1] + opcnts[ipar + (itype + (in + (
			    opcnts_dim3 << 1)) * opcnts_dim2) * opcnts_dim1];
/* L340: */
		}
	    }

/*           Time ZGEBRD+ZUNGBR+ZBDSQR (singular values and min(M,
N)   
             left singular vectors) for each pair NNB(j), LDAS(j) 
*/

	    if (timsub[7]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZGEBRD+ZUNGBR+ZBDSQR (singular val
ues and   
                   min(M,N) left singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L350:
		    zlacpy_("L", &m, &minmn, &h[1], &lda, &u[1], &lda);
		    zungbr_("Q", &m, &minmn, &ku, &u[1], &lda, &tauq[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___51.ciunit = *nout;
			s_wsfe(&io___51);
			do_fio(&c__1, subnam + 63, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L350;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			zlacpy_("L", &m, &minmn, &h[1], &lda, &u[1], &lda)
				;
/* L360: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + (times_dim3 << 3)) * 
			    times_dim2) * times_dim1] = max(d__1,0.) / (
			    doublereal) ic + times[ipar + (itype + (in + 
			    times_dim3) * times_dim2) * times_dim1] + times[
			    ipar + (itype + (in + times_dim3 * 3) * 
			    times_dim2) * times_dim1];
		    opcnts[ipar + (itype + (in + (opcnts_dim3 << 3)) * 
			    opcnts_dim2) * opcnts_dim1] = dopla2_("ZUNGBR", 
			    "Q", &m, &minmn, &ku, &c__0, &nb) + 
			    opcnts[ipar + (itype + (in + opcnts_dim3) * 
			    opcnts_dim2) * opcnts_dim1] + opcnts[ipar + (
			    itype + (in + opcnts_dim3 * 3) * opcnts_dim2) * 
			    opcnts_dim1];
/* L370: */
		}
	    }

/*           Time ZGEBRD+ZUNGBR+ZBDSQR (singular values and M   
             left singular vectors) for each pair NNB(j), LDAS(j) 
*/

	    if (timsub[8]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZGEBRD+ZUNGBR+ZBDSQR (singular val
ues and   
                   M left singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L380:
		    zlacpy_("L", &m, &minmn, &h[1], &lda, &u[1], &lda);
		    zungbr_("Q", &m, &m, &ku, &u[1], &lda, &tauq[1], &work[1],
			     lwork, &iinfo);
		    if (iinfo != 0) {
			io___52.ciunit = *nout;
			s_wsfe(&io___52);
			do_fio(&c__1, subnam + 72, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L380;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			zlacpy_("L", &m, &minmn, &h[1], &lda, &u[1], &lda)
				;
/* L390: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + times_dim3 * 9) * times_dim2) 
			    * times_dim1] = max(d__1,0.) / (doublereal) ic + 
			    times[ipar + (itype + (in + times_dim3) * 
			    times_dim2) * times_dim1] + times[ipar + (itype + 
			    (in + times_dim3 * 3) * times_dim2) * times_dim1];
		    opcnts[ipar + (itype + (in + opcnts_dim3 * 9) * 
			    opcnts_dim2) * opcnts_dim1] = dopla2_("ZUNGBR", 
			    "Q", &m, &m, &ku, &c__0, &nb) + opcnts[
			    ipar + (itype + (in + opcnts_dim3) * opcnts_dim2) 
			    * opcnts_dim1] + opcnts[ipar + (itype + (in + 
			    opcnts_dim3 * 3) * opcnts_dim2) * opcnts_dim1];
/* L400: */
		}
	    }

/*           Time ZGEBRD+ZUNGBR+ZBDSQR (singular values and N   
             right singular vectors) for each pair NNB(j), LDAS(j)
 */

	    if (timsub[9]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZGEBRD+ZUNGBR+ZBDSQR (singular val
ues and   
                   N right singular vectors) */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L410:
		    zlacpy_("U", &minmn, &n, &h[1], &lda, &vt[1], &lda);
		    zungbr_("P", &n, &n, &kvt, &vt[1], &lda, &taup[1], &work[
			    1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___53.ciunit = *nout;
			s_wsfe(&io___53);
			do_fio(&c__1, subnam + 81, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L410;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			zlacpy_("U", &minmn, &n, &h[1], &lda, &vt[1], &lda);
/* L420: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + times_dim3 * 10) * times_dim2)
			     * times_dim1] = max(d__1,0.) / (doublereal) ic + 
			    times[ipar + (itype + (in + times_dim3) * 
			    times_dim2) * times_dim1] + times[ipar + (itype + 
			    (in + (times_dim3 << 2)) * times_dim2) * 
			    times_dim1];
		    opcnts[ipar + (itype + (in + opcnts_dim3 * 10) * 
			    opcnts_dim2) * opcnts_dim1] = dopla2_("ZUNGBR", 
			    "P", &n, &n, &kvt, &c__0, &nb) + opcnts[
			    ipar + (itype + (in + opcnts_dim3) * opcnts_dim2) 
			    * opcnts_dim1] + opcnts[ipar + (itype + (in + (
			    opcnts_dim3 << 2)) * opcnts_dim2) * opcnts_dim1];
/* L430: */
		}
	    }

/*           Time ZGEBRD+ZUNGBR+ZBDSQR (singular values and min(M,
N) left   
             singular vectors and N right singular vectors) for ea
ch pair   
             NNB(j), LDAS(j) */

	    if (timsub[10]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];
/* Computing MIN */
		    i__3 = n, i__5 = nnb[ipar];
		    nb = min(i__3,i__5);
		    xlaenv_(&c__1, &nb);
		    xlaenv_(&c__2, &c__2);
		    xlaenv_(&c__3, &nb);

/*                 Time ZGEBRD+ZUNGBR+ZBDSQR (singular val
ues and   
                   min(M,N) left singular vectors and N ri
ght singular   
                   vectors) */

		    ic = 0;
		    latime_1.ops = 0.;
		    s1 = dsecnd_();
L440:
		    zlacpy_("L", &m, &minmn, &h[1], &lda, &u[1], &lda);
		    zungbr_("Q", &m, &minmn, &ku, &u[1], &lda, &tauq[1], &
			    work[1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___54.ciunit = *nout;
			s_wsfe(&io___54);
			do_fio(&c__1, subnam + 90, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    zlacpy_("U", &minmn, &n, &h[1], &lda, &vt[1], &lda);
		    zungbr_("P", &n, &n, &kvt, &vt[1], &lda, &taup[1], &work[
			    1], lwork, &iinfo);
		    if (iinfo != 0) {
			io___55.ciunit = *nout;
			s_wsfe(&io___55);
			do_fio(&c__1, subnam + 90, 9L);
			do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&itype, (ftnlen)sizeof(integer))
				;
			do_fio(&c__1, (char *)&ipar, (ftnlen)sizeof(integer));
			do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(
				integer));
			e_wsfe();
			*info = abs(iinfo);
			goto L670;
		    }
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			goto L440;
		    }

/*                 Subtract the time used in ZLACPY. */

		    s1 = dsecnd_();
		    i__3 = ic;
		    for (j = 1; j <= i__3; ++j) {
			zlacpy_("L", &minmn, &minmn, &h[1], &lda, &vt[1], &
				lda);
/* L450: */
		    }
		    s2 = dsecnd_();
		    untime = s2 - s1;

/* Computing MAX */
		    d__1 = time - untime;
		    times[ipar + (itype + (in + times_dim3 * 11) * times_dim2)
			     * times_dim1] = max(d__1,0.) / (doublereal) ic + 
			    times[ipar + (itype + (in + times_dim3) * 
			    times_dim2) * times_dim1] + times[ipar + (itype + 
			    (in + times_dim3 * 5) * times_dim2) * times_dim1];
		    opcnts[ipar + (itype + (in + opcnts_dim3 * 11) * 
			    opcnts_dim2) * opcnts_dim1] = dopla2_("ZUNGBR", 
			    "Q", &m, &minmn, &ku, &c__0, &nb) + 
			    dopla2_("ZUNGBR", "P", &n, &n, &kvt, &c__0, &nb) + opcnts[ipar + (itype + (in + 
			    opcnts_dim3) * opcnts_dim2) * opcnts_dim1] + 
			    opcnts[ipar + (itype + (in + opcnts_dim3 * 5) * 
			    opcnts_dim2) * opcnts_dim1];
/* L460: */
		}
	    }

/*           Time ZSVDC (singular values only) for each pair   
             NNB(j), LDAS(j) */

	    if (timsub[11]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L470: */
		    }

		    if (lastnl == 0) {

/*                    Time ZSVDC (singular values only
) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L480:
			zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
			zsvdc_(&h[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__0, &iinfo);
			if (iinfo != 0) {
			    io___56.ciunit = *nout;
			    s_wsfe(&io___56);
			    do_fio(&c__1, subnam + 99, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L480;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
/* L490: */
			}
			s2 = dsecnd_();
			untime = s2 - s1;

/* Computing MAX */
			d__1 = time - untime;
			times[ipar + (itype + (in + times_dim3 * 12) * 
				times_dim2) * times_dim1] = max(d__1,0.) / (
				doublereal) ic;
			opcnts[ipar + (itype + (in + opcnts_dim3 * 12) * 
				opcnts_dim2) * opcnts_dim1] = latime_1.ops / (
				doublereal) ic;

		    } else {

			times[ipar + (itype + (in + times_dim3 * 12) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 12) * times_dim2) *
				 times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 12) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 12) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L500: */
		}
	    }

/*           Time ZSVDC (singular values and min(M,N) left singula
r   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[12]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L510: */
		    }

		    if (lastnl == 0) {

/*                    Time ZSVDC (singular values and 
min(M,N) left   
                      singular vectors) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L520:
			zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
			zsvdc_(&h[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__20, &iinfo);
			if (iinfo != 0) {
			    io___57.ciunit = *nout;
			    s_wsfe(&io___57);
			    do_fio(&c__1, subnam + 108, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L520;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
/* L530: */
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

			times[ipar + (itype + (in + times_dim3 * 13) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 13) * times_dim2) *
				 times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 13) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 13) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L540: */
		}
	    }

/*           Time ZSVDC (singular values and M left singular   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[13]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L550: */
		    }

		    if (lastnl == 0) {

/*                    Time ZSVDC (singular values and 
M left singular   
                      vectors) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L560:
			zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
			zsvdc_(&h[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__10, &iinfo);
			if (iinfo != 0) {
			    io___58.ciunit = *nout;
			    s_wsfe(&io___58);
			    do_fio(&c__1, subnam + 117, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L560;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
/* L570: */
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

			times[ipar + (itype + (in + times_dim3 * 14) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 14) * times_dim2) *
				 times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 14) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 14) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L580: */
		}
	    }

/*           Time ZSVDC (singular values and N right singular   
             vectors) for each pair NNB(j), LDAS(j) */

	    if (timsub[14]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L590: */
		    }

		    if (lastnl == 0) {

/*                    Time ZSVDC (singular values and 
N right singular   
                      vectors) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L600:
			zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
			zsvdc_(&h[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__1, &iinfo);
			if (iinfo != 0) {
			    io___59.ciunit = *nout;
			    s_wsfe(&io___59);
			    do_fio(&c__1, subnam + 126, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L600;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
/* L610: */
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

			times[ipar + (itype + (in + times_dim3 * 15) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + times_dim3 * 15) * times_dim2) *
				 times_dim1];
			opcnts[ipar + (itype + (in + opcnts_dim3 * 15) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + opcnts_dim3 * 15) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L620: */
		}
	    }

/*           Time ZSVDC (singular values and min(M,N) left singula
r   
             vectors and N right singular vectors) for each pair 
  
             NNB(j), LDAS(j) */

	    if (timsub[15]) {
		i__4 = *nparms;
		for (ipar = 1; ipar <= i__4; ++ipar) {
		    lda = ldas[ipar];

/*                 If this value of LDA has been used befo
re, just   
                   use that value */

		    lastnl = 0;
		    i__3 = ipar - 1;
		    for (j = 1; j <= i__3; ++j) {
			if (lda == ldas[j]) {
			    lastnl = j;
			}
/* L630: */
		    }

		    if (lastnl == 0) {

/*                    Time ZSVDC (singular values and 
min(M,N) left   
                      singular vectors and N right sin
gular vectors) */

			ic = 0;
			latime_1.ops = 0.;
			s1 = dsecnd_();
L640:
			zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
			zsvdc_(&h[1], &lda, &m, &n, &dc[1], &ec[1], &u[1], &
				lda, &vt[1], &lda, &work[1], &c__21, &iinfo);
			if (iinfo != 0) {
			    io___60.ciunit = *nout;
			    s_wsfe(&io___60);
			    do_fio(&c__1, subnam + 135, 9L);
			    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(
				    integer));
			    do_fio(&c__1, (char *)&m, (ftnlen)sizeof(integer))
				    ;
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
			    goto L670;
			}
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    goto L640;
			}

/*                    Subtract the time used in ZLACPY
. */

			s1 = dsecnd_();
			i__3 = ic;
			for (j = 1; j <= i__3; ++j) {
			    zlacpy_("Full", &m, &n, &a[1], &m, &h[1], &lda);
/* L650: */
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

			times[ipar + (itype + (in + (times_dim3 << 4)) * 
				times_dim2) * times_dim1] = times[lastnl + (
				itype + (in + (times_dim3 << 4)) * times_dim2)
				 * times_dim1];
			opcnts[ipar + (itype + (in + (opcnts_dim3 << 4)) * 
				opcnts_dim2) * opcnts_dim1] = opcnts[lastnl + 
				(itype + (in + (opcnts_dim3 << 4)) * 
				opcnts_dim2) * opcnts_dim1];
		    }
/* L660: */
		}
	    }

L670:
	    ;
	}
/* L680: */
    }

/* -----------------------------------------------------------------------
   

       Print a table of results for each timed routine. */

    for (isub = 1; isub <= 16; ++isub) {
	if (timsub[isub - 1]) {
	    dprtbv_(subnam + (isub - 1) * 9, ntypes, &dotype[1], nsizes, &mm[
		    1], &nn[1], &inparm[isub - 1], pnames, nparms, &ldas[1], &
		    nnb[1], &opcnts[((isub * opcnts_dim3 + 1) * opcnts_dim2 + 
		    1) * opcnts_dim1 + 1], ldo1, ldo2, &times[((isub * 
		    times_dim3 + 1) * times_dim2 + 1) * times_dim1 + 1], ldt1,
		     ldt2, &rwork[1], &llwork[1], nout, 9L, 4L);
	}
/* L690: */
    }

    return 0;

/*     End of ZTIM26 */


} /* ztim26_ */

