/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static doublereal c_b20 = 0.;
static integer c__0 = 0;
static integer c__6 = 6;
static doublereal c_b34 = 1.;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c__3 = 3;

/* Subroutine */ int ddrvst_(integer *nsizes, integer *nn, integer *ntypes, 
	logical *dotype, integer *iseed, doublereal *thresh, integer *nounit, 
	doublereal *a, integer *lda, doublereal *d1, doublereal *d2, 
	doublereal *d3, doublereal *d4, doublereal *wa1, doublereal *wa2, 
	doublereal *wa3, doublereal *u, integer *ldu, doublereal *v, 
	doublereal *tau, doublereal *z, doublereal *work, integer *nwork, 
	integer *iwork, doublereal *result, integer *info)
{
    /* Initialized data */

    static integer ktype[18] = { 1,2,4,4,4,4,4,5,5,5,5,5,8,8,8,9,9,9 };
    static integer kmagn[18] = { 1,1,1,1,1,2,3,1,1,1,2,3,1,2,3,1,2,3 };
    static integer kmode[18] = { 0,0,4,3,1,4,4,4,3,1,4,4,0,0,0,4,4,4 };

    /* Format strings */
    static char fmt_9999[] = "(\002 DDRVST: \002,a,\002 returned INFO=\002,i"
	    "6,\002.\002,/9x,\002N=\002,i6,\002, JTYPE=\002,i6,\002, ISEED="
	    "(\002,3(i5,\002,\002),i5,\002)\002)";

    /* System generated locals */
    address a__1[3];
    integer a_dim1, a_offset, u_dim1, u_offset, v_dim1, v_offset, z_dim1, 
	    z_offset, i__1, i__2, i__3, i__4, i__5, i__6[3], i__7;
    doublereal d__1, d__2, d__3, d__4;
    char ch__1[10], ch__2[13], ch__3[11];

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);
    integer pow_ii(integer *, integer *), s_wsfe(cilist *), do_fio(integer *, 
	    char *, ftnlen), e_wsfe(void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static doublereal cond;
    static integer jcol, ihbw, indx, nmax;
    static doublereal unfl, ovfl;
    static char uplo[1];
    static integer irow;
    static doublereal temp1, temp2, temp3;
    extern doublereal dsxt1_(integer *, doublereal *, integer *, doublereal *,
	     integer *, doublereal *, doublereal *, doublereal *);
    static integer i, j, idiag, m, n;
    static logical badnn;
    static integer imode, lwedc;
    extern /* Subroutine */ int dsbev_(char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer iinfo;
    static doublereal aninv, anorm;
    static integer itemp;
    extern /* Subroutine */ int dspev_(char *, char *, integer *, doublereal *
	    , doublereal *, doublereal *, integer *, doublereal *, integer *);
    static integer nmats;
    extern /* Subroutine */ int dstt21_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *);
    static integer jsize, iuplo, itype, j1, j2, jtype, m2, m3, nerrs, ntest;
    extern /* Subroutine */ int dstev_(char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *), dstt22_(integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *), dsyev_(char *, char *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    integer *, integer *), dsyt21_(integer *, char *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *), dsyt22_(
	    integer *, char *, integer *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *);
    static integer iseed2[4], iseed3[4];
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    static integer kd, il;
    extern doublereal dlamch_(char *);
    static integer iu;
    static doublereal vl;
    extern doublereal dlarnd_(integer *, integer *);
    static integer liwedc;
    extern /* Subroutine */ int dsbevd_(char *, char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *);
    static doublereal vu;
    static integer idumma[1];
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *);
    static integer ioldsd[4];
    extern /* Subroutine */ int dlafts_(char *, integer *, integer *, integer 
	    *, integer *, doublereal *, integer *, doublereal *, integer *, 
	    integer *), dlaset_(char *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    xerbla_(char *, integer *), alasvm_(char *, integer *, 
	    integer *, integer *, integer *);
    static doublereal abstol;
    extern /* Subroutine */ int dlatmr_(integer *, integer *, char *, integer 
	    *, char *, doublereal *, integer *, doublereal *, doublereal *, 
	    char *, char *, doublereal *, integer *, doublereal *, doublereal 
	    *, integer *, doublereal *, char *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, char *, doublereal *, integer *, 
	    integer *, integer *), dlatms_(integer *, integer *, char *, integer *, char *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, char *, doublereal *, integer *, doublereal *, integer 
	    *), dspevd_(char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *, integer *, integer *, integer *), 
	    dstevd_(char *, integer *, doublereal *, doublereal *, doublereal 
	    *, integer *, doublereal *, integer *, integer *, integer *, 
	    integer *), dsbevx_(char *, char *, char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, integer *), dsyevd_(
	    char *, char *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *);
    static doublereal rtovfl, rtunfl;
    static integer mtypes, ntestt;
    static doublereal ulpinv;
    extern /* Subroutine */ int dspevx_(char *, char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *), dstevx_(char *, char *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *), 
	    dsyevx_(char *, char *, char *, integer *, doublereal *, integer *
	    , doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, integer *, integer *);
    static integer lgn;
    static doublereal ulp;

    /* Fortran I/O blocks */
    static cilist io___41 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___56 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___57 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___60 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___61 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___62 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___63 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___66 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___67 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___68 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___69 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___71 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___72 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___73 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___84 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___85 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___86 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___87 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___88 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___89 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___90 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___91 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___92 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___93 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___94 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___95 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___97 = { 0, 0, 0, fmt_9999, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

         DDRVST  checks the symmetric eigenvalue problem drivers.   

                 DSTEV computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric tridiagonal matrix.   

                 DSTEVX computes selected eigenvalues and, optionally,   
                 eigenvectors of a real symmetric tridiagonal matrix.   

                 DSYEV computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix.   

                 DSYEVX computes selected eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix.   

                 DSPEV computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix in packed   
                 storage.   

                 DSPEVX computes selected eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix in packed   
                 storage.   

                 DSBEV computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric band matrix.   

                 DSBEVX computes selected eigenvalues and, optionally,   
                 eigenvectors of a real symmetric band matrix.   

                 DSYEVD computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix using   
                 a divide and conquer algorithm.   

                 DSPEVD computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric matrix in packed   
                 storage, using a divide and conquer algorithm.   

                 DSBEVD computes all eigenvalues and, optionally,   
                 eigenvectors of a real symmetric band matrix,   
                 using a divide and conquer algorithm.   

         When DDRVST is called, a number of matrix "sizes" ("n's") and a 
  
         number of matrix "types" are specified.  For each size ("n")   
         and each type of matrix, one matrix will be generated and used   
         to test the appropriate drivers.  For each matrix and each   
         driver routine called, the following tests will be performed:   

         (1)     | A - Z D Z' | / ( |A| n ulp )   

         (2)     | I - Z Z' | / ( n ulp )   

         (3)     | D1 - D2 | / ( |D1| ulp )   

         where Z is the matrix of eigenvectors returned when the   
         eigenvector option is given and D1 and D2 are the eigenvalues   
         returned with and without the eigenvector option.   

         The "sizes" are specified by an array NN(1:NSIZES); the value of 
  
         each element NN(j) specifies one size.   
         The "types" are specified by a logical array DOTYPE( 1:NTYPES ); 
  
         if DOTYPE(j) is .TRUE., then matrix type "j" will be generated. 
  
         Currently, the list of possible types is:   

         (1)  The zero matrix.   
         (2)  The identity matrix.   

         (3)  A diagonal matrix with evenly spaced eigenvalues   
              1, ..., ULP  and random signs.   
              (ULP = (first number larger than 1) - 1 )   
         (4)  A diagonal matrix with geometrically spaced eigenvalues   
              1, ..., ULP  and random signs.   
         (5)  A diagonal matrix with "clustered" eigenvalues   
              1, ULP, ..., ULP and random signs.   

         (6)  Same as (4), but multiplied by SQRT( overflow threshold )   
         (7)  Same as (4), but multiplied by SQRT( underflow threshold ) 
  

         (8)  A matrix of the form  U' D U, where U is orthogonal and   
              D has evenly spaced entries 1, ..., ULP with random signs   
              on the diagonal.   

         (9)  A matrix of the form  U' D U, where U is orthogonal and   
              D has geometrically spaced entries 1, ..., ULP with random 
  
              signs on the diagonal.   

         (10) A matrix of the form  U' D U, where U is orthogonal and   
              D has "clustered" entries 1, ULP,..., ULP with random   
              signs on the diagonal.   

         (11) Same as (8), but multiplied by SQRT( overflow threshold )   
         (12) Same as (8), but multiplied by SQRT( underflow threshold ) 
  

         (13) Symmetric matrix with random entries chosen from (-1,1).   
         (14) Same as (13), but multiplied by SQRT( overflow threshold ) 
  
         (15) Same as (13), but multiplied by SQRT( underflow threshold ) 
  
         (16) A band matrix with half bandwidth randomly chosen between   
              0 and N-1, with evenly spaced eigenvalues 1, ..., ULP   
              with random signs.   
         (17) Same as (16), but multiplied by SQRT( overflow threshold ) 
  
         (18) Same as (16), but multiplied by SQRT( underflow threshold ) 
  

    Arguments   
    =========   

    NSIZES  INTEGER   
            The number of sizes of matrices to use.  If it is zero,   
            DDRVST does nothing.  It must be at least zero.   
            Not modified.   

    NN      INTEGER array, dimension (NSIZES)   
            An array containing the sizes to be used for the matrices.   
            Zero values will be skipped.  The values must be at least   
            zero.   
            Not modified.   

    NTYPES  INTEGER   
            The number of elements in DOTYPE.   If it is zero, DDRVST   
            does nothing.  It must be at least zero.  If it is MAXTYP+1   
            and NSIZES is 1, then an additional type, MAXTYP+1 is   
            defined, which is to use whatever matrix is in A.  This   
            is only useful if DOTYPE(1:MAXTYP) is .FALSE. and   
            DOTYPE(MAXTYP+1) is .TRUE. .   
            Not modified.   

    DOTYPE  LOGICAL array, dimension (NTYPES)   
            If DOTYPE(j) is .TRUE., then for each size in NN a   
            matrix of that size and of type j will be generated.   
            If NTYPES is smaller than the maximum number of types   
            defined (PARAMETER MAXTYP), then types NTYPES+1 through   
            MAXTYP will not be generated.  If NTYPES is larger   
            than MAXTYP, DOTYPE(MAXTYP+1) through DOTYPE(NTYPES)   
            will be ignored.   
            Not modified.   

    ISEED   INTEGER array, dimension (4)   
            On entry ISEED specifies the seed of the random number   
            generator. The array elements should be between 0 and 4095;   
            if not they will be reduced mod 4096.  Also, ISEED(4) must   
            be odd.  The random number generator uses a linear   
            congruential sequence limited to small integers, and so   
            should produce machine independent random numbers. The   
            values of ISEED are changed on exit, and can be used in the   
            next call to DDRVST to continue the same random number   
            sequence.   
            Modified.   

    THRESH  DOUBLE PRECISION   
            A test will count as "failed" if the "error", computed as   
            described above, exceeds THRESH.  Note that the error   
            is scaled to be O(1), so THRESH should be a reasonably   
            small multiple of 1, e.g., 10 or 100.  In particular,   
            it should not depend on the precision (single vs. double)   
            or the size of the matrix.  It must be at least zero.   
            Not modified.   

    NOUNIT  INTEGER   
            The FORTRAN unit number for printing out error messages   
            (e.g., if a routine returns IINFO not equal to 0.)   
            Not modified.   

    A       DOUBLE PRECISION array, dimension (LDA , max(NN))   
            Used to hold the matrix whose eigenvalues are to be   
            computed.  On exit, A contains the last matrix actually   
            used.   
            Modified.   

    LDA     INTEGER   
            The leading dimension of A.  It must be at   
            least 1 and at least max( NN ).   
            Not modified.   

    D1      DOUBLE PRECISION array, dimension (max(NN))   
            The eigenvalues of A, as computed by DSTEQR simlutaneously   
            with Z.  On exit, the eigenvalues in D1 correspond with the   
            matrix in A.   
            Modified.   

    D2      DOUBLE PRECISION array, dimension (max(NN))   
            The eigenvalues of A, as computed by DSTEQR if Z is not   
            computed.  On exit, the eigenvalues in D2 correspond with   
            the matrix in A.   
            Modified.   

    D3      DOUBLE PRECISION array, dimension (max(NN))   
            The eigenvalues of A, as computed by DSTERF.  On exit, the   
            eigenvalues in D3 correspond with the matrix in A.   
            Modified.   

    D4      DOUBLE PRECISION array, dimension   

    WA1     DOUBLE PRECISION array, dimension   

    WA2     DOUBLE PRECISION array, dimension   

    WA3     DOUBLE PRECISION array, dimension   

    U       DOUBLE PRECISION array, dimension (LDU, max(NN))   
            The orthogonal matrix computed by DSYTRD + DORGTR.   
            Modified.   

    LDU     INTEGER   
            The leading dimension of U, Z, and V.  It must be at   
            least 1 and at least max( NN ).   
            Not modified.   

    V       DOUBLE PRECISION array, dimension (LDU, max(NN))   
            The Housholder vectors computed by DSYTRD in reducing A to   
            tridiagonal form.   
            Modified.   

    TAU     DOUBLE PRECISION array, dimension (max(NN))   
            The Householder factors computed by DSYTRD in reducing A   
            to tridiagonal form.   
            Modified.   

    Z       DOUBLE PRECISION array, dimension (LDU, max(NN))   
            The orthogonal matrix of eigenvectors computed by DSTEQR,   
            DPTEQR, and DSTEIN.   
            Modified.   

    WORK    DOUBLE PRECISION array, dimension (NWORK)   
            Workspace.   
            Modified.   

    NWORK   INTEGER   
            The number of entries in WORK.  This must be at least   
            1 + 4 * Nmax + 2 * Nmax * lg Nmax + 4 * Nmax**2   
            where Nmax = max( NN(j), 2 ) and lg = log base 2.   
            Not modified.   

    IWORK   INTEGER array,   
               dimension (6 + 6*Nmax + 5 * Nmax * lg Nmax )   
            where Nmax = max( NN(j), 2 ) and lg = log base 2.   
            Workspace.   
            Modified.   

    RESULT  DOUBLE PRECISION array, dimension (105)   
            The values computed by the tests described above.   
            The values are currently limited to 1/ulp, to avoid   
            overflow.   
            Modified.   

    INFO    INTEGER   
            If 0, then everything ran OK.   
             -1: NSIZES < 0   
             -2: Some NN(j) < 0   
             -3: NTYPES < 0   
             -5: THRESH < 0   
             -9: LDA < 1 or LDA < NMAX, where NMAX is max( NN(j) ).   
            -16: LDU < 1 or LDU < NMAX.   
            -21: NWORK too small.   
            If  DLATMR, SLATMS, DSYTRD, DORGTR, DSTEQR, SSTERF,   
                or DORMTR returns an error code, the   
                absolute value of it is returned.   
            Modified.   

   -----------------------------------------------------------------------
   

         Some Local Variables and Parameters:   
         ---- ----- --------- --- ----------   
         ZERO, ONE       Real 0 and 1.   
         MAXTYP          The number of types defined.   
         NTEST           The number of tests performed, or which can   
                         be performed so far, for the current matrix.   
         NTESTT          The total number of tests performed so far.   
         NMAX            Largest value in NN.   
         NMATS           The number of matrices generated so far.   
         NERRS           The number of tests which have exceeded THRESH   
                         so far (computed by DLAFTS).   
         COND, IMODE     Values to be passed to the matrix generators.   
         ANORM           Norm of A; passed to matrix generators.   

         OVFL, UNFL      Overflow and underflow thresholds.   
         ULP, ULPINV     Finest relative precision and its inverse.   
         RTOVFL, RTUNFL  Square roots of the previous 2 values.   
                 The following four arrays decode JTYPE:   
         KTYPE(j)        The general type (1-10) for type "j".   
         KMODE(j)        The MODE value to be passed to the matrix   
                         generator for type "j".   
         KMAGN(j)        The order of magnitude ( O(1),   
                         O(overflow^(1/2) ), O(underflow^(1/2) )   

    ===================================================================== 
  

       Parameter adjustments */
    --nn;
    --dotype;
    --iseed;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --d1;
    --d2;
    --d3;
    --d4;
    --wa1;
    --wa2;
    --wa3;
    z_dim1 = *ldu;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    v_dim1 = *ldu;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --tau;
    --work;
    --iwork;
    --result;

    /* Function Body   

       1)      Check for errors */

    ntestt = 0;
    *info = 0;

    badnn = FALSE_;
    nmax = 1;
    i__1 = *nsizes;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = nmax, i__3 = nn[j];
	nmax = max(i__2,i__3);
	if (nn[j] < 0) {
	    badnn = TRUE_;
	}
/* L10: */
    }

/*     Check for errors */

    if (*nsizes < 0) {
	*info = -1;
    } else if (badnn) {
	*info = -2;
    } else if (*ntypes < 0) {
	*info = -3;
    } else if (*lda < nmax) {
	*info = -9;
    } else if (*ldu < nmax) {
	*info = -16;
    } else /* if(complicated condition) */ {
/* Computing 2nd power */
	i__1 = max(2,nmax);
	if (i__1 * i__1 << 1 > *nwork) {
	    *info = -21;
	}
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("DDRVST", &i__1);
	return 0;
    }

/*     Quick return if nothing to do */

    if (*nsizes == 0 || *ntypes == 0) {
	return 0;
    }

/*     More Important constants */

    unfl = dlamch_("Safe minimum");
    ovfl = dlamch_("Overflow");
    dlabad_(&unfl, &ovfl);
    ulp = dlamch_("Epsilon") * dlamch_("Base");
    ulpinv = 1. / ulp;
    rtunfl = sqrt(unfl);
    rtovfl = sqrt(ovfl);

/*     Loop over sizes, types */

    for (i = 1; i <= 4; ++i) {
	iseed2[i - 1] = iseed[i];
	iseed3[i - 1] = iseed[i];
/* L20: */
    }

    nerrs = 0;
    nmats = 0;

    i__1 = *nsizes;
    for (jsize = 1; jsize <= i__1; ++jsize) {
	n = nn[jsize];
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
	    lwedc = (n << 2) + 1 + (n << 1) * lgn + (i__2 * i__2 << 2);
	    liwedc = n * 6 + 6 + n * 5 * lgn;
	} else {
	    lwedc = 9;
	    liwedc = 12;
	}
	aninv = 1. / (doublereal) max(1,n);

	if (*nsizes != 1) {
	    mtypes = min(18,*ntypes);
	} else {
	    mtypes = min(19,*ntypes);
	}

	i__2 = mtypes;
	for (jtype = 1; jtype <= i__2; ++jtype) {
	    if (! dotype[jtype]) {
		goto L1510;
	    }
	    ++nmats;
	    ntest = 0;

	    for (j = 1; j <= 4; ++j) {
		ioldsd[j - 1] = iseed[j];
/* L30: */
	    }

/*           2)      Compute "A"   

                     Control parameters:   

                 KMAGN  KMODE        KTYPE   
             =1  O(1)   clustered 1  zero   
             =2  large  clustered 2  identity   
             =3  small  exponential  (none)   
             =4         arithmetic   diagonal, (w/ eigenvalues)   
             =5         random log   symmetric, w/ eigenvalues   
             =6         random       (none)   
             =7                      random diagonal   
             =8                      random symmetric   
             =9                      band symmetric, w/ eigenvalue
s */

	    if (mtypes > 18) {
		goto L110;
	    }

	    itype = ktype[jtype - 1];
	    imode = kmode[jtype - 1];

/*           Compute norm */

	    switch (kmagn[jtype - 1]) {
		case 1:  goto L40;
		case 2:  goto L50;
		case 3:  goto L60;
	    }

L40:
	    anorm = 1.;
	    goto L70;

L50:
	    anorm = rtovfl * ulp * aninv;
	    goto L70;

L60:
	    anorm = rtunfl * n * ulpinv;
	    goto L70;

L70:

	    dlaset_("Full", lda, &n, &c_b20, &c_b20, &a[a_offset], lda);
	    iinfo = 0;
	    cond = ulpinv;

/*           Special Matrices -- Identity & Jordan block   

                     Zero */

	    if (itype == 1) {
		iinfo = 0;

	    } else if (itype == 2) {

/*              Identity */

		i__3 = n;
		for (jcol = 1; jcol <= i__3; ++jcol) {
		    a[jcol + jcol * a_dim1] = anorm;
/* L80: */
		}

	    } else if (itype == 4) {

/*              Diagonal Matrix, [Eigen]values Specified */

		dlatms_(&n, &n, "S", &iseed[1], "S", &work[1], &imode, &cond, 
			&anorm, &c__0, &c__0, "N", &a[a_offset], lda, &work[n 
			+ 1], &iinfo);

	    } else if (itype == 5) {

/*              Symmetric, eigenvalues specified */

		dlatms_(&n, &n, "S", &iseed[1], "S", &work[1], &imode, &cond, 
			&anorm, &n, &n, "N", &a[a_offset], lda, &work[n + 1], 
			&iinfo);

	    } else if (itype == 7) {

/*              Diagonal, random eigenvalues */

		dlatmr_(&n, &n, "S", &iseed[1], "S", &work[1], &c__6, &c_b34, 
			&c_b34, "T", "N", &work[n + 1], &c__1, &c_b34, &work[(
			n << 1) + 1], &c__1, &c_b34, "N", idumma, &c__0, &
			c__0, &c_b20, &anorm, "NO", &a[a_offset], lda, &iwork[
			1], &iinfo);

	    } else if (itype == 8) {

/*              Symmetric, random eigenvalues */

		dlatmr_(&n, &n, "S", &iseed[1], "S", &work[1], &c__6, &c_b34, 
			&c_b34, "T", "N", &work[n + 1], &c__1, &c_b34, &work[(
			n << 1) + 1], &c__1, &c_b34, "N", idumma, &n, &n, &
			c_b20, &anorm, "NO", &a[a_offset], lda, &iwork[1], &
			iinfo);

	    } else if (itype == 9) {

/*              Symmetric banded, eigenvalues specified */

		ihbw = (integer) ((n - 1) * dlarnd_(&c__1, iseed3));
		dlatms_(&n, &n, "S", &iseed[1], "S", &work[1], &imode, &cond, 
			&anorm, &ihbw, &ihbw, "Z", &u[u_offset], ldu, &work[n 
			+ 1], &iinfo);

/*              Store as dense matrix for most routines. */

		dlaset_("Full", lda, &n, &c_b20, &c_b20, &a[a_offset], lda);
		i__3 = ihbw;
		for (idiag = -ihbw; idiag <= i__3; ++idiag) {
		    irow = ihbw - idiag + 1;
		    j1 = max(1,idiag);
/* Computing MIN */
		    i__4 = n, i__5 = n + idiag;
		    j2 = min(i__4,i__5);
		    i__4 = j2;
		    for (j = j1; j <= i__4; ++j) {
			i = j - idiag;
			a[i + j * a_dim1] = u[irow + j * u_dim1];
/* L90: */
		    }
/* L100: */
		}
	    } else {
		iinfo = 1;
	    }

	    if (iinfo != 0) {
		io___41.ciunit = *nounit;
		s_wsfe(&io___41);
		do_fio(&c__1, "Generator", 9L);
		do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer));
		e_wsfe();
		*info = abs(iinfo);
		return 0;
	    }

L110:

	    abstol = unfl + unfl;
	    if (n <= 1) {
		il = 1;
		iu = n;
	    } else {
		il = (integer) ((n - 1) * dlarnd_(&c__1, iseed2) + 1);
		iu = (integer) ((n - 1) * dlarnd_(&c__1, iseed2) + 1);
		if (il > iu) {
		    itemp = il;
		    il = iu;
		    iu = itemp;
		}
	    }

/*           3)      If matrix is tridiagonal, call DSTEV and SSTE
VX. */

	    if (jtype <= 7) {
		ntest = 1;
		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d1[i] = a[i + i * a_dim1];
/* L120: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d2[i] = a[i + 1 + i * a_dim1];
/* L130: */
		}
		dstev_("V", &n, &d1[1], &d2[1], &z[z_offset], ldu, &work[1], &
			iinfo);
		if (iinfo != 0) {
		    io___46.ciunit = *nounit;
		    s_wsfe(&io___46);
		    do_fio(&c__1, "DSTEV(V)", 8L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[1] = ulpinv;
			result[2] = ulpinv;
			result[3] = ulpinv;
			goto L180;
		    }
		}

/*              Do tests 1 and 2. */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d3[i] = a[i + i * a_dim1];
/* L140: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L150: */
		}
		dstt21_(&n, &c__0, &d3[1], &d4[1], &d1[1], &d2[1], &z[
			z_offset], ldu, &work[1], &result[1]);

		ntest = 3;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L160: */
		}
		dstev_("N", &n, &d3[1], &d4[1], &z[z_offset], ldu, &work[1], &
			iinfo);
		if (iinfo != 0) {
		    io___47.ciunit = *nounit;
		    s_wsfe(&io___47);
		    do_fio(&c__1, "DSTEV(N)", 8L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[3] = ulpinv;
			goto L180;
		    }
		}

/*              Do test 3. */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L170: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[3] = temp2 / max(d__1,d__2);

L180:

		ntest = 4;
		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d1[i] = a[i + i * a_dim1];
/* L190: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d2[i] = a[i + 1 + i * a_dim1];
/* L200: */
		}
		dstevx_("V", "A", &n, &d1[1], &d2[1], &vl, &vu, &il, &iu, &
			abstol, &m, &wa1[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___53.ciunit = *nounit;
		    s_wsfe(&io___53);
		    do_fio(&c__1, "DSTEVX(V,A)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[4] = ulpinv;
			result[5] = ulpinv;
			result[6] = ulpinv;
			goto L250;
		    }
		}
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}

/*              Do tests 4 and 5. */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d3[i] = a[i + i * a_dim1];
/* L210: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L220: */
		}
		dstt21_(&n, &c__0, &d3[1], &d4[1], &wa1[1], &d2[1], &z[
			z_offset], ldu, &work[1], &result[4]);

		ntest = 6;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L230: */
		}
		dstevx_("N", "A", &n, &d3[1], &d4[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___56.ciunit = *nounit;
		    s_wsfe(&io___56);
		    do_fio(&c__1, "DSTEVX(N,A)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[6] = ulpinv;
			goto L250;
		    }
		}

/*              Do test 6. */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = wa1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = wa2[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = wa1[j] - wa2[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L240: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[6] = temp2 / max(d__1,d__2);

L250:

		ntest = 7;
		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d1[i] = a[i + i * a_dim1];
/* L260: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d2[i] = a[i + 1 + i * a_dim1];
/* L270: */
		}
		dstevx_("V", "I", &n, &d1[1], &d2[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___57.ciunit = *nounit;
		    s_wsfe(&io___57);
		    do_fio(&c__1, "DSTEVX(V,I)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[7] = ulpinv;
			result[8] = ulpinv;
			result[9] = ulpinv;
			goto L310;
		    }
		}

/*              Do tests 7 and 8. */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d3[i] = a[i + i * a_dim1];
/* L280: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L290: */
		}
		i__3 = max(1,m2);
		dstt22_(&n, &m2, &c__0, &d3[1], &d4[1], &wa2[1], &d2[1], &z[
			z_offset], ldu, &work[1], &i__3, &result[7]);

		ntest = 9;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L300: */
		}
		dstevx_("N", "I", &n, &d3[1], &d4[1], &vl, &vu, &il, &iu, &
			abstol, &m3, &wa3[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___59.ciunit = *nounit;
		    s_wsfe(&io___59);
		    do_fio(&c__1, "DSTEVX(N,I)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[9] = ulpinv;
			goto L310;
		    }
		}

/*              Do test 9. */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * temp3;
		result[9] = (temp1 + temp2) / max(d__1,d__2);

L310:

		ntest = 10;
		if (n > 0) {
		    if (il != 1) {
/* Computing MAX */
			d__1 = (wa1[il] - wa1[il - 1]) * .5, d__2 = ulp * 10. 
				* temp3, d__1 = max(d__1,d__2), d__2 = rtunfl 
				* 10.;
			vl = wa1[il] - max(d__1,d__2);
		    } else {
/* Computing MAX */
			d__1 = (wa1[n] - wa1[1]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vl = wa1[1] - max(d__1,d__2);
		    }
		    if (iu != n) {
/* Computing MAX */
			d__1 = (wa1[iu + 1] - wa1[iu]) * .5, d__2 = ulp * 10. 
				* temp3, d__1 = max(d__1,d__2), d__2 = rtunfl 
				* 10.;
			vu = wa1[iu] + max(d__1,d__2);
		    } else {
/* Computing MAX */
			d__1 = (wa1[n] - wa1[1]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vu = wa1[n] + max(d__1,d__2);
		    }
		} else {
		    vl = 0.;
		    vu = 1.;
		}

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d1[i] = a[i + i * a_dim1];
/* L320: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d2[i] = a[i + 1 + i * a_dim1];
/* L330: */
		}

		dstevx_("V", "V", &n, &d1[1], &d2[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___60.ciunit = *nounit;
		    s_wsfe(&io___60);
		    do_fio(&c__1, "DSTEVX(V,V)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[10] = ulpinv;
			result[11] = ulpinv;
			result[12] = ulpinv;
			goto L370;
		    }
		}

		if (m2 == 0 && n > 0) {
		    result[10] = ulpinv;
		    result[11] = ulpinv;
		    result[12] = ulpinv;
		    goto L370;
		}

/*              Do tests 10 and 11. */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d3[i] = a[i + i * a_dim1];
/* L340: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L350: */
		}
		i__3 = max(1,m2);
		dstt22_(&n, &m2, &c__0, &d3[1], &d4[1], &wa2[1], &d2[1], &z[
			z_offset], ldu, &work[1], &i__3, &result[10]);

		ntest = 12;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L360: */
		}
		dstevx_("N", "V", &n, &d3[1], &d4[1], &vl, &vu, &il, &iu, &
			abstol, &m3, &wa3[1], &z[z_offset], ldu, &work[1], &
			iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___61.ciunit = *nounit;
		    s_wsfe(&io___61);
		    do_fio(&c__1, "DSTEVX(N,V)", 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[12] = ulpinv;
			goto L370;
		    }
		}

/*              Do test 12. */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[12] = (temp1 + temp2) / max(d__1,d__2);

L370:

		ntest = 13;
		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d1[i] = a[i + i * a_dim1];
/* L380: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d2[i] = a[i + 1 + i * a_dim1];
/* L390: */
		}
		dstevd_("V", &n, &d1[1], &d2[1], &z[z_offset], ldu, &work[1], 
			&lwedc, &iwork[1], &liwedc, &iinfo);
		if (iinfo != 0) {
		    io___62.ciunit = *nounit;
		    s_wsfe(&io___62);
		    do_fio(&c__1, "DSTEVD(V)", 9L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[13] = ulpinv;
			result[14] = ulpinv;
			result[15] = ulpinv;
			goto L440;
		    }
		}

/*              Do tests 13 and 14. */

		i__3 = n;
		for (i = 1; i <= i__3; ++i) {
		    d3[i] = a[i + i * a_dim1];
/* L400: */
		}
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L410: */
		}
		dstt21_(&n, &c__0, &d3[1], &d4[1], &d1[1], &d2[1], &z[
			z_offset], ldu, &work[1], &result[13]);

		ntest = 15;
		i__3 = n - 1;
		for (i = 1; i <= i__3; ++i) {
		    d4[i] = a[i + 1 + i * a_dim1];
/* L420: */
		}
		dstevd_("N", &n, &d3[1], &d4[1], &z[z_offset], ldu, &work[1], 
			&lwedc, &iwork[1], &liwedc, &iinfo);
		if (iinfo != 0) {
		    io___63.ciunit = *nounit;
		    s_wsfe(&io___63);
		    do_fio(&c__1, "DSTEVD(N)", 9L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[15] = ulpinv;
			goto L440;
		    }
		}

/*              Do test 15. */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L430: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[15] = temp2 / max(d__1,d__2);

L440:

		;
	    } else {

		for (i = 1; i <= 15; ++i) {
		    result[i] = 0.;
/* L450: */
		}
		ntest = 15;
	    }

/*           Perform remaining tests storing upper or lower triang
ular   
             part of matrix. */

	    for (iuplo = 0; iuplo <= 1; ++iuplo) {
		if (iuplo == 0) {
		    *(unsigned char *)uplo = 'L';
		} else {
		    *(unsigned char *)uplo = 'U';
		}

/*              4)      Call DSYEV and SSYEVX. */

		dlacpy_(" ", &n, &n, &a[a_offset], lda, &v[v_offset], ldu)
			;

		++ntest;
		dsyev_("V", uplo, &n, &a[a_offset], ldu, &d1[1], &work[1], 
			nwork, &iinfo);
		if (iinfo != 0) {
		    io___66.ciunit = *nounit;
		    s_wsfe(&io___66);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSYEV(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L470;
		    }
		}

/*              Do tests 16 and 17 (or 61 and 62). */

		dsyt21_(&c__1, uplo, &n, &c__0, &v[v_offset], ldu, &d1[1], &
			d2[1], &a[a_offset], ldu, &z[z_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		ntest += 2;
		dsyev_("N", uplo, &n, &a[a_offset], ldu, &d3[1], &work[1], 
			nwork, &iinfo);
		if (iinfo != 0) {
		    io___67.ciunit = *nounit;
		    s_wsfe(&io___67);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSYEV(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L470;
		    }
		}

/*              Do test 18 (or 63). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L460: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L470:
		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		++ntest;

		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(d1[1]), d__3 = (d__1 = d1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		    if (il != 1) {
/* Computing MAX */
			d__1 = (d1[il] - d1[il - 1]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vl = d1[il] - max(d__1,d__2);
		    } else if (n > 0) {
/* Computing MAX */
			d__1 = (d1[n] - d1[1]) * .5, d__2 = ulp * 10. * temp3,
				 d__1 = max(d__1,d__2), d__2 = rtunfl * 10.;
			vl = d1[1] - max(d__1,d__2);
		    }
		    if (iu != n) {
/* Computing MAX */
			d__1 = (d1[iu + 1] - d1[iu]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vu = d1[iu] + max(d__1,d__2);
		    } else if (n > 0) {
/* Computing MAX */
			d__1 = (d1[n] - d1[1]) * .5, d__2 = ulp * 10. * temp3,
				 d__1 = max(d__1,d__2), d__2 = rtunfl * 10.;
			vu = d1[n] + max(d__1,d__2);
		    }
		} else {
		    temp3 = 0.;
		    vl = 0.;
		    vu = 1.;
		}

		dsyevx_("V", "A", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m, &wa1[1], &z[z_offset], ldu, &work[1]
			, nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___68.ciunit = *nounit;
		    s_wsfe(&io___68);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(V,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L490;
		    }
		}

/*              Do tests 19 and 20 (or 64 and 65). */

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], ldu, &d1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		ntest += 2;
		dsyevx_("N", "A", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[
			1], nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___69.ciunit = *nounit;
		    s_wsfe(&io___69);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(N,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L490;
		    }
		}

/*              Do test 21 (or 66). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = wa1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = wa2[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = wa1[j] - wa2[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L480: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L490:

		++ntest;
		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;
		dsyevx_("V", "I", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[
			1], nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___70.ciunit = *nounit;
		    s_wsfe(&io___70);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(V,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L500;
		    }
		}

/*              Do tests 22 and 23 (or 67 and 68) */

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;
		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;
		dsyevx_("N", "I", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m3, &wa3[1], &z[z_offset], ldu, &work[
			1], nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___71.ciunit = *nounit;
		    s_wsfe(&io___71);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(N,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L500;
		    }
		}

/*              Do test 24 (or 69) */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * temp3;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);
L500:

		++ntest;
		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;
		dsyevx_("V", "V", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m2, &wa2[1], &z[z_offset], ldu, &work[
			1], nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___72.ciunit = *nounit;
		    s_wsfe(&io___72);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(V,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L510;
		    }
		}

/*              Do tests 25 and 26 (or 70 and 71). */

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;
		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;
		dsyevx_("N", "V", uplo, &n, &a[a_offset], ldu, &vl, &vu, &il, 
			&iu, &abstol, &m3, &wa3[1], &z[z_offset], ldu, &work[
			1], nwork, &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___73.ciunit = *nounit;
		    s_wsfe(&io___73);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSYEVX(N,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L510;
		    }
		}

		if (m3 == 0 && n > 0) {
		    result[ntest] = ulpinv;
		    goto L510;
		}

/*              Do test 27 (or 72). */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);

L510:

/*              5)      Call DSPEV and SSPEVX. */

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

/*              Load array WORK with the upper or lower triang
ular   
                part of the matrix in packed form. */

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L520: */
			}
/* L530: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L540: */
			}
/* L550: */
		    }
		}

		++ntest;
		dspev_("V", uplo, &n, &work[1], &d1[1], &z[z_offset], ldu, &v[
			v_offset], &iinfo);
		if (iinfo != 0) {
		    io___75.ciunit = *nounit;
		    s_wsfe(&io___75);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSPEV(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L610;
		    }
		}

/*              Do tests 28 and 29 (or 73 and 74). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], lda, &d1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L560: */
			}
/* L570: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L580: */
			}
/* L590: */
		    }
		}

		ntest += 2;
		dspev_("N", uplo, &n, &work[1], &d3[1], &z[z_offset], ldu, &v[
			v_offset], &iinfo);
		if (iinfo != 0) {
		    io___76.ciunit = *nounit;
		    s_wsfe(&io___76);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSPEV(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L610;
		    }
		}

/*              Do test 30 (or 75) */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L600: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

/*              Load array WORK with the upper or lower triang
ular part   
                of the matrix in packed form. */

L610:
		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L620: */
			}
/* L630: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L640: */
			}
/* L650: */
		    }
		}

		++ntest;

		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(d1[1]), d__3 = (d__1 = d1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		    if (il != 1) {
/* Computing MAX */
			d__1 = (d1[il] - d1[il - 1]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vl = d1[il] - max(d__1,d__2);
		    } else if (n > 0) {
/* Computing MAX */
			d__1 = (d1[n] - d1[1]) * .5, d__2 = ulp * 10. * temp3,
				 d__1 = max(d__1,d__2), d__2 = rtunfl * 10.;
			vl = d1[1] - max(d__1,d__2);
		    }
		    if (iu != n) {
/* Computing MAX */
			d__1 = (d1[iu + 1] - d1[iu]) * .5, d__2 = ulp * 10. * 
				temp3, d__1 = max(d__1,d__2), d__2 = rtunfl * 
				10.;
			vu = d1[iu] + max(d__1,d__2);
		    } else if (n > 0) {
/* Computing MAX */
			d__1 = (d1[n] - d1[1]) * .5, d__2 = ulp * 10. * temp3,
				 d__1 = max(d__1,d__2), d__2 = rtunfl * 10.;
			vu = d1[n] + max(d__1,d__2);
		    }
		} else {
		    temp3 = 0.;
		    vl = 0.;
		    vu = 1.;
		}

		dspevx_("V", "A", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m, &wa1[1], &z[z_offset], ldu, &v[v_offset], 
			&iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___77.ciunit = *nounit;
		    s_wsfe(&io___77);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(V,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L710;
		    }
		}

/*              Do tests 31 and 32 (or 76 and 77). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], ldu, &wa1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L660: */
			}
/* L670: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L680: */
			}
/* L690: */
		    }
		}

		dspevx_("N", "A", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &v[v_offset],
			 &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___78.ciunit = *nounit;
		    s_wsfe(&io___78);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(N,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L710;
		    }
		}

/*              Do test 33 (or 78). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = wa1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = wa2[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = wa1[j] - wa2[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L700: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L710:
		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L720: */
			}
/* L730: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L740: */
			}
/* L750: */
		    }
		}

		++ntest;

		dspevx_("V", "I", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &v[v_offset],
			 &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___79.ciunit = *nounit;
		    s_wsfe(&io___79);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(V,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L800;
		    }
		}

/*              Do tests 34 and 35 (or 79 and 80). */

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L760: */
			}
/* L770: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L780: */
			}
/* L790: */
		    }
		}

		dspevx_("N", "I", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m3, &wa3[1], &z[z_offset], ldu, &v[v_offset],
			 &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___80.ciunit = *nounit;
		    s_wsfe(&io___80);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(N,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L800;
		    }
		}

		if (m3 == 0 && n > 0) {
		    result[ntest] = ulpinv;
		    goto L800;
		}

/*              Do test 36 (or 81). */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);

L800:
		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L810: */
			}
/* L820: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L830: */
			}
/* L840: */
		    }
		}

		++ntest;

		dspevx_("V", "V", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m2, &wa2[1], &z[z_offset], ldu, &v[v_offset],
			 &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___81.ciunit = *nounit;
		    s_wsfe(&io___81);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(V,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L890;
		    }
		}

/*              Do tests 37 and 38 (or 82 and 83). */

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = j;
			for (i = 1; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L850: */
			}
/* L860: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = n;
			for (i = j; i <= i__4; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L870: */
			}
/* L880: */
		    }
		}

		dspevx_("N", "V", uplo, &n, &work[1], &vl, &vu, &il, &iu, &
			abstol, &m3, &wa3[1], &z[z_offset], ldu, &v[v_offset],
			 &iwork[1], &iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___82.ciunit = *nounit;
		    s_wsfe(&io___82);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSPEVX(N,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L890;
		    }
		}

		if (m3 == 0 && n > 0) {
		    result[ntest] = ulpinv;
		    goto L890;
		}

/*              Do test 39 (or 84). */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);

L890:

/*              6)      Call DSBEV and SSBEVX. */

		if (jtype <= 7) {
		    kd = 1;
		} else if (jtype >= 8 && jtype <= 15) {
/* Computing MAX */
		    i__3 = n - 1;
		    kd = max(i__3,0);
		} else {
		    kd = ihbw;
		}

/*              Load array V with the upper or lower triangula
r part   
                of the matrix in band form. */

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__4 = 1, i__5 = j - kd;
			i__7 = j;
			for (i = max(i__4,i__5); i <= i__7; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L900: */
			}
/* L910: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__4 = n, i__5 = j + kd;
			i__7 = min(i__4,i__5);
			for (i = j; i <= i__7; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L920: */
			}
/* L930: */
		    }
		}

		++ntest;
		dsbev_("V", uplo, &n, &kd, &v[v_offset], ldu, &d1[1], &z[
			z_offset], ldu, &work[1], &iinfo);
		if (iinfo != 0) {
		    io___84.ciunit = *nounit;
		    s_wsfe(&io___84);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSBEV(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L990;
		    }
		}

/*              Do tests 40 and 41 (or 85 and 86). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], lda, &d1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__7 = 1, i__4 = j - kd;
			i__5 = j;
			for (i = max(i__7,i__4); i <= i__5; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L940: */
			}
/* L950: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__7 = n, i__4 = j + kd;
			i__5 = min(i__7,i__4);
			for (i = j; i <= i__5; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L960: */
			}
/* L970: */
		    }
		}

		ntest += 2;
		dsbev_("N", uplo, &n, &kd, &v[v_offset], ldu, &d3[1], &z[
			z_offset], ldu, &work[1], &iinfo);
		if (iinfo != 0) {
		    io___85.ciunit = *nounit;
		    s_wsfe(&io___85);
/* Writing concatenation */
		    i__6[0] = 8, a__1[0] = "DSBEV(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__1, a__1, i__6, &c__3, 10L);
		    do_fio(&c__1, ch__1, 10L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L990;
		    }
		}

/*              Do test 42 (or 87). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L980: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

/*              Load array V with the upper or lower triangula
r part   
                of the matrix in band form. */

L990:
		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__5 = 1, i__7 = j - kd;
			i__4 = j;
			for (i = max(i__5,i__7); i <= i__4; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1000: */
			}
/* L1010: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__5 = n, i__7 = j + kd;
			i__4 = min(i__5,i__7);
			for (i = j; i <= i__4; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1020: */
			}
/* L1030: */
		    }
		}

		++ntest;
		dsbevx_("V", "A", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m, &wa2[
			1], &z[z_offset], ldu, &work[1], &iwork[1], &iwork[n *
			 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___86.ciunit = *nounit;
		    s_wsfe(&io___86);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(V,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1090;
		    }
		}

/*              Do tests 43 and 44 (or 88 and 89). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], ldu, &wa2[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__4 = 1, i__5 = j - kd;
			i__7 = j;
			for (i = max(i__4,i__5); i <= i__7; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1040: */
			}
/* L1050: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__4 = n, i__5 = j + kd;
			i__7 = min(i__4,i__5);
			for (i = j; i <= i__7; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1060: */
			}
/* L1070: */
		    }
		}

		dsbevx_("N", "A", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m3, &
			wa3[1], &z[z_offset], ldu, &work[1], &iwork[1], &
			iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___87.ciunit = *nounit;
		    s_wsfe(&io___87);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(N,A,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1090;
		    }
		}

/*              Do test 45 (or 90). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = wa2[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = wa3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = wa2[j] - wa3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L1080: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L1090:
		++ntest;
		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__7 = 1, i__4 = j - kd;
			i__5 = j;
			for (i = max(i__7,i__4); i <= i__5; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1100: */
			}
/* L1110: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__7 = n, i__4 = j + kd;
			i__5 = min(i__7,i__4);
			for (i = j; i <= i__5; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1120: */
			}
/* L1130: */
		    }
		}

		dsbevx_("V", "I", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m2, &
			wa2[1], &z[z_offset], ldu, &work[1], &iwork[1], &
			iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___88.ciunit = *nounit;
		    s_wsfe(&io___88);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(V,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1180;
		    }
		}

/*              Do tests 46 and 47 (or 91 and 92). */

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__5 = 1, i__7 = j - kd;
			i__4 = j;
			for (i = max(i__5,i__7); i <= i__4; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1140: */
			}
/* L1150: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__5 = n, i__7 = j + kd;
			i__4 = min(i__5,i__7);
			for (i = j; i <= i__4; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1160: */
			}
/* L1170: */
		    }
		}

		dsbevx_("N", "I", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m3, &
			wa3[1], &z[z_offset], ldu, &work[1], &iwork[1], &
			iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___89.ciunit = *nounit;
		    s_wsfe(&io___89);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(N,I,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1180;
		    }
		}

/*              Do test 48 (or 93). */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);

L1180:
		++ntest;
		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__4 = 1, i__5 = j - kd;
			i__7 = j;
			for (i = max(i__4,i__5); i <= i__7; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1190: */
			}
/* L1200: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__4 = n, i__5 = j + kd;
			i__7 = min(i__4,i__5);
			for (i = j; i <= i__7; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1210: */
			}
/* L1220: */
		    }
		}

		dsbevx_("V", "V", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m2, &
			wa2[1], &z[z_offset], ldu, &work[1], &iwork[1], &
			iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___90.ciunit = *nounit;
		    s_wsfe(&io___90);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(V,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1270;
		    }
		}

/*              Do tests 49 and 50 (or 94 and 95). */

		dsyt22_(&c__1, uplo, &n, &m2, &c__0, &a[a_offset], ldu, &wa2[
			1], &d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &
			tau[1], &work[1], &result[ntest]);

		ntest += 2;

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__7 = 1, i__4 = j - kd;
			i__5 = j;
			for (i = max(i__7,i__4); i <= i__5; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1230: */
			}
/* L1240: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__7 = n, i__4 = j + kd;
			i__5 = min(i__7,i__4);
			for (i = j; i <= i__5; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1250: */
			}
/* L1260: */
		    }
		}

		dsbevx_("N", "V", uplo, &n, &kd, &v[v_offset], ldu, &u[
			u_offset], ldu, &vl, &vu, &il, &iu, &abstol, &m3, &
			wa3[1], &z[z_offset], ldu, &work[1], &iwork[1], &
			iwork[n * 5 + 1], &iinfo);
		if (iinfo != 0) {
		    io___91.ciunit = *nounit;
		    s_wsfe(&io___91);
/* Writing concatenation */
		    i__6[0] = 11, a__1[0] = "DSBEVX(N,V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__2, a__1, i__6, &c__3, 13L);
		    do_fio(&c__1, ch__2, 13L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1270;
		    }
		}

		if (m3 == 0 && n > 0) {
		    result[ntest] = ulpinv;
		    goto L1270;
		}

/*              Do test 51 (or 96). */

		temp1 = dsxt1_(&c__1, &wa2[1], &m2, &wa3[1], &m3, &abstol, &
			ulp, &unfl);
		temp2 = dsxt1_(&c__1, &wa3[1], &m3, &wa2[1], &m2, &abstol, &
			ulp, &unfl);
		if (n > 0) {
/* Computing MAX */
		    d__2 = abs(wa1[1]), d__3 = (d__1 = wa1[n], abs(d__1));
		    temp3 = max(d__2,d__3);
		} else {
		    temp3 = 0.;
		}
/* Computing MAX */
		d__1 = unfl, d__2 = temp3 * ulp;
		result[ntest] = (temp1 + temp2) / max(d__1,d__2);

L1270:

/*              7)      Call DSYEVD */

		dlacpy_(" ", &n, &n, &a[a_offset], lda, &v[v_offset], ldu)
			;

		++ntest;
		dsyevd_("V", uplo, &n, &a[a_offset], ldu, &d1[1], &work[1], &
			lwedc, &iwork[1], &liwedc, &iinfo);
		if (iinfo != 0) {
		    io___92.ciunit = *nounit;
		    s_wsfe(&io___92);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSYEVD(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1290;
		    }
		}

/*              Do tests 52 and 53 (or 97 and 98). */

		dsyt21_(&c__1, uplo, &n, &c__0, &v[v_offset], ldu, &d1[1], &
			d2[1], &a[a_offset], ldu, &z[z_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

		ntest += 2;
		dsyevd_("N", uplo, &n, &a[a_offset], ldu, &d3[1], &work[1], &
			lwedc, &iwork[1], &liwedc, &iinfo);
		if (iinfo != 0) {
		    io___93.ciunit = *nounit;
		    s_wsfe(&io___93);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSYEVD(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1290;
		    }
		}

/*              Do test 54 (or 99). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L1280: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L1290:

/*              8)      Call DSPEVD. */

		dlacpy_(" ", &n, &n, &v[v_offset], ldu, &a[a_offset], lda)
			;

/*              Load array WORK with the upper or lower triang
ular   
                part of the matrix in packed form. */

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__5 = j;
			for (i = 1; i <= i__5; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L1300: */
			}
/* L1310: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__5 = n;
			for (i = j; i <= i__5; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L1320: */
			}
/* L1330: */
		    }
		}

		++ntest;
		i__3 = lwedc - indx + 1;
		dspevd_("V", uplo, &n, &work[1], &d1[1], &z[z_offset], ldu, &
			work[indx], &i__3, &iwork[1], &liwedc, &iinfo)
			;
		if (iinfo != 0) {
		    io___94.ciunit = *nounit;
		    s_wsfe(&io___94);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSPEVD(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1390;
		    }
		}

/*              Do tests 55 and 56 (or 100 and 101). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], lda, &d1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		if (iuplo == 1) {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__5 = j;
			for (i = 1; i <= i__5; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L1340: */
			}
/* L1350: */
		    }
		} else {
		    indx = 1;
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__5 = n;
			for (i = j; i <= i__5; ++i) {
			    work[indx] = a[i + j * a_dim1];
			    ++indx;
/* L1360: */
			}
/* L1370: */
		    }
		}

		ntest += 2;
		i__3 = lwedc - indx + 1;
		dspevd_("N", uplo, &n, &work[1], &d3[1], &z[z_offset], ldu, &
			work[indx], &i__3, &iwork[1], &liwedc, &iinfo)
			;
		if (iinfo != 0) {
		    io___95.ciunit = *nounit;
		    s_wsfe(&io___95);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSPEVD(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1390;
		    }
		}

/*              Do test 57 (or 102). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L1380: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);
L1390:

/*              9)      Call DSBEVD. */

		if (jtype <= 7) {
		    kd = 1;
		} else if (jtype >= 8 && jtype <= 15) {
/* Computing MAX */
		    i__3 = n - 1;
		    kd = max(i__3,0);
		} else {
		    kd = ihbw;
		}

/*              Load array V with the upper or lower triangula
r part   
                of the matrix in band form. */

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__5 = 1, i__7 = j - kd;
			i__4 = j;
			for (i = max(i__5,i__7); i <= i__4; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1400: */
			}
/* L1410: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__5 = n, i__7 = j + kd;
			i__4 = min(i__5,i__7);
			for (i = j; i <= i__4; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1420: */
			}
/* L1430: */
		    }
		}

		++ntest;
		dsbevd_("V", uplo, &n, &kd, &v[v_offset], ldu, &d1[1], &z[
			z_offset], ldu, &work[1], &lwedc, &iwork[1], &liwedc, 
			&iinfo);
		if (iinfo != 0) {
		    io___96.ciunit = *nounit;
		    s_wsfe(&io___96);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSBEVD(V,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			result[ntest + 1] = ulpinv;
			result[ntest + 2] = ulpinv;
			goto L1490;
		    }
		}

/*              Do tests 58 and 59 (or 103 and 104). */

		dsyt21_(&c__1, uplo, &n, &c__0, &a[a_offset], lda, &d1[1], &
			d2[1], &z[z_offset], ldu, &v[v_offset], ldu, &tau[1], 
			&work[1], &result[ntest]);

		if (iuplo == 1) {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
			i__4 = 1, i__5 = j - kd;
			i__7 = j;
			for (i = max(i__4,i__5); i <= i__7; ++i) {
			    v[kd + 1 + i - j + j * v_dim1] = a[i + j * a_dim1]
				    ;
/* L1440: */
			}
/* L1450: */
		    }
		} else {
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
			i__4 = n, i__5 = j + kd;
			i__7 = min(i__4,i__5);
			for (i = j; i <= i__7; ++i) {
			    v[i + 1 - j + j * v_dim1] = a[i + j * a_dim1];
/* L1460: */
			}
/* L1470: */
		    }
		}

		ntest += 2;
		dsbevd_("N", uplo, &n, &kd, &v[v_offset], ldu, &d3[1], &z[
			z_offset], ldu, &work[1], &lwedc, &iwork[1], &liwedc, 
			&iinfo);
		if (iinfo != 0) {
		    io___97.ciunit = *nounit;
		    s_wsfe(&io___97);
/* Writing concatenation */
		    i__6[0] = 9, a__1[0] = "DSBEVD(N,";
		    i__6[1] = 1, a__1[1] = uplo;
		    i__6[2] = 1, a__1[2] = ")";
		    s_cat(ch__3, a__1, i__6, &c__3, 11L);
		    do_fio(&c__1, ch__3, 11L);
		    do_fio(&c__1, (char *)&iinfo, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		    do_fio(&c__1, (char *)&jtype, (ftnlen)sizeof(integer));
		    do_fio(&c__4, (char *)&ioldsd[0], (ftnlen)sizeof(integer))
			    ;
		    e_wsfe();
		    *info = abs(iinfo);
		    if (iinfo < 0) {
			return 0;
		    } else {
			result[ntest] = ulpinv;
			goto L1490;
		    }
		}

/*              Do test 60 (or 105). */

		temp1 = 0.;
		temp2 = 0.;
		i__3 = n;
		for (j = 1; j <= i__3; ++j) {
/* Computing MAX */
		    d__3 = temp1, d__4 = (d__1 = d1[j], abs(d__1)), d__3 = 
			    max(d__3,d__4), d__4 = (d__2 = d3[j], abs(d__2));
		    temp1 = max(d__3,d__4);
/* Computing MAX */
		    d__2 = temp2, d__3 = (d__1 = d1[j] - d3[j], abs(d__1));
		    temp2 = max(d__2,d__3);
/* L1480: */
		}
/* Computing MAX */
		d__1 = unfl, d__2 = ulp * max(temp1,temp2);
		result[ntest] = temp2 / max(d__1,d__2);

L1490:

/* L1500: */
		;
	    }

/*           End of Loop -- Check for RESULT(j) > THRESH */

	    ntestt += ntest;
	    dlafts_("DST", &n, &n, &jtype, &ntest, &result[1], ioldsd, thresh,
		     nounit, &nerrs);

L1510:
	    ;
	}
/* L1520: */
    }

/*     Summary */

    alasvm_("DST", nounit, &nerrs, &ntestt, &c__0);


    return 0;

/*     End of DDRVST */

} /* ddrvst_ */

