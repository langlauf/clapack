/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};
static doublecomplex c_b2 = {1.,0.};

/* Subroutine */ int zlaqzh_(logical *ilq, logical *ilz, integer *n, integer *
	ilo, integer *ihi, doublecomplex *a, integer *lda, doublecomplex *b, 
	integer *ldb, doublecomplex *q, integer *ldq, doublecomplex *z, 
	integer *ldz, doublecomplex *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1, 
	    z_offset, i__1, i__2;

    /* Local variables */
    static integer iinfo, icols;
    static char compq[1], compz[1];
    static integer irows;
    extern /* Subroutine */ int zgghrd_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, integer *
	    ), zgeqrf_(integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, integer *)
	    , zlacpy_(char *, integer *, integer *, doublecomplex *, integer *
	    , doublecomplex *, integer *), zlaset_(char *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, doublecomplex *, 
	    integer *), zungqr_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *), zunmqr_(char *, char *, integer *, integer 
	    *, integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *);


/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    This calls the LAPACK routines to perform the function of   
    QZHES.  It is similar in function to ZGGHRD, except that   
    B is not assumed to be upper-triangular.   

    It reduces a pair of matrices (A,B) to a Hessenberg-triangular   
    pair (H,T).  More specifically, it computes unitary matrices   
    Q and Z, an (upper) Hessenberg matrix H, and an upper triangular   
    matrix T such that:   

      A = Q H Z*    and   B = Q T Z*   

    where * means conjugate transpose.   

    Arguments   
    =========   

    ILQ     (input) LOGICAL   
            = .FALSE. do not compute Q.   
            = .TRUE.  compute Q.   

    ILZ     (input) LOGICAL   
            = .FALSE. do not compute Z.   
            = .TRUE.  compute Z.   

    N       (input) INTEGER   
            The number of rows and columns in the matrices A, B, Q, and   
            Z.  N must be at least 0.   

    ILO     (input) INTEGER   
            Columns 1 through ILO-1 of A and B are assumed to be in   
            upper triangular form already, and will not be modified.   
            ILO must be at least 1.   

    IHI     (input) INTEGER   
            Rows IHI+1 through N of A and B are assumed to be in upper   
            triangular form already, and will not be touched.  IHI may   
            not be greater than N.   

    A       (input/output) COMPLEX*16 array, dimension (LDA, N)   
            On entry, the first of the pair of N x N general matrices to 
  
            be reduced.   
            On exit, the upper triangle and the first subdiagonal of A   
            are overwritten with the Hessenberg matrix H, and the rest   
            is set to zero.   

    LDA     (input) INTEGER   
            The leading dimension of A as declared in the calling   
            program. LDA must be at least max ( 1, N ) .   

    B       (input/output) COMPLEX*16 array, dimension (LDB, N)   
            On entry, the second of the pair of N x N general matrices to 
  
            be reduced.   
            On exit, the transformed matrix T = Q* B Z, which is upper   
            triangular.   

    LDB     (input) INTEGER   
            The leading dimension of B as declared in the calling   
            program. LDB must be at least max ( 1, N ) .   

    Q       (output) COMPLEX*16 array, dimension (LDQ,N)   
            If ILQ = .TRUE., Q will contain the unitary matrix Q.   
            (See "Purpose", above.)   
            Will not be referenced if ILQ = .FALSE.   

    LDQ     (input) INTEGER   
            The leading dimension of the matrix Q. LDQ must be at   
            least 1 and at least N.   

    Z       (output) COMPLEX*16 array, dimension (LDZ,N)   
            If ILZ = .TRUE., Z will contain the unitary matrix Z.   
            (See "Purpose", above.)   
            May be referenced even if ILZ = .FALSE.   

    LDZ     (input) INTEGER   
            The leading dimension of the matrix Z. LDZ must be at   
            least 1 and at least N.   

    WORK    (workspace) COMPLEX*16 array, dimension (N)   
            Workspace.   

    INFO    (output) INTEGER   
            = 0:  successful exit.   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   
            > 0:  errors that usually indicate LAPACK problems:   
                  = 2: error return from ZGEQRF;   
                  = 3: error return from ZUNMQR;   
                  = 4: error return from ZUNGQR;   
                  = 5: error return from ZGGHRD.   

    ===================================================================== 
  


       Quick return if possible   

       Parameter adjustments */
    --work;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    q_dim1 = *ldq;
    q_offset = q_dim1 + 1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = z_dim1 + 1;
    z -= z_offset;

    /* Function Body */
    if (*n == 0) {
	return 0;
    }

/*     Reduce B to triangular form, and initialize Q and/or Z */

    irows = *ihi + 1 - *ilo;
    icols = *n + 1 - *ilo;
    i__1 = *n * *ldz;
    zgeqrf_(&irows, &icols, &b[*ilo + *ilo * b_dim1], ldb, &work[1], &z[
	    z_offset], &i__1, &iinfo);
    if (iinfo != 0) {
	*info = 2;
	goto L10;
    }

    i__1 = *n * *ldz;
    zunmqr_("L", "C", &irows, &icols, &irows, &b[*ilo + *ilo * b_dim1], ldb, &
	    work[1], &a[*ilo + *ilo * a_dim1], lda, &z[z_offset], &i__1, &
	    iinfo);
    if (iinfo != 0) {
	*info = 3;
	goto L10;
    }

    if (*ilq) {
	zlaset_("Full", n, n, &c_b1, &c_b2, &q[q_offset], ldq);
	i__1 = irows - 1;
	i__2 = irows - 1;
	zlacpy_("L", &i__1, &i__2, &b[*ilo + 1 + *ilo * b_dim1], ldb, &q[*ilo 
		+ 1 + *ilo * q_dim1], ldq);
	i__1 = *n * *ldz;
	zungqr_(&irows, &irows, &irows, &q[*ilo + *ilo * q_dim1], ldq, &work[
		1], &z[z_offset], &i__1, &iinfo);
	if (iinfo != 0) {
	    *info = 4;
	    goto L10;
	}
    }

/*     Reduce to generalized Hessenberg form */

    if (*ilq) {
	*(unsigned char *)compq = 'V';
    } else {
	*(unsigned char *)compq = 'N';
    }

    if (*ilz) {
	*(unsigned char *)compz = 'I';
    } else {
	*(unsigned char *)compz = 'N';
    }

    zgghrd_(compq, compz, n, ilo, ihi, &a[a_offset], lda, &b[b_offset], ldb, &
	    q[q_offset], ldq, &z[z_offset], ldz, &iinfo);
    if (iinfo != 0) {
	*info = 5;
	goto L10;
    }

/*     End */

L10:

    return 0;

/*     End of ZLAQZH */

} /* zlaqzh_ */

