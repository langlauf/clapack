/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b1 = {0.,0.};

/* Subroutine */ int zget03_(integer *n, doublecomplex *a, integer *lda, 
	doublecomplex *ainv, integer *ldainv, doublecomplex *work, integer *
	ldwork, doublereal *rwork, doublereal *rcond, doublereal *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, ainv_dim1, ainv_offset, work_dim1, work_offset, 
	    i__1, i__2, i__3;
    doublecomplex z__1;

    /* Local variables */
    static integer i;
    static doublereal anorm;
    extern /* Subroutine */ int zgemm_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *);
    extern doublereal dlamch_(char *), zlange_(char *, integer *, 
	    integer *, doublecomplex *, integer *, doublereal *);
    static doublereal ainvnm, eps;


/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    ZGET03 computes the residual for a general matrix times its inverse: 
  
       norm( I - A*AINV ) / ( N * norm(A) * norm(AINV) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    ==========   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) COMPLEX*16 array, dimension (LDA,N)   
            The original N x N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    AINV    (input) COMPLEX*16 array, dimension (LDAINV,N)   
            The inverse of the matrix A.   

    LDAINV  (input) INTEGER   
            The leading dimension of the array AINV.  LDAINV >= max(1,N). 
  

    WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,N)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.  LDWORK >= max(1,N). 
  

    RWORK   (workspace) DOUBLE PRECISION array, dimension (N)   

    RCOND   (output) DOUBLE PRECISION   
            The reciprocal of the condition number of A, computed as   
            ( 1/norm(A) ) / norm(AINV).   

    RESID   (output) DOUBLE PRECISION   
            norm(I - A*AINV) / ( N * norm(A) * norm(AINV) * EPS )   

    ===================================================================== 
  


       Quick exit if N = 0.   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    ainv_dim1 = *ldainv;
    ainv_offset = ainv_dim1 + 1;
    ainv -= ainv_offset;
    work_dim1 = *ldwork;
    work_offset = work_dim1 + 1;
    work -= work_offset;
    --rwork;

    /* Function Body */
    if (*n <= 0) {
	*rcond = 1.;
	*resid = 0.;
	return 0;
    }

/*     Exit with RESID = 1/EPS if ANORM = 0 or AINVNM = 0. */

    eps = dlamch_("Epsilon");
    anorm = zlange_("1", n, n, &a[a_offset], lda, &rwork[1]);
    ainvnm = zlange_("1", n, n, &ainv[ainv_offset], ldainv, &rwork[1]);
    if (anorm <= 0. || ainvnm <= 0.) {
	*rcond = 0.;
	*resid = 1. / eps;
	return 0;
    }
    *rcond = 1. / anorm / ainvnm;

/*     Compute I - A * AINV */

    z__1.r = -1., z__1.i = 0.;
    zgemm_("No transpose", "No transpose", n, n, n, &z__1, &a[a_offset], lda, 
	    &ainv[ainv_offset], ldainv, &c_b1, &work[work_offset], ldwork);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i + i * work_dim1;
	i__3 = i + i * work_dim1;
	z__1.r = work[i__3].r + 1., z__1.i = work[i__3].i + 0.;
	work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L10: */
    }

/*     Compute norm(I - A*AINV) / (N * norm(A) * norm(AINV) * EPS) */

    *resid = zlange_("1", n, n, &work[work_offset], ldwork, &rwork[1]);

    *resid = *resid * *rcond / eps / (doublereal) (*n);

    return 0;

/*     End of ZGET03 */

} /* zget03_ */

