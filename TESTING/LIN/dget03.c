/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b7 = -1.;
static doublereal c_b8 = 0.;

/* Subroutine */ int dget03_(integer *n, doublereal *a, integer *lda, 
	doublereal *ainv, integer *ldainv, doublereal *work, integer *ldwork, 
	doublereal *rwork, doublereal *rcond, doublereal *resid)
{
    /* System generated locals */
    integer a_dim1, a_offset, ainv_dim1, ainv_offset, work_dim1, work_offset, 
	    i__1;

    /* Local variables */
    static integer i;
    extern /* Subroutine */ int dgemm_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    static doublereal anorm;
    extern doublereal dlamch_(char *), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    static doublereal ainvnm, eps;


/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DGET03 computes the residual for a general matrix times its inverse: 
  
       norm( I - A*AINV ) / ( N * norm(A) * norm(AINV) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    ==========   

    N       (input) INTEGER   
            The number of rows and columns of the matrix A.  N >= 0.   

    A       (input) DOUBLE PRECISION array, dimension (LDA,N)   
            The original N x N matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    AINV    (input) DOUBLE PRECISION array, dimension (LDAINV,N)   
            The inverse of the matrix A.   

    LDAINV  (input) INTEGER   
            The leading dimension of the array AINV.  LDAINV >= max(1,N). 
  

    WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,N)   

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
    anorm = dlange_("1", n, n, &a[a_offset], lda, &rwork[1]);
    ainvnm = dlange_("1", n, n, &ainv[ainv_offset], ldainv, &rwork[1]);
    if (anorm <= 0. || ainvnm <= 0.) {
	*rcond = 0.;
	*resid = 1. / eps;
	return 0;
    }
    *rcond = 1. / anorm / ainvnm;

/*     Compute I - A * AINV */

    dgemm_("No transpose", "No transpose", n, n, n, &c_b7, &a[a_offset], lda, 
	    &ainv[ainv_offset], ldainv, &c_b8, &work[work_offset], ldwork);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	work[i + i * work_dim1] += 1.;
/* L10: */
    }

/*     Compute norm(I - A*AINV) / (N * norm(A) * norm(AINV) * EPS) */

    *resid = dlange_("1", n, n, &work[work_offset], ldwork, &rwork[1]);

    *resid = *resid * *rcond / eps / (doublereal) (*n);

    return 0;

/*     End of DGET03 */

} /* dget03_ */

