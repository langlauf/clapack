/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int zgtt01_(integer *n, doublecomplex *dl, doublecomplex *d, 
	doublecomplex *du, doublecomplex *dlf, doublecomplex *df, 
	doublecomplex *duf, doublecomplex *du2, integer *ipiv, doublecomplex *
	work, integer *ldwork, doublereal *rwork, doublereal *resid)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2, i__3, i__4;
    doublecomplex z__1;

    /* Local variables */
    static integer i, j;
    static doublereal anorm;
    static integer lastj;
    extern /* Subroutine */ int zswap_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zaxpy_(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static doublecomplex li;
    extern doublereal dlamch_(char *);
    static integer ip;
    extern doublereal zlangt_(char *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *), zlanhs_(char *, 
	    integer *, doublecomplex *, integer *, doublereal *);
    static doublereal eps;


/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ZGTT01 reconstructs a tridiagonal matrix A from its LU factorization 
  
    and computes the residual   
       norm(L*U - A) / ( norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    N       (input) INTEGTER   
            The order of the matrix A.  N >= 0.   

    DL      (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) sub-diagonal elements of A.   

    D       (input) COMPLEX*16 array, dimension (N)   
            The diagonal elements of A.   

    DU      (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) super-diagonal elements of A.   

    DLF     (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) multipliers that define the matrix L from the   
            LU factorization of A.   

    DF      (input) COMPLEX*16 array, dimension (N)   
            The n diagonal elements of the upper triangular matrix U from 
  
            the LU factorization of A.   

    DUF     (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) elements of the first super-diagonal of U.   

    DU2     (input) COMPLEX*16 array, dimension (N-2)   
            The (n-2) elements of the second super-diagonal of U.   

    IPIV    (input) INTEGER array, dimension (N)   
            The pivot indices; for 1 <= i <= n, row i of the matrix was   
            interchanged with row IPIV(i).  IPIV(i) will always be either 
  
            i or i+1; IPIV(i) = i indicates a row interchange was not   
            required.   

    WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,N)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.  LDWORK >= max(1,N). 
  

    RWORK   (workspace) DOUBLE PRECISION array, dimension (N)   

    RESID   (output) DOUBLE PRECISION   
            The scaled residual:  norm(L*U - A) / (norm(A) * EPS)   

    ===================================================================== 
  


       Quick return if possible   

       Parameter adjustments */
    --dl;
    --d;
    --du;
    --dlf;
    --df;
    --duf;
    --du2;
    --ipiv;
    work_dim1 = *ldwork;
    work_offset = work_dim1 + 1;
    work -= work_offset;
    --rwork;

    /* Function Body */
    if (*n <= 0) {
	*resid = 0.;
	return 0;
    }

    eps = dlamch_("Epsilon");

/*     Copy the matrix U to WORK. */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    i__3 = i + j * work_dim1;
	    work[i__3].r = 0., work[i__3].i = 0.;
/* L10: */
	}
/* L20: */
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i == 1) {
	    i__2 = i + i * work_dim1;
	    i__3 = i;
	    work[i__2].r = df[i__3].r, work[i__2].i = df[i__3].i;
	    if (*n >= 2) {
		i__2 = i + (i + 1) * work_dim1;
		i__3 = i;
		work[i__2].r = duf[i__3].r, work[i__2].i = duf[i__3].i;
	    }
	    if (*n >= 3) {
		i__2 = i + (i + 2) * work_dim1;
		i__3 = i;
		work[i__2].r = du2[i__3].r, work[i__2].i = du2[i__3].i;
	    }
	} else if (i == *n) {
	    i__2 = i + i * work_dim1;
	    i__3 = i;
	    work[i__2].r = df[i__3].r, work[i__2].i = df[i__3].i;
	} else {
	    i__2 = i + i * work_dim1;
	    i__3 = i;
	    work[i__2].r = df[i__3].r, work[i__2].i = df[i__3].i;
	    i__2 = i + (i + 1) * work_dim1;
	    i__3 = i;
	    work[i__2].r = duf[i__3].r, work[i__2].i = duf[i__3].i;
	    if (i < *n - 1) {
		i__2 = i + (i + 2) * work_dim1;
		i__3 = i;
		work[i__2].r = du2[i__3].r, work[i__2].i = du2[i__3].i;
	    }
	}
/* L30: */
    }

/*     Multiply on the left by L. */

    lastj = *n;
    for (i = *n - 1; i >= 1; --i) {
	i__1 = i;
	li.r = dlf[i__1].r, li.i = dlf[i__1].i;
	i__1 = lastj - i + 1;
	zaxpy_(&i__1, &li, &work[i + i * work_dim1], ldwork, &work[i + 1 + i *
		 work_dim1], ldwork);
	ip = ipiv[i];
	if (ip == i) {
/* Computing MIN */
	    i__1 = i + 2;
	    lastj = min(i__1,*n);
	} else {
	    i__1 = lastj - i + 1;
	    zswap_(&i__1, &work[i + i * work_dim1], ldwork, &work[i + 1 + i * 
		    work_dim1], ldwork);
	}
/* L40: */
    }

/*     Subtract the matrix A. */

    i__1 = work_dim1 + 1;
    i__2 = work_dim1 + 1;
    z__1.r = work[i__2].r - d[1].r, z__1.i = work[i__2].i - d[1].i;
    work[i__1].r = z__1.r, work[i__1].i = z__1.i;
    if (*n > 1) {
	i__1 = (work_dim1 << 1) + 1;
	i__2 = (work_dim1 << 1) + 1;
	z__1.r = work[i__2].r - du[1].r, z__1.i = work[i__2].i - du[1].i;
	work[i__1].r = z__1.r, work[i__1].i = z__1.i;
	i__1 = *n + (*n - 1) * work_dim1;
	i__2 = *n + (*n - 1) * work_dim1;
	i__3 = *n - 1;
	z__1.r = work[i__2].r - dl[i__3].r, z__1.i = work[i__2].i - dl[i__3]
		.i;
	work[i__1].r = z__1.r, work[i__1].i = z__1.i;
	i__1 = *n + *n * work_dim1;
	i__2 = *n + *n * work_dim1;
	i__3 = *n;
	z__1.r = work[i__2].r - d[i__3].r, z__1.i = work[i__2].i - d[i__3].i;
	work[i__1].r = z__1.r, work[i__1].i = z__1.i;
	i__1 = *n - 1;
	for (i = 2; i <= i__1; ++i) {
	    i__2 = i + (i - 1) * work_dim1;
	    i__3 = i + (i - 1) * work_dim1;
	    i__4 = i - 1;
	    z__1.r = work[i__3].r - dl[i__4].r, z__1.i = work[i__3].i - dl[
		    i__4].i;
	    work[i__2].r = z__1.r, work[i__2].i = z__1.i;
	    i__2 = i + i * work_dim1;
	    i__3 = i + i * work_dim1;
	    i__4 = i;
	    z__1.r = work[i__3].r - d[i__4].r, z__1.i = work[i__3].i - d[i__4]
		    .i;
	    work[i__2].r = z__1.r, work[i__2].i = z__1.i;
	    i__2 = i + (i + 1) * work_dim1;
	    i__3 = i + (i + 1) * work_dim1;
	    i__4 = i;
	    z__1.r = work[i__3].r - du[i__4].r, z__1.i = work[i__3].i - du[
		    i__4].i;
	    work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L50: */
	}
    }

/*     Compute the 1-norm of the tridiagonal matrix A. */

    anorm = zlangt_("1", n, &dl[1], &d[1], &du[1]);

/*     Compute the 1-norm of WORK, which is only guaranteed to be   
       upper Hessenberg. */

    *resid = zlanhs_("1", n, &work[work_offset], ldwork, &rwork[1]);

/*     Compute norm(L*U - A) / (norm(A) * EPS) */

    if (anorm <= 0.) {
	if (*resid != 0.) {
	    *resid = 1. / eps;
	}
    } else {
	*resid = *resid / anorm / eps;
    }

    return 0;

/*     End of ZGTT01 */

} /* zgtt01_ */

