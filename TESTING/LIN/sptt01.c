/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int sptt01_(integer *n, real *d, real *e, real *df, real *ef,
	 real *work, real *resid)
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2, r__3, r__4, r__5;

    /* Local variables */
    static integer i;
    static real anorm, de;
    extern doublereal slamch_(char *);
    static real eps;


/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    SPTT01 reconstructs a tridiagonal matrix A from its L*D*L'   
    factorization and computes the residual   
       norm(L*D*L' - A) / ( n * norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    N       (input) INTEGTER   
            The order of the matrix A.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the tridiagonal matrix A.   

    E       (input) REAL array, dimension (N-1)   
            The (n-1) subdiagonal elements of the tridiagonal matrix A.   

    DF      (input) REAL array, dimension (N)   
            The n diagonal elements of the factor L from the L*D*L'   
            factorization of A.   

    EF      (input) REAL array, dimension (N-1)   
            The (n-1) subdiagonal elements of the factor L from the   
            L*D*L' factorization of A.   

    WORK    (workspace) REAL array, dimension (2*N)   

    RESID   (output) REAL   
            norm(L*D*L' - A) / (n * norm(A) * EPS)   

    ===================================================================== 
  


       Quick return if possible   

       Parameter adjustments */
    --work;
    --ef;
    --df;
    --e;
    --d;

    /* Function Body */
    if (*n <= 0) {
	*resid = 0.f;
	return 0;
    }

    eps = slamch_("Epsilon");

/*     Construct the difference L*D*L' - A. */

    work[1] = df[1] - d[1];
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	de = df[i] * ef[i];
	work[*n + i] = de - e[i];
	work[i + 1] = de * ef[i] + df[i + 1] - d[i + 1];
/* L10: */
    }

/*     Compute the 1-norms of the tridiagonal matrices A and WORK. */

    if (*n == 1) {
	anorm = d[1];
	*resid = dabs(work[1]);
    } else {
/* Computing MAX */
	r__2 = d[1] + dabs(e[1]), r__3 = d[*n] + (r__1 = e[*n - 1], dabs(r__1)
		);
	anorm = dmax(r__2,r__3);
/* Computing MAX */
	r__4 = dabs(work[1]) + (r__1 = work[*n + 1], dabs(r__1)), r__5 = (
		r__2 = work[*n], dabs(r__2)) + (r__3 = work[(*n << 1) - 1], 
		dabs(r__3));
	*resid = dmax(r__4,r__5);
	i__1 = *n - 1;
	for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
	    r__3 = anorm, r__4 = d[i] + (r__1 = e[i], dabs(r__1)) + (r__2 = e[
		    i - 1], dabs(r__2));
	    anorm = dmax(r__3,r__4);
/* Computing MAX */
	    r__4 = *resid, r__5 = (r__1 = work[i], dabs(r__1)) + (r__2 = work[
		    *n + i - 1], dabs(r__2)) + (r__3 = work[*n + i], dabs(
		    r__3));
	    *resid = dmax(r__4,r__5);
/* L20: */
	}
    }

/*     Compute norm(L*D*L' - A) / (n * norm(A) * EPS) */

    if (anorm <= 0.f) {
	if (*resid != 0.f) {
	    *resid = 1.f / eps;
	}
    } else {
	*resid = *resid / (real) (*n) / anorm / eps;
    }

    return 0;

/*     End of SPTT01 */

} /* sptt01_ */

