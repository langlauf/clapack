/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int cptt01_(integer *n, real *d, complex *e, real *df, 
	complex *ef, complex *work, real *resid)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1, r__2;
    doublereal d__1;
    complex q__1, q__2, q__3, q__4;

    /* Builtin functions */
    void r_cnjg(complex *, complex *);
    double c_abs(complex *);

    /* Local variables */
    static integer i;
    static real anorm;
    static complex de;
    extern doublereal slamch_(char *);
    static real eps;


/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CPTT01 reconstructs a tridiagonal matrix A from its L*D*L'   
    factorization and computes the residual   
       norm(L*D*L' - A) / ( n * norm(A) * EPS ),   
    where EPS is the machine epsilon.   

    Arguments   
    =========   

    N       (input) INTEGTER   
            The order of the matrix A.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the tridiagonal matrix A.   

    E       (input) COMPLEX array, dimension (N-1)   
            The (n-1) subdiagonal elements of the tridiagonal matrix A.   

    DF      (input) REAL array, dimension (N)   
            The n diagonal elements of the factor L from the L*D*L'   
            factorization of A.   

    EF      (input) COMPLEX array, dimension (N-1)   
            The (n-1) subdiagonal elements of the factor L from the   
            L*D*L' factorization of A.   

    WORK    (workspace) COMPLEX array, dimension (2*N)   

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

    d__1 = df[1] - d[1];
    work[1].r = d__1, work[1].i = 0.f;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	q__1.r = df[i__2] * ef[i__3].r, q__1.i = df[i__2] * ef[i__3].i;
	de.r = q__1.r, de.i = q__1.i;
	i__2 = *n + i;
	i__3 = i;
	q__1.r = de.r - e[i__3].r, q__1.i = de.i - e[i__3].i;
	work[i__2].r = q__1.r, work[i__2].i = q__1.i;
	i__2 = i + 1;
	r_cnjg(&q__4, &ef[i]);
	q__3.r = de.r * q__4.r - de.i * q__4.i, q__3.i = de.r * q__4.i + de.i 
		* q__4.r;
	i__3 = i + 1;
	q__2.r = q__3.r + df[i__3], q__2.i = q__3.i;
	i__4 = i + 1;
	q__1.r = q__2.r - d[i__4], q__1.i = q__2.i;
	work[i__2].r = q__1.r, work[i__2].i = q__1.i;
/* L10: */
    }

/*     Compute the 1-norms of the tridiagonal matrices A and WORK. */

    if (*n == 1) {
	anorm = d[1];
	*resid = c_abs(&work[1]);
    } else {
/* Computing MAX */
	r__1 = d[1] + c_abs(&e[1]), r__2 = d[*n] + c_abs(&e[*n - 1]);
	anorm = dmax(r__1,r__2);
/* Computing MAX */
	r__1 = c_abs(&work[1]) + c_abs(&work[*n + 1]), r__2 = c_abs(&work[*n])
		 + c_abs(&work[(*n << 1) - 1]);
	*resid = dmax(r__1,r__2);
	i__1 = *n - 1;
	for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
	    r__1 = anorm, r__2 = d[i] + c_abs(&e[i]) + c_abs(&e[i - 1]);
	    anorm = dmax(r__1,r__2);
/* Computing MAX */
	    r__1 = *resid, r__2 = c_abs(&work[i]) + c_abs(&work[*n + i - 1]) 
		    + c_abs(&work[*n + i]);
	    *resid = dmax(r__1,r__2);
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

/*     End of CPTT01 */

} /* cptt01_ */

