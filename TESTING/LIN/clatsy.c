/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;

/* Subroutine */ int clatsy_(char *uplo, integer *n, complex *x, integer *ldx,
	 integer *iseed)
{
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    double sqrt(doublereal), c_abs(complex *);

    /* Local variables */
    static real beta;
    static complex a, b, c;
    static integer i, j;
    static complex r;
    static real alpha;
    static integer n5;
    static real alpha3;
    extern /* Complex */ VOID clarnd_(complex *, integer *, integer *);


/*  -- LAPACK auxiliary test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    CLATSY generates a special test matrix for the complex symmetric   
    (indefinite) factorization.  The pivot blocks of the generated matrix 
  
    will be in the following order:   
       2x2 pivot block, non diagonalizable   
       1x1 pivot block   
       2x2 pivot block, diagonalizable   
       (cycle repeats)   
    A row interchange is required for each non-diagonalizable 2x2 block. 
  

    Arguments   
    =========   

    UPLO    (input) CHARACTER   
            Specifies whether the generated matrix is to be upper or   
            lower triangular.   
            = 'U':  Upper triangular   
            = 'L':  Lower triangular   

    N       (input) INTEGER   
            The dimension of the matrix to be generated.   

    X       (output) COMPLEX array, dimension (LDX,N)   
            The generated matrix, consisting of 3x3 and 2x2 diagonal   
            blocks which result in the pivot sequence given above.   
            The matrix outside of these diagonal blocks is zero.   

    LDX     (input) INTEGER   
            The leading dimension of the array X.   

    ISEED   (input/output) INTEGER array, dimension (4)   
            On entry, the seed for the random number generator.  The last 
  
            of the four integers must be odd.  (modified on exit)   

    ===================================================================== 
  


       Initialize constants   

       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    --iseed;

    /* Function Body */
    alpha = (sqrt(17.f) + 1.f) / 8.f;
    beta = alpha - .001f;
    alpha3 = alpha * alpha * alpha;

/*     UPLO = 'U':  Upper triangular storage */

    if (*(unsigned char *)uplo == 'U') {

/*        Fill the upper triangle of the matrix with zeros. */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = j;
	    for (i = 1; i <= i__2; ++i) {
		i__3 = i + j * x_dim1;
		x[i__3].r = 0.f, x[i__3].i = 0.f;
/* L10: */
	    }
/* L20: */
	}
	n5 = *n / 5;
	n5 = *n - n5 * 5 + 1;

	i__1 = n5;
	for (i = *n; i >= i__1; i += -5) {
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = alpha3 * q__2.r, q__1.i = alpha3 * q__2.i;
	    a.r = q__1.r, a.i = q__1.i;
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = q__2.r / alpha, q__1.i = q__2.i / alpha;
	    b.r = q__1.r, b.i = q__1.i;
	    q__3.r = b.r * 2.f, q__3.i = b.i * 2.f;
	    q__2.r = q__3.r * 0.f - q__3.i * 1.f, q__2.i = q__3.r * 1.f + 
		    q__3.i * 0.f;
	    q__1.r = a.r - q__2.r, q__1.i = a.i - q__2.i;
	    c.r = q__1.r, c.i = q__1.i;
	    q__1.r = c.r / beta, q__1.i = c.i / beta;
	    r.r = q__1.r, r.i = q__1.i;
	    i__2 = i + i * x_dim1;
	    x[i__2].r = a.r, x[i__2].i = a.i;
	    i__2 = i - 2 + i * x_dim1;
	    x[i__2].r = b.r, x[i__2].i = b.i;
	    i__2 = i - 2 + (i - 1) * x_dim1;
	    x[i__2].r = r.r, x[i__2].i = r.i;
	    i__2 = i - 2 + (i - 2) * x_dim1;
	    x[i__2].r = c.r, x[i__2].i = c.i;
	    i__2 = i - 1 + (i - 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    i__2 = i - 3 + (i - 3) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    i__2 = i - 4 + (i - 4) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    if (c_abs(&x[i - 3 + (i - 3) * x_dim1]) > c_abs(&x[i - 4 + (i - 4)
		     * x_dim1])) {
		i__2 = i - 4 + (i - 3) * x_dim1;
		i__3 = i - 3 + (i - 3) * x_dim1;
		q__1.r = x[i__3].r * 2.f, q__1.i = x[i__3].i * 2.f;
		x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    } else {
		i__2 = i - 4 + (i - 3) * x_dim1;
		i__3 = i - 4 + (i - 4) * x_dim1;
		q__1.r = x[i__3].r * 2.f, q__1.i = x[i__3].i * 2.f;
		x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    }
/* L30: */
	}

/*        Clean-up for N not a multiple of 5. */

	i = n5 - 1;
	if (i > 2) {
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = alpha3 * q__2.r, q__1.i = alpha3 * q__2.i;
	    a.r = q__1.r, a.i = q__1.i;
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = q__2.r / alpha, q__1.i = q__2.i / alpha;
	    b.r = q__1.r, b.i = q__1.i;
	    q__3.r = b.r * 2.f, q__3.i = b.i * 2.f;
	    q__2.r = q__3.r * 0.f - q__3.i * 1.f, q__2.i = q__3.r * 1.f + 
		    q__3.i * 0.f;
	    q__1.r = a.r - q__2.r, q__1.i = a.i - q__2.i;
	    c.r = q__1.r, c.i = q__1.i;
	    q__1.r = c.r / beta, q__1.i = c.i / beta;
	    r.r = q__1.r, r.i = q__1.i;
	    i__1 = i + i * x_dim1;
	    x[i__1].r = a.r, x[i__1].i = a.i;
	    i__1 = i - 2 + i * x_dim1;
	    x[i__1].r = b.r, x[i__1].i = b.i;
	    i__1 = i - 2 + (i - 1) * x_dim1;
	    x[i__1].r = r.r, x[i__1].i = r.i;
	    i__1 = i - 2 + (i - 2) * x_dim1;
	    x[i__1].r = c.r, x[i__1].i = c.i;
	    i__1 = i - 1 + (i - 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    i += -3;
	}
	if (i > 1) {
	    i__1 = i + i * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    i__1 = i - 1 + (i - 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    if (c_abs(&x[i + i * x_dim1]) > c_abs(&x[i - 1 + (i - 1) * x_dim1]
		    )) {
		i__1 = i - 1 + i * x_dim1;
		i__2 = i + i * x_dim1;
		q__1.r = x[i__2].r * 2.f, q__1.i = x[i__2].i * 2.f;
		x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    } else {
		i__1 = i - 1 + i * x_dim1;
		i__2 = i - 1 + (i - 1) * x_dim1;
		q__1.r = x[i__2].r * 2.f, q__1.i = x[i__2].i * 2.f;
		x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    }
	    i += -2;
	} else if (i == 1) {
	    i__1 = i + i * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    --i;
	}

/*     UPLO = 'L':  Lower triangular storage */

    } else {

/*        Fill the lower triangle of the matrix with zeros. */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i = j; i <= i__2; ++i) {
		i__3 = i + j * x_dim1;
		x[i__3].r = 0.f, x[i__3].i = 0.f;
/* L40: */
	    }
/* L50: */
	}
	n5 = *n / 5;
	n5 *= 5;

	i__1 = n5;
	for (i = 1; i <= i__1; i += 5) {
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = alpha3 * q__2.r, q__1.i = alpha3 * q__2.i;
	    a.r = q__1.r, a.i = q__1.i;
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = q__2.r / alpha, q__1.i = q__2.i / alpha;
	    b.r = q__1.r, b.i = q__1.i;
	    q__3.r = b.r * 2.f, q__3.i = b.i * 2.f;
	    q__2.r = q__3.r * 0.f - q__3.i * 1.f, q__2.i = q__3.r * 1.f + 
		    q__3.i * 0.f;
	    q__1.r = a.r - q__2.r, q__1.i = a.i - q__2.i;
	    c.r = q__1.r, c.i = q__1.i;
	    q__1.r = c.r / beta, q__1.i = c.i / beta;
	    r.r = q__1.r, r.i = q__1.i;
	    i__2 = i + i * x_dim1;
	    x[i__2].r = a.r, x[i__2].i = a.i;
	    i__2 = i + 2 + i * x_dim1;
	    x[i__2].r = b.r, x[i__2].i = b.i;
	    i__2 = i + 2 + (i + 1) * x_dim1;
	    x[i__2].r = r.r, x[i__2].i = r.i;
	    i__2 = i + 2 + (i + 2) * x_dim1;
	    x[i__2].r = c.r, x[i__2].i = c.i;
	    i__2 = i + 1 + (i + 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    i__2 = i + 3 + (i + 3) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    i__2 = i + 4 + (i + 4) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    if (c_abs(&x[i + 3 + (i + 3) * x_dim1]) > c_abs(&x[i + 4 + (i + 4)
		     * x_dim1])) {
		i__2 = i + 4 + (i + 3) * x_dim1;
		i__3 = i + 3 + (i + 3) * x_dim1;
		q__1.r = x[i__3].r * 2.f, q__1.i = x[i__3].i * 2.f;
		x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    } else {
		i__2 = i + 4 + (i + 3) * x_dim1;
		i__3 = i + 4 + (i + 4) * x_dim1;
		q__1.r = x[i__3].r * 2.f, q__1.i = x[i__3].i * 2.f;
		x[i__2].r = q__1.r, x[i__2].i = q__1.i;
	    }
/* L60: */
	}

/*        Clean-up for N not a multiple of 5. */

	i = n5 + 1;
	if (i < *n - 1) {
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = alpha3 * q__2.r, q__1.i = alpha3 * q__2.i;
	    a.r = q__1.r, a.i = q__1.i;
	    clarnd_(&q__2, &c__5, &iseed[1]);
	    q__1.r = q__2.r / alpha, q__1.i = q__2.i / alpha;
	    b.r = q__1.r, b.i = q__1.i;
	    q__3.r = b.r * 2.f, q__3.i = b.i * 2.f;
	    q__2.r = q__3.r * 0.f - q__3.i * 1.f, q__2.i = q__3.r * 1.f + 
		    q__3.i * 0.f;
	    q__1.r = a.r - q__2.r, q__1.i = a.i - q__2.i;
	    c.r = q__1.r, c.i = q__1.i;
	    q__1.r = c.r / beta, q__1.i = c.i / beta;
	    r.r = q__1.r, r.i = q__1.i;
	    i__1 = i + i * x_dim1;
	    x[i__1].r = a.r, x[i__1].i = a.i;
	    i__1 = i + 2 + i * x_dim1;
	    x[i__1].r = b.r, x[i__1].i = b.i;
	    i__1 = i + 2 + (i + 1) * x_dim1;
	    x[i__1].r = r.r, x[i__1].i = r.i;
	    i__1 = i + 2 + (i + 2) * x_dim1;
	    x[i__1].r = c.r, x[i__1].i = c.i;
	    i__1 = i + 1 + (i + 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    i += 3;
	}
	if (i < *n) {
	    i__1 = i + i * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    i__1 = i + 1 + (i + 1) * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    if (c_abs(&x[i + i * x_dim1]) > c_abs(&x[i + 1 + (i + 1) * x_dim1]
		    )) {
		i__1 = i + 1 + i * x_dim1;
		i__2 = i + i * x_dim1;
		q__1.r = x[i__2].r * 2.f, q__1.i = x[i__2].i * 2.f;
		x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    } else {
		i__1 = i + 1 + i * x_dim1;
		i__2 = i + 1 + (i + 1) * x_dim1;
		q__1.r = x[i__2].r * 2.f, q__1.i = x[i__2].i * 2.f;
		x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    }
	    i += 2;
	} else if (i == *n) {
	    i__1 = i + i * x_dim1;
	    clarnd_(&q__1, &c__2, &iseed[1]);
	    x[i__1].r = q__1.r, x[i__1].i = q__1.i;
	    ++i;
	}
    }

    return 0;

/*     End of CLATSY */

} /* clatsy_ */

