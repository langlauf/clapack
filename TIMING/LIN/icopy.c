/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int icopy_(integer *n, integer *sx, integer *incx, integer *
	sy, integer *incy)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, m, ix, iy, mp1;


/*  -- LAPACK auxiliary test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    ICOPY copies an integer vector x to an integer vector y.   
    Uses unrolled loops for increments equal to 1.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The length of the vectors SX and SY.   

    SX      (input) INTEGER array, dimension (1+(N-1)*abs(INCX))   
            The vector X.   

    INCX    (input) INTEGER   
            The spacing between consecutive elements of SX.   

    SY      (output) INTEGER array, dimension (1+(N-1)*abs(INCY))   
            The vector Y.   

    INCY    (input) INTEGER   
            The spacing between consecutive elements of SY.   

    ===================================================================== 
  


       Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*     Code for unequal increments or equal increments not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	sy[iy] = sx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*     Code for both increments equal to 1   

       Clean-up loop */

L20:
    m = *n % 7;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	sy[i] = sx[i];
/* L30: */
    }
    if (*n < 7) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= i__1; i += 7) {
	sy[i] = sx[i];
	sy[i + 1] = sx[i + 1];
	sy[i + 2] = sx[i + 2];
	sy[i + 3] = sx[i + 3];
	sy[i + 4] = sx[i + 4];
	sy[i + 5] = sx[i + 5];
	sy[i + 6] = sx[i + 6];
/* L50: */
    }
    return 0;

/*     End of ICOPY */

} /* icopy_ */

