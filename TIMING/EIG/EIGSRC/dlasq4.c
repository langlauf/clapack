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

static doublereal c_b4 = .7;

/* Subroutine */ int dlasq4_(integer *n, doublereal *q, doublereal *e, 
	doublereal *tau, doublereal *sup)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);

    /* Local variables */
    static doublereal xinf, d;
    static integer i;
    static doublereal dm;
    static integer ifl;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

       Purpose   
       =======   

       DLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This   
       routine improves the input value of SUP which is an upper bound   
       for the smallest eigenvalue for this matrix .   

       Arguments   
       =========   

    N       (input) INTEGER   
            On entry, N specifies the number of rows and columns   
            in the matrix. N must be at least 0.   

    Q       (input) DOUBLE PRECISION array, dimension (N)   
            Q array   

    E       (input) DOUBLE PRECISION array, dimension (N)   
            E array   

    TAU     (output) DOUBLE PRECISION   
            Estimate of the shift   

    SUP     (input/output) DOUBLE PRECISION   
            Upper bound for the smallest singular value   

    ===================================================================== 
  

       Parameter adjustments */
    --e;
    --q;

    /* Function Body */
    ifl = 1;
/* Computing MIN */
    d__1 = min(*sup,q[1]), d__1 = min(d__1,q[2]), d__1 = min(d__1,q[3]), d__2 
	    = q[*n], d__1 = min(d__1,d__2), d__2 = q[*n - 1], d__1 = min(d__1,
	    d__2), d__2 = q[*n - 2];
    *sup = min(d__1,d__2);
    latime_1.ops += 1;
    *tau = *sup * .9999;
    xinf = 0.;
L10:
    if (ifl == 5) {
	*tau = xinf;
	return 0;
    }
    latime_1.ops += 1;
    d = q[1] - *tau;
    dm = d;
    i__1 = *n - 2;
    for (i = 1; i <= i__1; ++i) {
	latime_1.ops += 4;
	d = d / (d + e[i]) * q[i + 1] - *tau;
	if (dm > d) {
	    dm = d;
	}
	if (d < 0.) {
	    *sup = *tau;
	    latime_1.ops += 3;
/* Computing MAX */
	    d__1 = *sup * pow_di(&c_b4, &ifl), d__2 = d + *tau;
	    *tau = max(d__1,d__2);
	    ++ifl;
	    goto L10;
	}
/* L20: */
    }
    latime_1.ops += 4;
    d = d / (d + e[*n - 1]) * q[*n] - *tau;
    if (dm > d) {
	dm = d;
    }
    if (d < 0.) {
	*sup = *tau;
	latime_1.ops += 3;
/* Computing MAX */
	d__1 = xinf, d__2 = d + *tau;
	xinf = max(d__1,d__2);
	if (*sup * pow_di(&c_b4, &ifl) <= xinf) {
	    *tau = xinf;
	} else {
	    latime_1.ops += 2;
	    *tau = *sup * pow_di(&c_b4, &ifl);
	    ++ifl;
	    goto L10;
	}
    } else {
	latime_1.ops += 1;
/* Computing MIN */
	d__1 = *sup, d__2 = dm + *tau;
	*sup = min(d__1,d__2);
    }
    return 0;

/*     End of DLASQ4 */

} /* dlasq4_ */

