/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real ops, itcnt;
} latime_;

#define latime_1 latime_

/* Table of constant values */

static real c_b4 = .7f;

/* Subroutine */ int slasq4_(integer *n, real *q, real *e, real *tau, real *
	sup)
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double pow_ri(real *, integer *);

    /* Local variables */
    static real xinf, d;
    static integer i;
    static real dm;
    static integer ifl;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

       Purpose   
       =======   

       SLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This   
       routine improves the input value of SUP which is an upper bound   
       for the smallest eigenvalue for this matrix .   

       Arguments   
       =========   

    N       (input) INTEGER   
            On entry, N specifies the number of rows and columns   
            in the matrix. N must be at least 0.   

    Q       (input) REAL array, dimension (N)   
            Q array   

    E       (input) REAL array, dimension (N)   
            E array   

    TAU     (output) REAL   
            Estimate of the shift   

    SUP     (input/output) REAL   
            Upper bound for the smallest singular value   

    ===================================================================== 
  

       Parameter adjustments */
    --e;
    --q;

    /* Function Body */
    ifl = 1;
/* Computing MIN */
    r__1 = min(*sup,q[1]), r__1 = min(r__1,q[2]), r__1 = min(r__1,q[3]), r__2 
	    = q[*n], r__1 = min(r__1,r__2), r__2 = q[*n - 1], r__1 = min(r__1,
	    r__2), r__2 = q[*n - 2];
    *sup = dmin(r__1,r__2);
    latime_1.ops += 1;
    *tau = *sup * .9999f;
    xinf = 0.f;
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
	if (d < 0.f) {
	    *sup = *tau;
	    latime_1.ops += 3;
/* Computing MAX */
	    r__1 = *sup * pow_ri(&c_b4, &ifl), r__2 = d + *tau;
	    *tau = dmax(r__1,r__2);
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
    if (d < 0.f) {
	*sup = *tau;
	latime_1.ops += 3;
/* Computing MAX */
	r__1 = xinf, r__2 = d + *tau;
	xinf = dmax(r__1,r__2);
	if (*sup * pow_ri(&c_b4, &ifl) <= xinf) {
	    *tau = xinf;
	} else {
	    latime_1.ops += 2;
	    *tau = *sup * pow_ri(&c_b4, &ifl);
	    ++ifl;
	    goto L10;
	}
    } else {
	latime_1.ops += 1;
/* Computing MIN */
	r__1 = *sup, r__2 = dm + *tau;
	*sup = dmin(r__1,r__2);
    }
    return 0;

/*     End of SLASQ4 */

} /* slasq4_ */

