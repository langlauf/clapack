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

/* Subroutine */ int slasq2_(integer *m, real *q, real *e, real *qq, real *ee,
	 real *eps, real *tol2, real *small2, real *sup, integer *kend, 
	integer *info)
{
    /* System generated locals */
    real r__1, r__2, r__3, r__4;

    /* Builtin functions */
    double sqrt(doublereal);
    integer i_nint(real *);

    /* Local variables */
    static real xinf;
    static integer n;
    static real sigma, qemax;
    static integer iconv;
    extern /* Subroutine */ int slasq3_(integer *, real *, real *, real *, 
	    real *, real *, real *, integer *, integer *, integer *, integer *
	    , real *, real *, real *);
    static integer iphase;
    static real xx, yy;
    static integer off, isp, off1;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

       Purpose   
       =======   

       SLASQ2 computes the singular values of a real N-by-N unreduced   
       bidiagonal matrix with squared diagonal elements in Q and   
       squared off-diagonal elements in E. The singular values are   
       computed to relative accuracy TOL, barring over/underflow or   
       denormalization.   

       Arguments   
       =========   

    M       (input) INTEGER   
            The number of rows and columns in the matrix. M >= 0.   

    Q       (output) REAL array, dimension (M)   
            On normal exit, contains the squared singular values.   

    E       (workspace) REAL array, dimension (M)   

    QQ      (input/output) REAL array, dimension (M)   
            On entry, QQ contains the squared diagonal elements of the   
            bidiagonal matrix whose SVD is desired.   
            On exit, QQ is overwritten.   

    EE      (input/output) REAL array, dimension (M)   
            On entry, EE(1:N-1) contains the squared off-diagonal   
            elements of the bidiagonal matrix whose SVD is desired.   
            On exit, EE is overwritten.   

    EPS     (input) REAL   
            Machine epsilon.   

    TOL2    (input) REAL   
            Desired relative accuracy of computed eigenvalues   
            as defined in SLASQ1.   

    SMALL2  (input) REAL   
            A threshold value as defined in SLASQ1.   

    SUP     (input/output) REAL   
            Upper bound for the smallest eigenvalue.   

    KEND    (input/output) INTEGER   
            Index where minimum d occurs.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  if INFO = i, the algorithm did not converge;  i   
                  specifies how many superdiagonals did not converge.   

    ===================================================================== 
  

       Parameter adjustments */
    --ee;
    --qq;
    --e;
    --q;

    /* Function Body */
    n = *m;

/*     Set the default maximum number of iterations */

    off = 0;
    off1 = off + 1;
    sigma = 0.f;
    xinf = 0.f;
    iconv = 0;
    iphase = 2;

/*     Try deflation at the bottom   

       1x1 deflation */

L10:
    if (n <= 2) {
	goto L20;
    }
    latime_1.ops += 1;
/* Computing MAX */
    r__1 = qq[n], r__1 = max(r__1,xinf);
    if (ee[n - 1] <= dmax(r__1,*small2) * *tol2) {
	q[n] = qq[n];
	--n;
	if (*kend > n) {
	    *kend = n;
	}
/* Computing MIN */
	r__1 = qq[n], r__2 = qq[n - 1];
	*sup = dmin(r__1,r__2);
	goto L10;
    }

/*     2x2 deflation */

    latime_1.ops += 5;
/* Computing MAX */
    r__1 = max(xinf,*small2), r__2 = qq[n] / (qq[n] + ee[n - 1] + qq[n - 1]) *
	     qq[n - 1];
    if (ee[n - 2] <= dmax(r__1,r__2) * *tol2) {
/* Computing MAX */
	r__1 = qq[n], r__2 = qq[n - 1], r__1 = max(r__1,r__2), r__2 = ee[n - 
		1];
	qemax = dmax(r__1,r__2);
	if (qemax != 0.f) {
	    latime_1.ops += 15;
	    if (qemax == qq[n - 1]) {
/* Computing 2nd power */
		r__1 = (qq[n] - qq[n - 1] + ee[n - 1]) / qemax;
		xx = (qq[n] + qq[n - 1] + ee[n - 1] + qemax * sqrt(r__1 * 
			r__1 + ee[n - 1] * 4.f / qemax)) * .5f;
	    } else if (qemax == qq[n]) {
/* Computing 2nd power */
		r__1 = (qq[n - 1] - qq[n] + ee[n - 1]) / qemax;
		xx = (qq[n] + qq[n - 1] + ee[n - 1] + qemax * sqrt(r__1 * 
			r__1 + ee[n - 1] * 4.f / qemax)) * .5f;
	    } else {
/* Computing 2nd power */
		r__1 = (qq[n] - qq[n - 1] + ee[n - 1]) / qemax;
		xx = (qq[n] + qq[n - 1] + ee[n - 1] + qemax * sqrt(r__1 * 
			r__1 + qq[n - 1] * 4.f / qemax)) * .5f;
	    }
/* Computing MAX */
	    r__1 = qq[n], r__2 = qq[n - 1];
/* Computing MIN */
	    r__3 = qq[n], r__4 = qq[n - 1];
	    yy = dmax(r__1,r__2) / xx * dmin(r__3,r__4);
	} else {
	    xx = 0.f;
	    yy = 0.f;
	}
	q[n - 1] = xx;
	q[n] = yy;
	n += -2;
	if (*kend > n) {
	    *kend = n;
	}
	*sup = qq[n];
	goto L10;
    }

L20:
    if (n == 0) {

/*         The lower branch is finished */

	if (off == 0) {

/*         No upper branch; return to SLASQ1 */

	    return 0;
	} else {

/*         Going back to upper branch */

	    xinf = 0.f;
	    if (ee[off] > 0.f) {
		isp = i_nint(&ee[off]);
		iphase = 1;
	    } else {
		isp = -i_nint(&ee[off]);
		iphase = 2;
	    }
	    sigma = e[off];
	    n = off - isp + 1;
	    off1 = isp;
	    off = off1 - 1;
	    if (n <= 2) {
		goto L20;
	    }
	    if (iphase == 1) {
/* Computing MIN */
		r__1 = q[n + off], r__2 = q[n - 1 + off], r__1 = min(r__1,
			r__2), r__2 = q[n - 2 + off];
		*sup = dmin(r__1,r__2);
	    } else {
/* Computing MIN */
		r__1 = qq[n + off], r__2 = qq[n - 1 + off], r__1 = min(r__1,
			r__2), r__2 = qq[n - 2 + off];
		*sup = dmin(r__1,r__2);
	    }
	    *kend = 0;
	    iconv = -3;
	}
    } else if (n == 1) {

/*     1x1 Solver */

	if (iphase == 1) {
	    q[off1] += sigma;
	} else {
	    q[off1] = qq[off1] + sigma;
	}
	n = 0;
	goto L20;

/*     2x2 Solver */

    } else if (n == 2) {
	if (iphase == 2) {
/* Computing MAX */
	    r__1 = qq[n + off], r__2 = qq[n - 1 + off], r__1 = max(r__1,r__2),
		     r__2 = ee[n - 1 + off];
	    qemax = dmax(r__1,r__2);
	    if (qemax != 0.f) {
		latime_1.ops += 15;
		if (qemax == qq[n - 1 + off]) {
/* Computing 2nd power */
		    r__1 = (qq[n + off] - qq[n - 1 + off] + ee[n - 1 + off]) /
			     qemax;
		    xx = (qq[n + off] + qq[n - 1 + off] + ee[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + ee[off + n - 1] * 4.f /
			     qemax)) * .5f;
		} else if (qemax == qq[n + off]) {
/* Computing 2nd power */
		    r__1 = (qq[n - 1 + off] - qq[n + off] + ee[n - 1 + off]) /
			     qemax;
		    xx = (qq[n + off] + qq[n - 1 + off] + ee[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + ee[n - 1 + off] * 4.f /
			     qemax)) * .5f;
		} else {
/* Computing 2nd power */
		    r__1 = (qq[n + off] - qq[n - 1 + off] + ee[n - 1 + off]) /
			     qemax;
		    xx = (qq[n + off] + qq[n - 1 + off] + ee[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + qq[n - 1 + off] * 4.f /
			     qemax)) * .5f;
		}
/* Computing MAX */
		r__1 = qq[n + off], r__2 = qq[n - 1 + off];
/* Computing MIN */
		r__3 = qq[n + off], r__4 = qq[n - 1 + off];
		yy = dmax(r__1,r__2) / xx * dmin(r__3,r__4);
	    } else {
		xx = 0.f;
		yy = 0.f;
	    }
	} else {
/* Computing MAX */
	    r__1 = q[n + off], r__2 = q[n - 1 + off], r__1 = max(r__1,r__2), 
		    r__2 = e[n - 1 + off];
	    qemax = dmax(r__1,r__2);
	    latime_1.ops += 15;
	    if (qemax != 0.f) {
		if (qemax == q[n - 1 + off]) {
/* Computing 2nd power */
		    r__1 = (q[n + off] - q[n - 1 + off] + e[n - 1 + off]) / 
			    qemax;
		    xx = (q[n + off] + q[n - 1 + off] + e[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + e[n - 1 + off] * 4.f / 
			    qemax)) * .5f;
		} else if (qemax == q[n + off]) {
/* Computing 2nd power */
		    r__1 = (q[n - 1 + off] - q[n + off] + e[n - 1 + off]) / 
			    qemax;
		    xx = (q[n + off] + q[n - 1 + off] + e[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + e[n - 1 + off] * 4.f / 
			    qemax)) * .5f;
		} else {
/* Computing 2nd power */
		    r__1 = (q[n + off] - q[n - 1 + off] + e[n - 1 + off]) / 
			    qemax;
		    xx = (q[n + off] + q[n - 1 + off] + e[n - 1 + off] + 
			    qemax * sqrt(r__1 * r__1 + q[n - 1 + off] * 4.f / 
			    qemax)) * .5f;
		}
/* Computing MAX */
		r__1 = q[n + off], r__2 = q[n - 1 + off];
/* Computing MIN */
		r__3 = q[n + off], r__4 = q[n - 1 + off];
		yy = dmax(r__1,r__2) / xx * dmin(r__3,r__4);
	    } else {
		xx = 0.f;
		yy = 0.f;
	    }
	}
	q[n - 1 + off] = sigma + xx;
	q[n + off] = yy + sigma;
	n = 0;
	goto L20;
    }
    slasq3_(&n, &q[off1], &e[off1], &qq[off1], &ee[off1], sup, &sigma, kend, &
	    off, &iphase, &iconv, eps, tol2, small2);
    if (*sup < 0.f) {
	*info = n + off;
	return 0;
    }
    off1 = off + 1;
    goto L20;

/*     End of SLASQ2 */

} /* slasq2_ */

