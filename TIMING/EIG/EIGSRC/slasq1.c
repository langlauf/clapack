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

static doublereal c_b8 = .125;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int slasq1_(integer *n, real *d, real *e, real *work, 
	integer *info)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2, r__3, r__4;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static integer kend, ierr;
    extern /* Subroutine */ int slas2_(real *, real *, real *, real *, real *)
	    ;
    static integer i, j, m;
    static real sfmin, sigmn, sigmx;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static real small2;
    extern /* Subroutine */ int slasq2_(integer *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, integer *, integer *);
    static integer ke;
    static real dm, dx;
    static integer ny;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int xerbla_(char *, integer *), slascl_(
	    char *, integer *, integer *, real *, real *, integer *, integer *
	    , real *, integer *, integer *);
    static real thresh;
    extern /* Subroutine */ int slasrt_(char *, integer *, real *, integer *);
    static real tolmul;
    static logical restrt;
    static real scl, eps, tol, sig1, sig2, tol2;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

       Purpose   
       =======   

       SLASQ1 computes the singular values of a real N-by-N bidiagonal   
       matrix with diagonal D and off-diagonal E. The singular values are 
  
       computed to high relative accuracy, barring over/underflow or   
       denormalization. The algorithm is described in   

       "Accurate singular values and differential qd algorithms," by   
       K. V. Fernando and B. N. Parlett,   
       Numer. Math., Vol-67, No. 2, pp. 191-230,1994.   

       See also   
       "Implementation of differential qd algorithms," by   
       K. V. Fernando and B. N. Parlett, Technical Report,   
       Department of Mathematics, University of California at Berkeley,   
       1994 (Under preparation).   

       Arguments   
       =========   

    N       (input) INTEGER   
            The number of rows and columns in the matrix. N >= 0.   

    D       (input/output) REAL array, dimension (N)   
            On entry, D contains the diagonal elements of the   
            bidiagonal matrix whose SVD is desired. On normal exit,   
            D contains the singular values in decreasing order.   

    E       (input/output) REAL array, dimension (N)   
            On entry, elements E(1:N-1) contain the off-diagonal elements 
  
            of the bidiagonal matrix whose SVD is desired.   
            On exit, E is overwritten.   

    WORK    (workspace) REAL array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  if INFO = i, the algorithm did not converge;  i   
                  specifies how many superdiagonals did not converge.   

    ===================================================================== 
  

       Parameter adjustments */
    --work;
    --e;
    --d;

    /* Function Body */
    *info = 0;
    if (*n < 0) {
	*info = -2;
	i__1 = -(*info);
	xerbla_("SLASQ1", &i__1);
	return 0;
    } else if (*n == 0) {
	return 0;
    } else if (*n == 1) {
	d[1] = dabs(d[1]);
	return 0;
    } else if (*n == 2) {
	latime_1.ops += 18;
	slas2_(&d[1], &e[1], &d[2], &sigmn, &sigmx);
	d[1] = sigmx;
	d[2] = sigmn;
	return 0;
    }

/*     Estimate the largest singular value */

    sigmx = 0.f;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	r__2 = sigmx, r__3 = (r__1 = e[i], dabs(r__1));
	sigmx = dmax(r__2,r__3);
/* L10: */
    }

/*     Early return if sigmx is zero (matrix is already diagonal) */

    if (sigmx == 0.f) {
	goto L70;
    }

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = (r__1 = d[i], dabs(r__1));
/* Computing MAX */
	r__1 = sigmx, r__2 = d[i];
	sigmx = dmax(r__1,r__2);
/* L20: */
    }

/*     Get machine parameters */

    eps = slamch_("EPSILON");
    sfmin = slamch_("SAFE MINIMUM");

/*     Compute singular values to relative accuracy TOL   
       It is assumed that tol**2 does not underflow. */

    latime_1.ops = latime_1.ops + (*n << 2) + 10;
/* Computing MAX   
   Computing MIN */
    d__1 = (doublereal) eps;
    r__3 = 100.f, r__4 = pow_dd(&d__1, &c_b8);
    r__1 = 10.f, r__2 = dmin(r__3,r__4);
    tolmul = dmax(r__1,r__2);
    tol = tolmul * eps;
/* Computing 2nd power */
    r__1 = tol;
    tol2 = r__1 * r__1;

    thresh = sigmx * sqrt(sfmin) * tol;

/*     Scale matrix so the square of the largest element is   
       1 / ( 256 * SFMIN ) */

    scl = sqrt(1.f / (sfmin * 256.f));
/* Computing 2nd power */
    r__1 = tolmul;
    small2 = 1.f / (r__1 * r__1 * 256.f);
    scopy_(n, &d[1], &c__1, &work[1], &c__1);
    i__1 = *n - 1;
    scopy_(&i__1, &e[1], &c__1, &work[*n + 1], &c__1);
    slascl_("G", &c__0, &c__0, &sigmx, &scl, n, &c__1, &work[1], n, &ierr)
	    ;
    i__1 = *n - 1;
    i__2 = *n - 1;
    slascl_("G", &c__0, &c__0, &sigmx, &scl, &i__1, &c__1, &work[*n + 1], &
	    i__2, &ierr);

/*     Square D and E (the input for the qd algorithm) */

    i__1 = (*n << 1) - 1;
    for (j = 1; j <= i__1; ++j) {
/* Computing 2nd power */
	r__1 = work[j];
	work[j] = r__1 * r__1;
/* L30: */
    }

/*     Apply qd algorithm */

    m = 0;
    e[*n] = 0.f;
    dx = work[1];
    dm = dx;
    ke = 0;
    restrt = FALSE_;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	latime_1.ops += 2;
	if ((r__1 = e[i], dabs(r__1)) <= thresh || work[*n + i] <= tol2 * (dm 
		/ (real) (i - m))) {
	    ny = i - m;
	    if (ny == 1) {
		goto L50;
	    } else if (ny == 2) {
		latime_1.ops += 18;
		slas2_(&d[m + 1], &e[m + 1], &d[m + 2], &sig1, &sig2);
		d[m + 1] = sig1;
		d[m + 2] = sig2;
	    } else {
		kend = ke + 1 - m;
		slasq2_(&ny, &d[m + 1], &e[m + 1], &work[m + 1], &work[m + *n 
			+ 1], &eps, &tol2, &small2, &dm, &kend, info);

/*                 Return, INFO = number of unconverged superd
iagonals */

		if (*info != 0) {
		    *info += i;
		    return 0;
		}

/*                 Undo scaling */

		latime_1.ops += ny << 1;
		i__2 = m + ny;
		for (j = m + 1; j <= i__2; ++j) {
		    d[j] = sqrt(d[j]);
/* L40: */
		}
		slascl_("G", &c__0, &c__0, &scl, &sigmx, &ny, &c__1, &d[m + 1]
			, &ny, &ierr);
	    }
L50:
	    m = i;
	    if (i != *n) {
		dx = work[i + 1];
		dm = dx;
		ke = i;
		restrt = TRUE_;
	    }
	}
	if (i != *n && ! restrt) {
	    latime_1.ops += 3;
	    dx = work[i + 1] * (dx / (dx + work[*n + i]));
	    if (dm > dx) {
		dm = dx;
		ke = i;
	    }
	}
	restrt = FALSE_;
/* L60: */
    }
    kend = ke + 1;

/*     Sort the singular values into decreasing order */

L70:
    slasrt_("D", n, &d[1], info);
    return 0;

/*     End of SLASQ1 */

} /* slasq1_ */

