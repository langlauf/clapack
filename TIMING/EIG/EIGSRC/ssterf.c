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

static integer c__0 = 0;
static integer c__1 = 1;
static real c_b32 = 1.f;

/* Subroutine */ int ssterf_(integer *n, real *d, real *e, integer *info)
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real oldc;
    static integer lend, jtot;
    extern /* Subroutine */ int slae2_(real *, real *, real *, real *, real *)
	    ;
    static real c;
    static integer i, l, m;
    static real p, gamma, r, s, alpha, sigma, anorm;
    static integer l1, lendm1, lendp1;
    static real bb;
    extern doublereal slapy2_(real *, real *);
    static integer iscale;
    static real oldgam;
    extern doublereal slamch_(char *);
    static real safmin;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static real safmax;
    extern /* Subroutine */ int slascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, real *, integer *, integer *);
    static integer lendsv;
    static real ssfmin;
    static integer nmaxit;
    static real ssfmax;
    extern doublereal slanst_(char *, integer *, real *, real *);
    extern /* Subroutine */ int slasrt_(char *, integer *, real *, integer *);
    static integer lm1, mm1, nm1;
    static real rt1, rt2, eps, rte;
    static integer lsv;
    static real tst, eps2;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

    Purpose   
    =======   

    SSTERF computes all eigenvalues of a symmetric tridiagonal matrix   
    using the Pal-Walker-Kahan variant of the QL or QR algorithm.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix.  N >= 0.   

    D       (input/output) REAL array, dimension (N)   
            On entry, the n diagonal elements of the tridiagonal matrix. 
  
            On exit, if INFO = 0, the eigenvalues in ascending order.   

    E       (input/output) REAL array, dimension (N-1)   
            On entry, the (n-1) subdiagonal elements of the tridiagonal   
            matrix.   
            On exit, E has been destroyed.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  the algorithm failed to find all of the eigenvalues in 
  
                  a total of 30*N iterations; if INFO = i, then i   
                  elements of E have not converged to zero.   

    ===================================================================== 
  


       Test the input parameters.   

       Parameter adjustments */
    --e;
    --d;

    /* Function Body */
    *info = 0;

/*     Quick return if possible */

    latime_1.itcnt = 0.f;
    if (*n < 0) {
	*info = -1;
	i__1 = -(*info);
	xerbla_("SSTERF", &i__1);
	return 0;
    }
    if (*n <= 1) {
	return 0;
    }

/*     Determine the unit roundoff for this environment. */

    latime_1.ops += 6;
    eps = slamch_("E");
/* Computing 2nd power */
    r__1 = eps;
    eps2 = r__1 * r__1;
    safmin = slamch_("S");
    safmax = 1.f / safmin;
    ssfmax = sqrt(safmax) / 3.f;
    ssfmin = sqrt(safmin) / eps2;

/*     Compute the eigenvalues of the tridiagonal matrix. */

    nmaxit = *n * 30;
    sigma = 0.f;
    jtot = 0;

/*     Determine where the matrix splits and choose QL or QR iteration   
       for each block, according to whether top or bottom diagonal   
       element is smaller. */

    l1 = 1;
    nm1 = *n - 1;

L10:
    if (l1 > *n) {
	goto L170;
    }
    if (l1 > 1) {
	e[l1 - 1] = 0.f;
    }
    if (l1 <= nm1) {
	i__1 = nm1;
	for (m = l1; m <= i__1; ++m) {
	    tst = (r__1 = e[m], dabs(r__1));
	    if (tst == 0.f) {
		goto L30;
	    }
	    latime_1.ops += 4;
	    if (tst <= sqrt((r__1 = d[m], dabs(r__1))) * sqrt((r__2 = d[m + 1]
		    , dabs(r__2))) * eps) {
		e[m] = 0.f;
		goto L30;
	    }
/* L20: */
	}
    }
    m = *n;

L30:
    l = l1;
    lsv = l;
    lend = m;
    lendsv = lend;
    l1 = m + 1;
    if (lend == l) {
	goto L10;
    }

/*     Scale submatrix in rows and columns L to LEND */

    latime_1.ops += lend - l + 1 << 1;
    i__1 = lend - l + 1;
    anorm = slanst_("I", &i__1, &d[l], &e[l]);
    iscale = 0;
    if (anorm > ssfmax) {
	iscale = 1;
	latime_1.ops = latime_1.ops + (lend - l << 1) + 1;
	i__1 = lend - l + 1;
	slascl_("G", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &d[l], n, 
		info);
	i__1 = lend - l;
	slascl_("G", &c__0, &c__0, &anorm, &ssfmax, &i__1, &c__1, &e[l], n, 
		info);
    } else if (anorm < ssfmin) {
	iscale = 2;
	latime_1.ops = latime_1.ops + (lend - l << 1) + 1;
	i__1 = lend - l + 1;
	slascl_("G", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &d[l], n, 
		info);
	i__1 = lend - l;
	slascl_("G", &c__0, &c__0, &anorm, &ssfmin, &i__1, &c__1, &e[l], n, 
		info);
    }

    latime_1.ops += lend - l << 1;
    i__1 = lend - 1;
    for (i = l; i <= i__1; ++i) {
/* Computing 2nd power */
	r__1 = e[i];
	e[i] = r__1 * r__1;
/* L40: */
    }

/*     Choose between QL and QR iteration */

    if ((r__1 = d[lend], dabs(r__1)) < (r__2 = d[l], dabs(r__2))) {
	lend = lsv;
	l = lendsv;
    }

    if (lend >= l) {

/*        QL Iteration   

          Look for small subdiagonal element. */

L50:
	if (l != lend) {
	    lendm1 = lend - 1;
	    i__1 = lendm1;
	    for (m = l; m <= i__1; ++m) {
		tst = (r__1 = e[m], dabs(r__1));
		latime_1.ops += 3;
		if (tst <= eps2 * (r__1 = d[m] * d[m + 1], dabs(r__1))) {
		    goto L70;
		}
/* L60: */
	    }
	}

	m = lend;

L70:
	if (m < lend) {
	    e[m] = 0.f;
	}
	p = d[l];
	if (m == l) {
	    goto L90;
	}

/*        If remaining matrix is 2 by 2, use SLAE2 to compute its   
          eigenvalues. */

	if (m == l + 1) {
	    latime_1.ops += 16;
	    rte = sqrt(e[l]);
	    slae2_(&d[l], &rte, &d[l + 1], &rt1, &rt2);
	    d[l] = rt1;
	    d[l + 1] = rt2;
	    e[l] = 0.f;
	    l += 2;
	    if (l <= lend) {
		goto L50;
	    }
	    goto L150;
	}

	if (jtot == nmaxit) {
	    goto L150;
	}
	++jtot;

/*        Form shift. */

	latime_1.ops += 14;
	rte = sqrt(e[l]);
	sigma = (d[l + 1] - p) / (rte * 2.f);
	r = slapy2_(&sigma, &c_b32);
	sigma = p - rte / (sigma + r_sign(&r, &sigma));

	c = 1.f;
	s = 0.f;
	gamma = d[m] - sigma;
	p = gamma * gamma;

/*        Inner loop */

	mm1 = m - 1;
	latime_1.ops += (m - l) * 12;
	i__1 = l;
	for (i = mm1; i >= i__1; --i) {
	    bb = e[i];
	    r = p + bb;
	    if (i != mm1) {
		e[i + 1] = s * r;
	    }
	    oldc = c;
	    c = p / r;
	    s = bb / r;
	    oldgam = gamma;
	    alpha = d[i];
	    gamma = c * (alpha - sigma) - s * oldgam;
	    d[i + 1] = oldgam + (alpha - gamma);
	    if (c != 0.f) {
		p = gamma * gamma / c;
	    } else {
		p = oldc * bb;
	    }
/* L80: */
	}

	latime_1.ops += 2;
	e[l] = s * p;
	d[l] = sigma + gamma;
	goto L50;

/*        Eigenvalue found. */

L90:
	d[l] = p;

	++l;
	if (l <= lend) {
	    goto L50;
	}
	goto L150;

    } else {

/*        QR Iteration   

          Look for small superdiagonal element. */

L100:
	if (l != lend) {
	    lendp1 = lend + 1;
	    i__1 = lendp1;
	    for (m = l; m >= i__1; --m) {
		tst = (r__1 = e[m - 1], dabs(r__1));
		latime_1.ops += 3;
		if (tst <= eps2 * (r__1 = d[m] * d[m - 1], dabs(r__1))) {
		    goto L120;
		}
/* L110: */
	    }
	}

	m = lend;

L120:
	if (m > lend) {
	    e[m - 1] = 0.f;
	}
	p = d[l];
	if (m == l) {
	    goto L140;
	}

/*        If remaining matrix is 2 by 2, use SLAE2 to compute its   
          eigenvalues. */

	if (m == l - 1) {
	    latime_1.ops += 16;
	    rte = sqrt(e[l - 1]);
	    slae2_(&d[l], &rte, &d[l - 1], &rt1, &rt2);
	    d[l] = rt1;
	    d[l - 1] = rt2;
	    e[l - 1] = 0.f;
	    l += -2;
	    if (l >= lend) {
		goto L100;
	    }
	    goto L150;
	}

	if (jtot == nmaxit) {
	    goto L150;
	}
	++jtot;

/*        Form shift. */

	latime_1.ops += 14;
	rte = sqrt(e[l - 1]);
	sigma = (d[l - 1] - p) / (rte * 2.f);
	r = slapy2_(&sigma, &c_b32);
	sigma = p - rte / (sigma + r_sign(&r, &sigma));

	c = 1.f;
	s = 0.f;
	gamma = d[m] - sigma;
	p = gamma * gamma;

/*        Inner loop */

	lm1 = l - 1;
	latime_1.ops += (lm1 - m + 1) * 12;
	i__1 = lm1;
	for (i = m; i <= i__1; ++i) {
	    bb = e[i];
	    r = p + bb;
	    if (i != m) {
		e[i - 1] = s * r;
	    }
	    oldc = c;
	    c = p / r;
	    s = bb / r;
	    oldgam = gamma;
	    alpha = d[i + 1];
	    gamma = c * (alpha - sigma) - s * oldgam;
	    d[i] = oldgam + (alpha - gamma);
	    if (c != 0.f) {
		p = gamma * gamma / c;
	    } else {
		p = oldc * bb;
	    }
/* L130: */
	}

	latime_1.ops += 2;
	e[lm1] = s * p;
	d[l] = sigma + gamma;
	goto L100;

/*        Eigenvalue found. */

L140:
	d[l] = p;

	--l;
	if (l >= lend) {
	    goto L100;
	}
	goto L150;

    }

/*     Undo scaling if necessary */

L150:
    if (iscale == 1) {
	latime_1.ops = latime_1.ops + lendsv - lsv + 1;
	i__1 = lendsv - lsv + 1;
	slascl_("G", &c__0, &c__0, &ssfmax, &anorm, &i__1, &c__1, &d[lsv], n, 
		info);
    }
    if (iscale == 2) {
	latime_1.ops = latime_1.ops + lendsv - lsv + 1;
	i__1 = lendsv - lsv + 1;
	slascl_("G", &c__0, &c__0, &ssfmin, &anorm, &i__1, &c__1, &d[lsv], n, 
		info);
    }

/*     Check for no convergence to an eigenvalue after a total   
       of N*MAXIT iterations. */

    if (jtot == nmaxit) {
	i__1 = *n - 1;
	for (i = 1; i <= i__1; ++i) {
	    if (e[i] != 0.f) {
		++(*info);
	    }
/* L160: */
	}
	return 0;
    }
    goto L10;

/*     Sort eigenvalues in increasing order. */

L170:
    slasrt_("I", n, &d[1], info);

    return 0;

/*     End of SSTERF */

} /* ssterf_ */

