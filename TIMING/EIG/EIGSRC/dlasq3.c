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

static integer c__1 = 1;
static integer c_n1 = -1;

/* Subroutine */ int dlasq3_(integer *n, doublereal *q, doublereal *e, 
	doublereal *qq, doublereal *ee, doublereal *sup, doublereal *sigma, 
	integer *kend, integer *off, integer *iphase, integer *iconv, 
	doublereal *eps, doublereal *tol2, doublereal *small2)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static logical ldef;
    static integer icnt;
    static doublereal tolx, toly, tolz;
    static integer k1end, k2end;
    static doublereal d;
    static integer i;
    static doublereal qemax;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer maxit, n1, n2;
    static doublereal t1;
    extern /* Subroutine */ int dlasq4_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    static integer ic, ke;
    static doublereal dm;
    static integer ip, ks;
    static doublereal xx, yy;
    static logical lsplit;
    static integer ifl;
    static doublereal tau;
    static integer isp;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

       Purpose   
       =======   

       DLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.   
       This can be described as the differential qd with shifts.   

       Arguments   
       =========   

    N       (input/output) INTEGER   
            On entry, N specifies the number of rows and columns   
            in the matrix. N must be at least 3.   
            On exit N is non-negative and less than the input value.   

    Q       (input/output) DOUBLE PRECISION array, dimension (N)   
            Q array in ping (see IPHASE below)   

    E       (input/output) DOUBLE PRECISION array, dimension (N)   
            E array in ping (see IPHASE below)   

    QQ      (input/output) DOUBLE PRECISION array, dimension (N)   
            Q array in pong (see IPHASE below)   

    EE      (input/output) DOUBLE PRECISION array, dimension (N)   
            E array in pong (see IPHASE below)   

    SUP     (input/output) DOUBLE PRECISION   
            Upper bound for the smallest eigenvalue   

    SIGMA   (input/output) DOUBLE PRECISION   
            Accumulated shift for the present submatrix   

    KEND    (input/output) INTEGER   
            Index where minimum D(i) occurs in recurrence for   
            splitting criterion   

    OFF     (input/output) INTEGER   
            Offset for arrays   

    IPHASE  (input/output) INTEGER   
            If IPHASE = 1 (ping) then data is in Q and E arrays   
            If IPHASE = 2 (pong) then data is in QQ and EE arrays   

    ICONV   (input) INTEGER   
            If ICONV = 0 a bottom part of a matrix (with a split)   
            If ICONV =-3 a top part of a matrix (with a split)   

    EPS     (input) DOUBLE PRECISION   
            Machine epsilon   

    TOL2    (input) DOUBLE PRECISION   
            Square of the relative tolerance TOL as defined in DLASQ1   

    SMALL2  (input) DOUBLE PRECISION   
            A threshold value as defined in DLASQ1   

    ===================================================================== 
  

       Parameter adjustments */
    --ee;
    --qq;
    --e;
    --q;

    /* Function Body */
    icnt = 0;
    tau = 0.;
    dm = *sup;
    latime_1.ops += 2;
    tolx = *sigma * *tol2;
    tolz = max(*small2,*sigma) * *tol2;

/*     Set maximum number of iterations */

    maxit = *n * 100;

/*     Flipping */

    ic = 2;
    if (*n > 3) {
	if (*iphase == 1) {
	    i__1 = *n - 2;
	    for (i = 1; i <= i__1; ++i) {
		if (q[i] > q[i + 1]) {
		    ++ic;
		}
		if (e[i] > e[i + 1]) {
		    ++ic;
		}
/* L10: */
	    }
	    if (q[*n - 1] > q[*n]) {
		++ic;
	    }
	    if (ic < *n) {
		dcopy_(n, &q[1], &c__1, &qq[1], &c_n1);
		i__1 = *n - 1;
		dcopy_(&i__1, &e[1], &c__1, &ee[1], &c_n1);
		if (*kend != 0) {
		    *kend = *n - *kend + 1;
		}
		*iphase = 2;
	    }
	} else {
	    i__1 = *n - 2;
	    for (i = 1; i <= i__1; ++i) {
		if (qq[i] > qq[i + 1]) {
		    ++ic;
		}
		if (ee[i] > ee[i + 1]) {
		    ++ic;
		}
/* L20: */
	    }
	    if (qq[*n - 1] > qq[*n]) {
		++ic;
	    }
	    if (ic < *n) {
		dcopy_(n, &qq[1], &c__1, &q[1], &c_n1);
		i__1 = *n - 1;
		dcopy_(&i__1, &ee[1], &c__1, &e[1], &c_n1);
		if (*kend != 0) {
		    *kend = *n - *kend + 1;
		}
		*iphase = 1;
	    }
	}
    }
    if (*iconv == -3) {
	if (*iphase == 1) {
	    goto L220;
	} else {
	    goto L100;
	}
    }
    if (*iphase == 2) {
	goto L150;
    }

/*     The ping section of the code */

L30:
    ifl = 0;

/*     Compute the shift */

    if (*kend == 0 || *sup == 0.) {
	tau = 0.;
    } else if (icnt > 0 && dm <= tolz) {
	tau = 0.;
    } else {
/* Computing MAX */
	i__1 = 5, i__2 = *n / 32;
	ip = max(i__1,i__2);
	n2 = (ip << 1) + 1;
	if (n2 >= *n) {
	    n1 = 1;
	    n2 = *n;
	} else if (*kend + ip > *n) {
	    n1 = *n - (ip << 1);
	} else if (*kend - ip < 1) {
	    n1 = 1;
	} else {
	    n1 = *kend - ip;
	}
	dlasq4_(&n2, &q[n1], &e[n1], &tau, sup);
    }
L40:
    ++icnt;
    if (icnt > maxit) {
	*sup = -1.;
	return 0;
    }
    if (tau == 0.) {

/*     dqd algorithm */

	d = q[1];
	dm = d;
	ke = 0;
	latime_1.ops = latime_1.ops + *n * 3 - 3;
	i__1 = *n - 3;
	for (i = 1; i <= i__1; ++i) {
	    qq[i] = d + e[i];
	    d = d / qq[i] * q[i + 1];
	    if (dm > d) {
		dm = d;
		ke = i;
	    }
/* L50: */
	}
	++ke;

/*     Penultimate dqd step (in ping) */

	k2end = ke;
	qq[*n - 2] = d + e[*n - 2];
	d = d / qq[*n - 2] * q[*n - 1];
	if (dm > d) {
	    dm = d;
	    ke = *n - 1;
	}

/*     Final dqd step (in ping) */

	k1end = ke;
	qq[*n - 1] = d + e[*n - 1];
	d = d / qq[*n - 1] * q[*n];
	if (dm > d) {
	    dm = d;
	    ke = *n;
	}
	qq[*n] = d;
    } else {

/*     The dqds algorithm (in ping) */

	d = q[1] - tau;
	dm = d;
	ke = 0;
	if (d < 0.) {
	    goto L140;
	}
	latime_1.ops += *n - 3 << 2;
	i__1 = *n - 3;
	for (i = 1; i <= i__1; ++i) {
	    qq[i] = d + e[i];
	    d = d / qq[i] * q[i + 1] - tau;
	    if (dm > d) {
		dm = d;
		ke = i;
		if (d < 0.) {
		    goto L70;
		}
	    }
/* L60: */
	}
	++ke;
	goto L80;
L70:
	latime_1.ops -= *n - 3 - ke << 2;
	goto L140;
L80:

/*     Penultimate dqds step (in ping) */

	latime_1.ops += 4;
	k2end = ke;
	qq[*n - 2] = d + e[*n - 2];
	d = d / qq[*n - 2] * q[*n - 1] - tau;
	if (dm > d) {
	    dm = d;
	    ke = *n - 1;
	    if (d < 0.) {
		goto L140;
	    }
	}

/*     Final dqds step (in ping) */

	latime_1.ops += 4;
	k1end = ke;
	qq[*n - 1] = d + e[*n - 1];
	d = d / qq[*n - 1] * q[*n] - tau;
	if (dm > d) {
	    dm = d;
	    ke = *n;
	}
	qq[*n] = d;
    }

/*        Convergence when QQ(N) is small (in ping) */

    latime_1.ops += 1;
    if ((d__1 = qq[*n], abs(d__1)) <= *sigma * *tol2) {
	qq[*n] = 0.;
	dm = 0.;
	ke = *n;
    }
    if (qq[*n] < 0.) {
	goto L140;
    }

/*     Non-negative qd array: Update the e's */

    latime_1.ops += *n - 1 << 1;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	ee[i] = e[i] / qq[i] * q[i + 1];
/* L90: */
    }

/*     Updating sigma and iphase in ping */

    latime_1.ops += 1;
    *sigma += tau;
    *iphase = 2;
L100:
    tolx = *sigma * *tol2;
    toly = *sigma * *eps;
    tolz = max(*sigma,*small2) * *tol2;
    latime_1.ops += 3;

/*     Checking for deflation and convergence (in ping) */

L110:
    if (*n <= 2) {
	return 0;
    }

/*        Deflation: bottom 1x1 (in ping) */

    ldef = FALSE_;
    if (ee[*n - 1] <= tolz) {
	ldef = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 2;
	if (ee[*n - 1] <= *eps * (*sigma + qq[*n])) {
	    latime_1.ops += 5;
	    if (ee[*n - 1] * (qq[*n] / (qq[*n] + *sigma)) <= *tol2 * (qq[*n] 
		    + *sigma)) {
		ldef = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 1;
	if (ee[*n - 1] <= qq[*n] * *tol2) {
	    ldef = TRUE_;
	}
    }
    if (ldef) {
	latime_1.ops += 1;
	q[*n] = qq[*n] + *sigma;
	--(*n);
	++(*iconv);
	goto L110;
    }

/*        Deflation: bottom 2x2 (in ping) */

    ldef = FALSE_;
    if (ee[*n - 2] <= tolz) {
	ldef = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 7;
	t1 = *sigma + ee[*n - 1] * (*sigma / (*sigma + qq[*n]));
	if (ee[*n - 2] * (t1 / (qq[*n - 1] + t1)) <= toly) {
	    latime_1.ops += 3;
	    if (ee[*n - 2] * (qq[*n - 1] / (qq[*n - 1] + t1)) <= tolx) {
		ldef = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 5;
	if (ee[*n - 2] <= qq[*n] / (qq[*n] + ee[*n - 1] + qq[*n - 1]) * qq[*n 
		- 1] * *tol2) {
	    ldef = TRUE_;
	}
    }
    if (ldef) {
/* Computing MAX */
	d__1 = qq[*n], d__2 = qq[*n - 1], d__1 = max(d__1,d__2), d__2 = ee[*n 
		- 1];
	qemax = max(d__1,d__2);
	if (qemax != 0.) {
	    latime_1.ops += 15;
	    if (qemax == qq[*n - 1]) {
/* Computing 2nd power */
		d__1 = (qq[*n] - qq[*n - 1] + ee[*n - 1]) / qemax;
		xx = (qq[*n] + qq[*n - 1] + ee[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + ee[*n - 1] * 4. / qemax)) * .5;
	    } else if (qemax == qq[*n]) {
/* Computing 2nd power */
		d__1 = (qq[*n - 1] - qq[*n] + ee[*n - 1]) / qemax;
		xx = (qq[*n] + qq[*n - 1] + ee[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + ee[*n - 1] * 4. / qemax)) * .5;
	    } else {
/* Computing 2nd power */
		d__1 = (qq[*n] - qq[*n - 1] + ee[*n - 1]) / qemax;
		xx = (qq[*n] + qq[*n - 1] + ee[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + qq[*n - 1] * 4. / qemax)) * .5;
	    }
/* Computing MAX */
	    d__1 = qq[*n], d__2 = qq[*n - 1];
/* Computing MIN */
	    d__3 = qq[*n], d__4 = qq[*n - 1];
	    yy = max(d__1,d__2) / xx * min(d__3,d__4);
	} else {
	    xx = 0.;
	    yy = 0.;
	}
	latime_1.ops += 2;
	q[*n - 1] = *sigma + xx;
	q[*n] = yy + *sigma;
	*n += -2;
	*iconv += 2;
	goto L110;
    }

/*     Updating bounds before going to pong */

    if (*iconv == 0) {
	*kend = ke;
/* Computing MIN */
	d__1 = dm, d__2 = *sup - tau;
	*sup = min(d__1,d__2);
    } else if (*iconv > 0) {
/* Computing MIN */
	d__1 = qq[*n], d__2 = qq[*n - 1], d__1 = min(d__1,d__2), d__2 = qq[*n 
		- 2], d__1 = min(d__1,d__2), d__1 = min(d__1,qq[1]), d__1 = 
		min(d__1,qq[2]);
	*sup = min(d__1,qq[3]);
	if (*iconv == 1) {
	    *kend = k1end;
	} else if (*iconv == 2) {
	    *kend = k2end;
	} else {
	    *kend = *n;
	}
	icnt = 0;
	maxit = *n * 100;
    }

/*     Checking for splitting in ping */

    lsplit = FALSE_;
    for (ks = *n - 3; ks >= 3; --ks) {
	if (ee[ks] <= toly) {
	    latime_1.ops += 3;
/* Computing MIN */
	    d__1 = qq[ks + 1], d__2 = qq[ks];
/* Computing MIN */
	    d__3 = qq[ks + 1], d__4 = qq[ks];
	    if (ee[ks] * (min(d__1,d__2) / (min(d__3,d__4) + *sigma)) <= tolx)
		     {
		lsplit = TRUE_;
		goto L130;
	    }
	}
/* L120: */
    }

    ks = 2;
    if (ee[2] <= tolz) {
	lsplit = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 7;
	t1 = *sigma + ee[1] * (*sigma / (*sigma + qq[1]));
	if (ee[2] * (t1 / (qq[1] + t1)) <= toly) {
	    latime_1.ops += 3;
	    if (ee[2] * (qq[1] / (qq[1] + t1)) <= tolx) {
		lsplit = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 5;
	if (ee[2] <= qq[1] / (qq[1] + ee[1] + qq[2]) * qq[2] * *tol2) {
	    lsplit = TRUE_;
	}
    }
    if (lsplit) {
	goto L130;
    }

    ks = 1;
    if (ee[1] <= tolz) {
	lsplit = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 2;
	if (ee[1] <= *eps * (*sigma + qq[1])) {
	    latime_1.ops += 5;
	    if (ee[1] * (qq[1] / (qq[1] + *sigma)) <= *tol2 * (qq[1] + *sigma)
		    ) {
		lsplit = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 1;
	if (ee[1] <= qq[1] * *tol2) {
	    lsplit = TRUE_;
	}
    }

L130:
    if (lsplit) {
/* Computing MIN */
	d__1 = qq[*n], d__2 = qq[*n - 1], d__1 = min(d__1,d__2), d__2 = qq[*n 
		- 2];
	*sup = min(d__1,d__2);
	isp = -(*off + 1);
	*off += ks;
	*n -= ks;
/* Computing MAX */
	i__1 = 1, i__2 = *kend - ks;
	*kend = max(i__1,i__2);
	e[ks] = *sigma;
	ee[ks] = (doublereal) isp;
	*iconv = 0;
	return 0;
    }

/*     Coincidence */

    if (tau == 0. && dm <= tolz && *kend != *n && *iconv == 0 && icnt > 0) {
	i__1 = *n - ke;
	dcopy_(&i__1, &e[ke], &c__1, &qq[ke], &c__1);
	qq[*n] = 0.;
	i__1 = *n - ke;
	dcopy_(&i__1, &q[ke + 1], &c__1, &ee[ke], &c__1);
	*sup = 0.;
    }
    *iconv = 0;
    goto L150;

/*     A new shift when the previous failed (in ping) */

L140:
    ++ifl;
    *sup = tau;

/*     SUP is small or   
       Too many bad shifts (ping) */

    if (*sup <= tolz || ifl >= 2) {
	tau = 0.;
	goto L40;

/*     The asymptotic shift (in ping) */

    } else {
/* Computing MAX */
	d__1 = tau + d;
	tau = max(d__1,0.);
	if (tau <= tolz) {
	    tau = 0.;
	}
	goto L40;
    }

/*     the pong section of the code */

L150:
    ifl = 0;

/*     Compute the shift (in pong) */

    if (*kend == 0 && *sup == 0.) {
	tau = 0.;
    } else if (icnt > 0 && dm <= tolz) {
	tau = 0.;
    } else {
/* Computing MAX */
	i__1 = 5, i__2 = *n / 32;
	ip = max(i__1,i__2);
	n2 = (ip << 1) + 1;
	if (n2 >= *n) {
	    n1 = 1;
	    n2 = *n;
	} else if (*kend + ip > *n) {
	    n1 = *n - (ip << 1);
	} else if (*kend - ip < 1) {
	    n1 = 1;
	} else {
	    n1 = *kend - ip;
	}
	dlasq4_(&n2, &qq[n1], &ee[n1], &tau, sup);
    }
L160:
    ++icnt;
    if (icnt > maxit) {
	*sup = -(*sup);
	return 0;
    }
    if (tau == 0.) {

/*     The dqd algorithm (in pong) */

	d = qq[1];
	dm = d;
	ke = 0;
	latime_1.ops = latime_1.ops + *n * 3 - 3;
	i__1 = *n - 3;
	for (i = 1; i <= i__1; ++i) {
	    q[i] = d + ee[i];
	    d = d / q[i] * qq[i + 1];
	    if (dm > d) {
		dm = d;
		ke = i;
	    }
/* L170: */
	}
	++ke;

/*     Penultimate dqd step (in pong) */

	k2end = ke;
	q[*n - 2] = d + ee[*n - 2];
	d = d / q[*n - 2] * qq[*n - 1];
	if (dm > d) {
	    dm = d;
	    ke = *n - 1;
	}

/*     Final dqd step (in pong) */

	k1end = ke;
	q[*n - 1] = d + ee[*n - 1];
	d = d / q[*n - 1] * qq[*n];
	if (dm > d) {
	    dm = d;
	    ke = *n;
	}
	q[*n] = d;
    } else {

/*     The dqds algorithm (in pong) */

	latime_1.ops = latime_1.ops + 1 + (*n - 3 << 2);
	d = qq[1] - tau;
	dm = d;
	ke = 0;
	if (d < 0.) {
	    goto L260;
	}
	i__1 = *n - 3;
	for (i = 1; i <= i__1; ++i) {
	    q[i] = d + ee[i];
	    d = d / q[i] * qq[i + 1] - tau;
	    if (dm > d) {
		dm = d;
		ke = i;
		if (d < 0.) {
		    goto L190;
		}
	    }
/* L180: */
	}
	++ke;
	goto L200;
L190:
	latime_1.ops -= *n - 3 - ke << 2;
	goto L260;
L200:

/*     Penultimate dqds step (in pong) */

	k2end = ke;
	latime_1.ops += 4;
	q[*n - 2] = d + ee[*n - 2];
	d = d / q[*n - 2] * qq[*n - 1] - tau;
	if (dm > d) {
	    dm = d;
	    ke = *n - 1;
	    if (d < 0.) {
		goto L260;
	    }
	}

/*     Final dqds step (in pong) */

	k1end = ke;
	latime_1.ops += 4;
	q[*n - 1] = d + ee[*n - 1];
	d = d / q[*n - 1] * qq[*n] - tau;
	if (dm > d) {
	    dm = d;
	    ke = *n;
	}
	q[*n] = d;
    }

/*        Convergence when is small (in pong) */

    latime_1.ops += 1;
    if ((d__1 = q[*n], abs(d__1)) <= *sigma * *tol2) {
	q[*n] = 0.;
	dm = 0.;
	ke = *n;
    }
    if (q[*n] < 0.) {
	goto L260;
    }

/*     Non-negative qd array: Update the e's */

    latime_1.ops = latime_1.ops + (*n << 1) - 1;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	e[i] = ee[i] / q[i] * qq[i + 1];
/* L210: */
    }

/*     Updating sigma and iphase in pong */

    *sigma += tau;
L220:
    *iphase = 1;
    latime_1.ops += 2;
    tolx = *sigma * *tol2;
    toly = *sigma * *eps;

/*     Checking for deflation and convergence (in pong) */

L230:
    if (*n <= 2) {
	return 0;
    }

/*        Deflation: bottom 1x1 (in pong) */

    ldef = FALSE_;
    if (e[*n - 1] <= tolz) {
	ldef = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 2;
	if (e[*n - 1] <= *eps * (*sigma + q[*n])) {
	    latime_1.ops += 5;
	    if (e[*n - 1] * (q[*n] / (q[*n] + *sigma)) <= *tol2 * (q[*n] + *
		    sigma)) {
		ldef = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 1;
	if (e[*n - 1] <= q[*n] * *tol2) {
	    ldef = TRUE_;
	}
    }
    if (ldef) {
	latime_1.ops += 1;
	q[*n] += *sigma;
	--(*n);
	++(*iconv);
	goto L230;
    }

/*        Deflation: bottom 2x2 (in pong) */

    ldef = FALSE_;
    if (e[*n - 2] <= tolz) {
	ldef = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 7;
	t1 = *sigma + e[*n - 1] * (*sigma / (*sigma + q[*n]));
	if (e[*n - 2] * (t1 / (q[*n - 1] + t1)) <= toly) {
	    latime_1.ops += 3;
	    if (e[*n - 2] * (q[*n - 1] / (q[*n - 1] + t1)) <= tolx) {
		ldef = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 5;
	if (e[*n - 2] <= q[*n] / (q[*n] + ee[*n - 1] + q[*n - 1]) * q[*n - 1] 
		* *tol2) {
	    ldef = TRUE_;
	}
    }
    if (ldef) {
/* Computing MAX */
	d__1 = q[*n], d__2 = q[*n - 1], d__1 = max(d__1,d__2), d__2 = e[*n - 
		1];
	qemax = max(d__1,d__2);
	if (qemax != 0.) {
	    latime_1.ops += 15;
	    if (qemax == q[*n - 1]) {
/* Computing 2nd power */
		d__1 = (q[*n] - q[*n - 1] + e[*n - 1]) / qemax;
		xx = (q[*n] + q[*n - 1] + e[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + e[*n - 1] * 4. / qemax)) * .5;
	    } else if (qemax == q[*n]) {
/* Computing 2nd power */
		d__1 = (q[*n - 1] - q[*n] + e[*n - 1]) / qemax;
		xx = (q[*n] + q[*n - 1] + e[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + e[*n - 1] * 4. / qemax)) * .5;
	    } else {
/* Computing 2nd power */
		d__1 = (q[*n] - q[*n - 1] + e[*n - 1]) / qemax;
		xx = (q[*n] + q[*n - 1] + e[*n - 1] + qemax * sqrt(d__1 * 
			d__1 + q[*n - 1] * 4. / qemax)) * .5;
	    }
/* Computing MAX */
	    d__1 = q[*n], d__2 = q[*n - 1];
/* Computing MIN */
	    d__3 = q[*n], d__4 = q[*n - 1];
	    yy = max(d__1,d__2) / xx * min(d__3,d__4);
	} else {
	    xx = 0.;
	    yy = 0.;
	}
	latime_1.ops += 2;
	q[*n - 1] = *sigma + xx;
	q[*n] = yy + *sigma;
	*n += -2;
	*iconv += 2;
	goto L230;
    }

/*     Updating bounds before going to pong */

    if (*iconv == 0) {
	*kend = ke;
/* Computing MIN */
	d__1 = dm, d__2 = *sup - tau;
	*sup = min(d__1,d__2);
    } else if (*iconv > 0) {
/* Computing MIN */
	d__1 = q[*n], d__2 = q[*n - 1], d__1 = min(d__1,d__2), d__2 = q[*n - 
		2], d__1 = min(d__1,d__2), d__1 = min(d__1,q[1]), d__1 = min(
		d__1,q[2]);
	*sup = min(d__1,q[3]);
	if (*iconv == 1) {
	    *kend = k1end;
	} else if (*iconv == 2) {
	    *kend = k2end;
	} else {
	    *kend = *n;
	}
	icnt = 0;
	maxit = *n * 100;
    }

/*     Checking for splitting in pong */

    lsplit = FALSE_;
    for (ks = *n - 3; ks >= 3; --ks) {
	latime_1.ops += 3;
	if (e[ks] <= toly) {
/* Computing MIN */
	    d__1 = q[ks + 1], d__2 = q[ks];
/* Computing MIN */
	    d__3 = q[ks + 1], d__4 = q[ks];
	    if (e[ks] * (min(d__1,d__2) / (min(d__3,d__4) + *sigma)) <= tolx) 
		    {
		lsplit = TRUE_;
		goto L250;
	    }
	}
/* L240: */
    }

    ks = 2;
    if (e[2] <= tolz) {
	lsplit = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 7;
	t1 = *sigma + e[1] * (*sigma / (*sigma + q[1]));
	if (e[2] * (t1 / (q[1] + t1)) <= toly) {
	    latime_1.ops += 3;
	    if (e[2] * (q[1] / (q[1] + t1)) <= tolx) {
		lsplit = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 5;
	if (e[2] <= q[1] / (q[1] + e[1] + q[2]) * q[2] * *tol2) {
	    lsplit = TRUE_;
	}
    }
    if (lsplit) {
	goto L250;
    }

    ks = 1;
    if (e[1] <= tolz) {
	lsplit = TRUE_;
    } else if (*sigma > 0.) {
	latime_1.ops += 2;
	if (e[1] <= *eps * (*sigma + q[1])) {
	    latime_1.ops += 5;
	    if (e[1] * (q[1] / (q[1] + *sigma)) <= *tol2 * (q[1] + *sigma)) {
		lsplit = TRUE_;
	    }
	}
    } else {
	latime_1.ops += 1;
	if (e[1] <= q[1] * *tol2) {
	    lsplit = TRUE_;
	}
    }

L250:
    if (lsplit) {
/* Computing MIN */
	d__1 = q[*n], d__2 = q[*n - 1], d__1 = min(d__1,d__2), d__2 = q[*n - 
		2];
	*sup = min(d__1,d__2);
	isp = *off + 1;
	*off += ks;
/* Computing MAX */
	i__1 = 1, i__2 = *kend - ks;
	*kend = max(i__1,i__2);
	*n -= ks;
	e[ks] = *sigma;
	ee[ks] = (doublereal) isp;
	*iconv = 0;
	return 0;
    }

/*     Coincidence */

    if (tau == 0. && dm <= tolz && *kend != *n && *iconv == 0 && icnt > 0) {
	i__1 = *n - ke;
	dcopy_(&i__1, &ee[ke], &c__1, &q[ke], &c__1);
	q[*n] = 0.;
	i__1 = *n - ke;
	dcopy_(&i__1, &qq[ke + 1], &c__1, &e[ke], &c__1);
	*sup = 0.;
    }
    *iconv = 0;
    goto L30;

/*     Computation of a new shift when the previous failed (in pong) */

L260:
    ++ifl;
    *sup = tau;

/*     SUP is small or   
       Too many bad shifts (in pong) */

    if (*sup <= tolz || ifl >= 2) {
	tau = 0.;
	goto L160;

/*     The asymptotic shift (in pong) */

    } else {
	latime_1.ops += 1;
/* Computing MAX */
	d__1 = tau + d;
	tau = max(d__1,0.);
	if (tau <= tolz) {
	    tau = 0.;
	}
	goto L160;
    }

/*     End of DLASQ3 */

    return 0;
} /* dlasq3_ */

