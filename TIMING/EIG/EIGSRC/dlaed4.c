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

/* Subroutine */ int dlaed4_(integer *n, integer *i, doublereal *d, 
	doublereal *z, doublereal *delta, doublereal *rho, doublereal *dlam, 
	integer *info)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal dphi, dpsi;
    static integer iter;
    static doublereal temp, prew, temp1, a, b, c;
    static integer j;
    static doublereal w;
    static integer niter;
    static logical swtch;
    extern /* Subroutine */ int dlaed5_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *), dlaed6_(integer *, 
	    logical *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *);
    static logical swtch3;
    static integer ii;
    extern doublereal dlamch_(char *);
    static doublereal dw, zz[3];
    static logical orgati;
    static doublereal erretm, rhoinv;
    static integer ip1;
    static doublereal del, eta, phi, eps, tau, psi;
    static integer iim1, iip1;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab, 
  
       Courant Institute, NAG Ltd., and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is unchanged, OPS is only incremented   

    Purpose   
    =======   

    This subroutine computes the I-th updated eigenvalue of a symmetric   
    rank-one modification to a diagonal matrix whose elements are   
    given in the array d, and that   

               D(i) < D(j)  for  i < j   

    and that RHO > 0.  This is arranged by the calling routine, and is   
    no loss in generality.  The rank-one modified system is thus   

               diag( D )  +  RHO *  Z * Z_transpose.   

    where we assume the Euclidean norm of Z is 1.   

    The method consists of approximating the rational functions in the   
    secular equation by simpler interpolating rational functions.   

    Arguments   
    =========   

    N      (input) INTEGER   
           The length of all arrays.   

    I      (input) INTEGER   
           The index of the eigenvalue to be computed.  1 <= I <= N.   

    D      (input) DOUBLE PRECISION array, dimension (N)   
           The original eigenvalues.  It is assumed that they are in   
           order, D(I) < D(J)  for I < J.   

    Z      (input) DOUBLE PRECISION array, dimension (N)   
           The components of the updating vector.   

    DELTA  (output) DOUBLE PRECISION array, dimension (N)   
           If N .ne. 1, DELTA contains (D(j) - lambda_I) in its  j-th   
           component.  If N = 1, then DELTA(1) = 1.  The vector DELTA   
           contains the information necessary to construct the   
           eigenvectors.   

    RHO    (input) DOUBLE PRECISION   
           The scalar in the symmetric updating formula.   

    DLAM   (output) DOUBLE PRECISION   
           The computed lambda_I, the I-th updated eigenvalue.   

    INFO   (output) INTEGER   
           = 0:  successful exit   
           > 0:  if INFO = 1, the updating process failed.   

    Internal Parameters   
    ===================   

    Logical variable ORGATI (origin-at-i?) is used for distinguishing   
    whether D(i) or D(i+1) is treated as the origin.   

              ORGATI = .true.    origin at i   
              ORGATI = .false.   origin at i+1   

     Logical variable SWTCH3 (switch-for-3-poles?) is for noting   
     if we are working with THREE poles!   

     MAXIT is the maximum number of iterations allowed for each   
     eigenvalue.   

    ===================================================================== 
  


       Since this routine is called in an inner loop, we do no argument   
       checking.   

       Quick return for N=1 and 2.   

       Parameter adjustments */
    --delta;
    --z;
    --d;

    /* Function Body */
    *info = 0;
    if (*n == 1) {

/*         Presumably, I=1 upon entry */

	latime_1.ops += 3;
	*dlam = d[1] + *rho * z[1] * z[1];
	delta[1] = 1.;
	return 0;
    }
    if (*n == 2) {
	dlaed5_(i, &d[1], &z[1], &delta[1], rho, dlam);
	return 0;
    }

/*     Compute machine epsilon */

    eps = dlamch_("Epsilon");
    latime_1.ops += 1;
    rhoinv = 1. / *rho;

/*     The case I = N */

    if (*i == *n) {

/*        Initialize some basic variables */

	ii = *n - 1;
	niter = 1;

/*        Calculate initial guess */

	latime_1.ops = latime_1.ops + *n * 5 + 1;
	temp = *rho / 2.;

/*        If ||Z||_2 is not one, then TEMP should be set to   
          RHO * ||Z||_2^2 / TWO */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    delta[j] = d[j] - d[*i] - temp;
/* L10: */
	}

	psi = 0.;
	i__1 = *n - 2;
	for (j = 1; j <= i__1; ++j) {
	    psi += z[j] * z[j] / delta[j];
/* L20: */
	}

	c = rhoinv + psi;
	w = c + z[ii] * z[ii] / delta[ii] + z[*n] * z[*n] / delta[*n];

	if (w <= 0.) {
	    latime_1.ops += 7;
	    temp = z[*n - 1] * z[*n - 1] / (d[*n] - d[*n - 1] + *rho) + z[*n] 
		    * z[*n] / *rho;
	    if (c <= temp) {
		tau = *rho;
	    } else {
		latime_1.ops += 14;
		del = d[*n] - d[*n - 1];
		a = -c * del + z[*n - 1] * z[*n - 1] + z[*n] * z[*n];
		b = z[*n] * z[*n] * del;
		if (a < 0.) {
		    tau = b * 2. / (sqrt(a * a + b * 4. * c) - a);
		} else {
		    tau = (a + sqrt(a * a + b * 4. * c)) / (c * 2.);
		}
	    }

/*           It can be proved that   
                 D(N)+RHO/2 <= LAMBDA(N) < D(N)+TAU <= D(N)+RHO */

	} else {
	    latime_1.ops += 16;
	    del = d[*n] - d[*n - 1];
	    a = -c * del + z[*n - 1] * z[*n - 1] + z[*n] * z[*n];
	    b = z[*n] * z[*n] * del;
	    if (a < 0.) {
		tau = b * 2. / (sqrt(a * a + b * 4. * c) - a);
	    } else {
		tau = (a + sqrt(a * a + b * 4. * c)) / (c * 2.);
	    }

/*           It can be proved that   
                 D(N) < D(N)+TAU < LAMBDA(N) < D(N)+RHO/2 */

	}

	latime_1.ops = latime_1.ops + (*n << 1) + ii * 6 + 14;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    delta[j] = d[j] - d[*i] - tau;
/* L30: */
	}

/*        Evaluate PSI and the derivative DPSI */

	dpsi = 0.;
	psi = 0.;
	erretm = 0.;
	i__1 = ii;
	for (j = 1; j <= i__1; ++j) {
	    temp = z[j] / delta[j];
	    psi += z[j] * temp;
	    dpsi += temp * temp;
	    erretm += psi;
/* L40: */
	}
	erretm = abs(erretm);

/*        Evaluate PHI and the derivative DPHI */

	temp = z[*n] / delta[*n];
	phi = z[*n] * temp;
	dphi = temp * temp;
	erretm = (-phi - psi) * 8. + erretm - phi + rhoinv + abs(tau) * (dpsi 
		+ dphi);

	w = rhoinv + phi + psi;

/*        Test for convergence */

	if (abs(w) <= eps * erretm) {
	    latime_1.ops += 1;
	    *dlam = d[*i] + tau;
	    goto L250;
	}

/*        Calculate the new step */

	latime_1.ops += 12;
	++niter;
	c = w - delta[*n - 1] * dpsi - delta[*n] * dphi;
	a = (delta[*n - 1] + delta[*n]) * w - delta[*n - 1] * delta[*n] * (
		dpsi + dphi);
	b = delta[*n - 1] * delta[*n] * w;
	if (c < 0.) {
	    c = abs(c);
	}
	if (c == 0.) {
/*           ETA = B/A */
	    latime_1.ops += 1;
	    eta = *rho - tau;
	} else if (a >= 0.) {
	    latime_1.ops += 8;
	    eta = (a + sqrt((d__1 = a * a - b * 4. * c, abs(d__1)))) / (c * 
		    2.);
	} else {
	    latime_1.ops += 8;
	    eta = b * 2. / (a - sqrt((d__1 = a * a - b * 4. * c, abs(d__1))));
	}

/*        Note, eta should be positive if w is negative, and   
          eta should be negative otherwise. However,   
          if for some reason caused by roundoff, eta*w > 0,   
          we simply use one Newton step instead. This way   
          will guarantee eta*w < 0. */

	latime_1.ops = latime_1.ops + *n + ii * 6 + 16;
	if (w * eta > 0.) {
	    latime_1.ops += 2;
	    eta = -w / (dpsi + dphi);
	}
	temp = tau + eta;
	if (temp > *rho) {
	    latime_1.ops += 1;
	    eta = *rho - tau;
	}
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    delta[j] -= eta;
/* L50: */
	}

	tau += eta;

/*        Evaluate PSI and the derivative DPSI */

	dpsi = 0.;
	psi = 0.;
	erretm = 0.;
	i__1 = ii;
	for (j = 1; j <= i__1; ++j) {
	    temp = z[j] / delta[j];
	    psi += z[j] * temp;
	    dpsi += temp * temp;
	    erretm += psi;
/* L60: */
	}
	erretm = abs(erretm);

/*        Evaluate PHI and the derivative DPHI */

	temp = z[*n] / delta[*n];
	phi = z[*n] * temp;
	dphi = temp * temp;
	erretm = (-phi - psi) * 8. + erretm - phi + rhoinv + abs(tau) * (dpsi 
		+ dphi);

	w = rhoinv + phi + psi;

/*        Main loop to update the values of the array   DELTA */

	iter = niter + 1;

	for (niter = iter; niter <= 20; ++niter) {

/*           Test for convergence */

	    latime_1.ops += 1;
	    if (abs(w) <= eps * erretm) {
		latime_1.ops += 1;
		*dlam = d[*i] + tau;
		goto L250;
	    }

/*           Calculate the new step */

	    latime_1.ops = latime_1.ops + 36 + *n + ii * 6;
	    c = w - delta[*n - 1] * dpsi - delta[*n] * dphi;
	    a = (delta[*n - 1] + delta[*n]) * w - delta[*n - 1] * delta[*n] * 
		    (dpsi + dphi);
	    b = delta[*n - 1] * delta[*n] * w;
	    if (a >= 0.) {
		eta = (a + sqrt((d__1 = a * a - b * 4. * c, abs(d__1)))) / (c 
			* 2.);
	    } else {
		eta = b * 2. / (a - sqrt((d__1 = a * a - b * 4. * c, abs(d__1)
			)));
	    }

/*           Note, eta should be positive if w is negative, and   
             eta should be negative otherwise. However,   
             if for some reason caused by roundoff, eta*w > 0,   
             we simply use one Newton step instead. This way   
             will guarantee eta*w < 0. */

	    if (w * eta > 0.) {
		eta = -w / (dpsi + dphi);
	    }
	    temp = tau + eta;
	    if (temp <= 0.) {
		eta /= 2.;
	    }
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		delta[j] -= eta;
/* L70: */
	    }

	    tau += eta;

/*           Evaluate PSI and the derivative DPSI */

	    dpsi = 0.;
	    psi = 0.;
	    erretm = 0.;
	    i__1 = ii;
	    for (j = 1; j <= i__1; ++j) {
		temp = z[j] / delta[j];
		psi += z[j] * temp;
		dpsi += temp * temp;
		erretm += psi;
/* L80: */
	    }
	    erretm = abs(erretm);

/*           Evaluate PHI and the derivative DPHI */

	    temp = z[*n] / delta[*n];
	    phi = z[*n] * temp;
	    dphi = temp * temp;
	    erretm = (-phi - psi) * 8. + erretm - phi + rhoinv + abs(tau) * (
		    dpsi + dphi);

	    w = rhoinv + phi + psi;
/* L90: */
	}

/*        Return with INFO = 1, NITER = MAXIT and not converged */

	*info = 1;
	latime_1.ops += 1;
	*dlam = d[*i] + tau;
	goto L250;

/*        End for the case I = N */

    } else {

/*        The case for I < N */

	latime_1.ops = latime_1.ops + *n * 13 + (iim1 - iip1) * 6 + 45;
	niter = 1;
	ip1 = *i + 1;

/*        Calculate initial guess */

	temp = (d[ip1] - d[*i]) / 2.;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    delta[j] = d[j] - d[*i] - temp;
/* L100: */
	}

	psi = 0.;
	i__1 = *i - 1;
	for (j = 1; j <= i__1; ++j) {
	    psi += z[j] * z[j] / delta[j];
/* L110: */
	}

	phi = 0.;
	i__1 = *i + 2;
	for (j = *n; j >= i__1; --j) {
	    phi += z[j] * z[j] / delta[j];
/* L120: */
	}
	c = rhoinv + psi + phi;
	w = c + z[*i] * z[*i] / delta[*i] + z[ip1] * z[ip1] / delta[ip1];

	if (w > 0.) {

/*           d(i)< the ith eigenvalue < (d(i)+d(i+1))/2   

             We choose d(i) as origin. */

	    orgati = TRUE_;
	    del = d[ip1] - d[*i];
	    a = c * del + z[*i] * z[*i] + z[ip1] * z[ip1];
	    b = z[*i] * z[*i] * del;
	    if (a > 0.) {
		tau = b * 2. / (a + sqrt((d__1 = a * a - b * 4. * c, abs(d__1)
			)));
	    } else {
		tau = (a - sqrt((d__1 = a * a - b * 4. * c, abs(d__1)))) / (c 
			* 2.);
	    }
	} else {

/*           (d(i)+d(i+1))/2 <= the ith eigenvalue < d(i+1)   

             We choose d(i+1) as origin. */

	    orgati = FALSE_;
	    del = d[ip1] - d[*i];
	    a = c * del - z[*i] * z[*i] - z[ip1] * z[ip1];
	    b = z[ip1] * z[ip1] * del;
	    if (a < 0.) {
		tau = b * 2. / (a - sqrt((d__1 = a * a + b * 4. * c, abs(d__1)
			)));
	    } else {
		tau = -(a + sqrt((d__1 = a * a + b * 4. * c, abs(d__1)))) / (
			c * 2.);
	    }
	}

	if (orgati) {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		delta[j] = d[j] - d[*i] - tau;
/* L130: */
	    }
	} else {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		delta[j] = d[j] - d[ip1] - tau;
/* L140: */
	    }
	}
	if (orgati) {
	    ii = *i;
	} else {
	    ii = *i + 1;
	}
	iim1 = ii - 1;
	iip1 = ii + 1;

/*        Evaluate PSI and the derivative DPSI */

	dpsi = 0.;
	psi = 0.;
	erretm = 0.;
	i__1 = iim1;
	for (j = 1; j <= i__1; ++j) {
	    temp = z[j] / delta[j];
	    psi += z[j] * temp;
	    dpsi += temp * temp;
	    erretm += psi;
/* L150: */
	}
	erretm = abs(erretm);

/*        Evaluate PHI and the derivative DPHI */

	dphi = 0.;
	phi = 0.;
	i__1 = iip1;
	for (j = *n; j >= i__1; --j) {
	    temp = z[j] / delta[j];
	    phi += z[j] * temp;
	    dphi += temp * temp;
	    erretm += phi;
/* L160: */
	}

	w = rhoinv + phi + psi;

/*        W is the value of the secular function with   
          its ii-th element removed. */

	swtch3 = FALSE_;
	if (orgati) {
	    if (w < 0.) {
		swtch3 = TRUE_;
	    }
	} else {
	    if (w > 0.) {
		swtch3 = TRUE_;
	    }
	}
	if (ii == 1 || ii == *n) {
	    swtch3 = FALSE_;
	}

	temp = z[ii] / delta[ii];
	dw = dpsi + dphi + temp * temp;
	temp = z[ii] * temp;
	w += temp;
	erretm = (phi - psi) * 8. + erretm + rhoinv * 2. + abs(temp) * 3. + 
		abs(tau) * dw;

/*        Test for convergence */

	if (abs(w) <= eps * erretm) {
	    if (orgati) {
		*dlam = d[*i] + tau;
	    } else {
		*dlam = d[ip1] + tau;
	    }
	    goto L250;
	}

/*        Calculate the new step */

	latime_1.ops += 14;
	++niter;
	if (! swtch3) {
	    if (orgati) {
/* Computing 2nd power */
		d__1 = z[*i] / delta[*i];
		c = w - delta[ip1] * dw - (d[*i] - d[ip1]) * (d__1 * d__1);
	    } else {
/* Computing 2nd power */
		d__1 = z[ip1] / delta[ip1];
		c = w - delta[*i] * dw - (d[ip1] - d[*i]) * (d__1 * d__1);
	    }
	    a = (delta[*i] + delta[ip1]) * w - delta[*i] * delta[ip1] * dw;
	    b = delta[*i] * delta[ip1] * w;
	    if (c == 0.) {
		if (a == 0.) {
		    latime_1.ops += 5;
		    if (orgati) {
			a = z[*i] * z[*i] + delta[ip1] * delta[ip1] * (dpsi + 
				dphi);
		    } else {
			a = z[ip1] * z[ip1] + delta[*i] * delta[*i] * (dpsi + 
				dphi);
		    }
		}
		eta = b / a;
	    } else if (a <= 0.) {
		latime_1.ops += 8;
		eta = (a - sqrt((d__1 = a * a - b * 4. * c, abs(d__1)))) / (c 
			* 2.);
	    } else {
		latime_1.ops += 8;
		eta = b * 2. / (a + sqrt((d__1 = a * a - b * 4. * c, abs(d__1)
			)));
	    }
	} else {

/*           Interpolation using THREE most relevant poles */

	    latime_1.ops += 15;
	    temp = rhoinv + psi + phi;
	    if (orgati) {
		temp1 = z[iim1] / delta[iim1];
		temp1 *= temp1;
		c = temp - delta[iip1] * (dpsi + dphi) - (d[iim1] - d[iip1]) *
			 temp1;
		zz[0] = z[iim1] * z[iim1];
		zz[2] = delta[iip1] * delta[iip1] * (dpsi - temp1 + dphi);
	    } else {
		temp1 = z[iip1] / delta[iip1];
		temp1 *= temp1;
		c = temp - delta[iim1] * (dpsi + dphi) - (d[iip1] - d[iim1]) *
			 temp1;
		zz[0] = delta[iim1] * delta[iim1] * (dpsi + (dphi - temp1));
		zz[2] = z[iip1] * z[iip1];
	    }
	    zz[1] = z[ii] * z[ii];
	    dlaed6_(&niter, &orgati, &c, &delta[iim1], zz, &w, &eta, info);
	    if (*info != 0) {
		goto L250;
	    }
	}

/*        Note, eta should be positive if w is negative, and   
          eta should be negative otherwise. However,   
          if for some reason caused by roundoff, eta*w > 0,   
          we simply use one Newton step instead. This way   
          will guarantee eta*w < 0. */

	latime_1.ops = latime_1.ops + 18 + *n * 7 + (iim1 - iip1) * 6;
	if (w * eta >= 0.) {
	    latime_1.ops += 1;
	    eta = -w / dw;
	}
	temp = tau + eta;
	del = (d[ip1] - d[*i]) / 2.;
	if (orgati) {
	    if (temp >= del) {
		latime_1.ops += 1;
		eta = del - tau;
	    }
	    if (temp <= 0.) {
		latime_1.ops += 1;
		eta /= 2.;
	    }
	} else {
	    if (temp <= -del) {
		latime_1.ops += 1;
		eta = -del - tau;
	    }
	    if (temp >= 0.) {
		latime_1.ops += 1;
		eta /= 2.;
	    }
	}

	prew = w;

/* L170: */
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    delta[j] -= eta;
/* L180: */
	}

/*        Evaluate PSI and the derivative DPSI */

	dpsi = 0.;
	psi = 0.;
	erretm = 0.;
	i__1 = iim1;
	for (j = 1; j <= i__1; ++j) {
	    temp = z[j] / delta[j];
	    psi += z[j] * temp;
	    dpsi += temp * temp;
	    erretm += psi;
/* L190: */
	}
	erretm = abs(erretm);

/*        Evaluate PHI and the derivative DPHI */

	dphi = 0.;
	phi = 0.;
	i__1 = iip1;
	for (j = *n; j >= i__1; --j) {
	    temp = z[j] / delta[j];
	    phi += z[j] * temp;
	    dphi += temp * temp;
	    erretm += phi;
/* L200: */
	}

	temp = z[ii] / delta[ii];
	dw = dpsi + dphi + temp * temp;
	temp = z[ii] * temp;
	w = rhoinv + phi + psi + temp;
	erretm = (phi - psi) * 8. + erretm + rhoinv * 2. + abs(temp) * 3. + (
		d__1 = tau + eta, abs(d__1)) * dw;

	swtch = FALSE_;
	if (orgati) {
	    if (-w > abs(prew) / 10.) {
		swtch = TRUE_;
	    }
	} else {
	    if (w > abs(prew) / 10.) {
		swtch = TRUE_;
	    }
	}

	tau += eta;

/*        Main loop to update the values of the array   DELTA */

	iter = niter + 1;

	for (niter = iter; niter <= 20; ++niter) {

/*           Test for convergence */

	    latime_1.ops += 1;
	    if (abs(w) <= eps * erretm) {
		latime_1.ops += 1;
		if (orgati) {
		    *dlam = d[*i] + tau;
		} else {
		    *dlam = d[ip1] + tau;
		}
		goto L250;
	    }

/*           Calculate the new step */

	    if (! swtch3) {
		latime_1.ops += 14;
		if (! swtch) {
		    if (orgati) {
/* Computing 2nd power */
			d__1 = z[*i] / delta[*i];
			c = w - delta[ip1] * dw - (d[*i] - d[ip1]) * (d__1 * 
				d__1);
		    } else {
/* Computing 2nd power */
			d__1 = z[ip1] / delta[ip1];
			c = w - delta[*i] * dw - (d[ip1] - d[*i]) * (d__1 * 
				d__1);
		    }
		} else {
		    temp = z[ii] / delta[ii];
		    if (orgati) {
			dpsi += temp * temp;
		    } else {
			dphi += temp * temp;
		    }
		    c = w - delta[*i] * dpsi - delta[ip1] * dphi;
		}
		a = (delta[*i] + delta[ip1]) * w - delta[*i] * delta[ip1] * 
			dw;
		b = delta[*i] * delta[ip1] * w;
		if (c == 0.) {
		    if (a == 0.) {
			latime_1.ops += 5;
			if (! swtch) {
			    if (orgati) {
				a = z[*i] * z[*i] + delta[ip1] * delta[ip1] * 
					(dpsi + dphi);
			    } else {
				a = z[ip1] * z[ip1] + delta[*i] * delta[*i] * 
					(dpsi + dphi);
			    }
			} else {
			    a = delta[*i] * delta[*i] * dpsi + delta[ip1] * 
				    delta[ip1] * dphi;
			}
		    }
		    latime_1.ops += 1;
		    eta = b / a;
		} else if (a <= 0.) {
		    latime_1.ops += 8;
		    eta = (a - sqrt((d__1 = a * a - b * 4. * c, abs(d__1)))) /
			     (c * 2.);
		} else {
		    latime_1.ops += 8;
		    eta = b * 2. / (a + sqrt((d__1 = a * a - b * 4. * c, abs(
			    d__1))));
		}
	    } else {

/*              Interpolation using THREE most relevant poles 
*/

		latime_1.ops += 2;
		temp = rhoinv + psi + phi;
		if (swtch) {
		    latime_1.ops += 10;
		    c = temp - delta[iim1] * dpsi - delta[iip1] * dphi;
		    zz[0] = delta[iim1] * delta[iim1] * dpsi;
		    zz[2] = delta[iip1] * delta[iip1] * dphi;
		} else {
		    latime_1.ops += 14;
		    if (orgati) {
			temp1 = z[iim1] / delta[iim1];
			temp1 *= temp1;
			c = temp - delta[iip1] * (dpsi + dphi) - (d[iim1] - d[
				iip1]) * temp1;
			zz[0] = z[iim1] * z[iim1];
			zz[2] = delta[iip1] * delta[iip1] * (dpsi - temp1 + 
				dphi);
		    } else {
			temp1 = z[iip1] / delta[iip1];
			temp1 *= temp1;
			c = temp - delta[iim1] * (dpsi + dphi) - (d[iip1] - d[
				iim1]) * temp1;
			zz[0] = delta[iim1] * delta[iim1] * (dpsi + (dphi - 
				temp1));
			zz[2] = z[iip1] * z[iip1];
		    }
		}
		dlaed6_(&niter, &orgati, &c, &delta[iim1], zz, &w, &eta, info)
			;
		if (*info != 0) {
		    goto L250;
		}
	    }

/*           Note, eta should be positive if w is negative, and   
             eta should be negative otherwise. However,   
             if for some reason caused by roundoff, eta*w > 0,   
             we simply use one Newton step instead. This way   
             will guarantee eta*w < 0. */

	    latime_1.ops = latime_1.ops + *n * 7 + (iim1 - iip1) * 6 + 18;
	    if (w * eta >= 0.) {
		latime_1.ops += 1;
		eta = -w / dw;
	    }
	    temp = tau + eta;
	    del = (d[ip1] - d[*i]) / 2.;
	    if (orgati) {
		if (temp >= del) {
		    eta = del - tau;
		    latime_1.ops += 1;
		}
		if (temp <= 0.) {
		    eta /= 2.;
		    latime_1.ops += 1;
		}
	    } else {
		if (temp <= -del) {
		    eta = -del - tau;
		    latime_1.ops += 1;
		}
		if (temp >= 0.) {
		    eta /= 2.;
		    latime_1.ops += 1;
		}
	    }

	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		delta[j] -= eta;
/* L210: */
	    }

	    tau += eta;
	    prew = w;

/*           Evaluate PSI and the derivative DPSI */

	    dpsi = 0.;
	    psi = 0.;
	    erretm = 0.;
	    i__1 = iim1;
	    for (j = 1; j <= i__1; ++j) {
		temp = z[j] / delta[j];
		psi += z[j] * temp;
		dpsi += temp * temp;
		erretm += psi;
/* L220: */
	    }
	    erretm = abs(erretm);

/*           Evaluate PHI and the derivative DPHI */

	    dphi = 0.;
	    phi = 0.;
	    i__1 = iip1;
	    for (j = *n; j >= i__1; --j) {
		temp = z[j] / delta[j];
		phi += z[j] * temp;
		dphi += temp * temp;
		erretm += phi;
/* L230: */
	    }

	    temp = z[ii] / delta[ii];
	    dw = dpsi + dphi + temp * temp;
	    temp = z[ii] * temp;
	    w = rhoinv + phi + psi + temp;
	    erretm = (phi - psi) * 8. + erretm + rhoinv * 2. + abs(temp) * 3. 
		    + abs(tau) * dw;
	    if (w * prew > 0. && abs(w) > abs(prew) / 10.) {
		swtch = ! swtch;
	    }

/* L240: */
	}

/*        Return with INFO = 1, NITER = MAXIT and not converged */

	*info = 1;
	latime_1.ops += 1;
	if (orgati) {
	    *dlam = d[*i] + tau;
	} else {
	    *dlam = d[ip1] + tau;
	}

    }

L250:
    return 0;

/*     End of DLAED4 */

} /* dlaed4_ */

