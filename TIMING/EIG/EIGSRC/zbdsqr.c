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

static doublereal c_b15 = -.125;
static integer c__1 = 1;
static doublereal c_b48 = 1.;
static doublereal c_b71 = -1.;

/* Subroutine */ int zbdsqr_(char *uplo, integer *n, integer *ncvt, integer *
	nru, integer *ncc, doublereal *d, doublereal *e, doublecomplex *vt, 
	integer *ldvt, doublecomplex *u, integer *ldu, doublecomplex *c, 
	integer *ldc, doublereal *rwork, integer *info)
{
    /* System generated locals */
    integer c_dim1, c_offset, u_dim1, u_offset, vt_dim1, vt_offset, i__1, 
	    i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal), d_sign(
	    doublereal *, doublereal *);

    /* Local variables */
    static doublereal abse;
    static integer idir;
    static doublereal abss;
    static integer oldm;
    static doublereal cosl;
    static integer isub, iter;
    static doublereal unfl, sinl, cosr, smin, smax, sinr;
    static integer irot;
    extern /* Subroutine */ int dlas2_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    static doublereal f, g, h;
    static integer i, j, m;
    static doublereal r;
    extern logical lsame_(char *, char *);
    static doublereal oldcs;
    static integer oldll;
    static doublereal shift, sigmn, oldsn;
    static integer maxit;
    static doublereal sminl, sigmx;
    static integer iuplo;
    extern /* Subroutine */ int zlasr_(char *, char *, char *, integer *, 
	    integer *, doublereal *, doublereal *, doublecomplex *, integer *), zdrot_(integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, doublereal *)
	    , zswap_(integer *, doublecomplex *, integer *, doublecomplex *, 
	    integer *), dlasq1_(integer *, doublereal *, doublereal *, 
	    doublereal *, integer *), dlasv2_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal cs;
    static integer ll;
    extern doublereal dlamch_(char *);
    static doublereal sn, mu;
    extern /* Subroutine */ int dlartg_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), xerbla_(char *, 
	    integer *), zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *);
    static doublereal sminoa, thresh;
    static logical rotate;
    static doublereal sminlo;
    static integer nm1;
    static doublereal tolmul;
    static integer nm12, nm13, lll;
    static doublereal eps, sll, tol;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

    Purpose   
    =======   

    ZBDSQR computes the singular value decomposition (SVD) of a real   
    N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'   
    denotes the transpose of P), where S is a diagonal matrix with   
    non-negative diagonal elements (the singular values of B), and Q   
    and P are orthogonal matrices.   

    The routine computes S, and optionally computes U * Q, P' * VT,   
    or Q' * C, for given complex input matrices U, VT, and C.   

    See "Computing  Small Singular Values of Bidiagonal Matrices With   
    Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,   
    LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,   
    no. 5, pp. 873-912, Sept 1990) and   
    "Accurate singular values and differential qd algorithms," by   
    B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics   
    Department, University of California at Berkeley, July 1992   
    for a detailed description of the algorithm.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            = 'U':  B is upper bidiagonal;   
            = 'L':  B is lower bidiagonal.   

    N       (input) INTEGER   
            The order of the matrix B.  N >= 0.   

    NCVT    (input) INTEGER   
            The number of columns of the matrix VT. NCVT >= 0.   

    NRU     (input) INTEGER   
            The number of rows of the matrix U. NRU >= 0.   

    NCC     (input) INTEGER   
            The number of columns of the matrix C. NCC >= 0.   

    D       (input/output) DOUBLE PRECISION array, dimension (N)   
            On entry, the n diagonal elements of the bidiagonal matrix B. 
  
            On exit, if INFO=0, the singular values of B in decreasing   
            order.   

    E       (input/output) DOUBLE PRECISION array, dimension (N)   
            On entry, the elements of E contain the   
            offdiagonal elements of of the bidiagonal matrix whose SVD   
            is desired. On normal exit (INFO = 0), E is destroyed.   
            If the algorithm does not converge (INFO > 0), D and E   
            will contain the diagonal and superdiagonal elements of a   
            bidiagonal matrix orthogonally equivalent to the one given   
            as input. E(N) is used for workspace.   

    VT      (input/output) COMPLEX*16 array, dimension (LDVT, NCVT)   
            On entry, an N-by-NCVT matrix VT.   
            On exit, VT is overwritten by P' * VT.   
            VT is not referenced if NCVT = 0.   

    LDVT    (input) INTEGER   
            The leading dimension of the array VT.   
            LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.   

    U       (input/output) COMPLEX*16 array, dimension (LDU, N)   
            On entry, an NRU-by-N matrix U.   
            On exit, U is overwritten by U * Q.   
            U is not referenced if NRU = 0.   

    LDU     (input) INTEGER   
            The leading dimension of the array U.  LDU >= max(1,NRU).   

    C       (input/output) COMPLEX*16 array, dimension (LDC, NCC)   
            On entry, an N-by-NCC matrix C.   
            On exit, C is overwritten by Q' * C.   
            C is not referenced if NCC = 0.   

    LDC     (input) INTEGER   
            The leading dimension of the array C.   
            LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.   

    RWORK   (workspace) DOUBLE PRECISION array, dimension   
              2*N  if only singular values wanted (NCVT = NRU = NCC = 0) 
  
              max( 1, 4*N-4 ) otherwise   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  If INFO = -i, the i-th argument had an illegal value   
            > 0:  the algorithm did not converge; D and E contain the   
                  elements of a bidiagonal matrix which is orthogonally   
                  similar to the input matrix B;  if INFO = i, i   
                  elements of E have not converged to zero.   

    Internal Parameters   
    ===================   

    TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))   
            TOLMUL controls the convergence criterion of the QR loop.   
            If it is positive, TOLMUL*EPS is the desired relative   
               precision in the computed singular values.   
            If it is negative, abs(TOLMUL*EPS*sigma_max) is the   
               desired absolute accuracy in the computed singular   
               values (corresponds to relative accuracy   
               abs(TOLMUL*EPS) in the largest singular value.   
            abs(TOLMUL) should be between 1 and 1/EPS, and preferably   
               between 10 (for fast convergence) and .1/EPS   
               (for there to be some accuracy in the results).   
            Default is to lose at either one eighth or 2 of the   
               available decimal digits in each computed singular value   
               (whichever is smaller).   

    MAXITR  INTEGER, default = 6   
            MAXITR controls the maximum number of passes of the   
            algorithm through its inner loop. The algorithms stops   
            (and so fails to converge) if the number of passes   
            through the inner loop exceeds MAXITR*N**2.   

    ===================================================================== 
  


       Test the input parameters.   

       Parameter adjustments */
    --d;
    --e;
    vt_dim1 = *ldvt;
    vt_offset = vt_dim1 + 1;
    vt -= vt_offset;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c -= c_offset;
    --rwork;

    /* Function Body */
    *info = 0;
    iuplo = 0;
    if (lsame_(uplo, "U")) {
	iuplo = 1;
    }
    if (lsame_(uplo, "L")) {
	iuplo = 2;
    }
    if (iuplo == 0) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*ncvt < 0) {
	*info = -3;
    } else if (*nru < 0) {
	*info = -4;
    } else if (*ncc < 0) {
	*info = -5;
    } else if (*ncvt == 0 && *ldvt < 1 || *ncvt > 0 && *ldvt < max(1,*n)) {
	*info = -9;
    } else if (*ldu < max(1,*nru)) {
	*info = -11;
    } else if (*ncc == 0 && *ldc < 1 || *ncc > 0 && *ldc < max(1,*n)) {
	*info = -13;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZBDSQR", &i__1);
	return 0;
    }
    if (*n == 0) {
	return 0;
    }
    if (*n == 1) {
	goto L150;
    }

/*     ROTATE is true if any singular vectors desired, false otherwise */

    rotate = *ncvt > 0 || *nru > 0 || *ncc > 0;

/*     If no singular vectors desired, use qd algorithm */

    if (! rotate) {
	dlasq1_(n, &d[1], &e[1], &rwork[1], info);
	return 0;
    }

    nm1 = *n - 1;
    nm12 = nm1 + nm1;
    nm13 = nm12 + nm1;

/*     Get machine constants */

    eps = dlamch_("Epsilon");
    unfl = dlamch_("Safe minimum");

/*     If matrix lower bidiagonal, rotate to be upper bidiagonal   
       by applying Givens rotations on the left */

    if (iuplo == 2) {
	latime_1.ops += (doublereal) (*n - 1) * ((*nru + *ncc) * 12 + 8);
	i__1 = *n - 1;
	for (i = 1; i <= i__1; ++i) {
	    dlartg_(&d[i], &e[i], &cs, &sn, &r);
	    d[i] = r;
	    e[i] = sn * d[i + 1];
	    d[i + 1] = cs * d[i + 1];
	    rwork[i] = cs;
	    rwork[nm1 + i] = sn;
/* L10: */
	}

/*        Update singular vectors if desired */

	if (*nru > 0) {
	    zlasr_("R", "V", "F", nru, n, &rwork[1], &rwork[*n], &u[u_offset],
		     ldu);
	}
	if (*ncc > 0) {
	    zlasr_("L", "V", "F", n, ncc, &rwork[1], &rwork[*n], &c[c_offset],
		     ldc);
	}
    }

/*     Compute singular values to relative accuracy TOL   
       (By setting TOL to be negative, algorithm will compute   
       singular values to absolute accuracy ABS(TOL)*norm(input matrix)) 
*/

    latime_1.ops += 4;
/* Computing MAX   
   Computing MIN */
    d__3 = 100., d__4 = pow_dd(&eps, &c_b15);
    d__1 = 10., d__2 = min(d__3,d__4);
    tolmul = max(d__1,d__2);
    tol = tolmul * eps;

/*     Compute approximate maximum, minimum singular values */

    smax = (d__1 = d[*n], abs(d__1));
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	d__3 = smax, d__4 = (d__1 = d[i], abs(d__1)), d__3 = max(d__3,d__4), 
		d__4 = (d__2 = e[i], abs(d__2));
	smax = max(d__3,d__4);
/* L20: */
    }
    sminl = 0.;
    if (tol >= 0.) {

/*        Relative accuracy desired */

	sminoa = abs(d[1]);
	if (sminoa == 0.) {
	    goto L40;
	}
	mu = sminoa;
	latime_1.ops = latime_1.ops + *n * 3 - 1;
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
	    mu = (d__1 = d[i], abs(d__1)) * (mu / (mu + (d__2 = e[i - 1], abs(
		    d__2))));
	    sminoa = min(sminoa,mu);
	    if (sminoa == 0.) {
		goto L40;
	    }
/* L30: */
	}
L40:
	sminoa /= sqrt((doublereal) (*n));
/* Computing MAX */
	d__1 = tol * sminoa, d__2 = *n * 6 * *n * unfl;
	thresh = max(d__1,d__2);
    } else {

/*        Absolute accuracy desired   

   Computing MAX */
	d__1 = abs(tol) * smax, d__2 = *n * 6 * *n * unfl;
	thresh = max(d__1,d__2);
    }

/*     Prepare for main iteration loop for the singular values   
       (MAXIT is the maximum number of passes through the inner   
       loop permitted before nonconvergence signalled.) */

    maxit = *n * 6 * *n;
    iter = 0;
    oldll = -1;
    oldm = -1;

/*     M points to last element of unconverged part of matrix */

    m = *n;

/*     Begin main iteration loop */

L50:

/*     Check for convergence or exceeding iteration count */

    if (m <= 1) {
	goto L150;
    }
    if (iter > maxit) {
	goto L190;
    }

/*     Find diagonal block of matrix to work on */

    if (tol < 0. && (d__1 = d[m], abs(d__1)) <= thresh) {
	d[m] = 0.;
    }
    smax = (d__1 = d[m], abs(d__1));
    smin = smax;
    i__1 = m;
    for (lll = 1; lll <= i__1; ++lll) {
	ll = m - lll;
	if (ll == 0) {
	    goto L80;
	}
	abss = (d__1 = d[ll], abs(d__1));
	abse = (d__1 = e[ll], abs(d__1));
	if (tol < 0. && abss <= thresh) {
	    d[ll] = 0.;
	}
	if (abse <= thresh) {
	    goto L70;
	}
	smin = min(smin,abss);
/* Computing MAX */
	d__1 = max(smax,abss);
	smax = max(d__1,abse);
/* L60: */
    }
L70:
    e[ll] = 0.;

/*     Matrix splits since E(LL) = 0 */

    if (ll == m - 1) {

/*        Convergence of bottom singular value, return to top of loop 
*/

	--m;
	goto L50;
    }
L80:
    ++ll;

/*     E(LL) through E(M-1) are nonzero, E(LL-1) is zero */

    if (ll == m - 1) {

/*        2 by 2 block, handle separately */

	latime_1.ops = latime_1.ops + 37 + (*ncvt + *nru + *ncc) * 12;
	dlasv2_(&d[m - 1], &e[m - 1], &d[m], &sigmn, &sigmx, &sinr, &cosr, &
		sinl, &cosl);
	d[m - 1] = sigmx;
	e[m - 1] = 0.;
	d[m] = sigmn;

/*        Compute singular vectors, if desired */

	if (*ncvt > 0) {
	    zdrot_(ncvt, &vt[m - 1 + vt_dim1], ldvt, &vt[m + vt_dim1], ldvt, &
		    cosr, &sinr);
	}
	if (*nru > 0) {
	    zdrot_(nru, &u[(m - 1) * u_dim1 + 1], &c__1, &u[m * u_dim1 + 1], &
		    c__1, &cosl, &sinl);
	}
	if (*ncc > 0) {
	    zdrot_(ncc, &c[m - 1 + c_dim1], ldc, &c[m + c_dim1], ldc, &cosl, &
		    sinl);
	}
	m += -2;
	goto L50;
    }

/*     If working on new submatrix, choose shift direction   
       (from larger end diagonal element towards smaller) */

    if (ll > oldm || m < oldll) {
	if ((d__1 = d[ll], abs(d__1)) >= (d__2 = d[m], abs(d__2))) {

/*           Chase bulge from top (big end) to bottom (small end) 
*/

	    idir = 1;
	} else {

/*           Chase bulge from bottom (big end) to top (small end) 
*/

	    idir = 2;
	}
    }

/*     Apply convergence tests */

    if (idir == 1) {

/*        Run convergence test in forward direction   
          First apply standard test to bottom of matrix */

	latime_1.ops += 1;
	if ((d__1 = e[m - 1], abs(d__1)) <= abs(tol) * (d__2 = d[m], abs(d__2)
		) || tol < 0. && (d__3 = e[m - 1], abs(d__3)) <= thresh) {
	    e[m - 1] = 0.;
	    goto L50;
	}

	if (tol >= 0.) {

/*           If relative accuracy desired,   
             apply convergence criterion forward */

	    mu = (d__1 = d[ll], abs(d__1));
	    sminl = mu;
	    i__1 = m - 1;
	    for (lll = ll; lll <= i__1; ++lll) {
		if ((d__1 = e[lll], abs(d__1)) <= tol * mu) {
		    e[lll] = 0.;
		    goto L50;
		}
		sminlo = sminl;
		latime_1.ops += 4;
		mu = (d__1 = d[lll + 1], abs(d__1)) * (mu / (mu + (d__2 = e[
			lll], abs(d__2))));
		sminl = min(sminl,mu);
/* L90: */
	    }
	}

    } else {

/*        Run convergence test in backward direction   
          First apply standard test to top of matrix */

	latime_1.ops += 1;
	if ((d__1 = e[ll], abs(d__1)) <= abs(tol) * (d__2 = d[ll], abs(d__2)) 
		|| tol < 0. && (d__3 = e[ll], abs(d__3)) <= thresh) {
	    e[ll] = 0.;
	    goto L50;
	}

	if (tol >= 0.) {

/*           If relative accuracy desired,   
             apply convergence criterion backward */

	    mu = (d__1 = d[m], abs(d__1));
	    sminl = mu;
	    i__1 = ll;
	    for (lll = m - 1; lll >= i__1; --lll) {
		if ((d__1 = e[lll], abs(d__1)) <= tol * mu) {
		    e[lll] = 0.;
		    goto L50;
		}
		sminlo = sminl;
		latime_1.ops += 4;
		mu = (d__1 = d[lll], abs(d__1)) * (mu / (mu + (d__2 = e[lll], 
			abs(d__2))));
		sminl = min(sminl,mu);
/* L100: */
	    }
	}
    }
    oldll = ll;
    oldm = m;

/*     Compute shift.  First, test if shifting would ruin relative   
       accuracy, and if so set the shift to zero. */

    latime_1.ops += 4;
/* Computing MAX */
    d__1 = eps, d__2 = tol * .01;
    if (tol >= 0. && *n * tol * (sminl / smax) <= max(d__1,d__2)) {

/*        Use a zero shift to avoid loss of relative accuracy */

	shift = 0.;
    } else {

/*        Compute the shift from 2-by-2 block at end of matrix */

	latime_1.ops += 20;
	if (idir == 1) {
	    sll = (d__1 = d[ll], abs(d__1));
	    dlas2_(&d[m - 1], &e[m - 1], &d[m], &shift, &r);
	} else {
	    sll = (d__1 = d[m], abs(d__1));
	    dlas2_(&d[ll], &e[ll], &d[ll + 1], &shift, &r);
	}

/*        Test if shift negligible, and if so set to zero */

	if (sll > 0.) {
/* Computing 2nd power */
	    d__1 = shift / sll;
	    if (d__1 * d__1 < eps) {
		shift = 0.;
	    }
	}
    }

/*     Increment iteration count */

    iter = iter + m - ll;

/*     If SHIFT = 0, do simplified QR iteration */

    if (shift == 0.) {
	latime_1.ops = latime_1.ops + 2 + (doublereal) (m - ll) * ((*ncvt + *
		nru + *ncc) * 12 + 20);
	if (idir == 1) {

/*           Chase bulge from top to bottom   
             Save cosines and sines for later singular vector upda
tes */

	    cs = 1.;
	    oldcs = 1.;
	    d__1 = d[ll] * cs;
	    dlartg_(&d__1, &e[ll], &cs, &sn, &r);
	    d__1 = oldcs * r;
	    d__2 = d[ll + 1] * sn;
	    dlartg_(&d__1, &d__2, &oldcs, &oldsn, &d[ll]);
	    rwork[1] = cs;
	    rwork[nm1 + 1] = sn;
	    rwork[nm12 + 1] = oldcs;
	    rwork[nm13 + 1] = oldsn;
	    irot = 1;
	    i__1 = m - 1;
	    for (i = ll + 1; i <= i__1; ++i) {
		d__1 = d[i] * cs;
		dlartg_(&d__1, &e[i], &cs, &sn, &r);
		e[i - 1] = oldsn * r;
		d__1 = oldcs * r;
		d__2 = d[i + 1] * sn;
		dlartg_(&d__1, &d__2, &oldcs, &oldsn, &d[i]);
		++irot;
		rwork[irot] = cs;
		rwork[irot + nm1] = sn;
		rwork[irot + nm12] = oldcs;
		rwork[irot + nm13] = oldsn;
/* L110: */
	    }
	    h = d[m] * cs;
	    d[m] = h * oldcs;
	    e[m - 1] = h * oldsn;

/*           Update singular vectors */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "F", &i__1, ncvt, &rwork[1], &rwork[*n], &vt[
			ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		zlasr_("R", "V", "F", nru, &i__1, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &u[ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "F", &i__1, ncc, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &c[ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((d__1 = e[m - 1], abs(d__1)) <= thresh) {
		e[m - 1] = 0.;
	    }

	} else {

/*           Chase bulge from bottom to top   
             Save cosines and sines for later singular vector upda
tes */

	    cs = 1.;
	    oldcs = 1.;
	    d__1 = d[m] * cs;
	    dlartg_(&d__1, &e[m - 1], &cs, &sn, &r);
	    d__1 = oldcs * r;
	    d__2 = d[m - 1] * sn;
	    dlartg_(&d__1, &d__2, &oldcs, &oldsn, &d[m]);
	    rwork[m - ll] = cs;
	    rwork[m - ll + nm1] = -sn;
	    rwork[m - ll + nm12] = oldcs;
	    rwork[m - ll + nm13] = -oldsn;
	    irot = m - ll;
	    i__1 = ll + 1;
	    for (i = m - 1; i >= i__1; --i) {
		d__1 = d[i] * cs;
		dlartg_(&d__1, &e[i - 1], &cs, &sn, &r);
		e[i] = oldsn * r;
		d__1 = oldcs * r;
		d__2 = d[i - 1] * sn;
		dlartg_(&d__1, &d__2, &oldcs, &oldsn, &d[i]);
		--irot;
		rwork[irot] = cs;
		rwork[irot + nm1] = -sn;
		rwork[irot + nm12] = oldcs;
		rwork[irot + nm13] = -oldsn;
/* L120: */
	    }
	    h = d[ll] * cs;
	    d[ll] = h * oldcs;
	    e[ll] = h * oldsn;

/*           Update singular vectors */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "B", &i__1, ncvt, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &vt[ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		zlasr_("R", "V", "B", nru, &i__1, &rwork[1], &rwork[*n], &u[
			ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "B", &i__1, ncc, &rwork[1], &rwork[*n], &c[
			ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((d__1 = e[ll], abs(d__1)) <= thresh) {
		e[ll] = 0.;
	    }
	}
    } else {

/*        Use nonzero shift */

	latime_1.ops = latime_1.ops + 2 + (doublereal) (m - ll) * ((*ncvt + *
		nru + *ncc) * 12 + 32);
	if (idir == 1) {

/*           Chase bulge from top to bottom   
             Save cosines and sines for later singular vector upda
tes */

	    f = ((d__1 = d[ll], abs(d__1)) - shift) * (d_sign(&c_b48, &d[ll]) 
		    + shift / d[ll]);
	    g = e[ll];
	    dlartg_(&f, &g, &cosr, &sinr, &r);
	    f = cosr * d[ll] + sinr * e[ll];
	    e[ll] = cosr * e[ll] - sinr * d[ll];
	    g = sinr * d[ll + 1];
	    d[ll + 1] = cosr * d[ll + 1];
	    dlartg_(&f, &g, &cosl, &sinl, &r);
	    d[ll] = r;
	    f = cosl * e[ll] + sinl * d[ll + 1];
	    d[ll + 1] = cosl * d[ll + 1] - sinl * e[ll];
	    g = sinl * e[ll + 1];
	    e[ll + 1] = cosl * e[ll + 1];
	    rwork[1] = cosr;
	    rwork[nm1 + 1] = sinr;
	    rwork[nm12 + 1] = cosl;
	    rwork[nm13 + 1] = sinl;
	    irot = 1;
	    i__1 = m - 2;
	    for (i = ll + 1; i <= i__1; ++i) {
		dlartg_(&f, &g, &cosr, &sinr, &r);
		e[i - 1] = r;
		f = cosr * d[i] + sinr * e[i];
		e[i] = cosr * e[i] - sinr * d[i];
		g = sinr * d[i + 1];
		d[i + 1] = cosr * d[i + 1];
		dlartg_(&f, &g, &cosl, &sinl, &r);
		d[i] = r;
		f = cosl * e[i] + sinl * d[i + 1];
		d[i + 1] = cosl * d[i + 1] - sinl * e[i];
		g = sinl * e[i + 1];
		e[i + 1] = cosl * e[i + 1];
		++irot;
		rwork[irot] = cosr;
		rwork[irot + nm1] = sinr;
		rwork[irot + nm12] = cosl;
		rwork[irot + nm13] = sinl;
/* L130: */
	    }
	    dlartg_(&f, &g, &cosr, &sinr, &r);
	    e[m - 2] = r;
	    f = cosr * d[m - 1] + sinr * e[m - 1];
	    e[m - 1] = cosr * e[m - 1] - sinr * d[m - 1];
	    g = sinr * d[m];
	    d[m] = cosr * d[m];
	    dlartg_(&f, &g, &cosl, &sinl, &r);
	    d[m - 1] = r;
	    f = cosl * e[m - 1] + sinl * d[m];
	    d[m] = cosl * d[m] - sinl * e[m - 1];
	    ++irot;
	    rwork[irot] = cosr;
	    rwork[irot + nm1] = sinr;
	    rwork[irot + nm12] = cosl;
	    rwork[irot + nm13] = sinl;
	    e[m - 1] = f;

/*           Update singular vectors */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "F", &i__1, ncvt, &rwork[1], &rwork[*n], &vt[
			ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		zlasr_("R", "V", "F", nru, &i__1, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &u[ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "F", &i__1, ncc, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &c[ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((d__1 = e[m - 1], abs(d__1)) <= thresh) {
		e[m - 1] = 0.;
	    }

	} else {

/*           Chase bulge from bottom to top   
             Save cosines and sines for later singular vector upda
tes */

	    f = ((d__1 = d[m], abs(d__1)) - shift) * (d_sign(&c_b48, &d[m]) + 
		    shift / d[m]);
	    g = e[m - 1];
	    dlartg_(&f, &g, &cosr, &sinr, &r);
	    f = cosr * d[m] + sinr * e[m - 1];
	    e[m - 1] = cosr * e[m - 1] - sinr * d[m];
	    g = sinr * d[m - 1];
	    d[m - 1] = cosr * d[m - 1];
	    dlartg_(&f, &g, &cosl, &sinl, &r);
	    d[m] = r;
	    f = cosl * e[m - 1] + sinl * d[m - 1];
	    d[m - 1] = cosl * d[m - 1] - sinl * e[m - 1];
	    g = sinl * e[m - 2];
	    e[m - 2] = cosl * e[m - 2];
	    rwork[m - ll] = cosr;
	    rwork[m - ll + nm1] = -sinr;
	    rwork[m - ll + nm12] = cosl;
	    rwork[m - ll + nm13] = -sinl;
	    irot = m - ll;
	    i__1 = ll + 2;
	    for (i = m - 1; i >= i__1; --i) {
		dlartg_(&f, &g, &cosr, &sinr, &r);
		e[i] = r;
		f = cosr * d[i] + sinr * e[i - 1];
		e[i - 1] = cosr * e[i - 1] - sinr * d[i];
		g = sinr * d[i - 1];
		d[i - 1] = cosr * d[i - 1];
		dlartg_(&f, &g, &cosl, &sinl, &r);
		d[i] = r;
		f = cosl * e[i - 1] + sinl * d[i - 1];
		d[i - 1] = cosl * d[i - 1] - sinl * e[i - 1];
		g = sinl * e[i - 2];
		e[i - 2] = cosl * e[i - 2];
		--irot;
		rwork[irot] = cosr;
		rwork[irot + nm1] = -sinr;
		rwork[irot + nm12] = cosl;
		rwork[irot + nm13] = -sinl;
/* L140: */
	    }
	    dlartg_(&f, &g, &cosr, &sinr, &r);
	    e[ll + 1] = r;
	    f = cosr * d[ll + 1] + sinr * e[ll];
	    e[ll] = cosr * e[ll] - sinr * d[ll + 1];
	    g = sinr * d[ll];
	    d[ll] = cosr * d[ll];
	    dlartg_(&f, &g, &cosl, &sinl, &r);
	    d[ll + 1] = r;
	    f = cosl * e[ll] + sinl * d[ll];
	    d[ll] = cosl * d[ll] - sinl * e[ll];
	    --irot;
	    rwork[irot] = cosr;
	    rwork[irot + nm1] = -sinr;
	    rwork[irot + nm12] = cosl;
	    rwork[irot + nm13] = -sinl;
	    e[ll] = f;

/*           Test convergence */

	    if ((d__1 = e[ll], abs(d__1)) <= thresh) {
		e[ll] = 0.;
	    }

/*           Update singular vectors if desired */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "B", &i__1, ncvt, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &vt[ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		zlasr_("R", "V", "B", nru, &i__1, &rwork[1], &rwork[*n], &u[
			ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		zlasr_("L", "V", "B", &i__1, ncc, &rwork[1], &rwork[*n], &c[
			ll + c_dim1], ldc);
	    }
	}
    }

/*     QR iteration finished, go back and check convergence */

    goto L50;

/*     All singular values converged, so make them positive */

L150:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (d[i] < 0.) {
	    d[i] = -d[i];

/*           Change sign of singular vectors, if desired */

	    latime_1.ops += *ncvt;
	    if (*ncvt > 0) {
		zdscal_(ncvt, &c_b71, &vt[i + vt_dim1], ldvt);
	    }
	}
/* L160: */
    }

/*     Sort the singular values into decreasing order (insertion sort on 
  
       singular values, but only one transposition per singular vector) */

    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {

/*        Scan for smallest D(I) */

	isub = 1;
	smin = d[1];
	i__2 = *n + 1 - i;
	for (j = 2; j <= i__2; ++j) {
	    if (d[j] <= smin) {
		isub = j;
		smin = d[j];
	    }
/* L170: */
	}
	if (isub != *n + 1 - i) {

/*           Swap singular values and vectors */

	    d[isub] = d[*n + 1 - i];
	    d[*n + 1 - i] = smin;
	    if (*ncvt > 0) {
		zswap_(ncvt, &vt[isub + vt_dim1], ldvt, &vt[*n + 1 - i + 
			vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		zswap_(nru, &u[isub * u_dim1 + 1], &c__1, &u[(*n + 1 - i) * 
			u_dim1 + 1], &c__1);
	    }
	    if (*ncc > 0) {
		zswap_(ncc, &c[isub + c_dim1], ldc, &c[*n + 1 - i + c_dim1], 
			ldc);
	    }
	}
/* L180: */
    }
    goto L210;

/*     Maximum number of iterations exceeded, failure to converge */

L190:
    *info = 0;
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
	if (e[i] != 0.) {
	    ++(*info);
	}
/* L200: */
    }
L210:
    return 0;

/*     End of ZBDSQR */

} /* zbdsqr_ */

