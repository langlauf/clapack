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

static doublereal c_b15 = -.125;
static integer c__1 = 1;
static real c_b48 = 1.f;
static real c_b71 = -1.f;

/* Subroutine */ int cbdsqr_(char *uplo, integer *n, integer *ncvt, integer *
	nru, integer *ncc, real *d, real *e, complex *vt, integer *ldvt, 
	complex *u, integer *ldu, complex *c, integer *ldc, real *rwork, 
	integer *info)
{
    /* System generated locals */
    integer c_dim1, c_offset, u_dim1, u_offset, vt_dim1, vt_offset, i__1, 
	    i__2;
    real r__1, r__2, r__3, r__4;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal), r_sign(real *
	    , real *);

    /* Local variables */
    static real abse;
    static integer idir;
    static real abss;
    static integer oldm;
    static real cosl;
    static integer isub, iter;
    static real unfl, sinl, cosr, smin, smax, sinr;
    static integer irot;
    extern /* Subroutine */ int slas2_(real *, real *, real *, real *, real *)
	    ;
    static real f, g, h;
    static integer i, j, m;
    static real r;
    extern logical lsame_(char *, char *);
    static real oldcs;
    extern /* Subroutine */ int clasr_(char *, char *, char *, integer *, 
	    integer *, real *, real *, complex *, integer *);
    static integer oldll;
    static real shift, sigmn, oldsn;
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer maxit;
    static real sminl, sigmx;
    static integer iuplo;
    extern /* Subroutine */ int csrot_(integer *, complex *, integer *, 
	    complex *, integer *, real *, real *), slasq1_(integer *, real *, 
	    real *, real *, integer *), slasv2_(real *, real *, real *, real *
	    , real *, real *, real *, real *, real *);
    static real cs;
    static integer ll;
    static real sn, mu;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int csscal_(integer *, real *, complex *, integer 
	    *), xerbla_(char *, integer *);
    static real sminoa;
    extern /* Subroutine */ int slartg_(real *, real *, real *, real *, real *
	    );
    static real thresh;
    static logical rotate;
    static real sminlo;
    static integer nm1;
    static real tolmul;
    static integer nm12, nm13, lll;
    static real eps, sll, tol;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is initialized to 0, OPS is only incremented   

    Purpose   
    =======   

    CBDSQR computes the singular value decomposition (SVD) of a real   
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

    D       (input/output) REAL array, dimension (N)   
            On entry, the n diagonal elements of the bidiagonal matrix B. 
  
            On exit, if INFO=0, the singular values of B in decreasing   
            order.   

    E       (input/output) REAL array, dimension (N)   
            On entry, the elements of E contain the   
            offdiagonal elements of of the bidiagonal matrix whose SVD   
            is desired. On normal exit (INFO = 0), E is destroyed.   
            If the algorithm does not converge (INFO > 0), D and E   
            will contain the diagonal and superdiagonal elements of a   
            bidiagonal matrix orthogonally equivalent to the one given   
            as input. E(N) is used for workspace.   

    VT      (input/output) COMPLEX array, dimension (LDVT, NCVT)   
            On entry, an N-by-NCVT matrix VT.   
            On exit, VT is overwritten by P' * VT.   
            VT is not referenced if NCVT = 0.   

    LDVT    (input) INTEGER   
            The leading dimension of the array VT.   
            LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.   

    U       (input/output) COMPLEX array, dimension (LDU, N)   
            On entry, an NRU-by-N matrix U.   
            On exit, U is overwritten by U * Q.   
            U is not referenced if NRU = 0.   

    LDU     (input) INTEGER   
            The leading dimension of the array U.  LDU >= max(1,NRU).   

    C       (input/output) COMPLEX array, dimension (LDC, NCC)   
            On entry, an N-by-NCC matrix C.   
            On exit, C is overwritten by Q' * C.   
            C is not referenced if NCC = 0.   

    LDC     (input) INTEGER   
            The leading dimension of the array C.   
            LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.   

    RWORK   (workspace) REAL array, dimension   
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

    TOLMUL  REAL, default = max(10,min(100,EPS**(-1/8)))   
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
	xerbla_("CBDSQR", &i__1);
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
	slasq1_(n, &d[1], &e[1], &rwork[1], info);
	return 0;
    }

    nm1 = *n - 1;
    nm12 = nm1 + nm1;
    nm13 = nm12 + nm1;

/*     Get machine constants */

    eps = slamch_("Epsilon");
    unfl = slamch_("Safe minimum");

/*     If matrix lower bidiagonal, rotate to be upper bidiagonal   
       by applying Givens rotations on the left */

    if (iuplo == 2) {
	latime_1.ops += (real) (*n - 1) * ((*nru + *ncc) * 12 + 8);
	i__1 = *n - 1;
	for (i = 1; i <= i__1; ++i) {
	    slartg_(&d[i], &e[i], &cs, &sn, &r);
	    d[i] = r;
	    e[i] = sn * d[i + 1];
	    d[i + 1] = cs * d[i + 1];
	    rwork[i] = cs;
	    rwork[nm1 + i] = sn;
/* L10: */
	}

/*        Update singular vectors if desired */

	if (*nru > 0) {
	    clasr_("R", "V", "F", nru, n, &rwork[1], &rwork[*n], &u[u_offset],
		     ldu);
	}
	if (*ncc > 0) {
	    clasr_("L", "V", "F", n, ncc, &rwork[1], &rwork[*n], &c[c_offset],
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
    d__1 = (doublereal) eps;
    r__3 = 100.f, r__4 = pow_dd(&d__1, &c_b15);
    r__1 = 10.f, r__2 = dmin(r__3,r__4);
    tolmul = dmax(r__1,r__2);
    tol = tolmul * eps;

/*     Compute approximate maximum, minimum singular values */

    smax = (r__1 = d[*n], dabs(r__1));
    i__1 = *n - 1;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	r__3 = smax, r__4 = (r__1 = d[i], dabs(r__1)), r__3 = max(r__3,r__4), 
		r__4 = (r__2 = e[i], dabs(r__2));
	smax = dmax(r__3,r__4);
/* L20: */
    }
    sminl = 0.f;
    if (tol >= 0.f) {

/*        Relative accuracy desired */

	sminoa = dabs(d[1]);
	if (sminoa == 0.f) {
	    goto L40;
	}
	mu = sminoa;
	latime_1.ops = latime_1.ops + *n * 3 - 1;
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
	    mu = (r__1 = d[i], dabs(r__1)) * (mu / (mu + (r__2 = e[i - 1], 
		    dabs(r__2))));
	    sminoa = dmin(sminoa,mu);
	    if (sminoa == 0.f) {
		goto L40;
	    }
/* L30: */
	}
L40:
	sminoa /= sqrt((real) (*n));
/* Computing MAX */
	r__1 = tol * sminoa, r__2 = *n * 6 * *n * unfl;
	thresh = dmax(r__1,r__2);
    } else {

/*        Absolute accuracy desired   

   Computing MAX */
	r__1 = dabs(tol) * smax, r__2 = *n * 6 * *n * unfl;
	thresh = dmax(r__1,r__2);
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

    if (tol < 0.f && (r__1 = d[m], dabs(r__1)) <= thresh) {
	d[m] = 0.f;
    }
    smax = (r__1 = d[m], dabs(r__1));
    smin = smax;
    i__1 = m;
    for (lll = 1; lll <= i__1; ++lll) {
	ll = m - lll;
	if (ll == 0) {
	    goto L80;
	}
	abss = (r__1 = d[ll], dabs(r__1));
	abse = (r__1 = e[ll], dabs(r__1));
	if (tol < 0.f && abss <= thresh) {
	    d[ll] = 0.f;
	}
	if (abse <= thresh) {
	    goto L70;
	}
	smin = dmin(smin,abss);
/* Computing MAX */
	r__1 = max(smax,abss);
	smax = dmax(r__1,abse);
/* L60: */
    }
L70:
    e[ll] = 0.f;

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
	slasv2_(&d[m - 1], &e[m - 1], &d[m], &sigmn, &sigmx, &sinr, &cosr, &
		sinl, &cosl);
	d[m - 1] = sigmx;
	e[m - 1] = 0.f;
	d[m] = sigmn;

/*        Compute singular vectors, if desired */

	if (*ncvt > 0) {
	    csrot_(ncvt, &vt[m - 1 + vt_dim1], ldvt, &vt[m + vt_dim1], ldvt, &
		    cosr, &sinr);
	}
	if (*nru > 0) {
	    csrot_(nru, &u[(m - 1) * u_dim1 + 1], &c__1, &u[m * u_dim1 + 1], &
		    c__1, &cosl, &sinl);
	}
	if (*ncc > 0) {
	    csrot_(ncc, &c[m - 1 + c_dim1], ldc, &c[m + c_dim1], ldc, &cosl, &
		    sinl);
	}
	m += -2;
	goto L50;
    }

/*     If working on new submatrix, choose shift direction   
       (from larger end diagonal element towards smaller) */

    if (ll > oldm || m < oldll) {
	if ((r__1 = d[ll], dabs(r__1)) >= (r__2 = d[m], dabs(r__2))) {

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
	if ((r__1 = e[m - 1], dabs(r__1)) <= dabs(tol) * (r__2 = d[m], dabs(
		r__2)) || tol < 0.f && (r__3 = e[m - 1], dabs(r__3)) <= 
		thresh) {
	    e[m - 1] = 0.f;
	    goto L50;
	}

	if (tol >= 0.f) {

/*           If relative accuracy desired,   
             apply convergence criterion forward */

	    mu = (r__1 = d[ll], dabs(r__1));
	    sminl = mu;
	    i__1 = m - 1;
	    for (lll = ll; lll <= i__1; ++lll) {
		if ((r__1 = e[lll], dabs(r__1)) <= tol * mu) {
		    e[lll] = 0.f;
		    goto L50;
		}
		sminlo = sminl;
		latime_1.ops += 4;
		mu = (r__1 = d[lll + 1], dabs(r__1)) * (mu / (mu + (r__2 = e[
			lll], dabs(r__2))));
		sminl = dmin(sminl,mu);
/* L90: */
	    }
	}

    } else {

/*        Run convergence test in backward direction   
          First apply standard test to top of matrix */

	latime_1.ops += 1;
	if ((r__1 = e[ll], dabs(r__1)) <= dabs(tol) * (r__2 = d[ll], dabs(
		r__2)) || tol < 0.f && (r__3 = e[ll], dabs(r__3)) <= thresh) {
	    e[ll] = 0.f;
	    goto L50;
	}

	if (tol >= 0.f) {

/*           If relative accuracy desired,   
             apply convergence criterion backward */

	    mu = (r__1 = d[m], dabs(r__1));
	    sminl = mu;
	    i__1 = ll;
	    for (lll = m - 1; lll >= i__1; --lll) {
		if ((r__1 = e[lll], dabs(r__1)) <= tol * mu) {
		    e[lll] = 0.f;
		    goto L50;
		}
		sminlo = sminl;
		latime_1.ops += 4;
		mu = (r__1 = d[lll], dabs(r__1)) * (mu / (mu + (r__2 = e[lll],
			 dabs(r__2))));
		sminl = dmin(sminl,mu);
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
    r__1 = eps, r__2 = tol * .01f;
    if (tol >= 0.f && *n * tol * (sminl / smax) <= dmax(r__1,r__2)) {

/*        Use a zero shift to avoid loss of relative accuracy */

	shift = 0.f;
    } else {

/*        Compute the shift from 2-by-2 block at end of matrix */

	latime_1.ops += 20;
	if (idir == 1) {
	    sll = (r__1 = d[ll], dabs(r__1));
	    slas2_(&d[m - 1], &e[m - 1], &d[m], &shift, &r);
	} else {
	    sll = (r__1 = d[m], dabs(r__1));
	    slas2_(&d[ll], &e[ll], &d[ll + 1], &shift, &r);
	}

/*        Test if shift negligible, and if so set to zero */

	if (sll > 0.f) {
/* Computing 2nd power */
	    r__1 = shift / sll;
	    if (r__1 * r__1 < eps) {
		shift = 0.f;
	    }
	}
    }

/*     Increment iteration count */

    iter = iter + m - ll;

/*     If SHIFT = 0, do simplified QR iteration */

    if (shift == 0.f) {
	latime_1.ops = latime_1.ops + 2 + (real) (m - ll) * ((*ncvt + *nru + *
		ncc) * 12 + 20);
	if (idir == 1) {

/*           Chase bulge from top to bottom   
             Save cosines and sines for later singular vector upda
tes */

	    cs = 1.f;
	    oldcs = 1.f;
	    r__1 = d[ll] * cs;
	    slartg_(&r__1, &e[ll], &cs, &sn, &r);
	    r__1 = oldcs * r;
	    r__2 = d[ll + 1] * sn;
	    slartg_(&r__1, &r__2, &oldcs, &oldsn, &d[ll]);
	    rwork[1] = cs;
	    rwork[nm1 + 1] = sn;
	    rwork[nm12 + 1] = oldcs;
	    rwork[nm13 + 1] = oldsn;
	    irot = 1;
	    i__1 = m - 1;
	    for (i = ll + 1; i <= i__1; ++i) {
		r__1 = d[i] * cs;
		slartg_(&r__1, &e[i], &cs, &sn, &r);
		e[i - 1] = oldsn * r;
		r__1 = oldcs * r;
		r__2 = d[i + 1] * sn;
		slartg_(&r__1, &r__2, &oldcs, &oldsn, &d[i]);
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
		clasr_("L", "V", "F", &i__1, ncvt, &rwork[1], &rwork[*n], &vt[
			ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		clasr_("R", "V", "F", nru, &i__1, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &u[ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "F", &i__1, ncc, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &c[ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((r__1 = e[m - 1], dabs(r__1)) <= thresh) {
		e[m - 1] = 0.f;
	    }

	} else {

/*           Chase bulge from bottom to top   
             Save cosines and sines for later singular vector upda
tes */

	    cs = 1.f;
	    oldcs = 1.f;
	    r__1 = d[m] * cs;
	    slartg_(&r__1, &e[m - 1], &cs, &sn, &r);
	    r__1 = oldcs * r;
	    r__2 = d[m - 1] * sn;
	    slartg_(&r__1, &r__2, &oldcs, &oldsn, &d[m]);
	    rwork[m - ll] = cs;
	    rwork[m - ll + nm1] = -(doublereal)sn;
	    rwork[m - ll + nm12] = oldcs;
	    rwork[m - ll + nm13] = -(doublereal)oldsn;
	    irot = m - ll;
	    i__1 = ll + 1;
	    for (i = m - 1; i >= i__1; --i) {
		r__1 = d[i] * cs;
		slartg_(&r__1, &e[i - 1], &cs, &sn, &r);
		e[i] = oldsn * r;
		r__1 = oldcs * r;
		r__2 = d[i - 1] * sn;
		slartg_(&r__1, &r__2, &oldcs, &oldsn, &d[i]);
		--irot;
		rwork[irot] = cs;
		rwork[irot + nm1] = -(doublereal)sn;
		rwork[irot + nm12] = oldcs;
		rwork[irot + nm13] = -(doublereal)oldsn;
/* L120: */
	    }
	    h = d[ll] * cs;
	    d[ll] = h * oldcs;
	    e[ll] = h * oldsn;

/*           Update singular vectors */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "B", &i__1, ncvt, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &vt[ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		clasr_("R", "V", "B", nru, &i__1, &rwork[1], &rwork[*n], &u[
			ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "B", &i__1, ncc, &rwork[1], &rwork[*n], &c[
			ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((r__1 = e[ll], dabs(r__1)) <= thresh) {
		e[ll] = 0.f;
	    }
	}
    } else {

/*        Use nonzero shift */

	latime_1.ops = latime_1.ops + 2 + (real) (m - ll) * ((*ncvt + *nru + *
		ncc) * 12 + 32);
	if (idir == 1) {

/*           Chase bulge from top to bottom   
             Save cosines and sines for later singular vector upda
tes */

	    f = ((r__1 = d[ll], dabs(r__1)) - shift) * (r_sign(&c_b48, &d[ll])
		     + shift / d[ll]);
	    g = e[ll];
	    slartg_(&f, &g, &cosr, &sinr, &r);
	    f = cosr * d[ll] + sinr * e[ll];
	    e[ll] = cosr * e[ll] - sinr * d[ll];
	    g = sinr * d[ll + 1];
	    d[ll + 1] = cosr * d[ll + 1];
	    slartg_(&f, &g, &cosl, &sinl, &r);
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
		slartg_(&f, &g, &cosr, &sinr, &r);
		e[i - 1] = r;
		f = cosr * d[i] + sinr * e[i];
		e[i] = cosr * e[i] - sinr * d[i];
		g = sinr * d[i + 1];
		d[i + 1] = cosr * d[i + 1];
		slartg_(&f, &g, &cosl, &sinl, &r);
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
	    slartg_(&f, &g, &cosr, &sinr, &r);
	    e[m - 2] = r;
	    f = cosr * d[m - 1] + sinr * e[m - 1];
	    e[m - 1] = cosr * e[m - 1] - sinr * d[m - 1];
	    g = sinr * d[m];
	    d[m] = cosr * d[m];
	    slartg_(&f, &g, &cosl, &sinl, &r);
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
		clasr_("L", "V", "F", &i__1, ncvt, &rwork[1], &rwork[*n], &vt[
			ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		clasr_("R", "V", "F", nru, &i__1, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &u[ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "F", &i__1, ncc, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &c[ll + c_dim1], ldc);
	    }

/*           Test convergence */

	    if ((r__1 = e[m - 1], dabs(r__1)) <= thresh) {
		e[m - 1] = 0.f;
	    }

	} else {

/*           Chase bulge from bottom to top   
             Save cosines and sines for later singular vector upda
tes */

	    f = ((r__1 = d[m], dabs(r__1)) - shift) * (r_sign(&c_b48, &d[m]) 
		    + shift / d[m]);
	    g = e[m - 1];
	    slartg_(&f, &g, &cosr, &sinr, &r);
	    f = cosr * d[m] + sinr * e[m - 1];
	    e[m - 1] = cosr * e[m - 1] - sinr * d[m];
	    g = sinr * d[m - 1];
	    d[m - 1] = cosr * d[m - 1];
	    slartg_(&f, &g, &cosl, &sinl, &r);
	    d[m] = r;
	    f = cosl * e[m - 1] + sinl * d[m - 1];
	    d[m - 1] = cosl * d[m - 1] - sinl * e[m - 1];
	    g = sinl * e[m - 2];
	    e[m - 2] = cosl * e[m - 2];
	    rwork[m - ll] = cosr;
	    rwork[m - ll + nm1] = -(doublereal)sinr;
	    rwork[m - ll + nm12] = cosl;
	    rwork[m - ll + nm13] = -(doublereal)sinl;
	    irot = m - ll;
	    i__1 = ll + 2;
	    for (i = m - 1; i >= i__1; --i) {
		slartg_(&f, &g, &cosr, &sinr, &r);
		e[i] = r;
		f = cosr * d[i] + sinr * e[i - 1];
		e[i - 1] = cosr * e[i - 1] - sinr * d[i];
		g = sinr * d[i - 1];
		d[i - 1] = cosr * d[i - 1];
		slartg_(&f, &g, &cosl, &sinl, &r);
		d[i] = r;
		f = cosl * e[i - 1] + sinl * d[i - 1];
		d[i - 1] = cosl * d[i - 1] - sinl * e[i - 1];
		g = sinl * e[i - 2];
		e[i - 2] = cosl * e[i - 2];
		--irot;
		rwork[irot] = cosr;
		rwork[irot + nm1] = -(doublereal)sinr;
		rwork[irot + nm12] = cosl;
		rwork[irot + nm13] = -(doublereal)sinl;
/* L140: */
	    }
	    slartg_(&f, &g, &cosr, &sinr, &r);
	    e[ll + 1] = r;
	    f = cosr * d[ll + 1] + sinr * e[ll];
	    e[ll] = cosr * e[ll] - sinr * d[ll + 1];
	    g = sinr * d[ll];
	    d[ll] = cosr * d[ll];
	    slartg_(&f, &g, &cosl, &sinl, &r);
	    d[ll + 1] = r;
	    f = cosl * e[ll] + sinl * d[ll];
	    d[ll] = cosl * d[ll] - sinl * e[ll];
	    --irot;
	    rwork[irot] = cosr;
	    rwork[irot + nm1] = -(doublereal)sinr;
	    rwork[irot + nm12] = cosl;
	    rwork[irot + nm13] = -(doublereal)sinl;
	    e[ll] = f;

/*           Test convergence */

	    if ((r__1 = e[ll], dabs(r__1)) <= thresh) {
		e[ll] = 0.f;
	    }

/*           Update singular vectors if desired */

	    if (*ncvt > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "B", &i__1, ncvt, &rwork[nm12 + 1], &rwork[
			nm13 + 1], &vt[ll + vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		i__1 = m - ll + 1;
		clasr_("R", "V", "B", nru, &i__1, &rwork[1], &rwork[*n], &u[
			ll * u_dim1 + 1], ldu);
	    }
	    if (*ncc > 0) {
		i__1 = m - ll + 1;
		clasr_("L", "V", "B", &i__1, ncc, &rwork[1], &rwork[*n], &c[
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
	if (d[i] < 0.f) {
	    d[i] = -(doublereal)d[i];

/*           Change sign of singular vectors, if desired */

	    latime_1.ops += *ncvt;
	    if (*ncvt > 0) {
		csscal_(ncvt, &c_b71, &vt[i + vt_dim1], ldvt);
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
		cswap_(ncvt, &vt[isub + vt_dim1], ldvt, &vt[*n + 1 - i + 
			vt_dim1], ldvt);
	    }
	    if (*nru > 0) {
		cswap_(nru, &u[isub * u_dim1 + 1], &c__1, &u[(*n + 1 - i) * 
			u_dim1 + 1], &c__1);
	    }
	    if (*ncc > 0) {
		cswap_(ncc, &c[isub + c_dim1], ldc, &c[*n + 1 - i + c_dim1], 
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
	if (e[i] != 0.f) {
	    ++(*info);
	}
/* L200: */
    }
L210:
    return 0;

/*     End of CBDSQR */

} /* cbdsqr_ */

