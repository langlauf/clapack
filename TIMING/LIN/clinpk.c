/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static complex c_b7 = {-1.f,0.f};

/* Subroutine */ int cgefa_(complex *a, integer *lda, integer *n, integer *
	ipvt, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;
    complex q__1;

    /* Builtin functions */
    double r_imag(complex *);
    void c_div(complex *, complex *, complex *);

    /* Local variables */
    static integer j, k, l;
    static complex t;
    extern /* Subroutine */ int cscal_(integer *, complex *, complex *, 
	    integer *), caxpy_(integer *, complex *, complex *, integer *, 
	    complex *, integer *);
    extern integer icamax_(integer *, complex *, integer *);
    static integer kp1, nm1;


/*     CGEFA FACTORS A COMPLEX MATRIX BY GAUSSIAN ELIMINATION.   

       CGEFA IS USUALLY CALLED BY CGECO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR CGECO) = (1 + 9/N)*(TIME FOR CGEFA) .   

       ON ENTRY   

          A       COMPLEX(LDA, N)   
                  THE MATRIX TO BE FACTORED.   

          LDA     INTEGER   
                  THE LEADING DIMENSION OF THE ARRAY  A .   

          N       INTEGER   
                  THE ORDER OF THE MATRIX  A .   

       ON RETURN   

          A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS   
                  WHICH WERE USED TO OBTAIN IT.   
                  THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE   
                  L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER   
                  TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.   

          IPVT    INTEGER(N)   
                  AN INTEGER VECTOR OF PIVOT INDICES.   

          INFO    INTEGER   
                  = 0  NORMAL VALUE.   
                  = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR   
                       CONDITION FOR THIS SUBROUTINE, BUT IT DOES   
                       INDICATE THAT CGESL OR CGEDI WILL DIVIDE BY ZERO   
                       IF CALLED.  USE  RCOND  IN CGECO FOR A RELIABLE   
                       INDICATION OF SINGULARITY.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS CAXPY,CSCAL,ICAMAX   
       FORTRAN ABS,AIMAG,REAL   

       INTERNAL VARIABLES   



       GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING   

       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --ipvt;

    /* Function Body */
    *info = 0;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L70;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        FIND L = PIVOT INDEX */

	i__2 = *n - k + 1;
	l = icamax_(&i__2, &a[k + k * a_dim1], &c__1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	i__2 = l + k * a_dim1;
	if ((r__1 = a[i__2].r, dabs(r__1)) + (r__2 = r_imag(&a[l + k * a_dim1]
		), dabs(r__2)) == 0.f) {
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	i__2 = l + k * a_dim1;
	t.r = a[i__2].r, t.i = a[i__2].i;
	i__2 = l + k * a_dim1;
	i__3 = k + k * a_dim1;
	a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
	i__2 = k + k * a_dim1;
	a[i__2].r = t.r, a[i__2].i = t.i;
L10:

/*           COMPUTE MULTIPLIERS */

	c_div(&q__1, &c_b7, &a[k + k * a_dim1]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = *n - k;
	cscal_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    i__3 = l + j * a_dim1;
	    t.r = a[i__3].r, t.i = a[i__3].i;
	    if (l == k) {
		goto L20;
	    }
	    i__3 = l + j * a_dim1;
	    i__4 = k + j * a_dim1;
	    a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
	    i__3 = k + j * a_dim1;
	    a[i__3].r = t.r, a[i__3].i = t.i;
L20:
	    i__3 = *n - k;
	    caxpy_(&i__3, &t, &a[k + 1 + k * a_dim1], &c__1, &a[k + 1 + j * 
		    a_dim1], &c__1);
/* L30: */
	}
	goto L50;
L40:
	*info = k;
L50:
/* L60: */
	;
    }
L70:
    ipvt[*n] = *n;
    i__1 = *n + *n * a_dim1;
    if ((r__1 = a[i__1].r, dabs(r__1)) + (r__2 = r_imag(&a[*n + *n * a_dim1]),
	     dabs(r__2)) == 0.f) {
	*info = *n;
    }
    return 0;
} /* cgefa_   

   Subroutine */ int cpofa_(complex *a, integer *lda, integer *n, integer *
	info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1;
    complex q__1, q__2;

    /* Builtin functions */
    void c_div(complex *, complex *, complex *), r_cnjg(complex *, complex *);
    double r_imag(complex *), sqrt(doublereal);

    /* Local variables */
    static integer j, k;
    static real s;
    static complex t;
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    static integer jm1;


/*     CPOFA FACTORS A COMPLEX HERMITIAN POSITIVE DEFINITE MATRIX.   

       CPOFA IS USUALLY CALLED BY CPOCO, BUT IT CAN BE CALLED   
       DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.   
       (TIME FOR CPOCO) = (1 + 18/N)*(TIME FOR CPOFA) .   

       ON ENTRY   

          A       COMPLEX(LDA, N)   
                  THE HERMITIAN MATRIX TO BE FACTORED.  ONLY THE   
                  DIAGONAL AND UPPER TRIANGLE ARE USED.   

          LDA     INTEGER   
                  THE LEADING DIMENSION OF THE ARRAY  A .   

          N       INTEGER   
                  THE ORDER OF THE MATRIX  A .   

       ON RETURN   

          A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A =   
                  CTRANS(R)*R WHERE  CTRANS(R)  IS THE CONJUGATE   
                  TRANSPOSE.  THE STRICT LOWER TRIANGLE IS UNALTERED.   
                  IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   

          INFO    INTEGER   
                  = 0  FOR NORMAL RETURN.   
                  = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR   
                       OF ORDER  K  IS NOT POSITIVE DEFINITE.   

       LINPACK.  THIS VERSION DATED 08/14/78 .   
       CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.   

       SUBROUTINES AND FUNCTIONS   

       BLAS CDOTC   
       FORTRAN AIMAG,CMPLX,CONJG,REAL,SQRT   

       INTERNAL VARIABLES   

       BEGIN BLOCK WITH ...EXITS TO 40   


       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	*info = j;
	s = 0.f;
	jm1 = j - 1;
	if (jm1 < 1) {
	    goto L20;
	}
	i__2 = jm1;
	for (k = 1; k <= i__2; ++k) {
	    i__3 = k + j * a_dim1;
	    i__4 = k - 1;
	    cdotc_(&q__2, &i__4, &a[k * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1]
		    , &c__1);
	    q__1.r = a[i__3].r - q__2.r, q__1.i = a[i__3].i - q__2.i;
	    t.r = q__1.r, t.i = q__1.i;
	    c_div(&q__1, &t, &a[k + k * a_dim1]);
	    t.r = q__1.r, t.i = q__1.i;
	    i__3 = k + j * a_dim1;
	    a[i__3].r = t.r, a[i__3].i = t.i;
	    r_cnjg(&q__2, &t);
	    q__1.r = t.r * q__2.r - t.i * q__2.i, q__1.i = t.r * q__2.i + t.i 
		    * q__2.r;
	    s += q__1.r;
/* L10: */
	}
L20:
	i__2 = j + j * a_dim1;
	s = a[i__2].r - s;
/*     ......EXIT */
	if (s <= 0.f || r_imag(&a[j + j * a_dim1]) != 0.f) {
	    goto L40;
	}
	i__2 = j + j * a_dim1;
	d__1 = sqrt(s);
	q__1.r = d__1, q__1.i = 0.f;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
/* L30: */
    }
    *info = 0;
L40:
    return 0;
} /* cpofa_   

   Subroutine */ int cgtsl_(integer *n, complex *c, complex *d, complex *e, 
	complex *b, integer *info)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2, q__3, q__4, q__5;

    /* Builtin functions */
    double r_imag(complex *);
    void c_div(complex *, complex *, complex *);

    /* Local variables */
    static integer k;
    static complex t;
    static integer kb, kp1, nm1, nm2;


/*     CGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND   
       SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N       INTEGER   
                  IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          C       COMPLEX(N)   
                  IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.   
                  ON OUTPUT C IS DESTROYED.   

          D       COMPLEX(N)   
                  IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  ON OUTPUT D IS DESTROYED.   

          E       COMPLEX(N)   
                  IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                  E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.   
                  ON OUTPUT E IS DESTROYED.   

          B       COMPLEX(N)   
                  IS THE RIGHT HAND SIDE VECTOR.   

       ON RETURN   

          B       IS THE SOLUTION VECTOR.   

          INFO    INTEGER   
                  = 0 NORMAL VALUE.   
                  = K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES   
                      EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN   
                      THIS IS DETECTED.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       JACK DONGARRA, ARGONNE NATIONAL LABORATORY.   

       NO EXTERNALS   
       FORTRAN ABS,AIMAG,REAL   

       INTERNAL VARIABLES   

       BEGIN BLOCK PERMITTING ...EXITS TO 100   

       Parameter adjustments */
    --b;
    --e;
    --d;
    --c;

    /* Function Body */
    *info = 0;
    c[1].r = d[1].r, c[1].i = d[1].i;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L40;
    }
    d[1].r = e[1].r, d[1].i = e[1].i;
    e[1].r = 0.f, e[1].i = 0.f;
    i__1 = *n;
    e[i__1].r = 0.f, e[i__1].i = 0.f;

    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*              FIND THE LARGEST OF THE TWO ROWS */

	i__2 = kp1;
	i__3 = k;
	if ((r__1 = c[i__2].r, dabs(r__1)) + (r__2 = r_imag(&c[kp1]), dabs(
		r__2)) < (r__3 = c[i__3].r, dabs(r__3)) + (r__4 = r_imag(&c[k]
		), dabs(r__4))) {
	    goto L10;
	}

/*                 INTERCHANGE ROW */

	i__2 = kp1;
	t.r = c[i__2].r, t.i = c[i__2].i;
	i__2 = kp1;
	i__3 = k;
	c[i__2].r = c[i__3].r, c[i__2].i = c[i__3].i;
	i__2 = k;
	c[i__2].r = t.r, c[i__2].i = t.i;
	i__2 = kp1;
	t.r = d[i__2].r, t.i = d[i__2].i;
	i__2 = kp1;
	i__3 = k;
	d[i__2].r = d[i__3].r, d[i__2].i = d[i__3].i;
	i__2 = k;
	d[i__2].r = t.r, d[i__2].i = t.i;
	i__2 = kp1;
	t.r = e[i__2].r, t.i = e[i__2].i;
	i__2 = kp1;
	i__3 = k;
	e[i__2].r = e[i__3].r, e[i__2].i = e[i__3].i;
	i__2 = k;
	e[i__2].r = t.r, e[i__2].i = t.i;
	i__2 = kp1;
	t.r = b[i__2].r, t.i = b[i__2].i;
	i__2 = kp1;
	i__3 = k;
	b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
	i__2 = k;
	b[i__2].r = t.r, b[i__2].i = t.i;
L10:

/*              ZERO ELEMENTS */

	i__2 = k;
	if ((r__1 = c[i__2].r, dabs(r__1)) + (r__2 = r_imag(&c[k]), dabs(r__2)
		) != 0.f) {
	    goto L20;
	}
	*info = k;
/*     ............EXIT */
	goto L100;
L20:
	i__2 = kp1;
	q__2.r = -(doublereal)c[i__2].r, q__2.i = -(doublereal)c[i__2].i;
	c_div(&q__1, &q__2, &c[k]);
	t.r = q__1.r, t.i = q__1.i;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	q__2.r = t.r * d[i__4].r - t.i * d[i__4].i, q__2.i = t.r * d[i__4].i 
		+ t.i * d[i__4].r;
	q__1.r = d[i__3].r + q__2.r, q__1.i = d[i__3].i + q__2.i;
	c[i__2].r = q__1.r, c[i__2].i = q__1.i;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	q__2.r = t.r * e[i__4].r - t.i * e[i__4].i, q__2.i = t.r * e[i__4].i 
		+ t.i * e[i__4].r;
	q__1.r = e[i__3].r + q__2.r, q__1.i = e[i__3].i + q__2.i;
	d[i__2].r = q__1.r, d[i__2].i = q__1.i;
	i__2 = kp1;
	e[i__2].r = 0.f, e[i__2].i = 0.f;
	i__2 = kp1;
	i__3 = kp1;
	i__4 = k;
	q__2.r = t.r * b[i__4].r - t.i * b[i__4].i, q__2.i = t.r * b[i__4].i 
		+ t.i * b[i__4].r;
	q__1.r = b[i__3].r + q__2.r, q__1.i = b[i__3].i + q__2.i;
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
/* L30: */
    }
L40:
    i__1 = *n;
    if ((r__1 = c[i__1].r, dabs(r__1)) + (r__2 = r_imag(&c[*n]), dabs(r__2)) 
	    != 0.f) {
	goto L50;
    }
    *info = *n;
    goto L90;
L50:

/*           BACK SOLVE */

    nm2 = *n - 2;
    i__1 = *n;
    c_div(&q__1, &b[*n], &c[*n]);
    b[i__1].r = q__1.r, b[i__1].i = q__1.i;
    if (*n == 1) {
	goto L80;
    }
    i__1 = nm1;
    i__2 = nm1;
    i__3 = nm1;
    i__4 = *n;
    q__3.r = d[i__3].r * b[i__4].r - d[i__3].i * b[i__4].i, q__3.i = d[i__3]
	    .r * b[i__4].i + d[i__3].i * b[i__4].r;
    q__2.r = b[i__2].r - q__3.r, q__2.i = b[i__2].i - q__3.i;
    c_div(&q__1, &q__2, &c[nm1]);
    b[i__1].r = q__1.r, b[i__1].i = q__1.i;
    if (nm2 < 1) {
	goto L70;
    }
    i__1 = nm2;
    for (kb = 1; kb <= i__1; ++kb) {
	k = nm2 - kb + 1;
	i__2 = k;
	i__3 = k;
	i__4 = k;
	i__5 = k + 1;
	q__4.r = d[i__4].r * b[i__5].r - d[i__4].i * b[i__5].i, q__4.i = d[
		i__4].r * b[i__5].i + d[i__4].i * b[i__5].r;
	q__3.r = b[i__3].r - q__4.r, q__3.i = b[i__3].i - q__4.i;
	i__6 = k;
	i__7 = k + 2;
	q__5.r = e[i__6].r * b[i__7].r - e[i__6].i * b[i__7].i, q__5.i = e[
		i__6].r * b[i__7].i + e[i__6].i * b[i__7].r;
	q__2.r = q__3.r - q__5.r, q__2.i = q__3.i - q__5.i;
	c_div(&q__1, &q__2, &c[k]);
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
/* L60: */
    }
L70:
L80:
L90:
L100:

    return 0;
} /* cgtsl_   

   Subroutine */ int cptsl_(integer *n, complex *d, complex *e, complex *b)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    complex q__1, q__2, q__3, q__4;

    /* Builtin functions */
    void c_div(complex *, complex *, complex *), r_cnjg(complex *, complex *);

    /* Local variables */
    static integer nm1d2, k;
    static complex t1, t2;
    static integer ke, kf, kp1, nm1, kbm1;


/*     CPTSL GIVEN A POSITIVE DEFINITE TRIDIAGONAL MATRIX AND A RIGHT   
       HAND SIDE WILL FIND THE SOLUTION.   

       ON ENTRY   

          N        INTEGER   
                   IS THE ORDER OF THE TRIDIAGONAL MATRIX.   

          D        COMPLEX(N)   
                   IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   ON OUTPUT D IS DESTROYED.   

          E        COMPLEX(N)   
                   IS THE OFFDIAGONAL OF THE TRIDIAGONAL MATRIX.   
                   E(1) THROUGH E(N-1) SHOULD CONTAIN THE   
                   OFFDIAGONAL.   

          B        COMPLEX(N)   
                   IS THE RIGHT HAND SIDE VECTOR.   

       ON RETURN   

          B        CONTAINS THE SOULTION.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
       JACK DONGARRA, ARGONNE NATIONAL LABORATORY.   

       NO EXTERNALS   
       FORTRAN CONJG,MOD   

       INTERNAL VARIABLES   


       CHECK FOR 1 X 1 CASE   

       Parameter adjustments */
    --b;
    --e;
    --d;

    /* Function Body */
    if (*n != 1) {
	goto L10;
    }
    c_div(&q__1, &b[1], &d[1]);
    b[1].r = q__1.r, b[1].i = q__1.i;
    goto L70;
L10:
    nm1 = *n - 1;
    nm1d2 = nm1 / 2;
    if (*n == 2) {
	goto L30;
    }
    kbm1 = *n - 1;

/*           ZERO TOP HALF OF SUBDIAGONAL AND BOTTOM HALF OF   
             SUPERDIAGONAL */

    i__1 = nm1d2;
    for (k = 1; k <= i__1; ++k) {
	r_cnjg(&q__2, &e[k]);
	c_div(&q__1, &q__2, &d[k]);
	t1.r = q__1.r, t1.i = q__1.i;
	i__2 = k + 1;
	i__3 = k + 1;
	i__4 = k;
	q__2.r = t1.r * e[i__4].r - t1.i * e[i__4].i, q__2.i = t1.r * e[i__4]
		.i + t1.i * e[i__4].r;
	q__1.r = d[i__3].r - q__2.r, q__1.i = d[i__3].i - q__2.i;
	d[i__2].r = q__1.r, d[i__2].i = q__1.i;
	i__2 = k + 1;
	i__3 = k + 1;
	i__4 = k;
	q__2.r = t1.r * b[i__4].r - t1.i * b[i__4].i, q__2.i = t1.r * b[i__4]
		.i + t1.i * b[i__4].r;
	q__1.r = b[i__3].r - q__2.r, q__1.i = b[i__3].i - q__2.i;
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
	c_div(&q__1, &e[kbm1], &d[kbm1 + 1]);
	t2.r = q__1.r, t2.i = q__1.i;
	i__2 = kbm1;
	i__3 = kbm1;
	r_cnjg(&q__3, &e[kbm1]);
	q__2.r = t2.r * q__3.r - t2.i * q__3.i, q__2.i = t2.r * q__3.i + t2.i 
		* q__3.r;
	q__1.r = d[i__3].r - q__2.r, q__1.i = d[i__3].i - q__2.i;
	d[i__2].r = q__1.r, d[i__2].i = q__1.i;
	i__2 = kbm1;
	i__3 = kbm1;
	i__4 = kbm1 + 1;
	q__2.r = t2.r * b[i__4].r - t2.i * b[i__4].i, q__2.i = t2.r * b[i__4]
		.i + t2.i * b[i__4].r;
	q__1.r = b[i__3].r - q__2.r, q__1.i = b[i__3].i - q__2.i;
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
	--kbm1;
/* L20: */
    }
L30:
    kp1 = nm1d2 + 1;

/*        CLEAN UP FOR POSSIBLE 2 X 2 BLOCK AT CENTER */

    if (*n % 2 != 0) {
	goto L40;
    }
    r_cnjg(&q__2, &e[kp1]);
    c_div(&q__1, &q__2, &d[kp1]);
    t1.r = q__1.r, t1.i = q__1.i;
    i__1 = kp1 + 1;
    i__2 = kp1 + 1;
    i__3 = kp1;
    q__2.r = t1.r * e[i__3].r - t1.i * e[i__3].i, q__2.i = t1.r * e[i__3].i + 
	    t1.i * e[i__3].r;
    q__1.r = d[i__2].r - q__2.r, q__1.i = d[i__2].i - q__2.i;
    d[i__1].r = q__1.r, d[i__1].i = q__1.i;
    i__1 = kp1 + 1;
    i__2 = kp1 + 1;
    i__3 = kp1;
    q__2.r = t1.r * b[i__3].r - t1.i * b[i__3].i, q__2.i = t1.r * b[i__3].i + 
	    t1.i * b[i__3].r;
    q__1.r = b[i__2].r - q__2.r, q__1.i = b[i__2].i - q__2.i;
    b[i__1].r = q__1.r, b[i__1].i = q__1.i;
    ++kp1;
L40:

/*        BACK SOLVE STARTING AT THE CENTER, GOING TOWARDS THE TOP   
          AND BOTTOM */

    i__1 = kp1;
    c_div(&q__1, &b[kp1], &d[kp1]);
    b[i__1].r = q__1.r, b[i__1].i = q__1.i;
    if (*n == 2) {
	goto L60;
    }
    k = kp1 - 1;
    ke = kp1 + nm1d2 - 1;
    i__1 = ke;
    for (kf = kp1; kf <= i__1; ++kf) {
	i__2 = k;
	i__3 = k;
	i__4 = k;
	i__5 = k + 1;
	q__3.r = e[i__4].r * b[i__5].r - e[i__4].i * b[i__5].i, q__3.i = e[
		i__4].r * b[i__5].i + e[i__4].i * b[i__5].r;
	q__2.r = b[i__3].r - q__3.r, q__2.i = b[i__3].i - q__3.i;
	c_div(&q__1, &q__2, &d[k]);
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
	i__2 = kf + 1;
	i__3 = kf + 1;
	r_cnjg(&q__4, &e[kf]);
	i__4 = kf;
	q__3.r = q__4.r * b[i__4].r - q__4.i * b[i__4].i, q__3.i = q__4.r * b[
		i__4].i + q__4.i * b[i__4].r;
	q__2.r = b[i__3].r - q__3.r, q__2.i = b[i__3].i - q__3.i;
	c_div(&q__1, &q__2, &d[kf + 1]);
	b[i__2].r = q__1.r, b[i__2].i = q__1.i;
	--k;
/* L50: */
    }
L60:
    if (*n % 2 == 0) {
	q__3.r = e[1].r * b[2].r - e[1].i * b[2].i, q__3.i = e[1].r * b[2].i 
		+ e[1].i * b[2].r;
	q__2.r = b[1].r - q__3.r, q__2.i = b[1].i - q__3.i;
	c_div(&q__1, &q__2, &d[1]);
	b[1].r = q__1.r, b[1].i = q__1.i;
    }
L70:
    return 0;
} /* cptsl_ */

