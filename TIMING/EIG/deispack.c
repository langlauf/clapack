/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

union {
    struct {
	doublereal ops, itcnt;
    } _1;
    struct {
	doublereal iops, itcnt;
    } _2;
} latime_;

#define latime_1 (latime_._1)
#define latime_2 (latime_._2)

struct {
    doublereal opst;
} pythop_;

#define pythop_1 pythop_

/* Table of constant values */

static doublereal c_b90 = 0.;
static doublereal c_b114 = 1.;
static integer c__1 = 1;
static doublereal c_b405 = -1.;

/* Subroutine */ int cdiv_(doublereal *ar, doublereal *ai, doublereal *br, 
	doublereal *bi, doublereal *cr, doublereal *ci)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal s, ais, bis, ars, brs;


/*     COMPLEX DIVISION, (CR,CI) = (AR,AI)/(BR,BI) */

    s = abs(*br) + abs(*bi);
    ars = *ar / s;
    ais = *ai / s;
    brs = *br / s;
    bis = *bi / s;
/* Computing 2nd power */
    d__1 = brs;
/* Computing 2nd power */
    d__2 = bis;
    s = d__1 * d__1 + d__2 * d__2;
    *cr = (ars * brs + ais * bis) / s;
    *ci = (ais * brs - ars * bis) / s;
    return 0;
} /* cdiv_ */

doublereal epslon_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    static doublereal a, b, c, eps;


/*     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.   


       THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS   
       SATISFYING THE FOLLOWING TWO ASSUMPTIONS,   
          1.  THE BASE USED IN REPRESENTING FLOATING POINT   
              NUMBERS IS NOT A POWER OF THREE.   
          2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO   
              THE ACCURACY USED IN FLOATING POINT VARIABLES   
              THAT ARE STORED IN MEMORY.   
       THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO   
       FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING   
       ASSUMPTION 2.   
       UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,   
              A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,   
              B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,   
              C  IS NOT EXACTLY EQUAL TO ONE,   
              EPS  MEASURES THE SEPARATION OF 1.0 FROM   
                   THE NEXT LARGER FLOATING POINT NUMBER.   
       THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED   
       ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.   

       THIS VERSION DATED 4/6/83. */

    a = 1.3333333333333333;
L10:
    b = a - 1.;
    c = b + b + b;
    eps = (d__1 = c - 1., abs(d__1));
    if (eps == 0.) {
	goto L10;
    }
    ret_val = eps * abs(*x);
    return ret_val;
} /* epslon_   

   Subroutine */ int hqr_(integer *nm, integer *n, integer *low, integer *igh,
	 doublereal *h, doublereal *wr, doublereal *wi, integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal unfl, ovfl, norm, opst;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y, small;
    static integer na, en, ll;
    extern doublereal dlamch_(char *);
    static integer mm;
    static doublereal zz;
    static logical notlas;
    static doublereal smlnum;
    static integer mp2, itn, its;
    static doublereal ulp;
    static integer enm2;
    static doublereal tst1, tst2;



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR,   
       NUM. MATH. 14, 219-231(1970) BY MARTIN, PETERS, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 359-371(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A REAL   
       UPPER HESSENBERG MATRIX BY THE QR METHOD.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          H CONTAINS THE UPPER HESSENBERG MATRIX.  INFORMATION ABOUT   
            THE TRANSFORMATIONS USED IN THE REDUCTION TO HESSENBERG   
            FORM BY  ELMHES  OR  ORTHES, IF PERFORMED, IS STORED   
            IN THE REMAINING TRIANGLE UNDER THE HESSENBERG MATRIX.   

       ON OUTPUT   

          H HAS BEEN DESTROYED.  THEREFORE, IT MUST BE SAVED   
            BEFORE CALLING  HQR  IF SUBSEQUENT CALCULATION AND   
            BACK TRANSFORMATION OF EIGENVECTORS IS TO BE PERFORMED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES   
            ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS   
            OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE   
            HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   
       MODIFIED ON 11/1/89; ADJUSTING INDICES OF LOOPS   
         200, 210, 230, AND 240 TO INCREASE PERFORMANCE. JACK DONGARRA   

       ------------------------------------------------------------------ 
  


       Parameter adjustments */
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }


/*     INITIALIZE */
    latime_1.itcnt = 0.;
    opst = 0.;
    *ierr = 0;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC   
                  AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	k = i;
	if (i >= *low && i <= *igh) {
	    goto L50;
	}
	wr[i] = h[i + i * h_dim1];
	wi[i] = 0.;
L50:
	;
    }

/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
    latime_1.ops += (*igh - *low + 1) * (*igh - *low + 2) / 2;

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.;
    i__1 = *igh;
    for (j = *low; j <= i__1; ++j) {
	s = 0.;
/* Computing MIN */
	i__3 = *igh, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i = *low; i <= i__2; ++i) {
	    s += (d__1 = h[i + j * h_dim1], abs(d__1));
/* L4: */
	}
	norm = max(norm,s);
/* L5: */
    }

    unfl = dlamch_("SAFE MINIMUM");
    ovfl = dlamch_("OVERFLOW");
    ulp = dlamch_("EPSILON") * dlamch_("BASE");
/* Computing MAX */
    d__1 = unfl * (*n / ulp), d__2 = *n / (ulp * ovfl);
    smlnum = max(d__1,d__2);
/* Computing MAX */
    d__1 = smlnum, d__2 = ulp * norm;
    small = max(d__1,d__2);

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L1001;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */

L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l 
		* h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
/* Computing MAX */
	d__2 = ulp * s;
	if ((d__1 = h[l + (l - 1) * h_dim1], abs(d__1)) <= max(d__2,small)) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 1;
    x = h[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT ..........   

          INCREMENT OP COUNT FOR FORMING EXCEPTIONAL SHIFT */
    latime_1.ops += en - *low + 6;
    t += x;

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
	h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (doublereal) (*n * 30 - itn);
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS.   
                  FOR M=EN-2 STEP -1 UNTIL L DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h[m + m * h_dim1];
	r = x - zz;
	s = y - zz;
	p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
	q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
	r = h[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r);
	p /= s;
	q /= s;
	r /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) + abs(r))
		;
/* Computing MAX */
	d__1 = ulp * tst1;
	if (tst2 <= max(d__1,small)) {
	    goto L150;
	}
/* L140: */
    }

L150:

/*        INCREMENT OPCOUNT FOR LOOP 140 */
    opst += (enm2 - m + 1) * 20;
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
	h[i + (i - 2) * h_dim1] = 0.;
	if (i == mp2) {
	    goto L160;
	}
	h[i + (i - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND   
                  COLUMNS M TO EN ..........   

          INCREMENT OPCOUNT FOR LOOP 260 */
    opst += (na - m + 1) * 18;
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h[k + (k - 1) * h_dim1];
	q = h[k + 1 + (k - 1) * h_dim1];
	r = 0.;
	if (notlas) {
	    r = h[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r /= x;
L170:
	d__1 = sqrt(p * p + q * q + r * r);
	s = d_sign(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r / s;
	q /= p;
	r /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (en - k + 1) * 6;
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (j - l + 1) * 6;
	i__2 = j;
	for (i = l; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (en - k + 1) * 10;
	i__2 = en;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 + 
		    j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
	    h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT */
	latime_1.ops += (j - l + 1) * 10;
	i__2 = j;
	for (i = l; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
		    i + (k + 2) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
	    h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    wr[en] = x + t;
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    x += t;

/*        INCREMENT OP COUNT FOR FINDING TWO ROOTS. */
    opst += 8;
    if (q < 0.) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;
L330:
    en = enm2;
    goto L60;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* hqr_   

   Subroutine */ int hqr2_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *h, doublereal *wr, doublereal *wi, doublereal *z, 
	integer *ierr)
{
    /* System generated locals */
    integer h_dim1, h_offset, z_dim1, z_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal unfl, ovfl, norm, opst;
    static integer i, j, k, l, m;
    static doublereal p, q, r, s, t, w, x, y, small;
    static integer na, ii, en, jj;
    static doublereal ra, sa;
    static integer ll;
    extern doublereal dlamch_(char *);
    static integer mm, nn;
    static doublereal vi, vr, zz;
    static logical notlas;
    static doublereal smlnum;
    static integer mp2, itn, its;
    static doublereal ulp;
    static integer enm2;
    static doublereal tst1, tst2;



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,   
       NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS   
       OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE   
       EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND   
       IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE   
       BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM   
       AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          H CONTAINS THE UPPER HESSENBERG MATRIX.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN   
            AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE   
            REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS   
            OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE   
            IDENTITY MATRIX.   

       ON OUTPUT   

          H HAS BEEN DESTROYED.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,   
            RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES   
            ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS   
            OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE   
            HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT   
            FOR INDICES IERR+1,...,N.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z   
            CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX 
  
            WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH   
            COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS   
            EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN   
            ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND. 
  

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       CALLS CDIV FOR COMPLEX DIVISION.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --wi;
    --wr;
    h_dim1 = *nm;
    h_offset = h_dim1 + 1;
    h -= h_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }

/*     INITIALIZE */

    latime_1.itcnt = 0.;
    opst = 0.;

    *ierr = 0;
    k = 1;
/*     .......... STORE ROOTS ISOLATED BY BALANC   
                  AND COMPUTE MATRIX NORM .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L50;
	}
	wr[i] = h[i + i * h_dim1];
	wi[i] = 0.;
L50:
	;
    }
/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
    latime_1.ops += (*igh - *low + 1) * (*igh - *low + 2) / 2;

/*     COMPUTE THE 1-NORM OF MATRIX H */

    norm = 0.;
    i__1 = *igh;
    for (j = *low; j <= i__1; ++j) {
	s = 0.;
/* Computing MIN */
	i__3 = *igh, i__4 = j + 1;
	i__2 = min(i__3,i__4);
	for (i = *low; i <= i__2; ++i) {
	    s += (d__1 = h[i + j * h_dim1], abs(d__1));
/* L4: */
	}
	norm = max(norm,s);
/* L5: */
    }

    unfl = dlamch_("SAFE MINIMUM");
    ovfl = dlamch_("OVERFLOW");
    ulp = dlamch_("EPSILON") * dlamch_("BASE");
/* Computing MAX */
    d__1 = unfl * (*n / ulp), d__2 = *n / (ulp * ovfl);
    smlnum = max(d__1,d__2);
/* Computing MAX   
   Computing MIN */
    d__3 = norm * smlnum * norm, d__4 = ulp * norm;
    d__1 = smlnum, d__2 = min(d__3,d__4);
    small = max(d__1,d__2);

    en = *igh;
    t = 0.;
    itn = *n * 30;
/*     .......... SEARCH FOR NEXT EIGENVALUES .......... */
L60:
    if (en < *low) {
	goto L340;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
/*     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT   
                  FOR L=EN STEP -1 UNTIL LOW DO -- ..........   
       REPLACE SPLITTING CRITERION WITH NEW ONE AS IN LAPACK */

L70:
    i__1 = en;
    for (ll = *low; ll <= i__1; ++ll) {
	l = en + *low - ll;
	if (l == *low) {
	    goto L100;
	}
	s = (d__1 = h[l - 1 + (l - 1) * h_dim1], abs(d__1)) + (d__2 = h[l + l 
		* h_dim1], abs(d__2));
	if (s == 0.) {
	    s = norm;
	}
/* Computing MAX */
	d__2 = ulp * s;
	if ((d__1 = h[l + (l - 1) * h_dim1], abs(d__1)) <= max(d__2,small)) {
	    goto L100;
	}
/* L80: */
    }
/*     .......... FORM SHIFT .......... */
L100:

/*        INCREMENT OP COUNT FOR CONVERGENCE TEST */
    latime_1.ops += en - l + 1 << 1;
    x = h[en + en * h_dim1];
    if (l == en) {
	goto L270;
    }
    y = h[na + na * h_dim1];
    w = h[en + na * h_dim1] * h[na + en * h_dim1];
    if (l == na) {
	goto L280;
    }
    if (itn == 0) {
	goto L1000;
    }
    if (its != 10 && its != 20) {
	goto L130;
    }
/*     .......... FORM EXCEPTIONAL SHIFT ..........   

          INCREMENT OP COUNT */
    latime_1.ops += en - *low + 6;
    t += x;

    i__1 = en;
    for (i = *low; i <= i__1; ++i) {
/* L120: */
	h[i + i * h_dim1] -= x;
    }

    s = (d__1 = h[en + na * h_dim1], abs(d__1)) + (d__2 = h[na + enm2 * 
	    h_dim1], abs(d__2));
    x = s * .75;
    y = x;
    w = s * -.4375 * s;
L130:
    ++its;
    --itn;

/*       UPDATE ITERATION NUMBER */
    latime_1.itcnt = (doublereal) (*n * 30 - itn);
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS.   
                  FOR M=EN-2 STEP -1 UNTIL L DO -- .......... */
    i__1 = enm2;
    for (mm = l; mm <= i__1; ++mm) {
	m = enm2 + l - mm;
	zz = h[m + m * h_dim1];
	r = x - zz;
	s = y - zz;
	p = (r * s - w) / h[m + 1 + m * h_dim1] + h[m + (m + 1) * h_dim1];
	q = h[m + 1 + (m + 1) * h_dim1] - zz - r - s;
	r = h[m + 2 + (m + 1) * h_dim1];
	s = abs(p) + abs(q) + abs(r);
	p /= s;
	q /= s;
	r /= s;
	if (m == l) {
	    goto L150;
	}
	tst1 = abs(p) * ((d__1 = h[m - 1 + (m - 1) * h_dim1], abs(d__1)) + 
		abs(zz) + (d__2 = h[m + 1 + (m + 1) * h_dim1], abs(d__2)));
	tst2 = (d__1 = h[m + (m - 1) * h_dim1], abs(d__1)) * (abs(q) + abs(r))
		;
/* Computing MAX */
	d__1 = ulp * tst1;
	if (tst2 <= max(d__1,small)) {
	    goto L150;
	}
/* L140: */
    }

L150:

/*        INCREMENT OPCOUNT FOR LOOP 140 */
    opst += (enm2 - m + 1) * 20;
    mp2 = m + 2;

    i__1 = en;
    for (i = mp2; i <= i__1; ++i) {
	h[i + (i - 2) * h_dim1] = 0.;
	if (i == mp2) {
	    goto L160;
	}
	h[i + (i - 3) * h_dim1] = 0.;
L160:
	;
    }
/*     .......... DOUBLE QR STEP INVOLVING ROWS L TO EN AND   
                  COLUMNS M TO EN ..........   

          INCREMENT OPCOUNT FOR LOOP 260 */
    opst += (na - m + 1) * 18;
    i__1 = na;
    for (k = m; k <= i__1; ++k) {
	notlas = k != na;
	if (k == m) {
	    goto L170;
	}
	p = h[k + (k - 1) * h_dim1];
	q = h[k + 1 + (k - 1) * h_dim1];
	r = 0.;
	if (notlas) {
	    r = h[k + 2 + (k - 1) * h_dim1];
	}
	x = abs(p) + abs(q) + abs(r);
	if (x == 0.) {
	    goto L260;
	}
	p /= x;
	q /= x;
	r /= x;
L170:
	d__1 = sqrt(p * p + q * q + r * r);
	s = d_sign(&d__1, &p);
	if (k == m) {
	    goto L180;
	}
	h[k + (k - 1) * h_dim1] = -s * x;
	goto L190;
L180:
	if (l != m) {
	    h[k + (k - 1) * h_dim1] = -h[k + (k - 1) * h_dim1];
	}
L190:
	p += s;
	x = p / s;
	y = q / s;
	zz = r / s;
	q /= p;
	r /= p;
	if (notlas) {
	    goto L225;
	}
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OP COUNT FOR LOOP 200 */
	latime_1.ops += (*n - k + 1) * 6;
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
/* L200: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 210 */
	latime_1.ops += j * 6;
	i__2 = j;
	for (i = 1; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
/* L210: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS ..........   

          INCREMENT OPCOUNT FOR LOOP 220 */
	latime_1.ops += (*igh - *low + 1) * 6;
	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1];
	    z[i + k * z_dim1] -= p;
	    z[i + (k + 1) * z_dim1] -= p * q;
/* L220: */
	}
	goto L255;
L225:
/*     .......... ROW MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 230 */
	latime_1.ops += (*n - k + 1) * 10;
	i__2 = *n;
	for (j = k; j <= i__2; ++j) {
	    p = h[k + j * h_dim1] + q * h[k + 1 + j * h_dim1] + r * h[k + 2 + 
		    j * h_dim1];
	    h[k + j * h_dim1] -= p * x;
	    h[k + 1 + j * h_dim1] -= p * y;
	    h[k + 2 + j * h_dim1] -= p * zz;
/* L230: */
	}

/* Computing MIN */
	i__2 = en, i__3 = k + 3;
	j = min(i__2,i__3);
/*     .......... COLUMN MODIFICATION ..........   

          INCREMENT OPCOUNT FOR LOOP 240 */
	latime_1.ops += j * 10;
	i__2 = j;
	for (i = 1; i <= i__2; ++i) {
	    p = x * h[i + k * h_dim1] + y * h[i + (k + 1) * h_dim1] + zz * h[
		    i + (k + 2) * h_dim1];
	    h[i + k * h_dim1] -= p;
	    h[i + (k + 1) * h_dim1] -= p * q;
	    h[i + (k + 2) * h_dim1] -= p * r;
/* L240: */
	}
/*     .......... ACCUMULATE TRANSFORMATIONS ..........   

          INCREMENT OPCOUNT FOR LOOP 250 */
	latime_1.ops += (*igh - *low + 1) * 10;
	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    p = x * z[i + k * z_dim1] + y * z[i + (k + 1) * z_dim1] + zz * z[
		    i + (k + 2) * z_dim1];
	    z[i + k * z_dim1] -= p;
	    z[i + (k + 1) * z_dim1] -= p * q;
	    z[i + (k + 2) * z_dim1] -= p * r;
/* L250: */
	}
L255:

L260:
	;
    }

    goto L70;
/*     .......... ONE ROOT FOUND .......... */
L270:
    h[en + en * h_dim1] = x + t;
    wr[en] = h[en + en * h_dim1];
    wi[en] = 0.;
    en = na;
    goto L60;
/*     .......... TWO ROOTS FOUND .......... */
L280:
    p = (y - x) / 2.;
    q = p * p + w;
    zz = sqrt((abs(q)));
    h[en + en * h_dim1] = x + t;
    x = h[en + en * h_dim1];
    h[na + na * h_dim1] = y + t;
    if (q < 0.) {
	goto L320;
    }
/*     .......... REAL PAIR .......... */
    zz = p + d_sign(&zz, &p);
    wr[na] = x + zz;
    wr[en] = wr[na];
    if (zz != 0.) {
	wr[en] = x - w / zz;
    }
    wi[na] = 0.;
    wi[en] = 0.;
    x = h[en + na * h_dim1];
    s = abs(x) + abs(zz);
    p = x / s;
    q = zz / s;
    r = sqrt(p * p + q * q);
    p /= r;
    q /= r;

/*        INCREMENT OP COUNT FOR FINDING TWO ROOTS. */
    opst += 18;

/*        INCREMENT OP COUNT FOR MODIFICATION AND ACCUMULATION   
          IN LOOP 290, 300, 310 */
    latime_1.ops = latime_1.ops + (*n - na + 1) * 6 + en * 6 + (*igh - *low + 
	    1) * 6;
/*     .......... ROW MODIFICATION .......... */
    i__1 = *n;
    for (j = na; j <= i__1; ++j) {
	zz = h[na + j * h_dim1];
	h[na + j * h_dim1] = q * zz + p * h[en + j * h_dim1];
	h[en + j * h_dim1] = q * h[en + j * h_dim1] - p * zz;
/* L290: */
    }
/*     .......... COLUMN MODIFICATION .......... */
    i__1 = en;
    for (i = 1; i <= i__1; ++i) {
	zz = h[i + na * h_dim1];
	h[i + na * h_dim1] = q * zz + p * h[i + en * h_dim1];
	h[i + en * h_dim1] = q * h[i + en * h_dim1] - p * zz;
/* L300: */
    }
/*     .......... ACCUMULATE TRANSFORMATIONS .......... */
    i__1 = *igh;
    for (i = *low; i <= i__1; ++i) {
	zz = z[i + na * z_dim1];
	z[i + na * z_dim1] = q * zz + p * z[i + en * z_dim1];
	z[i + en * z_dim1] = q * z[i + en * z_dim1] - p * zz;
/* L310: */
    }

    goto L330;
/*     .......... COMPLEX PAIR .......... */
L320:
    wr[na] = x + p;
    wr[en] = x + p;
    wi[na] = zz;
    wi[en] = -zz;

/*        INCREMENT OP COUNT FOR FINDING COMPLEX PAIR. */
    opst += 9;
L330:
    en = enm2;
    goto L60;
/*     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND   
                  VECTORS OF UPPER TRIANGULAR FORM .......... */
L340:
    if (norm == 0.) {
	goto L1001;
    }
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
	en = *n + 1 - nn;
	p = wr[en];
	q = wi[en];
	na = en - 1;
	if (q < 0.) {
	    goto L710;
	} else if (q == 0) {
	    goto L600;
	} else {
	    goto L800;
	}
/*     .......... REAL VECTOR .......... */
L600:
	m = en;
	h[en + en * h_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = en - ii;
	    w = h[i + i * h_dim1] - p;
	    r = 0.;


/*        INCREMENT OP COUNT FOR LOOP 610 */
	    opst += en - m + 1 << 1;
	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r += h[i + j * h_dim1] * h[j + en * h_dim1];
	    }

	    if (wi[i] >= 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r;
	    goto L700;
L630:
	    m = i;
	    if (wi[i] != 0.) {
		goto L640;
	    }
	    t = w;
	    if (t != 0.) {
		goto L635;
	    }
	    tst1 = norm;
	    t = tst1;
L632:
	    t *= .01;
	    tst2 = norm + t;
	    if (tst2 > tst1) {
		goto L632;
	    }
L635:
	    h[i + en * h_dim1] = -r / t;
	    goto L680;
/*     .......... SOLVE REAL EQUATIONS .......... */
L640:
	    x = h[i + (i + 1) * h_dim1];
	    y = h[i + 1 + i * h_dim1];
	    q = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
	    t = (x * s - zz * r) / q;

/*        INCREMENT OP COUNT FOR SOLVING REAL EQUATION. */
	    opst += 13;
	    h[i + en * h_dim1] = t;
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    h[i + 1 + en * h_dim1] = (-r - w * t) / x;
	    goto L680;
L650:
	    h[i + 1 + en * h_dim1] = (-s - y * t) / zz;

/*     .......... OVERFLOW CONTROL .......... */
L680:
	    t = (d__1 = h[i + en * h_dim1], abs(d__1));
	    if (t == 0.) {
		goto L700;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L700;
	    }

/*        INCREMENT OP COUNT. */
	    opst += en - i + 1;
	    i__3 = en;
	    for (j = i; j <= i__3; ++j) {
		h[j + en * h_dim1] /= t;
/* L690: */
	    }

L700:
	    ;
	}
/*     .......... END REAL VECTOR .......... */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT   
                  EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	if ((d__1 = h[en + na * h_dim1], abs(d__1)) <= (d__2 = h[na + en * 
		h_dim1], abs(d__2))) {
	    goto L720;
	}
	h[na + na * h_dim1] = q / h[en + na * h_dim1];
	h[na + en * h_dim1] = -(h[en + en * h_dim1] - p) / h[en + na * h_dim1]
		;

/*        INCREMENT OP COUNT. */
	opst += 3;
	goto L730;
L720:
	d__1 = -h[na + en * h_dim1];
	d__2 = h[na + na * h_dim1] - p;
	cdiv_(&c_b90, &d__1, &d__2, &q, &h[na + na * h_dim1], &h[na + en * 
		h_dim1]);

/*        INCREMENT OP COUNT IF (ABS(H(EN,NA)) .LE. ABS(H(NA,EN))) */
	opst += 16;
L730:
	h[en + na * h_dim1] = 0.;
	h[en + en * h_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L800;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = na - ii;
	    w = h[i + i * h_dim1] - p;
	    ra = 0.;
	    sa = 0.;


/*        INCREMENT OP COUNT FOR LOOP 760 */
	    opst += en - m + 1 << 2;
	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		ra += h[i + j * h_dim1] * h[j + na * h_dim1];
		sa += h[i + j * h_dim1] * h[j + en * h_dim1];
/* L760: */
	    }

	    if (wi[i] >= 0.) {
		goto L770;
	    }
	    zz = w;
	    r = ra;
	    s = sa;
	    goto L795;
L770:
	    m = i;
	    if (wi[i] != 0.) {
		goto L780;
	    }
	    d__1 = -ra;
	    d__2 = -sa;
	    cdiv_(&d__1, &d__2, &w, &q, &h[i + na * h_dim1], &h[i + en * 
		    h_dim1]);

/*        INCREMENT OP COUNT FOR CDIV */
	    opst += 16;
	    goto L790;
/*     .......... SOLVE COMPLEX EQUATIONS .......... */
L780:
	    x = h[i + (i + 1) * h_dim1];
	    y = h[i + 1 + i * h_dim1];
	    vr = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i] - q * q;
	    vi = (wr[i] - p) * 2. * q;

/*        INCREMENT OPCOUNT (AVERAGE) FOR SOLVING COMPLEX EQUATION
S */
	    opst += 42;
	    if (vr != 0. || vi != 0.) {
		goto L784;
	    }
	    tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz));
	    vr = tst1;
L783:
	    vr *= .01;
	    tst2 = tst1 + vr;
	    if (tst2 > tst1) {
		goto L783;
	    }
L784:
	    d__1 = x * r - zz * ra + q * sa;
	    d__2 = x * s - zz * sa - q * ra;
	    cdiv_(&d__1, &d__2, &vr, &vi, &h[i + na * h_dim1], &h[i + en * 
		    h_dim1]);
	    if (abs(x) <= abs(zz) + abs(q)) {
		goto L785;
	    }
	    h[i + 1 + na * h_dim1] = (-ra - w * h[i + na * h_dim1] + q * h[i 
		    + en * h_dim1]) / x;
	    h[i + 1 + en * h_dim1] = (-sa - w * h[i + en * h_dim1] - q * h[i 
		    + na * h_dim1]) / x;
	    goto L790;
L785:
	    d__1 = -r - y * h[i + na * h_dim1];
	    d__2 = -s - y * h[i + en * h_dim1];
	    cdiv_(&d__1, &d__2, &zz, &q, &h[i + 1 + na * h_dim1], &h[i + 1 + 
		    en * h_dim1]);

/*     .......... OVERFLOW CONTROL .......... */
L790:
/* Computing MAX */
	    d__3 = (d__1 = h[i + na * h_dim1], abs(d__1)), d__4 = (d__2 = h[i 
		    + en * h_dim1], abs(d__2));
	    t = max(d__3,d__4);
	    if (t == 0.) {
		goto L795;
	    }
	    tst1 = t;
	    tst2 = tst1 + 1. / tst1;
	    if (tst2 > tst1) {
		goto L795;
	    }

/*        INCREMENT OP COUNT. */
	    opst += en - i + 1 << 1;
	    i__3 = en;
	    for (j = i; j <= i__3; ++j) {
		h[j + na * h_dim1] /= t;
		h[j + en * h_dim1] /= t;
/* L792: */
	    }

L795:
	    ;
	}
/*     .......... END COMPLEX VECTOR .......... */
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION.   
                  VECTORS OF ISOLATED ROOTS .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i >= *low && i <= *igh) {
	    goto L840;
	}

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
/* L820: */
	    z[i + j * z_dim1] = h[i + j * h_dim1];
	}

L840:
	;
    }
/*     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE   
                  VECTORS OF ORIGINAL FULL MATRIX.   
                  FOR J=N STEP -1 UNTIL LOW DO -- .......... */
    i__1 = *n;
    for (jj = *low; jj <= i__1; ++jj) {
	j = *n + *low - jj;
	m = min(j,*igh);


/*        INCREMENT OP COUNT. */
	latime_1.ops += (*igh - *low + 1 << 1) * (m - *low + 1);
	i__2 = *igh;
	for (i = *low; i <= i__2; ++i) {
	    zz = 0.;

	    i__3 = m;
	    for (k = *low; k <= i__3; ++k) {
/* L860: */
		zz += z[i + k * z_dim1] * h[k + j * h_dim1];
	    }

	    z[i + j * z_dim1] = zz;
/* L880: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* hqr2_   

   Subroutine */ int imtql1_(integer *n, doublereal *d, doublereal *e, 
	integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal b, c, f, g;
    static integer i, j, l, m;
    static doublereal p, r, s;
    static integer ii;
    extern doublereal dlamch_(char *), pythag_(doublereal *, 
	    doublereal *);
    static integer mml;
    static doublereal eps, tst;


/*     EISPACK ROUTINE   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   


       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL1,   
       NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,   
       AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC   
       TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND   
            ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE   
            THE SMALLEST EIGENVALUES.   

          E HAS BEEN DESTROYED.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 40 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    --e;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.;
    pythop_1.opst = 0.;

/*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT. */

    eps = dlamch_("EPSILON");

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e[i - 1] = e[i];
    }

    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst = (d__1 = e[m], abs(d__1));
	    if (tst <= eps * ((d__1 = d[m], abs(d__1)) + (d__2 = d[m + 1], 
		    abs(d__2)))) {
		goto L120;
	    }
/*            TST1 = ABS(D(M)) + ABS(D(M+1))   
              TST2 = TST1 + ABS(E(M))   
              IF (TST2 .EQ. TST1) GO TO 120   
   L110: */
	}

L120:
	p = d[l];

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.   
   Computing MIN */
	i__2 = m, i__3 = *n - 1;
	latime_1.ops += min(i__2,i__3) - l + 1 << 1;
	if (m == l) {
	    goto L215;
	}
	if (j == 40) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	g = (d[l + 1] - p) / (e[l] * 2.);
	r = pythag_(&g, &c_b114);
	g = d[m] - p + e[l] / (g + d_sign(&r, &g));

/*        INCREMENT OPCOUNT FOR FORMING SHIFT. */
	latime_1.ops += 7;
	s = 1.;
	c = 1.;
	p = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = m - ii;
	    f = s * e[i];
	    b = c * e[i];
	    r = pythag_(&f, &g);
	    e[i + 1] = r;
	    if (r == 0.) {
		goto L210;
	    }
	    s = f / r;
	    c = g / r;
	    g = d[i + 1] - p;
	    r = (d[i] - g) * s + c * 2. * b;
	    p = s * r;
	    d[i + 1] = g + p;
	    g = c * r - b;
/* L200: */
	}

	d[l] -= p;
	e[l] = g;
	e[m] = 0.;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * 14 + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
	goto L105;
/*     .......... RECOVER FROM UNDERFLOW .......... */
L210:
	d[i + 1] -= p;
	e[m] = 0.;

/*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS. */
	latime_1.ops = latime_1.ops + 2 + (ii - 1) * 14 + 1;
	goto L105;
/*     .......... ORDER EIGENVALUES .......... */
L215:
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i = l + 2 - ii;
	    if (p >= d[i - 1]) {
		goto L270;
	    }
	    d[i] = d[i - 1];
/* L230: */
	}

L250:
	i = 1;
L270:
	d[i] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 40 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* imtql1_   

   Subroutine */ int imtql2_(integer *nm, integer *n, doublereal *d, 
	doublereal *e, doublereal *z, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal b, c, f, g;
    static integer i, j, k, l, m;
    static doublereal p, r, s;
    static integer ii;
    extern doublereal dlamch_(char *), pythag_(doublereal *, 
	    doublereal *);
    static integer mml;
    static doublereal eps, tst;


/*     EISPACK ROUTINE.  MODIFIED FOR COMPARISON WITH LAPACK.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,   
       NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,   
       AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).   

       THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS   
       OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.   
       THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO   
       BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS   
       FULL MATRIX TO TRIDIAGONAL FORM.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE   
            REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS   
            OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN   
            THE IDENTITY MATRIX.   

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT   
            UNORDERED FOR INDICES 1,2,...,IERR-1.   

          E HAS BEEN DESTROYED.   

          Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC   
            TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,   
            Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED   
            EIGENVALUES.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 40 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.;
    pythop_1.opst = 0.;

/*     DETERMINE UNIT ROUNDOFF FOR THIS MACHINE. */
    eps = dlamch_("EPSILON");

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e[i - 1] = e[i];
    }

    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
/*            TST1 = ABS(D(M)) + ABS(D(M+1))   
              TST2 = TST1 + ABS(E(M))   
              IF (TST2 .EQ. TST1) GO TO 120 */
	    tst = (d__1 = e[m], abs(d__1));
	    if (tst <= eps * ((d__1 = d[m], abs(d__1)) + (d__2 = d[m + 1], 
		    abs(d__2)))) {
		goto L120;
	    }
/* L110: */
	}

L120:
	p = d[l];

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT. */
	latime_1.ops += min(m,*n) - l + 1 << 1;
	if (m == l) {
	    goto L240;
	}
	if (j == 40) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	g = (d[l + 1] - p) / (e[l] * 2.);
	r = pythag_(&g, &c_b114);
	g = d[m] - p + e[l] / (g + d_sign(&r, &g));

/*        INCREMENT OPCOUNT FOR FORMING SHIFT. */
	latime_1.ops += 7;
	s = 1.;
	c = 1.;
	p = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = m - ii;
	    f = s * e[i];
	    b = c * e[i];
	    r = pythag_(&f, &g);
	    e[i + 1] = r;
	    if (r == 0.) {
		goto L210;
	    }
	    s = f / r;
	    c = g / r;
	    g = d[i + 1] - p;
	    r = (d[i] - g) * s + c * 2. * b;
	    p = s * r;
	    d[i + 1] = g + p;
	    g = c * r - b;
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		f = z[k + (i + 1) * z_dim1];
		z[k + (i + 1) * z_dim1] = s * z[k + i * z_dim1] + c * f;
		z[k + i * z_dim1] = c * z[k + i * z_dim1] - s * f;
/* L180: */
	    }

/* L200: */
	}

	d[l] -= p;
	e[l] = g;
	e[m] = 0.;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * (*n * 6 + 14) + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
	goto L105;
/*     .......... RECOVER FROM UNDERFLOW .......... */
L210:
	d[i + 1] -= p;
	e[m] = 0.;

/*        INCREMENT OPCOUNT FOR INNER LOOP, WHEN UNDERFLOW OCCURS. */
	latime_1.ops = latime_1.ops + 2 + (ii - 1) * (*n * 6 + 14) + 1;
	goto L105;
L240:
	;
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = ii - 1;
	k = i;
	p = d[i];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d[j];
L260:
	    ;
	}

	if (k == i) {
	    goto L300;
	}
	d[k] = d[i];
	d[i] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z[j + i * z_dim1];
	    z[j + i * z_dim1] = z[j + k * z_dim1];
	    z[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 40 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* imtql2_   

   Subroutine */ int invit_(integer *nm, integer *n, doublereal *a, 
	doublereal *wr, doublereal *wi, logical *select, integer *mm, integer 
	*m, doublereal *z, integer *ierr, doublereal *rm1, doublereal *rv1, 
	doublereal *rv2)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, rm1_dim1, rm1_offset, i__1, 
	    i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    static doublereal norm, opst;
    static integer i, j, k, l, s;
    static doublereal t, w, x, y;
    static integer n1;
    static doublereal normv;
    static integer ii;
    static doublereal ilambd;
    static integer ip;
    extern doublereal dlamch_(char *);
    static integer mp, ns, uk;
    static doublereal rlambd;
    extern doublereal pythag_(doublereal *, doublereal *);
    static integer km1, ip1;
    static doublereal growto, ukroot;
    static integer its;
    static doublereal ulp, eps3;



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE INVIT   
       BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A REAL UPPER   
       HESSENBERG MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,   
       USING INVERSE ITERATION.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          A CONTAINS THE HESSENBERG MATRIX.   

          WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY,   
            OF THE EIGENVALUES OF THE MATRIX.  THE EIGENVALUES MUST BE   
            STORED IN A MANNER IDENTICAL TO THAT OF SUBROUTINE  HQR,   
            WHICH RECOGNIZES POSSIBLE SPLITTING OF THE MATRIX.   

          SELECT SPECIFIES THE EIGENVECTORS TO BE FOUND. THE   
            EIGENVECTOR CORRESPONDING TO THE J-TH EIGENVALUE IS   
            SPECIFIED BY SETTING SELECT(J) TO .TRUE..   

          MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF   
            COLUMNS REQUIRED TO STORE THE EIGENVECTORS TO BE FOUND.   
            NOTE THAT TWO COLUMNS ARE REQUIRED TO STORE THE   
            EIGENVECTOR CORRESPONDING TO A COMPLEX EIGENVALUE.   

       ON OUTPUT   

          A AND WI ARE UNALTERED.   

          WR MAY HAVE BEEN ALTERED SINCE CLOSE EIGENVALUES ARE PERTURBED 
  
            SLIGHTLY IN SEARCHING FOR INDEPENDENT EIGENVECTORS.   

          SELECT MAY HAVE BEEN ALTERED.  IF THE ELEMENTS CORRESPONDING   
            TO A PAIR OF CONJUGATE COMPLEX EIGENVALUES WERE EACH   
            INITIALLY SET TO .TRUE., THE PROGRAM RESETS THE SECOND OF   
            THE TWO ELEMENTS TO .FALSE..   

          M IS THE NUMBER OF COLUMNS ACTUALLY USED TO STORE   
            THE EIGENVECTORS.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF THE NEXT SELECTED EIGENVALUE IS REAL, THE NEXT COLUMN   
            OF Z CONTAINS ITS EIGENVECTOR.  IF THE EIGENVALUE IS   
            COMPLEX, THE NEXT TWO COLUMNS OF Z CONTAIN THE REAL AND   
            IMAGINARY PARTS OF ITS EIGENVECTOR.  THE EIGENVECTORS ARE   
            NORMALIZED SO THAT THE COMPONENT OF LARGEST MAGNITUDE IS 1.   
            ANY VECTOR WHICH FAILS THE ACCEPTANCE TEST IS SET TO ZERO.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            -(2*N+1)   IF MORE THAN MM COLUMNS OF Z ARE NECESSARY   
                       TO STORE THE EIGENVECTORS CORRESPONDING TO   
                       THE SPECIFIED EIGENVALUES.   
            -K         IF THE ITERATION CORRESPONDING TO THE K-TH   
                       VALUE FAILS,   
            -(N+K)     IF BOTH ERROR SITUATIONS OCCUR.   

          RM1, RV1, AND RV2 ARE TEMPORARY STORAGE ARRAYS.  NOTE THAT RM1 
  
            IS SQUARE OF DIMENSION N BY N AND, AUGMENTED BY TWO COLUMNS   
            OF Z, IS THE TRANSPOSE OF THE CORRESPONDING ALGOL B ARRAY.   

       THE ALGOL PROCEDURE GUESSVEC APPEARS IN INVIT IN LINE.   

       CALLS CDIV FOR COMPLEX DIVISION.   
       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       GET ULP FROM DLAMCH FOR NEW SMALL PERTURBATION AS IN LAPACK   
       Parameter adjustments */
    --rv2;
    --rv1;
    rm1_dim1 = *n;
    rm1_offset = rm1_dim1 + 1;
    rm1 -= rm1_offset;
    --select;
    --wi;
    --wr;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    ulp = dlamch_("EPSILON");


/*     INITIALIZE */
    opst = 0.;
    *ierr = 0;
    uk = 0;
    s = 1;
/*     .......... IP = 0, REAL EIGENVALUE   
                       1, FIRST OF CONJUGATE COMPLEX PAIR   
                      -1, SECOND OF CONJUGATE COMPLEX PAIR .......... */
    ip = 0;
    n1 = *n - 1;

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	if (wi[k] == 0. || ip < 0) {
	    goto L100;
	}
	ip = 1;
	if (select[k] && select[k + 1]) {
	    select[k + 1] = FALSE_;
	}
L100:
	if (! select[k]) {
	    goto L960;
	}
	if (wi[k] != 0.) {
	    ++s;
	}
	if (s > *mm) {
	    goto L1000;
	}
	if (uk >= k) {
	    goto L200;
	}
/*     .......... CHECK FOR POSSIBLE SPLITTING .......... */
	i__2 = *n;
	for (uk = k; uk <= i__2; ++uk) {
	    if (uk == *n) {
		goto L140;
	    }
	    if (a[uk + 1 + uk * a_dim1] == 0.) {
		goto L140;
	    }
/* L120: */
	}
/*     .......... COMPUTE INFINITY NORM OF LEADING UK BY UK   
                  (HESSENBERG) MATRIX .......... */
L140:
	norm = 0.;
	mp = 1;


/*        INCREMENT OPCOUNT FOR COMPUTING MATRIX NORM */
	latime_1.ops += uk * (uk - 1) / 2;
	i__2 = uk;
	for (i = 1; i <= i__2; ++i) {
	    x = 0.;

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L160: */
		x += (d__1 = a[i + j * a_dim1], abs(d__1));
	    }

	    if (x > norm) {
		norm = x;
	    }
	    mp = i;
/* L180: */
	}
/*     .......... EPS3 REPLACES ZERO PIVOT IN DECOMPOSITION   
                  AND CLOSE ROOTS ARE MODIFIED BY EPS3 .......... */
	if (norm == 0.) {
	    norm = 1.;
	}
/*        EPS3 = EPSLON(NORM)   

          INCREMENT OPCOUNT */
	opst += 3;
	eps3 = norm * ulp;
/*     .......... GROWTO IS THE CRITERION FOR THE GROWTH .......... */
	ukroot = (doublereal) uk;
	ukroot = sqrt(ukroot);
	growto = .1 / ukroot;
L200:
	rlambd = wr[k];
	ilambd = wi[k];
	if (k == 1) {
	    goto L280;
	}
	km1 = k - 1;
	goto L240;
/*     .......... PERTURB EIGENVALUE IF IT IS CLOSE   
                  TO ANY PREVIOUS EIGENVALUE .......... */
L220:
	rlambd += eps3;
/*     .......... FOR I=K-1 STEP -1 UNTIL 1 DO -- .......... */
L240:
	i__2 = km1;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = k - ii;
	    if (select[i] && (d__1 = wr[i] - rlambd, abs(d__1)) < eps3 && (
		    d__2 = wi[i] - ilambd, abs(d__2)) < eps3) {
		goto L220;
	    }
/* L260: */
	}

/*        INCREMENT OPCOUNT FOR LOOP 260 (ASSUME THAT ALL EIGENVALUES 
  
          ARE DIFFERENT) */
	opst += k - 1 << 1;

	wr[k] = rlambd;
/*     .......... PERTURB CONJUGATE EIGENVALUE TO MATCH .......... */
	ip1 = k + ip;
	wr[ip1] = rlambd;
/*     .......... FORM UPPER HESSENBERG A-RLAMBD*I (TRANSPOSED)   
                  AND INITIAL REAL VECTOR .......... */
L280:
	mp = 1;


/*        INCREMENT OP COUNT FOR LOOP 320 */
	latime_1.ops += uk;
	i__2 = uk;
	for (i = 1; i <= i__2; ++i) {

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
/* L300: */
		rm1[j + i * rm1_dim1] = a[i + j * a_dim1];
	    }

	    rm1[i + i * rm1_dim1] -= rlambd;
	    mp = i;
	    rv1[i] = eps3;
/* L320: */
	}

	its = 0;
	if (ilambd != 0.) {
	    goto L520;
	}
/*     .......... REAL EIGENVALUE.   
                  TRIANGULAR DECOMPOSITION WITH INTERCHANGES,   
                  REPLACING ZERO PIVOTS BY EPS3 .......... */
	if (uk == 1) {
	    goto L420;
	}


/*        INCREMENT OPCOUNT LU DECOMPOSITION */
	latime_1.ops += (uk - 1) * (uk + 2);
	i__2 = uk;
	for (i = 2; i <= i__2; ++i) {
	    mp = i - 1;
	    if ((d__1 = rm1[mp + i * rm1_dim1], abs(d__1)) <= (d__2 = rm1[mp 
		    + mp * rm1_dim1], abs(d__2))) {
		goto L360;
	    }

	    i__3 = uk;
	    for (j = mp; j <= i__3; ++j) {
		y = rm1[j + i * rm1_dim1];
		rm1[j + i * rm1_dim1] = rm1[j + mp * rm1_dim1];
		rm1[j + mp * rm1_dim1] = y;
/* L340: */
	    }

L360:
	    if (rm1[mp + mp * rm1_dim1] == 0.) {
		rm1[mp + mp * rm1_dim1] = eps3;
	    }
	    x = rm1[mp + i * rm1_dim1] / rm1[mp + mp * rm1_dim1];
	    if (x == 0.) {
		goto L400;
	    }

	    i__3 = uk;
	    for (j = i; j <= i__3; ++j) {
/* L380: */
		rm1[j + i * rm1_dim1] -= x * rm1[j + mp * rm1_dim1];
	    }

L400:
	    ;
	}

L420:
	if (rm1[uk + uk * rm1_dim1] == 0.) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR REAL VECTOR   
                  FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L440:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = uk + 1 - ii;
	    y = rv1[i];
	    if (i == uk) {
		goto L480;
	    }
	    ip1 = i + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
/* L460: */
		y -= rm1[j + i * rm1_dim1] * rv1[j];
	    }

L480:
	    rv1[i] = y / rm1[i + i * rm1_dim1];
/* L500: */
	}

/*        INCREMENT OP COUNT FOR BACK SUBSTITUTION LOOP 500 */
	latime_1.ops += uk * (uk + 1);

	goto L740;
/*     .......... COMPLEX EIGENVALUE.   
                  TRIANGULAR DECOMPOSITION WITH INTERCHANGES,   
                  REPLACING ZERO PIVOTS BY EPS3.  STORE IMAGINARY   
                  PARTS IN UPPER TRIANGLE STARTING AT (1,3) ..........
 */
L520:
	ns = *n - s;
	z[(s - 1) * z_dim1 + 1] = -ilambd;
	z[s * z_dim1 + 1] = 0.;
	if (*n == 2) {
	    goto L550;
	}
	rm1[rm1_dim1 * 3 + 1] = -ilambd;
	z[(s - 1) * z_dim1 + 1] = 0.;
	if (*n == 3) {
	    goto L550;
	}

	i__2 = *n;
	for (i = 4; i <= i__2; ++i) {
/* L540: */
	    rm1[i * rm1_dim1 + 1] = 0.;
	}

L550:
	i__2 = uk;
	for (i = 2; i <= i__2; ++i) {
	    mp = i - 1;
	    w = rm1[mp + i * rm1_dim1];
	    if (i < *n) {
		t = rm1[mp + (i + 1) * rm1_dim1];
	    }
	    if (i == *n) {
		t = z[mp + (s - 1) * z_dim1];
	    }
	    x = rm1[mp + mp * rm1_dim1] * rm1[mp + mp * rm1_dim1] + t * t;
	    if (w * w <= x) {
		goto L580;
	    }
	    x = rm1[mp + mp * rm1_dim1] / w;
	    y = t / w;
	    rm1[mp + mp * rm1_dim1] = w;
	    if (i < *n) {
		rm1[mp + (i + 1) * rm1_dim1] = 0.;
	    }
	    if (i == *n) {
		z[mp + (s - 1) * z_dim1] = 0.;
	    }


/*        INCREMENT OPCOUNT FOR LOOP 560 */
	    latime_1.ops += uk - i + 1 << 2;
	    i__3 = uk;
	    for (j = i; j <= i__3; ++j) {
		w = rm1[j + i * rm1_dim1];
		rm1[j + i * rm1_dim1] = rm1[j + mp * rm1_dim1] - x * w;
		rm1[j + mp * rm1_dim1] = w;
		if (j < n1) {
		    goto L555;
		}
		l = j - ns;
		z[i + l * z_dim1] = z[mp + l * z_dim1] - y * w;
		z[mp + l * z_dim1] = 0.;
		goto L560;
L555:
		rm1[i + (j + 2) * rm1_dim1] = rm1[mp + (j + 2) * rm1_dim1] - 
			y * w;
		rm1[mp + (j + 2) * rm1_dim1] = 0.;
L560:
		;
	    }

	    rm1[i + i * rm1_dim1] -= y * ilambd;
	    if (i < n1) {
		goto L570;
	    }
	    l = i - ns;
	    z[mp + l * z_dim1] = -ilambd;
	    z[i + l * z_dim1] += x * ilambd;
	    goto L640;
L570:
	    rm1[mp + (i + 2) * rm1_dim1] = -ilambd;
	    rm1[i + (i + 2) * rm1_dim1] += x * ilambd;
	    goto L640;
L580:
	    if (x != 0.) {
		goto L600;
	    }
	    rm1[mp + mp * rm1_dim1] = eps3;
	    if (i < *n) {
		rm1[mp + (i + 1) * rm1_dim1] = 0.;
	    }
	    if (i == *n) {
		z[mp + (s - 1) * z_dim1] = 0.;
	    }
	    t = 0.;
	    x = eps3 * eps3;
L600:
	    w /= x;
	    x = rm1[mp + mp * rm1_dim1] * w;
	    y = -t * w;


/*        INCREMENT OPCOUNT FOR LOOP 620 */
	    latime_1.ops += (uk - i + 1) * 6;
	    i__3 = uk;
	    for (j = i; j <= i__3; ++j) {
		if (j < n1) {
		    goto L610;
		}
		l = j - ns;
		t = z[mp + l * z_dim1];
		z[i + l * z_dim1] = -x * t - y * rm1[j + mp * rm1_dim1];
		goto L615;
L610:
		t = rm1[mp + (j + 2) * rm1_dim1];
		rm1[i + (j + 2) * rm1_dim1] = -x * t - y * rm1[j + mp * 
			rm1_dim1];
L615:
		rm1[j + i * rm1_dim1] = rm1[j + i * rm1_dim1] - x * rm1[j + 
			mp * rm1_dim1] + y * t;
/* L620: */
	    }

	    if (i < n1) {
		goto L630;
	    }
	    l = i - ns;
	    z[i + l * z_dim1] -= ilambd;
	    goto L640;
L630:
	    rm1[i + (i + 2) * rm1_dim1] -= ilambd;
L640:
	    ;
	}

/*        INCREMENT OP COUNT (AVERAGE) FOR COMPUTING   
          THE SCALARS IN LOOP 640 */
	latime_1.ops += (uk - 1) * 10;

	if (uk < n1) {
	    goto L650;
	}
	l = uk - ns;
	t = z[uk + l * z_dim1];
	goto L655;
L650:
	t = rm1[uk + (uk + 2) * rm1_dim1];
L655:
	if (rm1[uk + uk * rm1_dim1] == 0. && t == 0.) {
	    rm1[uk + uk * rm1_dim1] = eps3;
	}
/*     .......... BACK SUBSTITUTION FOR COMPLEX VECTOR   
                  FOR I=UK STEP -1 UNTIL 1 DO -- .......... */
L660:
	i__2 = uk;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = uk + 1 - ii;
	    x = rv1[i];
	    y = 0.;
	    if (i == uk) {
		goto L700;
	    }
	    ip1 = i + 1;

	    i__3 = uk;
	    for (j = ip1; j <= i__3; ++j) {
		if (j < n1) {
		    goto L670;
		}
		l = j - ns;
		t = z[i + l * z_dim1];
		goto L675;
L670:
		t = rm1[i + (j + 2) * rm1_dim1];
L675:
		x = x - rm1[j + i * rm1_dim1] * rv1[j] + t * rv2[j];
		y = y - rm1[j + i * rm1_dim1] * rv2[j] - t * rv1[j];
/* L680: */
	    }

L700:
	    if (i < n1) {
		goto L710;
	    }
	    l = i - ns;
	    t = z[i + l * z_dim1];
	    goto L715;
L710:
	    t = rm1[i + (i + 2) * rm1_dim1];
L715:
	    cdiv_(&x, &y, &rm1[i + i * rm1_dim1], &t, &rv1[i], &rv2[i]);
/* L720: */
	}

/*        INCREMENT OP COUNT FOR LOOP 720. */
	latime_1.ops += (uk << 2) * (uk + 3);
/*     .......... ACCEPTANCE TEST FOR REAL OR COMPLEX   
                  EIGENVECTOR AND NORMALIZATION .......... */
L740:
	++its;
	norm = 0.;
	normv = 0.;

	i__2 = uk;
	for (i = 1; i <= i__2; ++i) {
	    if (ilambd == 0.) {
		x = (d__1 = rv1[i], abs(d__1));
	    }
	    if (ilambd != 0.) {
		x = pythag_(&rv1[i], &rv2[i]);
	    }
	    if (normv >= x) {
		goto L760;
	    }
	    normv = x;
	    j = i;
L760:
	    norm += x;
/* L780: */
	}

/*        INCREMENT OP COUNT ACCEPTANCE TEST */
	if (ilambd == 0.) {
	    latime_1.ops += uk;
	}
	if (ilambd != 0.) {
	    latime_1.ops += uk << 4;
	}

	if (norm < growto) {
	    goto L840;
	}
/*     .......... ACCEPT VECTOR .......... */
	x = rv1[j];
	if (ilambd == 0.) {
	    x = 1. / x;
	}
	if (ilambd != 0.) {
	    y = rv2[j];
	}


/*        INCREMENT OPCOUNT FOR LOOP 820 */
	if (ilambd == 0.) {
	    latime_1.ops += uk;
	}
	if (ilambd != 0.) {
	    latime_1.ops += uk << 4;
	}
	i__2 = uk;
	for (i = 1; i <= i__2; ++i) {
	    if (ilambd != 0.) {
		goto L800;
	    }
	    z[i + s * z_dim1] = rv1[i] * x;
	    goto L820;
L800:
	    cdiv_(&rv1[i], &rv2[i], &x, &y, &z[i + (s - 1) * z_dim1], &z[i + 
		    s * z_dim1]);
L820:
	    ;
	}

	if (uk == *n) {
	    goto L940;
	}
	j = uk + 1;
	goto L900;
/*     .......... IN-LINE PROCEDURE FOR CHOOSING   
                  A NEW STARTING VECTOR .......... */
L840:
	if (its >= uk) {
	    goto L880;
	}
	x = ukroot;
	y = eps3 / (x + 1.);
	rv1[1] = eps3;

	i__2 = uk;
	for (i = 2; i <= i__2; ++i) {
/* L860: */
	    rv1[i] = y;
	}

	j = uk - its + 1;
	rv1[j] -= eps3 * x;
	if (ilambd == 0.) {
	    goto L440;
	}
	goto L660;
/*     .......... SET ERROR -- UNACCEPTED EIGENVECTOR .......... */
L880:
	j = 1;
	*ierr = -k;
/*     .......... SET REMAINING VECTOR COMPONENTS TO ZERO .......... 
*/
L900:
	i__2 = *n;
	for (i = j; i <= i__2; ++i) {
	    z[i + s * z_dim1] = 0.;
	    if (ilambd != 0.) {
		z[i + (s - 1) * z_dim1] = 0.;
	    }
/* L920: */
	}

L940:
	++s;
L960:
	if (ip == -1) {
	    ip = 0;
	}
	if (ip == 1) {
	    ip = -1;
	}
/* L980: */
    }

    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF EIGENVECTOR   
                  SPACE REQUIRED .......... */
L1000:
    if (*ierr != 0) {
	*ierr -= *n;
    }
    if (*ierr == 0) {
	*ierr = -((*n << 1) + 1);
    }
L1001:
    *m = s - 1 - abs(ip);

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += opst;
    return 0;
} /* invit_   

   Subroutine */ int orthes_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *a, doublereal *ort)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, m;
    static doublereal scale;
    static integer la, ii, jj, mp, kp1;



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,   
       NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).   

       GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE   
       REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS   
       LOW THROUGH IGH TO UPPER HESSENBERG FORM BY   
       ORTHOGONAL SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING   
            SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,   
            SET LOW=1, IGH=N.   

          A CONTAINS THE INPUT MATRIX.   

       ON OUTPUT   

          A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT   
            THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION   
            IS STORED IN THE REMAINING TRIANGLE UNDER THE   
            HESSENBERG MATRIX.   

          ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.   
            ONLY ELEMENTS LOW THROUGH IGH ARE USED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --ort;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }


/*     INCREMENT OP COUNR FOR COMPUTING G,H,ORT(M),.. IN LOOP 180 */
    latime_1.ops += (la - kp1 + 1) * 6;
    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h = 0.;
	ort[m] = 0.;
	scale = 0.;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... 
  

       INCREMENT OP COUNT FOR LOOP 90 */
	latime_1.ops += *igh - m + 1;
	i__2 = *igh;
	for (i = m; i <= i__2; ++i) {
/* L90: */
	    scale += (d__1 = a[i + (m - 1) * a_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........   

       INCREMENT OP COUNT FOR LOOP 100 */
	latime_1.ops += (*igh - m + 1) * 3;
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i = mp - ii;
	    ort[i] = a[i + (m - 1) * a_dim1] / scale;
	    h += ort[i] * ort[i];
/* L100: */
	}

	d__1 = sqrt(h);
	g = -d_sign(&d__1, &ort[m]);
	h -= ort[m] * g;
	ort[m] -= g;
/*     .......... FORM (I-(U*UT)/H) * A ..........   

       INCREMENT OP COUNT FOR LOOP 130 AND 160 */
	latime_1.ops += (*n - m + 1 + *igh) * ((*igh - m + 1 << 2) + 1);
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    f = 0.;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i = mp - ii;
		f += ort[i] * a[i + j * a_dim1];
/* L110: */
	    }

	    f /= h;

	    i__3 = *igh;
	    for (i = m; i <= i__3; ++i) {
/* L120: */
		a[i + j * a_dim1] -= f * ort[i];
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i = 1; i <= i__2; ++i) {
	    f = 0.;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		f += ort[j] * a[i + j * a_dim1];
/* L140: */
	    }

	    f /= h;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
/* L150: */
		a[i + j * a_dim1] -= f * ort[j];
	    }

/* L160: */
	}

	ort[m] = scale * ort[m];
	a[m + (m - 1) * a_dim1] = scale * g;
L180:
	;
    }

L200:
    return 0;
} /* orthes_ */

doublereal pythag_(doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    static doublereal p, r, s, t, u;


/*     FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW   


       COMMON BLOCK TO RETURN OPERATION COUNT   
       OPST IS ONLY INCREMENTED HERE   
   Computing MAX */
    d__1 = abs(*a), d__2 = abs(*b);
    p = max(d__1,d__2);
    if (p == 0.) {
	goto L20;
    }
/* Computing MIN */
    d__2 = abs(*a), d__3 = abs(*b);
/* Computing 2nd power */
    d__1 = min(d__2,d__3) / p;
    r = d__1 * d__1;

/*     INCREMENT OPST */
    pythop_1.opst += 2;
L10:
    t = r + 4.;
    if (t == 4.) {
	goto L20;
    }
    s = r / t;
    u = s * 2. + 1.;
    p = u * p;
/* Computing 2nd power */
    d__1 = s / u;
    r = d__1 * d__1 * r;

/*        INCREMENT OPST */
    pythop_1.opst += 8;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag_   

   Subroutine */ int tqlrat_(integer *n, doublereal *d, doublereal *e2, 
	integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal b, c, f, g, h;
    static integer i, j, l, m;
    static doublereal p, r, s, t;
    static integer l1, ii;
    extern doublereal dlamch_(char *), pythag_(doublereal *, 
	    doublereal *), epslon_(doublereal *);
    static integer mml;
    static doublereal eps, tst;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEQR.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE CONTRIBUTIONS TO OPS FROM   
       FUNCTION PYTHAG.  IT IS PASSED TO AND FROM PYTHAG   
       THROUGH COMMON BLOCK PYTHOP.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,   
       ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.   

       THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC   
       TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE   
            INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY. 
  

        ON OUTPUT   

          D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN   
            ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND   
            ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE   
            THE SMALLEST EIGENVALUES.   

          E2 HAS BEEN DESTROYED.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE J-TH EIGENVALUE HAS NOT BEEN   
                       DETERMINED AFTER 30 ITERATIONS.   

       CALLS PYTHAG FOR  SQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    --e2;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

/*        INITIALIZE ITERATION COUNT AND OPST */
    latime_1.itcnt = 0.;
    pythop_1.opst = 0.;

/*     DETERMINE THE UNIT ROUNDOFF FOR THIS ENVIRONMENT. */

    eps = dlamch_("EPSILON");

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e2[i - 1] = e2[i];
    }

    f = 0.;
    t = 0.;
    e2[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h = (d__1 = d[l], abs(d__1)) + sqrt(e2[l]);
	if (t > h) {
	    goto L105;
	}
	t = h;
	b = epslon_(&t);
	c = b * b;

/*     INCREMENT OPCOUNT FOR THIS SECTION.   
       (FUNCTION EPSLON IS COUNTED AS 6 FLOPS.  THIS IS THE MINIMUM   
       NUMBER REQUIRED, BUT COUNTING THEM EXACTLY WOULD AFFECT   
       THE TIMING.) */
	latime_1.ops += 9;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ........
.. */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst = sqrt((d__1 = e2[m], abs(d__1)));
	    if (tst <= eps * ((d__1 = d[m], abs(d__1)) + (d__2 = d[m + 1], 
		    abs(d__2)))) {
		goto L120;
	    }
/*            IF (E2(M) .LE. C) GO TO 120   
       .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT   
                  THROUGH THE BOTTOM OF THE LOOP ..........   
   L110: */
	}

L120:

/*        INCREMENT OPCOUNT FOR FINDING SMALL SUBDIAGONAL ELEMENT.   
   Computing MIN */
	i__2 = m, i__3 = *n - 1;
	latime_1.ops += (min(i__2,i__3) - l + 1) * 3;
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d[l];
	p = (d[l1] - g) / (s * 2.);
	r = pythag_(&p, &c_b114);
	d[l] = s / (p + d_sign(&r, &p));
	h = g - d[l];

	i__2 = *n;
	for (i = l1; i <= i__2; ++i) {
/* L140: */
	    d[i] -= h;
	}

	f += h;

/*        INCREMENT OPCOUNT FOR FORMING SHIFT AND SUBTRACTING. */
	latime_1.ops = latime_1.ops + 8 + (i - l1 + 1);
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d[m];
	if (g == 0.) {
	    g = b;
	}
	h = g;
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = m - ii;
	    p = g * h;
	    r = p + e2[i];
	    e2[i + 1] = s * r;
	    s = e2[i] / r;
	    d[i + 1] = h + s * (h + d[i]);
	    g = d[i] - e2[i] / g;
	    if (g == 0.) {
		g = b;
	    }
	    h = g * p / r;
/* L200: */
	}

	e2[l] = s * g;
	d[l] = h;

/*        INCREMENT OPCOUNT FOR INNER LOOP. */
	latime_1.ops = latime_1.ops + mml * 11 + 1;

/*        INCREMENT ITERATION COUNTER */
	latime_1.itcnt += 1;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ........
.. */
	if (h == 0.) {
	    goto L210;
	}
	if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c / h, abs(d__2))) {
	    goto L210;
	}
	e2[l] = h * e2[l];
	if (e2[l] != 0.) {
	    goto L130;
	}
L210:
	p = d[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i = l + 2 - ii;
	    if (p >= d[i - 1]) {
		goto L270;
	    }
	    d[i] = d[i - 1];
/* L230: */
	}

L250:
	i = 1;
L270:
	d[i] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN   
                  EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:

/*     COMPUTE FINAL OP COUNT */
    latime_1.ops += pythop_1.opst;
    return 0;
} /* tqlrat_   

   Subroutine */ int tred1_(integer *nm, integer *n, doublereal *a, 
	doublereal *d, doublereal *e, doublereal *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, k, l;
    static doublereal scale;
    static integer ii, jp1;



/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT.   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED.   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,   
       NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).   

       THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX   
       TO A SYMMETRIC TRIDIAGONAL MATRIX USING   
       ORTHOGONAL SIMILARITY TRANSFORMATIONS.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE   
            LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.   

       ON OUTPUT   

          A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-   
            FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER   
            TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL   
            MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  


       Parameter adjustments */
    --e2;
    --e;
    --d;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body   
   Computing MAX   
   Computing 3rd power */
    d__3 = (doublereal) (*n), d__4 = d__3;
/* Computing 2nd power */
    d__5 = (doublereal) (*n);
    d__1 = 0., d__2 = d__4 * (d__3 * d__3) * 1.3333333333333333 + d__5 * d__5 
	    * 12. + *n * 3.6666666666666665 - 22;
    latime_1.ops += max(d__1,d__2);

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = a[*n + i * a_dim1];
	a[*n + i * a_dim1] = a[i + i * a_dim1];
/* L100: */
    }
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i = *n + 1 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d[k], abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i + j * a_dim1];
	    a[i + j * a_dim1] = 0.;
/* L125: */
	}

L130:
	e[i] = 0.;
	e2[i] = 0.;
	goto L300;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d[k] /= scale;
	    h += d[k] * d[k];
/* L150: */
	}

	e2[i] = scale * scale * h;
	f = d[l];
	d__1 = sqrt(h);
	g = -d_sign(&d__1, &f);
	e[i] = scale * g;
	h -= f * g;
	d[l] = f - g;
	if (l == 1) {
	    goto L285;
	}
/*     .......... FORM A*U .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j] + a[j + j * a_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += a[k + j * a_dim1] * d[k];
		e[k] += a[k + j * a_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h;
	    f += e[j] * d[j];
/* L245: */
	}

	h = f / (h + h);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= h * d[j];
	}
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		a[k + j * a_dim1] = a[k + j * a_dim1] - f * e[k] - g * d[k];
	    }

/* L280: */
	}

L285:
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    d[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i + j * a_dim1];
	    a[i + j * a_dim1] = f * scale;
/* L290: */
	}

L300:
	;
    }

    return 0;
} /* tred1_   

   Subroutine */ int bisect_(integer *n, doublereal *eps1, doublereal *d, 
	doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, 
	integer *mm, integer *m, doublereal *w, integer *ind, integer *ierr, 
	doublereal *rv4, doublereal *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Local variables */
    static integer i, j, k, l, p, q, r, s;
    static doublereal u, v, atoli, rtoli;
    static integer m1, m2;
    static doublereal tnorm, t1, t2, x0, x1;
    static integer ii;
    extern doublereal dlamch_(char *);
    static doublereal safemn, xu;
    extern doublereal epslon_(doublereal *);
    static doublereal pivmin;
    static integer isturm, tag;
    static doublereal ulp, tmp1, tmp2;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEBZ.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE BISECTION TECHNIQUE   
       IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL   
       SYMMETRIC MATRIX WHICH LIE IN A SPECIFIED INTERVAL,   
       USING BISECTION.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED   
            EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,   
            IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,   
            NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE   
            PRECISION AND THE 1-NORM OF THE SUBMATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2(1) IS ARBITRARY.   

          LB AND UB DEFINE THE INTERVAL TO BE SEARCHED FOR EIGENVALUES.   
            IF LB IS NOT LESS THAN UB, NO EIGENVALUES WILL BE FOUND.   

          MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF   
            EIGENVALUES IN THE INTERVAL.  WARNING. IF MORE THAN   
            MM EIGENVALUES ARE DETERMINED TO LIE IN THE INTERVAL,   
            AN ERROR RETURN IS MADE WITH NO EIGENVALUES FOUND.   

       ON OUTPUT   

          EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS   
            (LAST) DEFAULT VALUE.   

          D AND E ARE UNALTERED.   

          ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED   
            AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE   
            MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.   
            E2(1) IS ALSO SET TO ZERO.   

          M IS THE NUMBER OF EIGENVALUES DETERMINED TO LIE IN (LB,UB).   

          W CONTAINS THE M EIGENVALUES IN ASCENDING ORDER.   

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.. 
  

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            3*N+1      IF M EXCEEDS MM.   

          RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.   

       THE ALGOL PROCEDURE STURMCNT CONTAINED IN TRISTURM   
       APPEARS IN BISECT IN-LINE.   

       NOTE THAT SUBROUTINE TQL1 OR IMTQL1 IS GENERALLY FASTER THAN   
       BISECT, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.;
    safemn = dlamch_("S");
    ulp = dlamch_("E") * dlamch_("B");
    rtoli = ulp * 2.;
    *ierr = 0;
    tag = 0;
    t1 = *lb;
    t2 = *ub;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES .......... */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	if (i == 1) {
	    goto L20;
	}
/* CC         TST1 = DABS(D(I)) + DABS(D(I-1))   
   CC         TST2 = TST1 + DABS(E(I))   
   CC         IF (TST2 .GT. TST1) GO TO 40   
   Computing 2nd power */
	d__1 = e[i];
	tmp1 = d__1 * d__1;
/* Computing 2nd power */
	d__2 = ulp;
	if ((d__1 = d[i] * d[i - 1], abs(d__1)) * (d__2 * d__2) + safemn <= 
		tmp1) {
	    goto L40;
	}
L20:
	e2[i] = 0.;
L40:
	;
    }
/*           INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS. */
    latime_1.ops += (*n - 1) * 5;

/*                COMPUTE QUANTITIES NEEDED FOR CONVERGENCE TEST. */
    tmp1 = d[1] - abs(e[2]);
    tmp2 = d[1] + abs(e[2]);
    pivmin = 1.;
    i__1 = *n - 1;
    for (i = 2; i <= i__1; ++i) {
/* Computing MIN */
	d__3 = tmp1, d__4 = d[i] - (d__1 = e[i], abs(d__1)) - (d__2 = e[i + 1]
		, abs(d__2));
	tmp1 = min(d__3,d__4);
/* Computing MAX */
	d__3 = tmp2, d__4 = d[i] + (d__1 = e[i], abs(d__1)) + (d__2 = e[i + 1]
		, abs(d__2));
	tmp2 = max(d__3,d__4);
/* Computing MAX   
   Computing 2nd power */
	d__3 = e[i];
	d__1 = pivmin, d__2 = d__3 * d__3;
	pivmin = max(d__1,d__2);
/* L41: */
    }
/* Computing MIN */
    d__2 = tmp1, d__3 = d[*n] - (d__1 = e[*n], abs(d__1));
    tmp1 = min(d__2,d__3);
/* Computing MAX */
    d__2 = tmp2, d__3 = d[*n] + (d__1 = e[*n], abs(d__1));
    tmp2 = max(d__2,d__3);
/* Computing MAX   
   Computing 2nd power */
    d__3 = e[*n];
    d__1 = pivmin, d__2 = d__3 * d__3;
    pivmin = max(d__1,d__2);
    pivmin *= safemn;
/* Computing MAX */
    d__1 = abs(tmp1), d__2 = abs(tmp2);
    tnorm = max(d__1,d__2);
    atoli = ulp * tnorm;
/*        INCREMENT OPCOUNT FOR COMPUTING THESE QUANTITIES. */
    latime_1.ops += *n - 1 << 2;

/*     .......... DETERMINE THE NUMBER OF EIGENVALUES   
                  IN THE INTERVAL .......... */
    p = 1;
    q = *n;
    x1 = *ub;
    isturm = 1;
    goto L320;
L60:
    *m = s;
    x1 = *lb;
    isturm = 2;
    goto L320;
L80:
    *m -= s;
    if (*m > *mm) {
	goto L980;
    }
    q = 0;
    r = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING   
                  INTERVAL BY THE GERSCHGORIN BOUNDS .......... */
L100:
    if (r == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d[p];
    x0 = d[p];
    u = 0.;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.;
	v = 0.;
	if (q == *n) {
	    goto L110;
	}
	u = (d__1 = e[q + 1], abs(d__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	d__1 = d[q] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d[q] + (x1 + u);
	x0 = max(d__1,x0);
	if (v == 0.) {
	    goto L140;
	}
/* L120: */
    }
/*        INCREMENT OPCOUNT FOR REFINING INTERVAL. */
    latime_1.ops += *n - p + 1 << 1;

L140:
/* Computing MAX */
    d__2 = abs(xu), d__3 = abs(x0);
    d__1 = max(d__2,d__3);
    x1 = epslon_(&d__1);
    if (*eps1 <= 0.) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL .......... */
    if (t1 > d[p] || d[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d[p];
    goto L900;
L180:
    x1 *= q - p + 1;
/* Computing MAX */
    d__1 = t1, d__2 = xu - x1;
    *lb = max(d__1,d__2);
/* Computing MIN */
    d__1 = t2, d__2 = x0 + x1;
    *ub = min(d__1,d__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... FIND ROOTS BY BISECTION .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i = m1; i <= i__1; ++i) {
	rv5[i] = *ub;
	rv4[i] = *lb;
/* L240: */
    }
/*     .......... LOOP FOR K-TH EIGENVALUE   
                  FOR K=M2 STEP -1 UNTIL M1 DO --   
                  (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) .......... 
*/
    k = m2;
L250:
    xu = *lb;
/*     .......... FOR I=K STEP -1 UNTIL M1 DO -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i = m1 + k - ii;
	if (xu >= rv4[i]) {
	    goto L260;
	}
	xu = rv4[i];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... NEXT BISECTION STEP .......... */
L300:
    x1 = (xu + x0) * .5;
/* CC         IF ((X0 - XU) .LE. DABS(EPS1)) GO TO 420   
   CC         TST1 = 2.0D0 * (DABS(XU) + DABS(X0))   
   CC         TST2 = TST1 + (X0 - XU)   
   CC         IF (TST2 .EQ. TST1) GO TO 420 */
    tmp1 = (d__1 = x0 - xu, abs(d__1));
/* Computing MAX */
    d__1 = abs(x0), d__2 = abs(xu);
    tmp2 = max(d__1,d__2);
/* Computing MAX */
    d__1 = max(atoli,pivmin), d__2 = rtoli * tmp2;
    if (tmp1 < max(d__1,d__2)) {
	goto L420;
    }
/*     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE .......... */
L320:
    s = p - 1;
    u = 1.;

    i__1 = q;
    for (i = p; i <= i__1; ++i) {
	if (u != 0.) {
	    goto L325;
	}
	v = (d__1 = e[i], abs(d__1)) / epslon_(&c_b114);
	if (e2[i] == 0.) {
	    v = 0.;
	}
	goto L330;
L325:
	v = e2[i] / u;
L330:
	u = d[i] - x1 - v;
	if (u < 0.) {
	    ++s;
	}
/* L340: */
    }
/*           INCREMENT OPCOUNT FOR STURM SEQUENCE. */
    latime_1.ops += (q - p + 1) * 3;
/*           INCREMENT ITERATION COUNTER. */
    latime_1.itcnt += 1;

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... REFINE INTERVALS .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... K-TH EIGENVALUE FOUND .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... ORDER EIGENVALUES TAGGED WITH THEIR   
                  SUBMATRIX ASSOCIATIONS .......... */
L900:
    s = r;
    r = r + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i = l + s - ii;
	    w[i + 1] = w[i];
	    ind[i + 1] = ind[i];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF NUMBER OF   
                  EIGENVALUES IN INTERVAL .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* bisect_   

   Subroutine */ int tinvit_(integer *nm, integer *n, doublereal *d, 
	doublereal *e, doublereal *e2, integer *m, doublereal *w, integer *
	ind, doublereal *z, integer *ierr, doublereal *rv1, doublereal *rv2, 
	doublereal *rv3, doublereal *rv4, doublereal *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal norm;
    static integer i, j, p, q, r, s;
    static doublereal u, v, order;
    static integer group;
    static doublereal x0, x1;
    static integer ii, jj, ip;
    static doublereal uk, xu;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    static integer tag, its;
    static doublereal eps2, eps3, eps4;


/*     EISPACK ROUTINE.   

       CONVERGENCE TEST WAS NOT MODIFIED, SINCE IT SHOULD GIVE   
       APPROXIMATELY THE SAME LEVEL OF ACCURACY AS LAPACK ROUTINE,   
       ALTHOUGH THE EIGENVECTORS MAY NOT BE AS CLOSE TO ORTHOGONAL.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE INVERSE ITERATION TECH-   
       NIQUE IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A TRIDIAGONAL   
       SYMMETRIC MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES,   
       USING INVERSE ITERATION.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E,   
            WITH ZEROS CORRESPONDING TO NEGLIGIBLE ELEMENTS OF E.   
            E(I) IS CONSIDERED NEGLIGIBLE IF IT IS NOT LARGER THAN   
            THE PRODUCT OF THE RELATIVE MACHINE PRECISION AND THE SUM   
            OF THE MAGNITUDES OF D(I) AND D(I-1).  E2(1) MUST CONTAIN   
            0.0D0 IF THE EIGENVALUES ARE IN ASCENDING ORDER, OR 2.0D0   
            IF THE EIGENVALUES ARE IN DESCENDING ORDER.  IF  BISECT,   
            TRIDIB, OR  IMTQLV  HAS BEEN USED TO FIND THE EIGENVALUES,   
            THEIR OUTPUT E2 ARRAY IS EXACTLY WHAT IS EXPECTED HERE.   

          M IS THE NUMBER OF SPECIFIED EIGENVALUES.   

          W CONTAINS THE M EIGENVALUES IN ASCENDING OR DESCENDING ORDER. 
  

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC. 
  

       ON OUTPUT   

          ALL INPUT ARRAYS ARE UNALTERED.   

          Z CONTAINS THE ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS.   
            ANY VECTOR WHICH FAILS TO CONVERGE IS SET TO ZERO.   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            -R         IF THE EIGENVECTOR CORRESPONDING TO THE R-TH   
                       EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS.   

          RV1, RV2, RV3, RV4, AND RV6 ARE TEMPORARY STORAGE ARRAYS.   

       CALLS PYTHAG FOR  DSQRT(A*A + B*B) .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv6;
    --rv4;
    --rv3;
    --rv2;
    --rv1;
    --e2;
    --e;
    --d;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.;
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    tag = 0;
    order = 1. - e2[1];
    q = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX .......... */
L100:
    p = q + 1;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	if (q == *n) {
	    goto L140;
	}
	if (e2[q + 1] == 0.) {
	    goto L140;
	}
/* L120: */
    }
/*     .......... FIND VECTORS BY INVERSE ITERATION .......... */
L140:
    ++tag;
    s = 0;

    i__1 = *m;
    for (r = 1; r <= i__1; ++r) {
	if (ind[r] != tag) {
	    goto L920;
	}
	its = 1;
	x1 = w[r];
	if (s != 0) {
	    goto L510;
	}
/*     .......... CHECK FOR ISOLATED ROOT .......... */
	xu = 1.;
	if (p != q) {
	    goto L490;
	}
	rv6[p] = 1.;
	goto L870;
L490:
	norm = (d__1 = d[p], abs(d__1));
	ip = p + 1;

	i__2 = q;
	for (i = ip; i <= i__2; ++i) {
/* L500:   
   Computing MAX */
	    d__3 = norm, d__4 = (d__1 = d[i], abs(d__1)) + (d__2 = e[i], abs(
		    d__2));
	    norm = max(d__3,d__4);
	}
/*     .......... EPS2 IS THE CRITERION FOR GROUPING,   
                  EPS3 REPLACES ZERO PIVOTS AND EQUAL   
                  ROOTS ARE MODIFIED BY EPS3,   
                  EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW .........
. */
	eps2 = norm * .001;
	eps3 = epslon_(&norm);
	uk = (doublereal) (q - p + 1);
	eps4 = uk * eps3;
	uk = eps4 / sqrt(uk);
/*           INCREMENT OPCOUNT FOR COMPUTING CRITERIA. */
	latime_1.ops += q - ip + 4;
	s = p;
L505:
	group = 0;
	goto L520;
/*     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS .......... */
L510:
	if ((d__1 = x1 - x0, abs(d__1)) >= eps2) {
	    goto L505;
	}
	++group;
	if (order * (x1 - x0) <= 0.) {
	    x1 = x0 + order * eps3;
	}
/*     .......... ELIMINATION WITH INTERCHANGES AND   
                  INITIALIZATION OF VECTOR .......... */
L520:
	v = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
	    rv6[i] = uk;
	    if (i == p) {
		goto L560;
	    }
	    if ((d__1 = e[i], abs(d__1)) < abs(u)) {
		goto L540;
	    }
/*     .......... WARNING -- A DIVIDE CHECK MAY OCCUR HERE IF   
                  E2 ARRAY HAS NOT BEEN SPECIFIED CORRECTLY ......
.... */
	    xu = u / e[i];
	    rv4[i] = xu;
	    rv1[i - 1] = e[i];
	    rv2[i - 1] = d[i] - x1;
	    rv3[i - 1] = 0.;
	    if (i != q) {
		rv3[i - 1] = e[i + 1];
	    }
	    u = v - xu * rv2[i - 1];
	    v = -xu * rv3[i - 1];
	    goto L580;
L540:
	    xu = e[i] / u;
	    rv4[i] = xu;
	    rv1[i - 1] = u;
	    rv2[i - 1] = v;
	    rv3[i - 1] = 0.;
L560:
	    u = d[i] - x1 - xu * v;
	    if (i != q) {
		v = e[i + 1];
	    }
L580:
	    ;
	}
/*           INCREMENT OPCOUNT FOR ELIMINATION. */
	latime_1.ops += (q - p + 1) * 5;

	if (u == 0.) {
	    u = eps3;
	}
	rv1[q] = u;
	rv2[q] = 0.;
	rv3[q] = 0.;
/*     .......... BACK SUBSTITUTION   
                  FOR I=Q STEP -1 UNTIL P DO -- .......... */
L600:
	i__2 = q;
	for (ii = p; ii <= i__2; ++ii) {
	    i = p + q - ii;
	    rv6[i] = (rv6[i] - u * rv2[i] - v * rv3[i]) / rv1[i];
	    v = u;
	    u = rv6[i];
/* L620: */
	}
/*           INCREMENT OPCOUNT FOR BACK SUBSTITUTION. */
	latime_1.ops += (q - p + 1) * 5;
/*     .......... ORTHOGONALIZE WITH RESPECT TO PREVIOUS   
                  MEMBERS OF GROUP .......... */
	if (group == 0) {
	    goto L700;
	}
	j = r;

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
L630:
	    --j;
	    if (ind[j] != tag) {
		goto L630;
	    }
	    xu = 0.;

	    i__3 = q;
	    for (i = p; i <= i__3; ++i) {
/* L640: */
		xu += rv6[i] * z[i + j * z_dim1];
	    }

	    i__3 = q;
	    for (i = p; i <= i__3; ++i) {
/* L660: */
		rv6[i] -= xu * z[i + j * z_dim1];
	    }

/*              INCREMENT OPCOUNT FOR ORTHOGONALIZING. */
	    latime_1.ops += q - p + 1 << 2;
/* L680: */
	}

L700:
	norm = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L720: */
	    norm += (d__1 = rv6[i], abs(d__1));
	}
/*           INCREMENT OPCOUNT FOR COMPUTING NORM. */
	latime_1.ops += q - p + 1;

	if (norm >= 1.) {
	    goto L840;
	}
/*     .......... FORWARD SUBSTITUTION .......... */
	if (its == 5) {
	    goto L830;
	}
	if (norm != 0.) {
	    goto L740;
	}
	rv6[s] = eps4;
	++s;
	if (s > q) {
	    s = p;
	}
	goto L780;
L740:
	xu = eps4 / norm;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L760: */
	    rv6[i] *= xu;
	}
/*     .......... ELIMINATION OPERATIONS ON NEXT VECTOR   
                  ITERATE .......... */
L780:
	i__2 = q;
	for (i = ip; i <= i__2; ++i) {
	    u = rv6[i];
/*     .......... IF RV1(I-1) .EQ. E(I), A ROW INTERCHANGE   
                  WAS PERFORMED EARLIER IN THE   
                  TRIANGULARIZATION PROCESS .......... */
	    if (rv1[i - 1] != e[i]) {
		goto L800;
	    }
	    u = rv6[i - 1];
	    rv6[i - 1] = rv6[i];
L800:
	    rv6[i] = u - rv4[i] * rv6[i - 1];
/* L820: */
	}
/*           INCREMENT OPCOUNT FOR FORWARD SUBSTITUTION. */
	latime_1.ops = latime_1.ops + (q - p + 1) + (q - ip + 1 << 1);

	++its;
/*           INCREMENT ITERATION COUNTER. */
	latime_1.itcnt += 1;
	goto L600;
/*     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR .......... */
L830:
	*ierr = -r;
	xu = 0.;
	goto L870;
/*     .......... NORMALIZE SO THAT SUM OF SQUARES IS   
                  1 AND EXPAND TO FULL ORDER .......... */
L840:
	u = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L860: */
	    u = pythag_(&u, &rv6[i]);
	}

	xu = 1. / u;

L870:
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
/* L880: */
	    z[i + r * z_dim1] = 0.;
	}

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L900: */
	    z[i + r * z_dim1] = rv6[i] * xu;
	}
/*           INCREMENT OPCOUNT FOR NORMALIZING. */
	latime_1.ops += q - p + 1;

	x0 = x1;
L920:
	;
    }

    if (q < *n) {
	goto L100;
    }
/*        INCREMENT OPCOUNT FOR USE OF FUNCTION PYTHAG. */
    latime_1.ops += pythop_1.opst;
L1001:
    return 0;
} /* tinvit_   

   Subroutine */ int tridib_(integer *n, doublereal *eps1, doublereal *d, 
	doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, 
	integer *m11, integer *m, doublereal *w, integer *ind, integer *ierr, 
	doublereal *rv4, doublereal *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    static integer i, j, k, l, p, q, r, s;
    static doublereal u, v, atoli, rtoli;
    static integer m1, m2;
    static doublereal tnorm, t1, t2, x0, x1;
    static integer m22, ii;
    extern doublereal dlamch_(char *);
    static doublereal safemn, xu;
    extern doublereal epslon_(doublereal *);
    static doublereal pivmin;
    static integer isturm, tag;
    static doublereal ulp, tmp1, tmp2;


/*     EISPACK ROUTINE.   
       MODIFIED FOR COMPARISON WITH LAPACK ROUTINES.   

       CONVERGENCE TEST WAS MODIFIED TO BE THE SAME AS IN DSTEBZ.   



       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   

       THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE BISECT,   
       NUM. MATH. 9, 386-393(1967) BY BARTH, MARTIN, AND WILKINSON.   
       HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 249-256(1971).   

       THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL   
       SYMMETRIC MATRIX BETWEEN SPECIFIED BOUNDARY INDICES,   
       USING BISECTION.   

       ON INPUT   

          N IS THE ORDER OF THE MATRIX.   

          EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED   
            EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE,   
            IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE,   
            NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE   
            PRECISION AND THE 1-NORM OF THE SUBMATRIX.   

          D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.   

          E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX   
            IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.   

          E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.   
            E2(1) IS ARBITRARY.   

          M11 SPECIFIES THE LOWER BOUNDARY INDEX FOR THE DESIRED   
            EIGENVALUES.   

          M SPECIFIES THE NUMBER OF EIGENVALUES DESIRED.  THE UPPER   
            BOUNDARY INDEX M22 IS THEN OBTAINED AS M22=M11+M-1.   

       ON OUTPUT   

          EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS   
            (LAST) DEFAULT VALUE.   

          D AND E ARE UNALTERED.   

          ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED   
            AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE   
            MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES.   
            E2(1) IS ALSO SET TO ZERO.   

          LB AND UB DEFINE AN INTERVAL CONTAINING EXACTLY THE DESIRED   
            EIGENVALUES.   

          W CONTAINS, IN ITS FIRST M POSITIONS, THE EIGENVALUES   
            BETWEEN INDICES M11 AND M22 IN ASCENDING ORDER.   

          IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES   
            ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W --   
            1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM   
            THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.. 
  

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            3*N+1      IF MULTIPLE EIGENVALUES AT INDEX M11 MAKE   
                       UNIQUE SELECTION IMPOSSIBLE,   
            3*N+2      IF MULTIPLE EIGENVALUES AT INDEX M22 MAKE   
                       UNIQUE SELECTION IMPOSSIBLE.   

          RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS.   

       NOTE THAT SUBROUTINE TQL1, IMTQL1, OR TQLRAT IS GENERALLY FASTER   
       THAN TRIDIB, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

          INITIALIZE ITERATION COUNT.   
       Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d;
    --ind;
    --w;

    /* Function Body */
    latime_1.itcnt = 0.;
    safemn = dlamch_("S");
    ulp = dlamch_("E") * dlamch_("B");
    rtoli = ulp * 2.;
    *ierr = 0;
    tag = 0;
    xu = d[1];
    x0 = d[1];
    u = 0.;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES AND DETERMINE AN   
                  INTERVAL CONTAINING ALL THE EIGENVALUES .......... */
    pivmin = 1.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	x1 = u;
	u = 0.;
	if (i != *n) {
	    u = (d__1 = e[i + 1], abs(d__1));
	}
/* Computing MIN */
	d__1 = d[i] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d[i] + (x1 + u);
	x0 = max(d__1,x0);
	if (i == 1) {
	    goto L20;
	}
/* CC         TST1 = DABS(D(I)) + DABS(D(I-1))   
   CC         TST2 = TST1 + DABS(E(I))   
   CC         IF (TST2 .GT. TST1) GO TO 40   
   Computing 2nd power */
	d__1 = e[i];
	tmp1 = d__1 * d__1;
/* Computing 2nd power */
	d__2 = ulp;
	if ((d__1 = d[i] * d[i - 1], abs(d__1)) * (d__2 * d__2) + safemn <= 
		tmp1) {
	    pivmin = max(pivmin,tmp1);
	    goto L40;
	}
L20:
	e2[i] = 0.;
L40:
	;
    }
    pivmin *= safemn;
/* Computing MAX */
    d__1 = abs(xu), d__2 = abs(x0);
    tnorm = max(d__1,d__2);
    atoli = ulp * tnorm;
/*        INCREMENT OPCOUNT FOR DETERMINING IF MATRIX SPLITS. */
    latime_1.ops += (*n - 1) * 9;

    x1 = (doublereal) (*n);
/* Computing MAX */
    d__2 = abs(xu), d__3 = abs(x0);
    d__1 = max(d__2,d__3);
    x1 *= epslon_(&d__1);
    xu -= x1;
    t1 = xu;
    x0 += x1;
    t2 = x0;
/*     .......... DETERMINE AN INTERVAL CONTAINING EXACTLY   
                  THE DESIRED EIGENVALUES .......... */
    p = 1;
    q = *n;
    m1 = *m11 - 1;
    if (m1 == 0) {
	goto L75;
    }
    isturm = 1;
L50:
    v = x1;
    x1 = xu + (x0 - xu) * .5;
    if (x1 == v) {
	goto L980;
    }
    goto L320;
L60:
    if ((i__1 = s - m1) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L73;
    } else {
	goto L70;
    }
L65:
    xu = x1;
    goto L50;
L70:
    x0 = x1;
    goto L50;
L73:
    xu = x1;
    t1 = x1;
L75:
    m22 = m1 + *m;
    if (m22 == *n) {
	goto L90;
    }
    x0 = t2;
    isturm = 2;
    goto L50;
L80:
    if ((i__1 = s - m22) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L85;
    } else {
	goto L70;
    }
L85:
    t2 = x1;
L90:
    q = 0;
    r = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING   
                  INTERVAL BY THE GERSCHGORIN BOUNDS .......... */
L100:
    if (r == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d[p];
    x0 = d[p];
    u = 0.;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.;
	v = 0.;
	if (q == *n) {
	    goto L110;
	}
	u = (d__1 = e[q + 1], abs(d__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	d__1 = d[q] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d[q] + (x1 + u);
	x0 = max(d__1,x0);
	if (v == 0.) {
	    goto L140;
	}
/* L120: */
    }
/*        INCREMENT OPCOUNT FOR REFINING INTERVAL. */
    latime_1.ops += *n - p + 1 << 1;

L140:
/* Computing MAX */
    d__2 = abs(xu), d__3 = abs(x0);
    d__1 = max(d__2,d__3);
    x1 = epslon_(&d__1);
    if (*eps1 <= 0.) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL .......... */
    if (t1 > d[p] || d[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d[p];
    goto L900;
L180:
    x1 *= q - p + 1;
/* Computing MAX */
    d__1 = t1, d__2 = xu - x1;
    *lb = max(d__1,d__2);
/* Computing MIN */
    d__1 = t2, d__2 = x0 + x1;
    *ub = min(d__1,d__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... FIND ROOTS BY BISECTION .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i = m1; i <= i__1; ++i) {
	rv5[i] = *ub;
	rv4[i] = *lb;
/* L240: */
    }
/*     .......... LOOP FOR K-TH EIGENVALUE   
                  FOR K=M2 STEP -1 UNTIL M1 DO --   
                  (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) .......... 
*/
    k = m2;
L250:
    xu = *lb;
/*     .......... FOR I=K STEP -1 UNTIL M1 DO -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i = m1 + k - ii;
	if (xu >= rv4[i]) {
	    goto L260;
	}
	xu = rv4[i];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... NEXT BISECTION STEP .......... */
L300:
    x1 = (xu + x0) * .5;
/* CC         IF ((X0 - XU) .LE. DABS(EPS1)) GO TO 420   
   CC         TST1 = 2.0D0 * (DABS(XU) + DABS(X0))   
   CC         TST2 = TST1 + (X0 - XU)   
   CC         IF (TST2 .EQ. TST1) GO TO 420 */
    tmp1 = (d__1 = x0 - xu, abs(d__1));
/* Computing MAX */
    d__1 = abs(x0), d__2 = abs(xu);
    tmp2 = max(d__1,d__2);
/* Computing MAX */
    d__1 = max(atoli,pivmin), d__2 = rtoli * tmp2;
    if (tmp1 < max(d__1,d__2)) {
	goto L420;
    }
/*     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE .......... */
L320:
    s = p - 1;
    u = 1.;

    i__1 = q;
    for (i = p; i <= i__1; ++i) {
	if (u != 0.) {
	    goto L325;
	}
	v = (d__1 = e[i], abs(d__1)) / epslon_(&c_b114);
	if (e2[i] == 0.) {
	    v = 0.;
	}
	goto L330;
L325:
	v = e2[i] / u;
L330:
	u = d[i] - x1 - v;
	if (u < 0.) {
	    ++s;
	}
/* L340: */
    }
/*           INCREMENT OPCOUNT FOR STURM SEQUENCE. */
    latime_1.ops += (q - p + 1) * 3;
/*           INCREMENT ITERATION COUNTER. */
    latime_1.itcnt += 1;

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... REFINE INTERVALS .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... K-TH EIGENVALUE FOUND .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... ORDER EIGENVALUES TAGGED WITH THEIR   
                  SUBMATRIX ASSOCIATIONS .......... */
L900:
    s = r;
    r = r + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i = l + s - ii;
	    w[i + 1] = w[i];
	    ind[i + 1] = ind[i];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- INTERVAL CANNOT BE FOUND CONTAINING   
                  EXACTLY THE DESIRED EIGENVALUES .......... */
L980:
    *ierr = *n * 3 + isturm;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* tridib_   

   Subroutine */ int dsvdc_(doublereal *x, integer *ldx, integer *n, integer *
	p, doublereal *s, doublereal *e, doublereal *u, integer *ldu, 
	doublereal *v, integer *ldv, doublereal *work, integer *job, integer *
	info)
{
    /* System generated locals */
    integer x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static integer kase;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer jobu, iter;
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *);
    static doublereal test;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static integer nctp1;
    static doublereal b, c;
    static integer nrtp1;
    static doublereal f, g;
    static integer i, j, k, l, m;
    static doublereal t, scale;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    static doublereal shift;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), drotg_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static integer maxit;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    static doublereal iopst;
    static logical wantu, wantv;
    static doublereal t1, el;
    static integer kk;
    static doublereal cs;
    static integer ll;
    extern doublereal dlamch_(char *);
    static integer mm, ls;
    static doublereal sl;
    static integer lu;
    static doublereal sm, sn;
    static integer lm1, mm1, lp1, mp1, nct, ncu;
    static doublereal eps;
    static integer lls, nrt;
    static doublereal emm1, smm1;


/*     COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, IOPS IS ONLY INCREMENTED   
       IOPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO IOPS   
       TO AVOID ROUNDOFF ERROR   


       DSVDC IS A SUBROUTINE TO REDUCE A DOUBLE PRECISION NXP MATRIX X   
       BY ORTHOGONAL TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE   
       DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE   
       COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,   
       AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.   

       ON ENTRY   

           X         DOUBLE PRECISION(LDX,P), WHERE LDX.GE.N.   
                     X CONTAINS THE MATRIX WHOSE SINGULAR VALUE   
                     DECOMPOSITION IS TO BE COMPUTED.  X IS   
                     DESTROYED BY DSVDC.   

           LDX       INTEGER.   
                     LDX IS THE LEADING DIMENSION OF THE ARRAY X.   

           N         INTEGER.   
                     N IS THE NUMBER OF ROWS OF THE MATRIX X.   

           P         INTEGER.   
                     P IS THE NUMBER OF COLUMNS OF THE MATRIX X.   

           LDU       INTEGER.   
                     LDU IS THE LEADING DIMENSION OF THE ARRAY U.   
                     (SEE BELOW).   

           LDV       INTEGER.   
                     LDV IS THE LEADING DIMENSION OF THE ARRAY V.   
                     (SEE BELOW).   

           WORK      DOUBLE PRECISION(N).   
                     WORK IS A SCRATCH ARRAY.   

           JOB       INTEGER.   
                     JOB CONTROLS THE COMPUTATION OF THE SINGULAR   
                     VECTORS.  IT HAS THE DECIMAL EXPANSION AB   
                     WITH THE FOLLOWING MEANING   

                          A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR   
                                    VECTORS.   
                          A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS   
                                    IN U.   
                          A.GE.2    RETURN THE FIRST MIN(N,P) SINGULAR   
                                    VECTORS IN U.   
                          B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR   
                                    VECTORS.   
                          B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS   
                                    IN V.   

       ON RETURN   

           S         DOUBLE PRECISION(MM), WHERE MM=MIN(N+1,P).   
                     THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE   
                     SINGULAR VALUES OF X ARRANGED IN DESCENDING   
                     ORDER OF MAGNITUDE.   

           E         DOUBLE PRECISION(P),   
                     E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE   
                     DISCUSSION OF INFO FOR EXCEPTIONS.   

           U         DOUBLE PRECISION(LDU,K), WHERE LDU.GE.N.  IF   
                                     JOBA.EQ.1 THEN K.EQ.N, IF JOBA.GE.2 
  
                                     THEN K.EQ.MIN(N,P).   
                     U CONTAINS THE MATRIX OF LEFT SINGULAR VECTORS.   
                     U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P   
                     OR IF JOBA.EQ.2, THEN U MAY BE IDENTIFIED WITH X   
                     IN THE SUBROUTINE CALL.   

           V         DOUBLE PRECISION(LDV,P), WHERE LDV.GE.P.   
                     V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.   
                     V IS NOT REFERENCED IF JOB.EQ.0.  IF P.LE.N,   
                     THEN V MAY BE IDENTIFIED WITH X IN THE   
                     SUBROUTINE CALL.   

           INFO      INTEGER.   
                     THE SINGULAR VALUES (AND THEIR CORRESPONDING   
                     SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)   
                     ARE CORRECT (HERE M=MIN(N,P)).  THUS IF   
                     INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR   
                     VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX   
                     B = TRANS(U)*X*V IS THE BIDIAGONAL MATRIX   
                     WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE   
                     ELEMENTS OF E ON ITS SUPER-DIAGONAL (TRANS(U)   
                     IS THE TRANSPOSE OF U).  THUS THE SINGULAR   
                     VALUES OF X AND B ARE THE SAME.   

       LINPACK. THIS VERSION DATED 08/14/78 .   
                CORRECTION MADE TO SHIFT 2/84.   
       G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.   

       DSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.   

       EXTERNAL DROT   
       BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2,DROTG   
       FORTRAN DABS,DMAX1,MAX0,MIN0,MOD,DSQRT   

       INTERNAL VARIABLES   

       DOUBLE PRECISION ZTEST,R   

       GET EPS FROM DLAMCH FOR NEW STOPPING CRITERION   
       Parameter adjustments */
    x_dim1 = *ldx;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    --s;
    --e;
    u_dim1 = *ldu;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    v_dim1 = *ldv;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    --work;

    /* Function Body */
    if (*n <= 0 || *p <= 0) {
	return 0;
    }
    eps = dlamch_("EPSILON");



/*     SET THE MAXIMUM NUMBER OF ITERATIONS. */

    maxit = 50;

/*     DETERMINE WHAT IS TO BE COMPUTED. */

    wantu = FALSE_;
    wantv = FALSE_;
    jobu = *job % 100 / 10;
    ncu = *n;
    if (jobu > 1) {
	ncu = min(*n,*p);
    }
    if (jobu != 0) {
	wantu = TRUE_;
    }
    if (*job % 10 != 0) {
	wantv = TRUE_;
    }

/*     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS   
       IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.   


       INITIALIZE OP COUNT */
    iopst = 0.;
    *info = 0;
/* Computing MIN */
    i__1 = *n - 1;
    nct = min(i__1,*p);
/* Computing MAX   
   Computing MIN */
    i__3 = *p - 2;
    i__1 = 0, i__2 = min(i__3,*n);
    nrt = max(i__1,i__2);
    lu = max(nct,nrt);
    if (lu < 1) {
	goto L170;
    }
    i__1 = lu;
    for (l = 1; l <= i__1; ++l) {
	lp1 = l + 1;
	if (l > nct) {
	    goto L20;
	}

/*           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND   
             PLACE THE L-TH DIAGONAL IN S(L).   


             INCREMENT OP COUNT */
	latime_2.iops += (*n - l + 1 << 1) + 1;
	i__2 = *n - l + 1;
	s[l] = dnrm2_(&i__2, &x[l + l * x_dim1], &c__1);
	if (s[l] == 0.) {
	    goto L10;
	}
	if (x[l + l * x_dim1] != 0.) {
	    s[l] = d_sign(&s[l], &x[l + l * x_dim1]);
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += *n - l + 3;
	i__2 = *n - l + 1;
	d__1 = 1. / s[l];
	dscal_(&i__2, &d__1, &x[l + l * x_dim1], &c__1);
	x[l + l * x_dim1] += 1.;
L10:
	s[l] = -s[l];
L20:
	if (*p < lp1) {
	    goto L50;
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    if (l > nct) {
		goto L30;
	    }
	    if (s[l] == 0.) {
		goto L30;
	    }

/*              APPLY THE TRANSFORMATION.   


                INCREMENT OP COUNT */
	    latime_2.iops += (*n - l << 2) + 5;
	    i__3 = *n - l + 1;
	    t = -ddot_(&i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1) / x[l + l * x_dim1];
	    i__3 = *n - l + 1;
	    daxpy_(&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
		    c__1);
L30:

/*           PLACE THE L-TH ROW OF X INTO  E FOR THE   
             SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION. */

	    e[j] = x[l + j * x_dim1];
/* L40: */
	}
L50:
	if (! wantu || l > nct) {
	    goto L70;
	}

/*           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK   
             MULTIPLICATION. */

	i__2 = *n;
	for (i = l; i <= i__2; ++i) {
	    u[i + l * u_dim1] = x[i + l * x_dim1];
/* L60: */
	}
L70:
	if (l > nrt) {
	    goto L150;
	}

/*           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE   
             L-TH SUPER-DIAGONAL IN E(L).   


             INCREMENT OP COUNT */
	latime_2.iops += (*p - l << 1) + 1;
	i__2 = *p - l;
	e[l] = dnrm2_(&i__2, &e[lp1], &c__1);
	if (e[l] == 0.) {
	    goto L80;
	}
	if (e[lp1] != 0.) {
	    e[l] = d_sign(&e[l], &e[lp1]);
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += *p - l + 2;
	i__2 = *p - l;
	d__1 = 1. / e[l];
	dscal_(&i__2, &d__1, &e[lp1], &c__1);
	e[lp1] += 1.;
L80:
	e[l] = -e[l];
	if (lp1 > *n || e[l] == 0.) {
	    goto L120;
	}

/*              APPLY THE TRANSFORMATION. */

	i__2 = *n;
	for (i = lp1; i <= i__2; ++i) {
	    work[i] = 0.;
/* L90: */
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (doublereal) ((*n - l << 2) + 1) * (*p - l);
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    daxpy_(&i__3, &e[j], &x[lp1 + j * x_dim1], &c__1, &work[lp1], &
		    c__1);
/* L100: */
	}
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l;
	    d__1 = -e[j] / e[lp1];
	    daxpy_(&i__3, &d__1, &work[lp1], &c__1, &x[lp1 + j * x_dim1], &
		    c__1);
/* L110: */
	}
L120:
	if (! wantv) {
	    goto L140;
	}

/*              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT   
                BACK MULTIPLICATION. */

	i__2 = *p;
	for (i = lp1; i <= i__2; ++i) {
	    v[i + l * v_dim1] = e[i];
/* L130: */
	}
L140:
L150:
/* L160: */
	;
    }
L170:

/*     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.   

   Computing MIN */
    i__1 = *p, i__2 = *n + 1;
    m = min(i__1,i__2);
    nctp1 = nct + 1;
    nrtp1 = nrt + 1;
    if (nct < *p) {
	s[nctp1] = x[nctp1 + nctp1 * x_dim1];
    }
    if (*n < m) {
	s[m] = 0.;
    }
    if (nrtp1 < m) {
	e[nrtp1] = x[nrtp1 + m * x_dim1];
    }
    e[m] = 0.;

/*     IF REQUIRED, GENERATE U. */

    if (! wantu) {
	goto L300;
    }
    if (ncu < nctp1) {
	goto L200;
    }
    i__1 = ncu;
    for (j = nctp1; j <= i__1; ++j) {
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    u[i + j * u_dim1] = 0.;
/* L180: */
	}
	u[j + j * u_dim1] = 1.;
/* L190: */
    }
L200:
    if (nct < 1) {
	goto L290;
    }
    i__1 = nct;
    for (ll = 1; ll <= i__1; ++ll) {
	l = nct - ll + 1;
	if (s[l] == 0.) {
	    goto L250;
	}
	lp1 = l + 1;
	if (ncu < lp1) {
	    goto L220;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (doublereal) ((*n - l << 2) + 5) * (ncu - l) + (*n - 
		l + 2);
	i__2 = ncu;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *n - l + 1;
	    t = -ddot_(&i__3, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1) / u[l + l * u_dim1];
	    i__3 = *n - l + 1;
	    daxpy_(&i__3, &t, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
		    c__1);
/* L210: */
	}
L220:
	i__2 = *n - l + 1;
	dscal_(&i__2, &c_b405, &u[l + l * u_dim1], &c__1);
	u[l + l * u_dim1] += 1.;
	lm1 = l - 1;
	if (lm1 < 1) {
	    goto L240;
	}
	i__2 = lm1;
	for (i = 1; i <= i__2; ++i) {
	    u[i + l * u_dim1] = 0.;
/* L230: */
	}
L240:
	goto L270;
L250:
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    u[i + l * u_dim1] = 0.;
/* L260: */
	}
	u[l + l * u_dim1] = 1.;
L270:
/* L280: */
	;
    }
L290:
L300:

/*     IF IT IS REQUIRED, GENERATE V. */

    if (! wantv) {
	goto L350;
    }
    i__1 = *p;
    for (ll = 1; ll <= i__1; ++ll) {
	l = *p - ll + 1;
	lp1 = l + 1;
	if (l > nrt) {
	    goto L320;
	}
	if (e[l] == 0.) {
	    goto L320;
	}

/*              INCREMENT OP COUNT */
	latime_2.iops += (doublereal) ((*p - l << 2) + 1) * (*p - l);
	i__2 = *p;
	for (j = lp1; j <= i__2; ++j) {
	    i__3 = *p - l;
	    t = -ddot_(&i__3, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1) / v[lp1 + l * v_dim1];
	    i__3 = *p - l;
	    daxpy_(&i__3, &t, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j * 
		    v_dim1], &c__1);
/* L310: */
	}
L320:
	i__2 = *p;
	for (i = 1; i <= i__2; ++i) {
	    v[i + l * v_dim1] = 0.;
/* L330: */
	}
	v[l + l * v_dim1] = 1.;
/* L340: */
    }
L350:

/*     MAIN ITERATION LOOP FOR THE SINGULAR VALUES. */

    mm = m;

/*     INITIALIZE ITERATION COUNTER */
    latime_2.itcnt = 0.;
    iter = 0;
L360:

/*        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.   

       ...EXIT */
    if (m == 0) {
	goto L620;
    }

/*        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET   
          FLAG AND RETURN.   


          UPDATE ITERATION COUNTER */
    latime_2.itcnt = (doublereal) iter;
    if (iter < maxit) {
	goto L370;
    }
    *info = m;
/*     ......EXIT */
    goto L620;
L370:

/*        THIS SECTION OF THE PROGRAM INSPECTS FOR   
          NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON   
          COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS.   

             KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M   
             KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M   
             KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND   
                          S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP).   
             KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE). */

    i__1 = m;
    for (ll = 1; ll <= i__1; ++ll) {
	l = m - ll;
/*        ...EXIT */
	if (l == 0) {
	    goto L400;
	}

/*           INCREMENT OP COUNT */
	iopst += 2;
	test = (d__1 = s[l], abs(d__1)) + (d__2 = s[l + 1], abs(d__2));

/*           REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK   

             ZTEST = TEST + DABS(E(L))   
             IF (ZTEST .NE. TEST) GO TO 380 */
	if ((d__1 = e[l], abs(d__1)) > eps * test) {
	    goto L380;
	}

	e[l] = 0.;
/*        ......EXIT */
	goto L400;
L380:
/* L390: */
	;
    }
L400:
    if (l != m - 1) {
	goto L410;
    }
    kase = 4;
    goto L480;
L410:
    lp1 = l + 1;
    mp1 = m + 1;
    i__1 = mp1;
    for (lls = lp1; lls <= i__1; ++lls) {
	ls = m - lls + lp1;
/*           ...EXIT */
	if (ls == l) {
	    goto L440;
	}
	test = 0.;

/*              INCREMENT OP COUNT */
	iopst += 3;
	if (ls != m) {
	    test += (d__1 = e[ls], abs(d__1));
	}
	if (ls != l + 1) {
	    test += (d__1 = e[ls - 1], abs(d__1));
	}

/*              REPLACE STOPPING CRITERION WITH NEW ONE AS IN LAPACK 
  

                ZTEST = TEST + DABS(S(LS))   
                IF (ZTEST .NE. TEST) GO TO 420 */
	if ((d__1 = s[ls], abs(d__1)) > eps * test) {
	    goto L420;
	}

	s[ls] = 0.;
/*           ......EXIT */
	goto L440;
L420:
/* L430: */
	;
    }
L440:
    if (ls != l) {
	goto L450;
    }
    kase = 3;
    goto L470;
L450:
    if (ls != m) {
	goto L460;
    }
    kase = 1;
    goto L470;
L460:
    kase = 2;
    l = ls;
L470:
L480:
    ++l;

/*        PERFORM THE TASK INDICATED BY KASE. */

    switch (kase) {
	case 1:  goto L490;
	case 2:  goto L520;
	case 3:  goto L540;
	case 4:  goto L570;
    }

/*        DEFLATE NEGLIGIBLE S(M). */

L490:
    mm1 = m - 1;
    f = e[m - 1];
    e[m - 1] = 0.;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 13 - 2;
    if (wantv) {
	latime_2.iops += (doublereal) (mm1 - l + 1) * 6 * *p;
    }
    i__1 = mm1;
    for (kk = l; kk <= i__1; ++kk) {
	k = mm1 - kk + l;
	t1 = s[k];
	drotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	if (k == l) {
	    goto L500;
	}
	f = -sn * e[k - 1];
	e[k - 1] = cs * e[k - 1];
L500:
	if (wantv) {
	    drot_(p, &v[k * v_dim1 + 1], &c__1, &v[m * v_dim1 + 1], &c__1, &
		    cs, &sn);
	}
/* L510: */
    }
    goto L610;

/*        SPLIT AT NEGLIGIBLE S(L). */

L520:
    f = e[l - 1];
    e[l - 1] = 0.;

/*           INCREMENT OP COUNT */
    latime_2.iops += (m - l + 1) * 13;
    if (wantu) {
	latime_2.iops += (doublereal) (m - l + 1) * 6 * *n;
    }
    i__1 = m;
    for (k = l; k <= i__1; ++k) {
	t1 = s[k];
	drotg_(&t1, &f, &cs, &sn);
	s[k] = t1;
	f = -sn * e[k];
	e[k] = cs * e[k];
	if (wantu) {
	    drot_(n, &u[k * u_dim1 + 1], &c__1, &u[(l - 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L530: */
    }
    goto L610;

/*        PERFORM ONE QR STEP. */

L540:

/*           CALCULATE THE SHIFT.   


             INCREMENT OP COUNT */
    iopst += 23;
/* Computing MAX */
    d__6 = (d__1 = s[m], abs(d__1)), d__7 = (d__2 = s[m - 1], abs(d__2)), 
	    d__6 = max(d__6,d__7), d__7 = (d__3 = e[m - 1], abs(d__3)), d__6 =
	     max(d__6,d__7), d__7 = (d__4 = s[l], abs(d__4)), d__6 = max(d__6,
	    d__7), d__7 = (d__5 = e[l], abs(d__5));
    scale = max(d__6,d__7);
    sm = s[m] / scale;
    smm1 = s[m - 1] / scale;
    emm1 = e[m - 1] / scale;
    sl = s[l] / scale;
    el = e[l] / scale;
/* Computing 2nd power */
    d__1 = emm1;
    b = ((smm1 + sm) * (smm1 - sm) + d__1 * d__1) / 2.;
/* Computing 2nd power */
    d__1 = sm * emm1;
    c = d__1 * d__1;
    shift = 0.;
    if (b == 0. && c == 0.) {
	goto L550;
    }
/* Computing 2nd power */
    d__1 = b;
    shift = sqrt(d__1 * d__1 + c);
    if (b < 0.) {
	shift = -shift;
    }
    shift = c / (b + shift);
L550:
    f = (sl + sm) * (sl - sm) + shift;
    g = sl * el;

/*           CHASE ZEROS. */

    mm1 = m - 1;

/*           INCREMENT OP COUNT */
    latime_2.iops += (mm1 - l + 1) * 38;
    if (wantv) {
	latime_2.iops += (doublereal) (mm1 - l + 1) * 6 * *p;
    }
    if (wantu) {
/* Computing MAX   
   Computing MIN */
	i__2 = mm1, i__3 = *n - 1;
	i__1 = min(i__2,i__3) - l + 1;
	latime_2.iops += (doublereal) max(i__1,0) * 6 * *n;
    }
    i__1 = mm1;
    for (k = l; k <= i__1; ++k) {
	drotg_(&f, &g, &cs, &sn);
	if (k != l) {
	    e[k - 1] = f;
	}
	f = cs * s[k] + sn * e[k];
	e[k] = cs * e[k] - sn * s[k];
	g = sn * s[k + 1];
	s[k + 1] = cs * s[k + 1];
	if (wantv) {
	    drot_(p, &v[k * v_dim1 + 1], &c__1, &v[(k + 1) * v_dim1 + 1], &
		    c__1, &cs, &sn);
	}
	drotg_(&f, &g, &cs, &sn);
	s[k] = f;
	f = cs * e[k] + sn * s[k + 1];
	s[k + 1] = -sn * e[k] + cs * s[k + 1];
	g = sn * e[k + 1];
	e[k + 1] = cs * e[k + 1];
	if (wantu && k < *n) {
	    drot_(n, &u[k * u_dim1 + 1], &c__1, &u[(k + 1) * u_dim1 + 1], &
		    c__1, &cs, &sn);
	}
/* L560: */
    }
    e[m - 1] = f;
    ++iter;
    goto L610;

/*        CONVERGENCE. */

L570:

/*           MAKE THE SINGULAR VALUE  POSITIVE. */

    if (s[l] >= 0.) {
	goto L580;
    }
    s[l] = -s[l];

/*              INCREMENT OP COUNT */
    if (wantv) {
	latime_2.iops += *p;
    }
    if (wantv) {
	dscal_(p, &c_b405, &v[l * v_dim1 + 1], &c__1);
    }
L580:

/*           ORDER THE SINGULAR VALUE. */

L590:
    if (l == mm) {
	goto L600;
    }
/*           ...EXIT */
    if (s[l] >= s[l + 1]) {
	goto L600;
    }
    t = s[l];
    s[l] = s[l + 1];
    s[l + 1] = t;
    if (wantv && l < *p) {
	dswap_(p, &v[l * v_dim1 + 1], &c__1, &v[(l + 1) * v_dim1 + 1], &c__1);
    }
    if (wantu && l < *n) {
	dswap_(n, &u[l * u_dim1 + 1], &c__1, &u[(l + 1) * u_dim1 + 1], &c__1);
    }
    ++l;
    goto L590;
L600:
    iter = 0;
    --m;
L610:
    goto L360;
L620:

/*     COMPUTE FINAL OPCOUNT */
    latime_2.iops += iopst;
    return 0;
} /* dsvdc_   

   Subroutine */ int qzhes_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, logical *matz, doublereal *z)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static integer i, j, k, l;
    static doublereal r, s, t;
    static integer l1;
    static doublereal u1, u2, v1, v2;
    static integer lb, nk1, nm1, nm2;
    static doublereal rho;



/*     ---------------------- BEGIN TIMING CODE ------------------------- 
  
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE -------------------------- 
  


       THIS SUBROUTINE IS THE FIRST STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL GENERAL MATRICES AND   
       REDUCES ONE OF THEM TO UPPER HESSENBERG FORM AND THE OTHER   
       TO UPPER TRIANGULAR FORM USING ORTHOGONAL TRANSFORMATIONS.   
       IT IS USUALLY FOLLOWED BY  QZIT,  QZVAL  AND, POSSIBLY,  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL GENERAL MATRIX.   

          B CONTAINS A REAL GENERAL MATRIX.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
  
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

       ON OUTPUT   

          A HAS BEEN REDUCED TO UPPER HESSENBERG FORM.  THE ELEMENTS   
            BELOW THE FIRST SUBDIAGONAL HAVE BEEN SET TO ZERO.   

          B HAS BEEN REDUCED TO UPPER TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE MAIN DIAGONAL HAVE BEEN SET TO ZERO.   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS IF   
            MATZ HAS BEEN SET TO .TRUE.  OTHERWISE, Z IS NOT REFERENCED. 
  

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       .......... INITIALIZE Z ..........   
       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (! (*matz)) {
	goto L10;
    }

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    z[i + j * z_dim1] = 0.;
/* L2: */
	}

	z[j + j * z_dim1] = 1.;
/* L3: */
    }
/*     .......... REDUCE B TO UPPER TRIANGULAR FORM .......... */
L10:
    if (*n <= 1) {
	goto L170;
    }
    nm1 = *n - 1;

    i__1 = nm1;
    for (l = 1; l <= i__1; ++l) {
	l1 = l + 1;
	s = 0.;

	i__2 = *n;
	for (i = l1; i <= i__2; ++i) {
	    s += (d__1 = b[i + l * b_dim1], abs(d__1));
/* L20: */
	}

	if (s == 0.) {
	    goto L100;
	}
	s += (d__1 = b[l + l * b_dim1], abs(d__1));
	r = 0.;

	i__2 = *n;
	for (i = l; i <= i__2; ++i) {
	    b[i + l * b_dim1] /= s;
/* Computing 2nd power */
	    d__1 = b[i + l * b_dim1];
	    r += d__1 * d__1;
/* L25: */
	}

	d__1 = sqrt(r);
	r = d_sign(&d__1, &b[l + l * b_dim1]);
	b[l + l * b_dim1] += r;
	rho = r * b[l + l * b_dim1];

	i__2 = *n;
	for (j = l1; j <= i__2; ++j) {
	    t = 0.;

	    i__3 = *n;
	    for (i = l; i <= i__3; ++i) {
		t += b[i + l * b_dim1] * b[i + j * b_dim1];
/* L30: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i = l; i <= i__3; ++i) {
		b[i + j * b_dim1] += t * b[i + l * b_dim1];
/* L40: */
	    }

/* L50: */
	}

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    t = 0.;

	    i__3 = *n;
	    for (i = l; i <= i__3; ++i) {
		t += b[i + l * b_dim1] * a[i + j * a_dim1];
/* L60: */
	    }

	    t = -t / rho;

	    i__3 = *n;
	    for (i = l; i <= i__3; ++i) {
		a[i + j * a_dim1] += t * b[i + l * b_dim1];
/* L70: */
	    }

/* L80: */
	}

	b[l + l * b_dim1] = -s * r;

	i__2 = *n;
	for (i = l1; i <= i__2; ++i) {
	    b[i + l * b_dim1] = 0.;
/* L90: */
	}

L100:
	;
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
  
   Computing 2nd power */
    i__1 = *n;
    latime_1.ops += (doublereal) ((i__1 * i__1 << 3) + *n * 17 + 24) * (
	    doublereal) (*n - 1) / 3.;
/*     ----------------------- END TIMING CODE -------------------------- 
  

       .......... REDUCE A TO UPPER HESSENBERG FORM, WHILE   
                  KEEPING B TRIANGULAR .......... */
    if (*n == 2) {
	goto L170;
    }
    nm2 = *n - 2;

    i__1 = nm2;
    for (k = 1; k <= i__1; ++k) {
	nk1 = nm1 - k;
/*     .......... FOR L=N-1 STEP -1 UNTIL K+1 DO -- .......... */
	i__2 = nk1;
	for (lb = 1; lb <= i__2; ++lb) {
	    l = *n - lb;
	    l1 = l + 1;
/*     .......... ZERO A(L+1,K) .......... */
	    s = (d__1 = a[l + k * a_dim1], abs(d__1)) + (d__2 = a[l1 + k * 
		    a_dim1], abs(d__2));
	    if (s == 0.) {
		goto L150;
	    }
	    u1 = a[l + k * a_dim1] / s;
	    u2 = a[l1 + k * a_dim1] / s;
	    d__1 = sqrt(u1 * u1 + u2 * u2);
	    r = d_sign(&d__1, &u1);
	    v1 = -(u1 + r) / r;
	    v2 = -u2 / r;
	    u2 = v2 / v1;

	    i__3 = *n;
	    for (j = k; j <= i__3; ++j) {
		t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
		a[l + j * a_dim1] += t * v1;
		a[l1 + j * a_dim1] += t * v2;
/* L110: */
	    }

	    a[l1 + k * a_dim1] = 0.;

	    i__3 = *n;
	    for (j = l; j <= i__3; ++j) {
		t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
		b[l + j * b_dim1] += t * v1;
		b[l1 + j * b_dim1] += t * v2;
/* L120: */
	    }
/*     .......... ZERO B(L+1,L) .......... */
	    s = (d__1 = b[l1 + l1 * b_dim1], abs(d__1)) + (d__2 = b[l1 + l * 
		    b_dim1], abs(d__2));
	    if (s == 0.) {
		goto L150;
	    }
	    u1 = b[l1 + l1 * b_dim1] / s;
	    u2 = b[l1 + l * b_dim1] / s;
	    d__1 = sqrt(u1 * u1 + u2 * u2);
	    r = d_sign(&d__1, &u1);
	    v1 = -(u1 + r) / r;
	    v2 = -u2 / r;
	    u2 = v2 / v1;

	    i__3 = l1;
	    for (i = 1; i <= i__3; ++i) {
		t = b[i + l1 * b_dim1] + u2 * b[i + l * b_dim1];
		b[i + l1 * b_dim1] += t * v1;
		b[i + l * b_dim1] += t * v2;
/* L130: */
	    }

	    b[l1 + l * b_dim1] = 0.;

	    i__3 = *n;
	    for (i = 1; i <= i__3; ++i) {
		t = a[i + l1 * a_dim1] + u2 * a[i + l * a_dim1];
		a[i + l1 * a_dim1] += t * v1;
		a[i + l * a_dim1] += t * v2;
/* L140: */
	    }

	    if (! (*matz)) {
		goto L150;
	    }

	    i__3 = *n;
	    for (i = 1; i <= i__3; ++i) {
		t = z[i + l1 * z_dim1] + u2 * z[i + l * z_dim1];
		z[i + l1 * z_dim1] += t * v1;
		z[i + l * z_dim1] += t * v2;
/* L145: */
	    }

L150:
	    ;
	}

/* L160: */
    }


/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    if (*matz) {
	latime_1.ops += (doublereal) (*n * 11 + 20) * (doublereal) (*n - 1) * 
		(doublereal) (*n - 2);
    } else {
	latime_1.ops += (doublereal) ((*n << 3) + 20) * (doublereal) (*n - 1) 
		* (doublereal) (*n - 2);
    }
/*     ----------------------- END TIMING CODE -------------------------- 
*/

L170:
    return 0;
} /* qzhes_   

   Subroutine */ int qzit_(integer *nm, integer *n, doublereal *a, doublereal 
	*b, doublereal *eps1, logical *matz, doublereal *z, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal epsa, epsb, opst;
    static integer i, j, k, l;
    static doublereal r, s, t, anorm, bnorm;
    static integer enorn;
    static doublereal a1, a2, a3;
    static integer k1, k2, l1;
    static doublereal u1, u2, u3, v1, v2, v3, a11, a12, a21, a22, a33, a34, 
	    a43, a44, b11, b12, b22, b33;
    static integer na, ld;
    static doublereal b34, b44;
    static integer en;
    static doublereal ep;
    static integer ll;
    static doublereal sh;
    extern doublereal epslon_(doublereal *);
    static logical notlas;
    static integer km1, lm1;
    static doublereal ani, bni;
    static integer ish, itn, its, enm2, lor1;



/*     ---------------------- BEGIN TIMING CODE ------------------------- 
  
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE -------------------------- 
  


       THIS SUBROUTINE IS THE SECOND STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART,   
       AS MODIFIED IN TECHNICAL NOTE NASA TN D-7305(1973) BY WARD.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM   
       IN UPPER HESSENBERG FORM AND THE OTHER IN UPPER TRIANGULAR FORM.   
       IT REDUCES THE HESSENBERG MATRIX TO QUASI-TRIANGULAR FORM USING   
       ORTHOGONAL TRANSFORMATIONS WHILE MAINTAINING THE TRIANGULAR FORM   
       OF THE OTHER MATRIX.  IT IS USUALLY PRECEDED BY  QZHES  AND   
       FOLLOWED BY  QZVAL  AND, POSSIBLY,  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER HESSENBERG MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.   

          EPS1 IS A TOLERANCE USED TO DETERMINE NEGLIGIBLE ELEMENTS.   
            EPS1 = 0.0 (OR NEGATIVE) MAY BE INPUT, IN WHICH CASE AN   
            ELEMENT WILL BE NEGLECTED ONLY IF IT IS LESS THAN ROUNDOFF   
            ERROR TIMES THE NORM OF ITS MATRIX.  IF THE INPUT EPS1 IS   
            POSITIVE, THEN AN ELEMENT WILL BE CONSIDERED NEGLIGIBLE   
            IF IT IS LESS THAN EPS1 TIMES THE NORM OF ITS MATRIX.  A   
            POSITIVE VALUE OF EPS1 MAY RESULT IN FASTER EXECUTION,   
            BUT LESS ACCURATE RESULTS.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
  
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

          Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE   
            TRANSFORMATION MATRIX PRODUCED IN THE REDUCTION   
            BY  QZHES, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.   
            IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.   

       ON OUTPUT   

          A HAS BEEN REDUCED TO QUASI-TRIANGULAR FORM.  THE ELEMENTS   
            BELOW THE FIRST SUBDIAGONAL ARE STILL ZERO AND NO TWO   
            CONSECUTIVE SUBDIAGONAL ELEMENTS ARE NONZERO.   

          B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS   
            HAVE BEEN ALTERED.  THE LOCATION B(N,1) IS USED TO STORE   
            EPS1 TIMES THE NORM OF B FOR LATER USE BY  QZVAL  AND  QZVEC. 
  

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS   
            (FOR BOTH STEPS) IF MATZ HAS BEEN SET TO .TRUE..   

          IERR IS SET TO   
            ZERO       FOR NORMAL RETURN,   
            J          IF THE LIMIT OF 30*N ITERATIONS IS EXHAUSTED   
                       WHILE THE J-TH EIGENVALUE IS BEING SOUGHT.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
/*     .......... COMPUTE EPSA,EPSB .......... */
    anorm = 0.;
    bnorm = 0.;

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	ani = 0.;
	if (i != 1) {
	    ani = (d__1 = a[i + (i - 1) * a_dim1], abs(d__1));
	}
	bni = 0.;

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
	    ani += (d__1 = a[i + j * a_dim1], abs(d__1));
	    bni += (d__1 = b[i + j * b_dim1], abs(d__1));
/* L20: */
	}

	if (ani > anorm) {
	    anorm = ani;
	}
	if (bni > bnorm) {
	    bnorm = bni;
	}
/* L30: */
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    latime_1.ops += (doublereal) (*n * (*n + 1));
    opst = 0.;
    latime_1.itcnt = 0.;
/*     ----------------------- END TIMING CODE -------------------------- 
*/


    if (anorm == 0.) {
	anorm = 1.;
    }
    if (bnorm == 0.) {
	bnorm = 1.;
    }
    ep = *eps1;
    if (ep > 0.) {
	goto L50;
    }
/*     .......... USE ROUNDOFF LEVEL IF EPS1 IS ZERO .......... */
    ep = epslon_(&c_b114);
L50:
    epsa = ep * anorm;
    epsb = ep * bnorm;
/*     .......... REDUCE A TO QUASI-TRIANGULAR FORM, WHILE   
                  KEEPING B TRIANGULAR .......... */
    lor1 = 1;
    enorn = *n;
    en = *n;
    itn = *n * 30;
/*     .......... BEGIN QZ STEP .......... */
L60:
    if (en <= 2) {
	goto L1001;
    }
    if (! (*matz)) {
	enorn = en;
    }
    its = 0;
    na = en - 1;
    enm2 = na - 1;
L70:
    ish = 2;

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    latime_1.ops += opst;
    opst = 0.;
    latime_1.itcnt += 1;
/*     ----------------------- END TIMING CODE -------------------------- 
  

       .......... CHECK FOR CONVERGENCE OR REDUCIBILITY.   
                  FOR L=EN STEP -1 UNTIL 1 DO -- .......... */
    i__1 = en;
    for (ll = 1; ll <= i__1; ++ll) {
	lm1 = en - ll;
	l = lm1 + 1;
	if (l == 1) {
	    goto L95;
	}
	if ((d__1 = a[l + lm1 * a_dim1], abs(d__1)) <= epsa) {
	    goto L90;
	}
/* L80: */
    }

L90:
    a[l + lm1 * a_dim1] = 0.;
    if (l < na) {
	goto L95;
    }
/*     .......... 1-BY-1 OR 2-BY-2 BLOCK ISOLATED .......... */
    en = lm1;
    goto L60;
/*     .......... CHECK FOR SMALL TOP OF B .......... */
L95:
    ld = l;
L100:
    l1 = l + 1;
    b11 = b[l + l * b_dim1];
    if (abs(b11) > epsb) {
	goto L120;
    }
    b[l + l * b_dim1] = 0.;
    s = (d__1 = a[l + l * a_dim1], abs(d__1)) + (d__2 = a[l1 + l * a_dim1], 
	    abs(d__2));
    u1 = a[l + l * a_dim1] / s;
    u2 = a[l1 + l * a_dim1] / s;
    d__1 = sqrt(u1 * u1 + u2 * u2);
    r = d_sign(&d__1, &u1);
    v1 = -(u1 + r) / r;
    v2 = -u2 / r;
    u2 = v2 / v1;

    i__1 = enorn;
    for (j = l; j <= i__1; ++j) {
	t = a[l + j * a_dim1] + u2 * a[l1 + j * a_dim1];
	a[l + j * a_dim1] += t * v1;
	a[l1 + j * a_dim1] += t * v2;
	t = b[l + j * b_dim1] + u2 * b[l1 + j * b_dim1];
	b[l + j * b_dim1] += t * v1;
	b[l1 + j * b_dim1] += t * v2;
/* L110: */
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    opst += (doublereal) ((enorn + 1 - l) * 12 + 11);
/*     ----------------------- END TIMING CODE -------------------------- 
*/
    if (l != 1) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    lm1 = l;
    l = l1;
    goto L90;
L120:
    a11 = a[l + l * a_dim1] / b11;
    a21 = a[l1 + l * a_dim1] / b11;
    if (ish == 1) {
	goto L140;
    }
/*     .......... ITERATION STRATEGY .......... */
    if (itn == 0) {
	goto L1000;
    }
    if (its == 10) {
	goto L155;
    }
/*     .......... DETERMINE TYPE OF SHIFT .......... */
    b22 = b[l1 + l1 * b_dim1];
    if (abs(b22) < epsb) {
	b22 = epsb;
    }
    b33 = b[na + na * b_dim1];
    if (abs(b33) < epsb) {
	b33 = epsb;
    }
    b44 = b[en + en * b_dim1];
    if (abs(b44) < epsb) {
	b44 = epsb;
    }
    a33 = a[na + na * a_dim1] / b33;
    a34 = a[na + en * a_dim1] / b44;
    a43 = a[en + na * a_dim1] / b33;
    a44 = a[en + en * a_dim1] / b44;
    b34 = b[na + en * b_dim1] / b44;
    t = (a43 * b34 - a33 - a44) * .5;
    r = t * t + a34 * a43 - a33 * a44;
/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    opst += 16.;
/*     ----------------------- END TIMING CODE -------------------------- 
*/
    if (r < 0.) {
	goto L150;
    }
/*     .......... DETERMINE SINGLE SHIFT ZEROTH COLUMN OF A .......... */
    ish = 1;
    r = sqrt(r);
    sh = -t + r;
    s = -t - r;
    if ((d__1 = s - a44, abs(d__1)) < (d__2 = sh - a44, abs(d__2))) {
	sh = s;
    }
/*     .......... LOOK FOR TWO CONSECUTIVE SMALL   
                  SUB-DIAGONAL ELEMENTS OF A.   
                  FOR L=EN-2 STEP -1 UNTIL LD DO -- .......... */
    i__1 = enm2;
    for (ll = ld; ll <= i__1; ++ll) {
	l = enm2 + ld - ll;
	if (l == ld) {
	    goto L140;
	}
	lm1 = l - 1;
	l1 = l + 1;
	t = a[l + l * a_dim1];
	if ((d__1 = b[l + l * b_dim1], abs(d__1)) > epsb) {
	    t -= sh * b[l + l * b_dim1];
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	if ((d__1 = a[l + lm1 * a_dim1], abs(d__1)) <= (d__2 = t / a[l1 + l * 
		a_dim1], abs(d__2)) * epsa) {
	    opst += (doublereal) ((ll + 1 - ld << 2) + 5);
	    goto L100;
	}
/*        ---------------------- END TIMING CODE --------------------
----   
   L130: */
    }
/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    opst += (doublereal) ((enm2 + 1 - ld << 2) + 5);
/*     ----------------------- END TIMING CODE -------------------------- 
*/

L140:
    a1 = a11 - sh;
    a2 = a21;
    if (l != ld) {
	a[l + lm1 * a_dim1] = -a[l + lm1 * a_dim1];
    }
    goto L160;
/*     .......... DETERMINE DOUBLE SHIFT ZEROTH COLUMN OF A .......... */
L150:
    a12 = a[l + l1 * a_dim1] / b22;
    a22 = a[l1 + l1 * a_dim1] / b22;
    b12 = b[l + l1 * b_dim1] / b22;
    a1 = ((a33 - a11) * (a44 - a11) - a34 * a43 + a43 * b34 * a11) / a21 + 
	    a12 - a11 * b12;
    a2 = a22 - a11 - a21 * b12 - (a33 - a11) - (a44 - a11) + a43 * b34;
    a3 = a[l1 + 1 + l1 * a_dim1] / b22;
/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    opst += 25.;
/*     ----------------------- END TIMING CODE -------------------------- 
*/
    goto L160;
/*     .......... AD HOC SHIFT .......... */
L155:
    a1 = 0.;
    a2 = 1.;
    a3 = 1.1605;
L160:
    ++its;
    --itn;
    if (! (*matz)) {
	lor1 = ld;
    }
/*     .......... MAIN LOOP .......... */
    i__1 = na;
    for (k = l; k <= i__1; ++k) {
	notlas = k != na && ish == 2;
	k1 = k + 1;
	k2 = k + 2;
/* Computing MAX */
	i__2 = k - 1;
	km1 = max(i__2,l);
/* Computing MIN */
	i__2 = en, i__3 = k1 + ish;
	ll = min(i__2,i__3);
	if (notlas) {
	    goto L190;
	}
/*     .......... ZERO A(K+1,K-1) .......... */
	if (k == l) {
	    goto L170;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
L170:
	s = abs(a1) + abs(a2);
	if (s == 0.) {
	    goto L70;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	u2 = v2 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
/* L180: */
	}

/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) ((enorn + 1 - km1) * 12 + 11);
/*        ---------------------- END TIMING CODE --------------------
---- */
	if (k != l) {
	    a[k1 + km1 * a_dim1] = 0.;
	}
	goto L240;
/*     .......... ZERO A(K+1,K-1) AND A(K+2,K-1) .......... */
L190:
	if (k == l) {
	    goto L200;
	}
	a1 = a[k + km1 * a_dim1];
	a2 = a[k1 + km1 * a_dim1];
	a3 = a[k2 + km1 * a_dim1];
L200:
	s = abs(a1) + abs(a2) + abs(a3);
	if (s == 0.) {
	    goto L260;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	u3 = a3 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	v3 = -u3 / r;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = enorn;
	for (j = km1; j <= i__2; ++j) {
	    t = a[k + j * a_dim1] + u2 * a[k1 + j * a_dim1] + u3 * a[k2 + j * 
		    a_dim1];
	    a[k + j * a_dim1] += t * v1;
	    a[k1 + j * a_dim1] += t * v2;
	    a[k2 + j * a_dim1] += t * v3;
	    t = b[k + j * b_dim1] + u2 * b[k1 + j * b_dim1] + u3 * b[k2 + j * 
		    b_dim1];
	    b[k + j * b_dim1] += t * v1;
	    b[k1 + j * b_dim1] += t * v2;
	    b[k2 + j * b_dim1] += t * v3;
/* L210: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) ((enorn + 1 - km1) * 20 + 17);
/*        ---------------------- END TIMING CODE --------------------
---- */

	if (k == l) {
	    goto L220;
	}
	a[k1 + km1 * a_dim1] = 0.;
	a[k2 + km1 * a_dim1] = 0.;
/*     .......... ZERO B(K+2,K+1) AND B(K+2,K) .......... */
L220:
	s = (d__1 = b[k2 + k2 * b_dim1], abs(d__1)) + (d__2 = b[k2 + k1 * 
		b_dim1], abs(d__2)) + (d__3 = b[k2 + k * b_dim1], abs(d__3));
	if (s == 0.) {
	    goto L240;
	}
	u1 = b[k2 + k2 * b_dim1] / s;
	u2 = b[k2 + k1 * b_dim1] / s;
	u3 = b[k2 + k * b_dim1] / s;
	d__1 = sqrt(u1 * u1 + u2 * u2 + u3 * u3);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	v3 = -u3 / r;
	u2 = v2 / v1;
	u3 = v3 / v1;

	i__2 = ll;
	for (i = lor1; i <= i__2; ++i) {
	    t = a[i + k2 * a_dim1] + u2 * a[i + k1 * a_dim1] + u3 * a[i + k * 
		    a_dim1];
	    a[i + k2 * a_dim1] += t * v1;
	    a[i + k1 * a_dim1] += t * v2;
	    a[i + k * a_dim1] += t * v3;
	    t = b[i + k2 * b_dim1] + u2 * b[i + k1 * b_dim1] + u3 * b[i + k * 
		    b_dim1];
	    b[i + k2 * b_dim1] += t * v1;
	    b[i + k1 * b_dim1] += t * v2;
	    b[i + k * b_dim1] += t * v3;
/* L230: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) ((ll + 1 - lor1) * 20 + 17);
/*        ---------------------- END TIMING CODE --------------------
---- */

	b[k2 + k * b_dim1] = 0.;
	b[k2 + k1 * b_dim1] = 0.;
	if (! (*matz)) {
	    goto L240;
	}

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    t = z[i + k2 * z_dim1] + u2 * z[i + k1 * z_dim1] + u3 * z[i + k * 
		    z_dim1];
	    z[i + k2 * z_dim1] += t * v1;
	    z[i + k1 * z_dim1] += t * v2;
	    z[i + k * z_dim1] += t * v3;
/* L235: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) (*n * 10);
/*        ---------------------- END TIMING CODE --------------------
----   
       .......... ZERO B(K+1,K) .......... */
L240:
	s = (d__1 = b[k1 + k1 * b_dim1], abs(d__1)) + (d__2 = b[k1 + k * 
		b_dim1], abs(d__2));
	if (s == 0.) {
	    goto L260;
	}
	u1 = b[k1 + k1 * b_dim1] / s;
	u2 = b[k1 + k * b_dim1] / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	u2 = v2 / v1;

	i__2 = ll;
	for (i = lor1; i <= i__2; ++i) {
	    t = a[i + k1 * a_dim1] + u2 * a[i + k * a_dim1];
	    a[i + k1 * a_dim1] += t * v1;
	    a[i + k * a_dim1] += t * v2;
	    t = b[i + k1 * b_dim1] + u2 * b[i + k * b_dim1];
	    b[i + k1 * b_dim1] += t * v1;
	    b[i + k * b_dim1] += t * v2;
/* L250: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) ((ll + 1 - lor1) * 12 + 11);
/*        ---------------------- END TIMING CODE --------------------
---- */

	b[k1 + k * b_dim1] = 0.;
	if (! (*matz)) {
	    goto L260;
	}

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    t = z[i + k1 * z_dim1] + u2 * z[i + k * z_dim1];
	    z[i + k1 * z_dim1] += t * v1;
	    z[i + k * z_dim1] += t * v2;
/* L255: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += (doublereal) (*n * 6);
/*        ---------------------- END TIMING CODE --------------------
---- */

L260:
	;
    }
/*     .......... END QZ STEP .......... */
    goto L70;
/*     .......... SET ERROR -- ALL EIGENVALUES HAVE NOT   
                  CONVERGED AFTER 30*N ITERATIONS .......... */
L1000:
    *ierr = en;
/*     .......... SAVE EPSB FOR USE BY QZVAL AND QZVEC .......... */
L1001:
    if (*n > 1) {
	b[*n + b_dim1] = epsb;
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    latime_1.ops += opst;
    opst = 0.;
/*     ----------------------- END TIMING CODE -------------------------- 
*/

    return 0;
} /* qzit_   

   Subroutine */ int qzval_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, 
	logical *matz, doublereal *z)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal epsb, opst, c, d, e, opst2;
    static integer i, j;
    static doublereal r, s, t, a1, a2, u1, u2, v1, v2, a11, a12, a21, a22, 
	    b11, b12, b22, di, ei;
    static integer na;
    static doublereal an, bn;
    static integer en;
    static doublereal cq, dr;
    static integer nn;
    static doublereal cz, ti, tr, a1i, a2i, a11i, a12i, a22i, a11r, a12r, 
	    a22r, sqi, ssi;
    static integer isw;
    static doublereal sqr, szi, ssr, szr;



/*     ---------------------- BEGIN TIMING CODE ------------------------- 
  
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE -------------------------- 
  


       THIS SUBROUTINE IS THE THIRD STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM   
       IN QUASI-TRIANGULAR FORM AND THE OTHER IN UPPER TRIANGULAR FORM.   
       IT REDUCES THE QUASI-TRIANGULAR MATRIX FURTHER, SO THAT ANY   
       REMAINING 2-BY-2 BLOCKS CORRESPOND TO PAIRS OF COMPLEX   
       EIGENVALUES, AND RETURNS QUANTITIES WHOSE RATIOS GIVE THE   
       GENERALIZED EIGENVALUES.  IT IS USUALLY PRECEDED BY  QZHES   
       AND  QZIT  AND MAY BE FOLLOWED BY  QZVEC.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,   
            LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)   
            COMPUTED AND SAVED IN  QZIT.   

          MATZ SHOULD BE SET TO .TRUE. IF THE RIGHT HAND TRANSFORMATIONS 
  
            ARE TO BE ACCUMULATED FOR LATER USE IN COMPUTING   
            EIGENVECTORS, AND TO .FALSE. OTHERWISE.   

          Z CONTAINS, IF MATZ HAS BEEN SET TO .TRUE., THE   
            TRANSFORMATION MATRIX PRODUCED IN THE REDUCTIONS BY QZHES   
            AND QZIT, IF PERFORMED, OR ELSE THE IDENTITY MATRIX.   
            IF MATZ HAS BEEN SET TO .FALSE., Z IS NOT REFERENCED.   

       ON OUTPUT   

          A HAS BEEN REDUCED FURTHER TO A QUASI-TRIANGULAR MATRIX   
            IN WHICH ALL NONZERO SUBDIAGONAL ELEMENTS CORRESPOND TO   
            PAIRS OF COMPLEX EIGENVALUES.   

          B IS STILL IN UPPER TRIANGULAR FORM, ALTHOUGH ITS ELEMENTS   
            HAVE BEEN ALTERED.  B(N,1) IS UNALTERED.   

          ALFR AND ALFI CONTAIN THE REAL AND IMAGINARY PARTS OF THE   
            DIAGONAL ELEMENTS OF THE TRIANGULAR MATRIX THAT WOULD BE   
            OBTAINED IF A WERE REDUCED COMPLETELY TO TRIANGULAR FORM   
            BY UNITARY TRANSFORMATIONS.  NON-ZERO VALUES OF ALFI OCCUR   
            IN PAIRS, THE FIRST MEMBER POSITIVE AND THE SECOND NEGATIVE. 
  

          BETA CONTAINS THE DIAGONAL ELEMENTS OF THE CORRESPONDING B,   
            NORMALIZED TO BE REAL AND NON-NEGATIVE.  THE GENERALIZED   
            EIGENVALUES ARE THEN THE RATIOS ((ALFR+I*ALFI)/BETA).   

          Z CONTAINS THE PRODUCT OF THE RIGHT HAND TRANSFORMATIONS   
            (FOR ALL THREE STEPS) IF MATZ HAS BEEN SET TO .TRUE.   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --beta;
    --alfi;
    --alfr;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    epsb = b[*n + b_dim1];
    isw = 1;
/*     .......... FIND EIGENVALUES OF QUASI-TRIANGULAR MATRICES.   
                  FOR EN=N STEP -1 UNTIL 1 DO -- ..........   

       ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    opst = 0.;
    opst2 = 0.;
/*     ----------------------- END TIMING CODE -------------------------- 
*/

    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {

/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst += opst2;
	opst2 = 0.;
/*        ---------------------- END TIMING CODE --------------------
---- */

	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L505;
	}
	if (en == 1) {
	    goto L410;
	}
	if (a[en + na * a_dim1] != 0.) {
	    goto L420;
	}
/*     .......... 1-BY-1 BLOCK, ONE REAL ROOT .......... */
L410:
	alfr[en] = a[en + en * a_dim1];
	if (b[en + en * b_dim1] < 0.) {
	    alfr[en] = -alfr[en];
	}
	beta[en] = (d__1 = b[en + en * b_dim1], abs(d__1));
	alfi[en] = 0.;
	goto L510;
/*     .......... 2-BY-2 BLOCK .......... */
L420:
	if ((d__1 = b[na + na * b_dim1], abs(d__1)) <= epsb) {
	    goto L455;
	}
	if ((d__1 = b[en + en * b_dim1], abs(d__1)) > epsb) {
	    goto L430;
	}
	a1 = a[en + en * a_dim1];
	a2 = a[en + na * a_dim1];
	bn = 0.;
	goto L435;
L430:
	an = (d__1 = a[na + na * a_dim1], abs(d__1)) + (d__2 = a[na + en * 
		a_dim1], abs(d__2)) + (d__3 = a[en + na * a_dim1], abs(d__3)) 
		+ (d__4 = a[en + en * a_dim1], abs(d__4));
	bn = (d__1 = b[na + na * b_dim1], abs(d__1)) + (d__2 = b[na + en * 
		b_dim1], abs(d__2)) + (d__3 = b[en + en * b_dim1], abs(d__3));
	a11 = a[na + na * a_dim1] / an;
	a12 = a[na + en * a_dim1] / an;
	a21 = a[en + na * a_dim1] / an;
	a22 = a[en + en * a_dim1] / an;
	b11 = b[na + na * b_dim1] / bn;
	b12 = b[na + en * b_dim1] / bn;
	b22 = b[en + en * b_dim1] / bn;
	e = a11 / b11;
	ei = a22 / b22;
	s = a21 / (b11 * b22);
	t = (a22 - e * b22) / b22;
	if (abs(e) <= abs(ei)) {
	    goto L431;
	}
	e = ei;
	t = (a11 - e * b11) / b11;
L431:
	c = (t - s * b12) * .5;
	d = c * c + s * (a12 - e * b12);
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += 28.;
/*        ---------------------- END TIMING CODE --------------------
---- */
	if (d < 0.) {
	    goto L480;
	}
/*     .......... TWO REAL ROOTS.   
                  ZERO BOTH A(EN,NA) AND B(EN,NA) .......... */
	d__1 = sqrt(d);
	e += c + d_sign(&d__1, &c);
	a11 -= e * b11;
	a12 -= e * b12;
	a22 -= e * b22;
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += 11.;
/*        ---------------------- END TIMING CODE --------------------
---- */
	if (abs(a11) + abs(a12) < abs(a21) + abs(a22)) {
	    goto L432;
	}
	a1 = a12;
	a2 = a11;
	goto L435;
L432:
	a1 = a22;
	a2 = a21;
/*     .......... CHOOSE AND APPLY REAL Z .......... */
L435:
	s = abs(a1) + abs(a2);
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	u2 = v2 / v1;

	i__2 = en;
	for (i = 1; i <= i__2; ++i) {
	    t = a[i + en * a_dim1] + u2 * a[i + na * a_dim1];
	    a[i + en * a_dim1] += t * v1;
	    a[i + na * a_dim1] += t * v2;
	    t = b[i + en * b_dim1] + u2 * b[i + na * b_dim1];
	    b[i + en * b_dim1] += t * v1;
	    b[i + na * b_dim1] += t * v2;
/* L440: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += (doublereal) (en * 12 + 11);
/*        ---------------------- END TIMING CODE --------------------
---- */

	if (! (*matz)) {
	    goto L450;
	}

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    t = z[i + en * z_dim1] + u2 * z[i + na * z_dim1];
	    z[i + en * z_dim1] += t * v1;
	    z[i + na * z_dim1] += t * v2;
/* L445: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += (doublereal) (*n * 6);
/*        ---------------------- END TIMING CODE --------------------
---- */

L450:
	if (bn == 0.) {
	    goto L475;
	}
	if (an < abs(e) * bn) {
	    goto L455;
	}
	a1 = b[na + na * b_dim1];
	a2 = b[en + na * b_dim1];
	goto L460;
L455:
	a1 = a[na + na * a_dim1];
	a2 = a[en + na * a_dim1];
/*     .......... CHOOSE AND APPLY REAL Q .......... */
L460:
	s = abs(a1) + abs(a2);
	if (s == 0.) {
	    goto L475;
	}
	u1 = a1 / s;
	u2 = a2 / s;
	d__1 = sqrt(u1 * u1 + u2 * u2);
	r = d_sign(&d__1, &u1);
	v1 = -(u1 + r) / r;
	v2 = -u2 / r;
	u2 = v2 / v1;

	i__2 = *n;
	for (j = na; j <= i__2; ++j) {
	    t = a[na + j * a_dim1] + u2 * a[en + j * a_dim1];
	    a[na + j * a_dim1] += t * v1;
	    a[en + j * a_dim1] += t * v2;
	    t = b[na + j * b_dim1] + u2 * b[en + j * b_dim1];
	    b[na + j * b_dim1] += t * v1;
	    b[en + j * b_dim1] += t * v2;
/* L470: */
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += (doublereal) ((*n + 1 - na) * 12 + 11);
/*        ---------------------- END TIMING CODE --------------------
---- */

L475:
	a[en + na * a_dim1] = 0.;
	b[en + na * b_dim1] = 0.;
	alfr[na] = a[na + na * a_dim1];
	alfr[en] = a[en + en * a_dim1];
	if (b[na + na * b_dim1] < 0.) {
	    alfr[na] = -alfr[na];
	}
	if (b[en + en * b_dim1] < 0.) {
	    alfr[en] = -alfr[en];
	}
	beta[na] = (d__1 = b[na + na * b_dim1], abs(d__1));
	beta[en] = (d__1 = b[en + en * b_dim1], abs(d__1));
	alfi[en] = 0.;
	alfi[na] = 0.;
	goto L505;
/*     .......... TWO COMPLEX ROOTS .......... */
L480:
	e += c;
	ei = sqrt(-d);
	a11r = a11 - e * b11;
	a11i = ei * b11;
	a12r = a12 - e * b12;
	a12i = ei * b12;
	a22r = a22 - e * b22;
	a22i = ei * b22;
	if (abs(a11r) + abs(a11i) + abs(a12r) + abs(a12i) < abs(a21) + abs(
		a22r) + abs(a22i)) {
	    goto L482;
	}
	a1 = a12r;
	a1i = a12i;
	a2 = -a11r;
	a2i = -a11i;
	goto L485;
L482:
	a1 = a22r;
	a1i = a22i;
	a2 = -a21;
	a2i = 0.;
/*     .......... CHOOSE COMPLEX Z .......... */
L485:
	cz = sqrt(a1 * a1 + a1i * a1i);
	if (cz == 0.) {
	    goto L487;
	}
	szr = (a1 * a2 + a1i * a2i) / cz;
	szi = (a1 * a2i - a1i * a2) / cz;
	r = sqrt(cz * cz + szr * szr + szi * szi);
	cz /= r;
	szr /= r;
	szi /= r;
	goto L490;
L487:
	szr = 1.;
	szi = 0.;
L490:
	if (an < (abs(e) + ei) * bn) {
	    goto L492;
	}
	a1 = cz * b11 + szr * b12;
	a1i = szi * b12;
	a2 = szr * b22;
	a2i = szi * b22;
	goto L495;
L492:
	a1 = cz * a11 + szr * a12;
	a1i = szi * a12;
	a2 = cz * a21 + szr * a22;
	a2i = szi * a22;
/*     .......... CHOOSE COMPLEX Q .......... */
L495:
	cq = sqrt(a1 * a1 + a1i * a1i);
	if (cq == 0.) {
	    goto L497;
	}
	sqr = (a1 * a2 + a1i * a2i) / cq;
	sqi = (a1 * a2i - a1i * a2) / cq;
	r = sqrt(cq * cq + sqr * sqr + sqi * sqi);
	cq /= r;
	sqr /= r;
	sqi /= r;
	goto L500;
L497:
	sqr = 1.;
	sqi = 0.;
/*     .......... COMPUTE DIAGONAL ELEMENTS THAT WOULD RESULT   
                  IF TRANSFORMATIONS WERE APPLIED .......... */
L500:
	ssr = sqr * szr + sqi * szi;
	ssi = sqr * szi - sqi * szr;
	i = 1;
	tr = cq * cz * a11 + cq * szr * a12 + sqr * cz * a21 + ssr * a22;
	ti = cq * szi * a12 - sqi * cz * a21 + ssi * a22;
	dr = cq * cz * b11 + cq * szr * b12 + ssr * b22;
	di = cq * szi * b12 + ssi * b22;
	goto L503;
L502:
	i = 2;
	tr = ssr * a11 - sqr * cz * a12 - cq * szr * a21 + cq * cz * a22;
	ti = -ssi * a11 - sqi * cz * a12 + cq * szi * a21;
	dr = ssr * b11 - sqr * cz * b12 + cq * cz * b22;
	di = -ssi * b11 - sqi * cz * b12;
L503:
	t = ti * dr - tr * di;
	j = na;
	if (t < 0.) {
	    j = en;
	}
	r = sqrt(dr * dr + di * di);
	beta[j] = bn * r;
	alfr[j] = an * (tr * dr + ti * di) / r;
	alfi[j] = an * t / r;
	if (i == 1) {
	    goto L502;
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	opst2 += 151.;
/*        ---------------------- END TIMING CODE --------------------
---- */
L505:
	isw = 3 - isw;
L510:
	;
    }

/*     ---------------------- BEGIN TIMING CODE ------------------------- 
*/
    latime_1.ops += opst + opst2;
/*     ----------------------- END TIMING CODE -------------------------- 
*/

    b[*n + b_dim1] = epsb;

    return 0;
} /* qzval_   

   Subroutine */ int qzvec_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *alfr, doublereal *alfi, doublereal *beta, 
	doublereal *z)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal alfm, almi, betm, epsb, almr, d;
    static integer i, j, k, m;
    static doublereal q, r, s, t, w, x, y;
    static integer in2by2;
    static doublereal t1, t2, w1, x1, z1, di;
    static integer na, ii, en, jj;
    static doublereal ra, dr, sa;
    static integer nn;
    static doublereal ti, rr, tr, zz;
    static integer isw, enm2;



/*     ---------------------- BEGIN TIMING CODE ------------------------- 
  
       COMMON BLOCK TO RETURN OPERATION COUNT AND ITERATION COUNT   
       ITCNT IS INITIALIZED TO 0, OPS IS ONLY INCREMENTED   
       OPST IS USED TO ACCUMULATE SMALL CONTRIBUTIONS TO OPS   
       TO AVOID ROUNDOFF ERROR   
       ----------------------- END TIMING CODE -------------------------- 
  


       THIS SUBROUTINE IS THE OPTIONAL FOURTH STEP OF THE QZ ALGORITHM   
       FOR SOLVING GENERALIZED MATRIX EIGENVALUE PROBLEMS,   
       SIAM J. NUMER. ANAL. 10, 241-256(1973) BY MOLER AND STEWART.   

       THIS SUBROUTINE ACCEPTS A PAIR OF REAL MATRICES, ONE OF THEM IN   
       QUASI-TRIANGULAR FORM (IN WHICH EACH 2-BY-2 BLOCK CORRESPONDS TO   
       A PAIR OF COMPLEX EIGENVALUES) AND THE OTHER IN UPPER TRIANGULAR   
       FORM.  IT COMPUTES THE EIGENVECTORS OF THE TRIANGULAR PROBLEM AND 
  
       TRANSFORMS THE RESULTS BACK TO THE ORIGINAL COORDINATE SYSTEM.   
       IT IS USUALLY PRECEDED BY  QZHES,  QZIT, AND  QZVAL.   

       ON INPUT   

          NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL   
            ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM   
            DIMENSION STATEMENT.   

          N IS THE ORDER OF THE MATRICES.   

          A CONTAINS A REAL UPPER QUASI-TRIANGULAR MATRIX.   

          B CONTAINS A REAL UPPER TRIANGULAR MATRIX.  IN ADDITION,   
            LOCATION B(N,1) CONTAINS THE TOLERANCE QUANTITY (EPSB)   
            COMPUTED AND SAVED IN  QZIT.   

          ALFR, ALFI, AND BETA  ARE VECTORS WITH COMPONENTS WHOSE   
            RATIOS ((ALFR+I*ALFI)/BETA) ARE THE GENERALIZED   
            EIGENVALUES.  THEY ARE USUALLY OBTAINED FROM  QZVAL.   

          Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE   
            REDUCTIONS BY  QZHES,  QZIT, AND  QZVAL, IF PERFORMED.   
            IF THE EIGENVECTORS OF THE TRIANGULAR PROBLEM ARE   
            DESIRED, Z MUST CONTAIN THE IDENTITY MATRIX.   

       ON OUTPUT   

          A IS UNALTERED.  ITS SUBDIAGONAL ELEMENTS PROVIDE INFORMATION   
             ABOUT THE STORAGE OF THE COMPLEX EIGENVECTORS.   

          B HAS BEEN DESTROYED.   

          ALFR, ALFI, AND BETA ARE UNALTERED.   

          Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.   
            IF ALFI(I) .EQ. 0.0, THE I-TH EIGENVALUE IS REAL AND   
              THE I-TH COLUMN OF Z CONTAINS ITS EIGENVECTOR.   
            IF ALFI(I) .NE. 0.0, THE I-TH EIGENVALUE IS COMPLEX.   
              IF ALFI(I) .GT. 0.0, THE EIGENVALUE IS THE FIRST OF   
                A COMPLEX PAIR AND THE I-TH AND (I+1)-TH COLUMNS   
                OF Z CONTAIN ITS EIGENVECTOR.   
              IF ALFI(I) .LT. 0.0, THE EIGENVALUE IS THE SECOND OF   
                A COMPLEX PAIR AND THE (I-1)-TH AND I-TH COLUMNS   
                OF Z CONTAIN THE CONJUGATE OF ITS EIGENVECTOR.   
            EACH EIGENVECTOR IS NORMALIZED SO THAT THE MODULUS   
            OF ITS LARGEST COMPONENT IS 1.0 .   

       QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,   
       MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
  

       THIS VERSION DATED AUGUST 1983.   

       ------------------------------------------------------------------ 
  

       Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --beta;
    --alfi;
    --alfr;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    epsb = b[*n + b_dim1];
    isw = 1;
/*     .......... FOR EN=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (nn = 1; nn <= i__1; ++nn) {
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	in2by2 = 0;
/*        ---------------------- END TIMING CODE --------------------
---- */
	en = *n + 1 - nn;
	na = en - 1;
	if (isw == 2) {
	    goto L795;
	}
	if (alfi[en] != 0.) {
	    goto L710;
	}
/*     .......... REAL VECTOR .......... */
	m = en;
	b[en + en * b_dim1] = 1.;
	if (na == 0) {
	    goto L800;
	}
	alfm = alfr[m];
	betm = beta[m];
/*     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = na;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = en - ii;
	    w = betm * a[i + i * a_dim1] - alfm * b[i + i * b_dim1];
	    r = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
/* L610: */
		r += (betm * a[i + j * a_dim1] - alfm * b[i + j * b_dim1]) * 
			b[j + en * b_dim1];
	    }

	    if (i == 1 || isw == 2) {
		goto L630;
	    }
	    if (betm * a[i + (i - 1) * a_dim1] == 0.) {
		goto L630;
	    }
	    zz = w;
	    s = r;
	    goto L690;
L630:
	    m = i;
	    if (isw == 2) {
		goto L640;
	    }
/*     .......... REAL 1-BY-1 BLOCK .......... */
	    t = w;
	    if (w == 0.) {
		t = epsb;
	    }
	    b[i + en * b_dim1] = -r / t;
	    goto L700;
/*     .......... REAL 2-BY-2 BLOCK .......... */
L640:
	    x = betm * a[i + (i + 1) * a_dim1] - alfm * b[i + (i + 1) * 
		    b_dim1];
	    y = betm * a[i + 1 + i * a_dim1];
	    q = w * zz - x * y;
	    t = (x * s - zz * r) / q;
	    b[i + en * b_dim1] = t;
/*           ------------------- BEGIN TIMING CODE --------------
-------- */
	    ++in2by2;
/*           -------------------- END TIMING CODE ---------------
-------- */
	    if (abs(x) <= abs(zz)) {
		goto L650;
	    }
	    b[i + 1 + en * b_dim1] = (-r - w * t) / x;
	    goto L690;
L650:
	    b[i + 1 + en * b_dim1] = (-s - y * t) / zz;
L690:
	    isw = 3 - isw;
L700:
	    ;
	}
/*     .......... END REAL VECTOR ..........   
          --------------------- BEGIN TIMING CODE -------------------
---- */
	latime_1.ops += (doublereal) ((en + 2) * (en - 1) + in2by2) * 2.5;
/*        ---------------------- END TIMING CODE --------------------
---- */
	goto L800;
/*     .......... COMPLEX VECTOR .......... */
L710:
	m = na;
	almr = alfr[m];
	almi = alfi[m];
	betm = beta[m];
/*     .......... LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT   
                  EIGENVECTOR MATRIX IS TRIANGULAR .......... */
	y = betm * a[en + na * a_dim1];
	b[na + na * b_dim1] = -almi * b[en + en * b_dim1] / y;
	b[na + en * b_dim1] = (almr * b[en + en * b_dim1] - betm * a[en + en *
		 a_dim1]) / y;
	b[en + na * b_dim1] = 0.;
	b[en + en * b_dim1] = 1.;
	enm2 = na - 1;
	if (enm2 == 0) {
	    goto L795;
	}
/*     .......... FOR I=EN-2 STEP -1 UNTIL 1 DO -- .......... */
	i__2 = enm2;
	for (ii = 1; ii <= i__2; ++ii) {
	    i = na - ii;
	    w = betm * a[i + i * a_dim1] - almr * b[i + i * b_dim1];
	    w1 = -almi * b[i + i * b_dim1];
	    ra = 0.;
	    sa = 0.;

	    i__3 = en;
	    for (j = m; j <= i__3; ++j) {
		x = betm * a[i + j * a_dim1] - almr * b[i + j * b_dim1];
		x1 = -almi * b[i + j * b_dim1];
		ra = ra + x * b[j + na * b_dim1] - x1 * b[j + en * b_dim1];
		sa = sa + x * b[j + en * b_dim1] + x1 * b[j + na * b_dim1];
/* L760: */
	    }

	    if (i == 1 || isw == 2) {
		goto L770;
	    }
	    if (betm * a[i + (i - 1) * a_dim1] == 0.) {
		goto L770;
	    }
	    zz = w;
	    z1 = w1;
	    r = ra;
	    s = sa;
	    isw = 2;
	    goto L790;
L770:
	    m = i;
	    if (isw == 2) {
		goto L780;
	    }
/*     .......... COMPLEX 1-BY-1 BLOCK .......... */
	    tr = -ra;
	    ti = -sa;
L773:
	    dr = w;
	    di = w1;
/*     .......... COMPLEX DIVIDE (T1,T2) = (TR,TI) / (DR,DI) .....
..... */
L775:
	    if (abs(di) > abs(dr)) {
		goto L777;
	    }
	    rr = di / dr;
	    d = dr + di * rr;
	    t1 = (tr + ti * rr) / d;
	    t2 = (ti - tr * rr) / d;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
L777:
	    rr = dr / di;
	    d = dr * rr + di;
	    t1 = (tr * rr + ti) / d;
	    t2 = (ti * rr - tr) / d;
	    switch (isw) {
		case 1:  goto L787;
		case 2:  goto L782;
	    }
/*     .......... COMPLEX 2-BY-2 BLOCK .......... */
L780:
	    x = betm * a[i + (i + 1) * a_dim1] - almr * b[i + (i + 1) * 
		    b_dim1];
	    x1 = -almi * b[i + (i + 1) * b_dim1];
	    y = betm * a[i + 1 + i * a_dim1];
	    tr = y * ra - w * r + w1 * s;
	    ti = y * sa - w * s - w1 * r;
	    dr = w * zz - w1 * z1 - x * y;
	    di = w * z1 + w1 * zz - x1 * y;
/*           ------------------- BEGIN TIMING CODE --------------
-------- */
	    ++in2by2;
/*           -------------------- END TIMING CODE ---------------
-------- */
	    if (dr == 0. && di == 0.) {
		dr = epsb;
	    }
	    goto L775;
L782:
	    b[i + 1 + na * b_dim1] = t1;
	    b[i + 1 + en * b_dim1] = t2;
	    isw = 1;
	    if (abs(y) > abs(w) + abs(w1)) {
		goto L785;
	    }
	    tr = -ra - x * b[i + 1 + na * b_dim1] + x1 * b[i + 1 + en * 
		    b_dim1];
	    ti = -sa - x * b[i + 1 + en * b_dim1] - x1 * b[i + 1 + na * 
		    b_dim1];
	    goto L773;
L785:
	    t1 = (-r - zz * b[i + 1 + na * b_dim1] + z1 * b[i + 1 + en * 
		    b_dim1]) / y;
	    t2 = (-s - zz * b[i + 1 + en * b_dim1] - z1 * b[i + 1 + na * 
		    b_dim1]) / y;
L787:
	    b[i + na * b_dim1] = t1;
	    b[i + en * b_dim1] = t2;
L790:
	    ;
	}
/*        --------------------- BEGIN TIMING CODE -------------------
---- */
	latime_1.ops += (doublereal) ((en * 6 - 7) * (en - 2) + in2by2 * 31);
/*        ---------------------- END TIMING CODE --------------------
----   
       .......... END COMPLEX VECTOR .......... */
L795:
	isw = 3 - isw;
L800:
	;
    }
/*     .......... END BACK SUBSTITUTION.   
                  TRANSFORM TO ORIGINAL COORDINATE SYSTEM.   
                  FOR J=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (jj = 1; jj <= i__1; ++jj) {
	j = *n + 1 - jj;

	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    zz = 0.;

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
/* L860: */
		zz += z[i + k * z_dim1] * b[k + j * b_dim1];
	    }

	    z[i + j * z_dim1] = zz;
/* L880: */
	}
    }
/*     ----------------------- BEGIN TIMING CODE ------------------------ 
  
   Computing 2nd power */
    i__2 = *n;
    latime_1.ops += (doublereal) (i__2 * i__2) * (doublereal) (*n + 1);
/*     ------------------------ END TIMING CODE ------------------------- 
  
       .......... NORMALIZE SO THAT MODULUS OF LARGEST   
                  COMPONENT OF EACH VECTOR IS 1.   
                  (ISW IS 1 INITIALLY FROM BEFORE) ..........   
       ------------------------ BEGIN TIMING CODE ----------------------- 
*/
    in2by2 = 0;
/*     ------------------------- END TIMING CODE ------------------------ 
*/
    i__2 = *n;
    for (j = 1; j <= i__2; ++j) {
	d = 0.;
	if (isw == 2) {
	    goto L920;
	}
	if (alfi[j] != 0.) {
	    goto L945;
	}

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
	    if ((d__1 = z[i + j * z_dim1], abs(d__1)) > d) {
		d = (d__2 = z[i + j * z_dim1], abs(d__2));
	    }
/* L890: */
	}

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
/* L900: */
	    z[i + j * z_dim1] /= d;
	}

	goto L950;

L920:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
	    r = (d__1 = z[i + (j - 1) * z_dim1], abs(d__1)) + (d__2 = z[i + j 
		    * z_dim1], abs(d__2));
	    if (r != 0.) {
/* Computing 2nd power */
		d__1 = z[i + (j - 1) * z_dim1] / r;
/* Computing 2nd power */
		d__2 = z[i + j * z_dim1] / r;
		r *= sqrt(d__1 * d__1 + d__2 * d__2);
	    }
	    if (r > d) {
		d = r;
	    }
/* L930: */
	}

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
	    z[i + (j - 1) * z_dim1] /= d;
	    z[i + j * z_dim1] /= d;
/* L940: */
	}
/*        ---------------------- BEGIN TIMING CODE ------------------
---- */
	++in2by2;
/*        ----------------------- END TIMING CODE -------------------
---- */

L945:
	isw = 3 - isw;
L950:
	;
    }
/*     ------------------------ BEGIN TIMING CODE ----------------------- 
*/
    latime_1.ops += (doublereal) (*n * (*n + in2by2 * 5));
/*     ------------------------- END TIMING CODE ------------------------ 
*/

    return 0;
} /* qzvec_ */

