/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int orthes_(integer *nm, integer *n, integer *low, integer *
	igh, real *a, real *ort)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h;
    static integer i, j, m;
    static real scale;
    static integer la, ii, jj, mp, kp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,   
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
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h = 0.f;
	ort[m] = 0.f;
	scale = 0.f;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... 
*/
	i__2 = *igh;
	for (i = m; i <= i__2; ++i) {
/* L90: */
	    scale += (r__1 = a[i + (m - 1) * a_dim1], dabs(r__1));
	}

	if (scale == 0.f) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i = mp - ii;
	    ort[i] = a[i + (m - 1) * a_dim1] / scale;
	    h += ort[i] * ort[i];
/* L100: */
	}

	r__1 = sqrt(h);
	g = -(doublereal)r_sign(&r__1, &ort[m]);
	h -= ort[m] * g;
	ort[m] -= g;
/*     .......... FORM (I-(U*UT)/H) * A .......... */
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    f = 0.f;
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
	    f = 0.f;
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
} /* orthes_   

   Subroutine */ int tred1_(integer *nm, integer *n, real *a, real *d, real *
	e, real *e2)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static real f, g, h;
    static integer i, j, k, l;
    static real scale;
    static integer ii, jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,   
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

    /* Function Body */
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
	h = 0.f;
	scale = 0.f;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (r__1 = d[k], dabs(r__1));
	}

	if (scale != 0.f) {
	    goto L140;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = a[l + j * a_dim1];
	    a[l + j * a_dim1] = a[i + j * a_dim1];
	    a[i + j * a_dim1] = 0.f;
/* L125: */
	}

L130:
	e[i] = 0.f;
	e2[i] = 0.f;
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
	r__1 = sqrt(h);
	g = -(doublereal)r_sign(&r__1, &f);
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
	    e[j] = 0.f;
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
	f = 0.f;

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
} /* tred1_ */

