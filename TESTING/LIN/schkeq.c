/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static real c_b7 = 10.f;
static integer c_n1 = -1;
static integer c__5 = 5;
static integer c__13 = 13;
static integer c__1 = 1;

/* Subroutine */ int schkeq_(real *thresh, integer *nout)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,\002All tests for \002,a3,\002 routines pa"
	    "ssed the threshold\002)";
    static char fmt_9998[] = "(\002 SGEEQU failed test with value \002,e10"
	    ".3,\002 exceeding\002,\002 threshold \002,e10.3)";
    static char fmt_9997[] = "(\002 SGBEQU failed test with value \002,e10"
	    ".3,\002 exceeding\002,\002 threshold \002,e10.3)";
    static char fmt_9996[] = "(\002 SPOEQU failed test with value \002,e10"
	    ".3,\002 exceeding\002,\002 threshold \002,e10.3)";
    static char fmt_9995[] = "(\002 SPPEQU failed test with value \002,e10"
	    ".3,\002 exceeding\002,\002 threshold \002,e10.3)";
    static char fmt_9994[] = "(\002 SPBEQU failed test with value \002,e10"
	    ".3,\002 exceeding\002,\002 threshold \002,e10.3)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;
    real r__1, r__2, r__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double pow_ri(real *, integer *);
    integer pow_ii(integer *, integer *), s_wsle(cilist *), e_wsle(void), 
	    s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer info;
    static char path[3];
    static real norm, rpow[11], a[25]	/* was [5][5] */, c[5];
    static integer i, j, m, n;
    static real r[5], ccond, rcond, rcmin, rcmax, ratio, ab[65]	/* was [13][5]
	     */, ap[15];
    static integer kl;
    static logical ok;
    static integer ku;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int sgbequ_(integer *, integer *, integer *, 
	    integer *, real *, integer *, real *, real *, real *, real *, 
	    real *, integer *), sgeequ_(integer *, integer *, real *, integer 
	    *, real *, real *, real *, real *, real *, integer *), spbequ_(
	    char *, integer *, integer *, real *, integer *, real *, real *, 
	    real *, integer *);
    static real reslts[5];
    extern /* Subroutine */ int spoequ_(integer *, real *, integer *, real *, 
	    real *, real *, integer *), sppequ_(char *, integer *, real *, 
	    real *, real *, real *, integer *);
    static real eps, pow[11];

    /* Fortran I/O blocks */
    static cilist io___25 = { 0, 0, 0, 0, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9994, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    SCHKEQ tests SGEEQU, SGBEQU, SPOEQU, SPPEQU and SPBEQU   

    Arguments   
    =========   

    THRESH  (input) REAL   
            Threshold for testing routines. Should be between 2 and 10.   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
*/


    *(unsigned char *)path = 'S';
    s_copy(path + 1, "EQ", 2L, 2L);

    eps = slamch_("P");
    for (i = 1; i <= 5; ++i) {
	reslts[i - 1] = 0.f;
/* L10: */
    }
    for (i = 1; i <= 11; ++i) {
	i__1 = i - 1;
	pow[i - 1] = pow_ri(&c_b7, &i__1);
	rpow[i - 1] = 1.f / pow[i - 1];
/* L20: */
    }

/*     Test SGEEQU */

    for (n = 0; n <= 5; ++n) {
	for (m = 0; m <= 5; ++m) {

	    for (j = 1; j <= 5; ++j) {
		for (i = 1; i <= 5; ++i) {
		    if (i <= m && j <= n) {
			i__1 = i + j;
			a[i + j * 5 - 6] = pow[i + j] * pow_ii(&c_n1, &i__1);
		    } else {
			a[i + j * 5 - 6] = 0.f;
		    }
/* L30: */
		}
/* L40: */
	    }

	    sgeequ_(&m, &n, a, &c__5, r, c, &rcond, &ccond, &norm, &info);

	    if (info != 0) {
		reslts[0] = 1.f;
	    } else {
		if (n != 0 && m != 0) {
/* Computing MAX */
		    r__2 = reslts[0], r__3 = (r__1 = (rcond - rpow[m - 1]) / 
			    rpow[m - 1], dabs(r__1));
		    reslts[0] = dmax(r__2,r__3);
/* Computing MAX */
		    r__2 = reslts[0], r__3 = (r__1 = (ccond - rpow[n - 1]) / 
			    rpow[n - 1], dabs(r__1));
		    reslts[0] = dmax(r__2,r__3);
/* Computing MAX */
		    r__2 = reslts[0], r__3 = (r__1 = (norm - pow[n + m]) / 
			    pow[n + m], dabs(r__1));
		    reslts[0] = dmax(r__2,r__3);
		    i__1 = m;
		    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
			r__2 = reslts[0], r__3 = (r__1 = (r[i - 1] - rpow[i + 
				n]) / rpow[i + n], dabs(r__1));
			reslts[0] = dmax(r__2,r__3);
/* L50: */
		    }
		    i__1 = n;
		    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
			r__2 = reslts[0], r__3 = (r__1 = (c[j - 1] - pow[n - 
				j]) / pow[n - j], dabs(r__1));
			reslts[0] = dmax(r__2,r__3);
/* L60: */
		    }
		}
	    }

/* L70: */
	}
/* L80: */
    }

/*     Test with zero rows and columns */

    for (j = 1; j <= 5; ++j) {
	a[4 + j * 5 - 6] = 0.f;
/* L90: */
    }
    sgeequ_(&c__5, &c__5, a, &c__5, r, c, &rcond, &ccond, &norm, &info);
    if (info != 4) {
	reslts[0] = 1.f;
    }

    for (j = 1; j <= 5; ++j) {
	a[4 + j * 5 - 6] = 1.f;
/* L100: */
    }
    for (i = 1; i <= 5; ++i) {
	a[i + 20 - 6] = 0.f;
/* L110: */
    }
    sgeequ_(&c__5, &c__5, a, &c__5, r, c, &rcond, &ccond, &norm, &info);
    if (info != 9) {
	reslts[0] = 1.f;
    }
    reslts[0] /= eps;

/*     Test SGBEQU */

    for (n = 0; n <= 5; ++n) {
	for (m = 0; m <= 5; ++m) {
/* Computing MAX */
	    i__2 = m - 1;
	    i__1 = max(i__2,0);
	    for (kl = 0; kl <= i__1; ++kl) {
/* Computing MAX */
		i__3 = n - 1;
		i__2 = max(i__3,0);
		for (ku = 0; ku <= i__2; ++ku) {

		    for (j = 1; j <= 5; ++j) {
			for (i = 1; i <= 13; ++i) {
			    ab[i + j * 13 - 14] = 0.f;
/* L120: */
			}
/* L130: */
		    }
		    i__3 = n;
		    for (j = 1; j <= i__3; ++j) {
			i__4 = m;
			for (i = 1; i <= i__4; ++i) {
/* Computing MIN */
			    i__5 = m, i__6 = j + kl;
/* Computing MAX */
			    i__7 = 1, i__8 = j - ku;
			    if (i <= min(i__5,i__6) && i >= max(i__7,i__8) && 
				    j <= n) {
				i__5 = i + j;
				ab[ku + 1 + i - j + j * 13 - 14] = pow[i + j] 
					* pow_ii(&c_n1, &i__5);
			    }
/* L140: */
			}
/* L150: */
		    }

		    sgbequ_(&m, &n, &kl, &ku, ab, &c__13, r, c, &rcond, &
			    ccond, &norm, &info);

		    if (info != 0) {
			if (! (n + kl < m && info == n + kl + 1 || m + ku < n 
				&& info == (m << 1) + ku + 1)) {
			    reslts[1] = 1.f;
			}
		    } else {
			if (n != 0 && m != 0) {

			    rcmin = r[0];
			    rcmax = r[0];
			    i__3 = m;
			    for (i = 1; i <= i__3; ++i) {
/* Computing MIN */
				r__1 = rcmin, r__2 = r[i - 1];
				rcmin = dmin(r__1,r__2);
/* Computing MAX */
				r__1 = rcmax, r__2 = r[i - 1];
				rcmax = dmax(r__1,r__2);
/* L160: */
			    }
			    ratio = rcmin / rcmax;
/* Computing MAX */
			    r__2 = reslts[1], r__3 = (r__1 = (rcond - ratio) /
				     ratio, dabs(r__1));
			    reslts[1] = dmax(r__2,r__3);

			    rcmin = c[0];
			    rcmax = c[0];
			    i__3 = n;
			    for (j = 1; j <= i__3; ++j) {
/* Computing MIN */
				r__1 = rcmin, r__2 = c[j - 1];
				rcmin = dmin(r__1,r__2);
/* Computing MAX */
				r__1 = rcmax, r__2 = c[j - 1];
				rcmax = dmax(r__1,r__2);
/* L170: */
			    }
			    ratio = rcmin / rcmax;
/* Computing MAX */
			    r__2 = reslts[1], r__3 = (r__1 = (ccond - ratio) /
				     ratio, dabs(r__1));
			    reslts[1] = dmax(r__2,r__3);

/* Computing MAX */
			    r__2 = reslts[1], r__3 = (r__1 = (norm - pow[n + 
				    m]) / pow[n + m], dabs(r__1));
			    reslts[1] = dmax(r__2,r__3);
			    i__3 = m;
			    for (i = 1; i <= i__3; ++i) {
				rcmax = 0.f;
				i__4 = n;
				for (j = 1; j <= i__4; ++j) {
				    if (i <= j + kl && i >= j - ku) {
					ratio = (r__1 = r[i - 1] * pow[i + j] 
						* c[j - 1], dabs(r__1));
					rcmax = dmax(rcmax,ratio);
				    }
/* L180: */
				}
/* Computing MAX */
				r__2 = reslts[1], r__3 = (r__1 = 1.f - rcmax, 
					dabs(r__1));
				reslts[1] = dmax(r__2,r__3);
/* L190: */
			    }

			    i__3 = n;
			    for (j = 1; j <= i__3; ++j) {
				rcmax = 0.f;
				i__4 = m;
				for (i = 1; i <= i__4; ++i) {
				    if (i <= j + kl && i >= j - ku) {
					ratio = (r__1 = r[i - 1] * pow[i + j] 
						* c[j - 1], dabs(r__1));
					rcmax = dmax(rcmax,ratio);
				    }
/* L200: */
				}
/* Computing MAX */
				r__2 = reslts[1], r__3 = (r__1 = 1.f - rcmax, 
					dabs(r__1));
				reslts[1] = dmax(r__2,r__3);
/* L210: */
			    }
			}
		    }

/* L220: */
		}
/* L230: */
	    }
/* L240: */
	}
/* L250: */
    }
    reslts[1] /= eps;

/*     Test SPOEQU */

    for (n = 0; n <= 5; ++n) {

	for (i = 1; i <= 5; ++i) {
	    for (j = 1; j <= 5; ++j) {
		if (i <= n && j == i) {
		    i__1 = i + j;
		    a[i + j * 5 - 6] = pow[i + j] * pow_ii(&c_n1, &i__1);
		} else {
		    a[i + j * 5 - 6] = 0.f;
		}
/* L260: */
	    }
/* L270: */
	}

	spoequ_(&n, a, &c__5, r, &rcond, &norm, &info);

	if (info != 0) {
	    reslts[2] = 1.f;
	} else {
	    if (n != 0) {
/* Computing MAX */
		r__2 = reslts[2], r__3 = (r__1 = (rcond - rpow[n - 1]) / rpow[
			n - 1], dabs(r__1));
		reslts[2] = dmax(r__2,r__3);
/* Computing MAX */
		r__2 = reslts[2], r__3 = (r__1 = (norm - pow[n * 2]) / pow[n *
			 2], dabs(r__1));
		reslts[2] = dmax(r__2,r__3);
		i__1 = n;
		for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
		    r__2 = reslts[2], r__3 = (r__1 = (r[i - 1] - rpow[i]) / 
			    rpow[i], dabs(r__1));
		    reslts[2] = dmax(r__2,r__3);
/* L280: */
		}
	    }
	}
/* L290: */
    }
    a[18] = -1.f;
    spoequ_(&c__5, a, &c__5, r, &rcond, &norm, &info);
    if (info != 4) {
	reslts[2] = 1.f;
    }
    reslts[2] /= eps;

/*     Test SPPEQU */

    for (n = 0; n <= 5; ++n) {

/*        Upper triangular packed storage */

	i__1 = n * (n + 1) / 2;
	for (i = 1; i <= i__1; ++i) {
	    ap[i - 1] = 0.f;
/* L300: */
	}
	i__1 = n;
	for (i = 1; i <= i__1; ++i) {
	    ap[i * (i + 1) / 2 - 1] = pow[i * 2];
/* L310: */
	}

	sppequ_("U", &n, ap, r, &rcond, &norm, &info);

	if (info != 0) {
	    reslts[3] = 1.f;
	} else {
	    if (n != 0) {
/* Computing MAX */
		r__2 = reslts[3], r__3 = (r__1 = (rcond - rpow[n - 1]) / rpow[
			n - 1], dabs(r__1));
		reslts[3] = dmax(r__2,r__3);
/* Computing MAX */
		r__2 = reslts[3], r__3 = (r__1 = (norm - pow[n * 2]) / pow[n *
			 2], dabs(r__1));
		reslts[3] = dmax(r__2,r__3);
		i__1 = n;
		for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
		    r__2 = reslts[3], r__3 = (r__1 = (r[i - 1] - rpow[i]) / 
			    rpow[i], dabs(r__1));
		    reslts[3] = dmax(r__2,r__3);
/* L320: */
		}
	    }
	}

/*        Lower triangular packed storage */

	i__1 = n * (n + 1) / 2;
	for (i = 1; i <= i__1; ++i) {
	    ap[i - 1] = 0.f;
/* L330: */
	}
	j = 1;
	i__1 = n;
	for (i = 1; i <= i__1; ++i) {
	    ap[j - 1] = pow[i * 2];
	    j += n - i + 1;
/* L340: */
	}

	sppequ_("L", &n, ap, r, &rcond, &norm, &info);

	if (info != 0) {
	    reslts[3] = 1.f;
	} else {
	    if (n != 0) {
/* Computing MAX */
		r__2 = reslts[3], r__3 = (r__1 = (rcond - rpow[n - 1]) / rpow[
			n - 1], dabs(r__1));
		reslts[3] = dmax(r__2,r__3);
/* Computing MAX */
		r__2 = reslts[3], r__3 = (r__1 = (norm - pow[n * 2]) / pow[n *
			 2], dabs(r__1));
		reslts[3] = dmax(r__2,r__3);
		i__1 = n;
		for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
		    r__2 = reslts[3], r__3 = (r__1 = (r[i - 1] - rpow[i]) / 
			    rpow[i], dabs(r__1));
		    reslts[3] = dmax(r__2,r__3);
/* L350: */
		}
	    }
	}

/* L360: */
    }
    i = 13;
    ap[i - 1] = -1.f;
    sppequ_("L", &c__5, ap, r, &rcond, &norm, &info);
    if (info != 4) {
	reslts[3] = 1.f;
    }
    reslts[3] /= eps;

/*     Test SPBEQU */

    for (n = 0; n <= 5; ++n) {
/* Computing MAX */
	i__2 = n - 1;
	i__1 = max(i__2,0);
	for (kl = 0; kl <= i__1; ++kl) {

/*           Test upper triangular storage */

	    for (j = 1; j <= 5; ++j) {
		for (i = 1; i <= 13; ++i) {
		    ab[i + j * 13 - 14] = 0.f;
/* L370: */
		}
/* L380: */
	    }
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		ab[kl + 1 + j * 13 - 14] = pow[j * 2];
/* L390: */
	    }

	    spbequ_("U", &n, &kl, ab, &c__13, r, &rcond, &norm, &info);

	    if (info != 0) {
		reslts[4] = 1.f;
	    } else {
		if (n != 0) {
/* Computing MAX */
		    r__2 = reslts[4], r__3 = (r__1 = (rcond - rpow[n - 1]) / 
			    rpow[n - 1], dabs(r__1));
		    reslts[4] = dmax(r__2,r__3);
/* Computing MAX */
		    r__2 = reslts[4], r__3 = (r__1 = (norm - pow[n * 2]) / 
			    pow[n * 2], dabs(r__1));
		    reslts[4] = dmax(r__2,r__3);
		    i__2 = n;
		    for (i = 1; i <= i__2; ++i) {
/* Computing MAX */
			r__2 = reslts[4], r__3 = (r__1 = (r[i - 1] - rpow[i]) 
				/ rpow[i], dabs(r__1));
			reslts[4] = dmax(r__2,r__3);
/* L400: */
		    }
		}
	    }
	    if (n != 0) {
/* Computing MAX */
		i__2 = n - 1;
		ab[kl + 1 + max(i__2,1) * 13 - 14] = -1.f;
		spbequ_("U", &n, &kl, ab, &c__13, r, &rcond, &norm, &info)
			;
/* Computing MAX */
		i__2 = n - 1;
		if (info != max(i__2,1)) {
		    reslts[4] = 1.f;
		}
	    }

/*           Test lower triangular storage */

	    for (j = 1; j <= 5; ++j) {
		for (i = 1; i <= 13; ++i) {
		    ab[i + j * 13 - 14] = 0.f;
/* L410: */
		}
/* L420: */
	    }
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		ab[j * 13 - 13] = pow[j * 2];
/* L430: */
	    }

	    spbequ_("L", &n, &kl, ab, &c__13, r, &rcond, &norm, &info);

	    if (info != 0) {
		reslts[4] = 1.f;
	    } else {
		if (n != 0) {
/* Computing MAX */
		    r__2 = reslts[4], r__3 = (r__1 = (rcond - rpow[n - 1]) / 
			    rpow[n - 1], dabs(r__1));
		    reslts[4] = dmax(r__2,r__3);
/* Computing MAX */
		    r__2 = reslts[4], r__3 = (r__1 = (norm - pow[n * 2]) / 
			    pow[n * 2], dabs(r__1));
		    reslts[4] = dmax(r__2,r__3);
		    i__2 = n;
		    for (i = 1; i <= i__2; ++i) {
/* Computing MAX */
			r__2 = reslts[4], r__3 = (r__1 = (r[i - 1] - rpow[i]) 
				/ rpow[i], dabs(r__1));
			reslts[4] = dmax(r__2,r__3);
/* L440: */
		    }
		}
	    }
	    if (n != 0) {
/* Computing MAX */
		i__2 = n - 1;
		ab[max(i__2,1) * 13 - 13] = -1.f;
		spbequ_("L", &n, &kl, ab, &c__13, r, &rcond, &norm, &info)
			;
/* Computing MAX */
		i__2 = n - 1;
		if (info != max(i__2,1)) {
		    reslts[4] = 1.f;
		}
	    }
/* L450: */
	}
/* L460: */
    }
    reslts[4] /= eps;
    ok = reslts[0] <= *thresh && reslts[1] <= *thresh && reslts[2] <= *thresh 
	    && reslts[3] <= *thresh && reslts[4] <= *thresh;
    io___25.ciunit = *nout;
    s_wsle(&io___25);
    e_wsle();
    if (ok) {
	io___26.ciunit = *nout;
	s_wsfe(&io___26);
	do_fio(&c__1, path, 3L);
	e_wsfe();
    } else {
	if (reslts[0] > *thresh) {
	    io___27.ciunit = *nout;
	    s_wsfe(&io___27);
	    do_fio(&c__1, (char *)&reslts[0], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&(*thresh), (ftnlen)sizeof(real));
	    e_wsfe();
	}
	if (reslts[1] > *thresh) {
	    io___28.ciunit = *nout;
	    s_wsfe(&io___28);
	    do_fio(&c__1, (char *)&reslts[1], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&(*thresh), (ftnlen)sizeof(real));
	    e_wsfe();
	}
	if (reslts[2] > *thresh) {
	    io___29.ciunit = *nout;
	    s_wsfe(&io___29);
	    do_fio(&c__1, (char *)&reslts[2], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&(*thresh), (ftnlen)sizeof(real));
	    e_wsfe();
	}
	if (reslts[3] > *thresh) {
	    io___30.ciunit = *nout;
	    s_wsfe(&io___30);
	    do_fio(&c__1, (char *)&reslts[3], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&(*thresh), (ftnlen)sizeof(real));
	    e_wsfe();
	}
	if (reslts[4] > *thresh) {
	    io___31.ciunit = *nout;
	    s_wsfe(&io___31);
	    do_fio(&c__1, (char *)&reslts[4], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&(*thresh), (ftnlen)sizeof(real));
	    e_wsfe();
	}
    }
    return 0;

/*     End of SCHKEQ */

} /* schkeq_ */

