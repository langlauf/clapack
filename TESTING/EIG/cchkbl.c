/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__6 = 6;
static integer c__4 = 4;
static integer c__20 = 20;

/* Subroutine */ int cchkbl_(integer *nin, integer *nout)
{
    /* Format strings */
    static char fmt_9999[] = "(1x,\002.. test output of CGEBAL .. \002)";
    static char fmt_9998[] = "(1x,\002value of largest test error           "
	    "  = \002,e12.3)";
    static char fmt_9997[] = "(1x,\002example number where info is not zero "
	    "  = \002,i4)";
    static char fmt_9996[] = "(1x,\002example number where ILO or IHI wrong "
	    "  = \002,i4)";
    static char fmt_9995[] = "(1x,\002example number having largest error   "
	    "  = \002,i4)";
    static char fmt_9994[] = "(1x,\002number of examples where info is not 0"
	    "  = \002,i4)";
    static char fmt_9993[] = "(1x,\002total number of examples tested       "
	    "  = \002,i4)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1, r__2, r__3, r__4;
    complex q__1, q__2;

    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);
    double r_imag(complex *);
    integer s_wsfe(cilist *), e_wsfe(void), do_fio(integer *, char *, ftnlen);

    /* Local variables */
    static integer info, lmax[3];
    static real rmax, vmax;
    static complex a[400]	/* was [20][20] */;
    static integer i, j, n;
    static real scale[20];
    static integer ihiin, ninfo, iloin;
    static real anorm, dummy[1];
    extern /* Subroutine */ int cgebal_(char *, integer *, complex *, integer 
	    *, integer *, integer *, real *, integer *);
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *), slamch_(char *);
    static real scalin[20];
    static complex ain[400]	/* was [20][20] */;
    static integer ihi, ilo, knt;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 0, 0, 0, 0 };
    static cilist io___8 = { 0, 0, 0, 0, 0 };
    static cilist io___11 = { 0, 0, 0, 0, 0 };
    static cilist io___14 = { 0, 0, 0, 0, 0 };
    static cilist io___16 = { 0, 0, 0, 0, 0 };
    static cilist io___25 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9993, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CCHKBL tests CGEBAL, a routine for balancing a general complex   
    matrix and isolating some of its eigenvalues.   

    Arguments   
    =========   

    NIN     (input) INTEGER   
            The logical unit number for input.  NIN > 0.   

    NOUT    (input) INTEGER   
            The logical unit number for output.  NOUT > 0.   

   ====================================================================== 
*/


    lmax[0] = 0;
    lmax[1] = 0;
    lmax[2] = 0;
    ninfo = 0;
    knt = 0;
    rmax = 0.f;

L10:

    io___5.ciunit = *nin;
    s_rsle(&io___5);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_rsle();
    if (n == 0) {
	goto L70;
    }
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	io___8.ciunit = *nin;
	s_rsle(&io___8);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__6, &c__1, (char *)&a[i + j * 20 - 21], (ftnlen)sizeof(
		    complex));
	}
	e_rsle();
/* L20: */
    }

    io___11.ciunit = *nin;
    s_rsle(&io___11);
    do_lio(&c__3, &c__1, (char *)&iloin, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&ihiin, (ftnlen)sizeof(integer));
    e_rsle();
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	io___14.ciunit = *nin;
	s_rsle(&io___14);
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    do_lio(&c__6, &c__1, (char *)&ain[i + j * 20 - 21], (ftnlen)
		    sizeof(complex));
	}
	e_rsle();
/* L30: */
    }
    io___16.ciunit = *nin;
    s_rsle(&io___16);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__4, &c__1, (char *)&scalin[i - 1], (ftnlen)sizeof(real));
    }
    e_rsle();

    anorm = clange_("M", &n, &n, a, &c__20, dummy);
    ++knt;
    cgebal_("B", &n, a, &c__20, &ilo, &ihi, scale, &info);

    if (info != 0) {
	++ninfo;
	lmax[0] = knt;
    }

    if (ilo != iloin || ihi != ihiin) {
	++ninfo;
	lmax[1] = knt;
    }

    vmax = 0.f;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
	    i__3 = i + j * 20 - 21;
	    i__4 = i + j * 20 - 21;
	    q__2.r = a[i__3].r - ain[i__4].r, q__2.i = a[i__3].i - ain[i__4]
		    .i;
	    q__1.r = q__2.r, q__1.i = q__2.i;
/* Computing MAX */
	    r__3 = vmax, r__4 = (r__1 = q__1.r, dabs(r__1)) + (r__2 = r_imag(&
		    q__1), dabs(r__2));
	    vmax = dmax(r__3,r__4);
/* L40: */
	}
/* L50: */
    }

    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
	r__2 = vmax, r__3 = (r__1 = scale[i - 1] - scalin[i - 1], dabs(r__1));
	vmax = dmax(r__2,r__3);
/* L60: */
    }

    vmax = vmax / anorm / slamch_("E");

    if (vmax > rmax) {
	lmax[2] = knt;
	rmax = vmax;
    }

    goto L10;

L70:

    io___25.ciunit = *nout;
    s_wsfe(&io___25);
    e_wsfe();

    io___26.ciunit = *nout;
    s_wsfe(&io___26);
    do_fio(&c__1, (char *)&rmax, (ftnlen)sizeof(real));
    e_wsfe();
    io___27.ciunit = *nout;
    s_wsfe(&io___27);
    do_fio(&c__1, (char *)&lmax[0], (ftnlen)sizeof(integer));
    e_wsfe();
    io___28.ciunit = *nout;
    s_wsfe(&io___28);
    do_fio(&c__1, (char *)&lmax[1], (ftnlen)sizeof(integer));
    e_wsfe();
    io___29.ciunit = *nout;
    s_wsfe(&io___29);
    do_fio(&c__1, (char *)&lmax[2], (ftnlen)sizeof(integer));
    e_wsfe();
    io___30.ciunit = *nout;
    s_wsfe(&io___30);
    do_fio(&c__1, (char *)&ninfo, (ftnlen)sizeof(integer));
    e_wsfe();
    io___31.ciunit = *nout;
    s_wsfe(&io___31);
    do_fio(&c__1, (char *)&knt, (ftnlen)sizeof(integer));
    e_wsfe();

    return 0;

/*     End of CCHKBL */

} /* cchkbl_ */

