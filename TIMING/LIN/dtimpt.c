/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__13 = 13;
static integer c__0 = 0;

/* Subroutine */ int dtimpt_(char *line, integer *nm, integer *mval, integer *
	nns, integer *nsval, integer *nlda, integer *ldaval, doublereal *
	timmin, doublereal *a, doublereal *b, doublereal *reslts, integer *
	ldr1, integer *ldr2, integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char subnam[6*4] = "DPTTRF" "DPTTRS" "DPTSV " "DPTSL ";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static doublereal time;
    static integer isub, nrhs, i, m, n;
    static char cname[6];
    static integer laval[1];
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern /* Subroutine */ int dptsl_(integer *, doublereal *, doublereal *, 
	    doublereal *), dptsv_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *);
    static doublereal s1, s2;
    static integer ic, im;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dtimmg_(
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *), dprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *, integer 
	    *, integer *, ftnlen, ftnlen);
    static doublereal untime;
    static logical timsub[4];
    extern /* Subroutine */ int dpttrf_(integer *, doublereal *, doublereal *,
	     integer *), dpttrs_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *);
    static integer ldb, icl;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___22 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___23 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___24 = { 0, 0, 0, 0, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMPT times DPTTRF, -TRS, -SV, and -SL.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for 
  
            a full description of the format of the input line.   

    NM      (input) INTEGER   
            The number of values of M contained in the vector MVAL.   

    MVAL    (input) INTEGER array, dimension (NM)   
            The values of the matrix size M.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) DOUBLE PRECISION array, dimension (NMAX*2)   
            where NMAX is the maximum value permitted for N.   

    B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) DOUBLE PRECISION array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= 1.   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NM).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --mval;
    --nsval;
    --ldaval;
    --a;
    --b;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = reslts_dim1 * (reslts_dim2 * (reslts_dim3 + 1) + 1) + 1;
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Double precision", 1L, 16L);
    s_copy(path + 1, "PT", 2L, 2L);
    atimin_(path, line, &c__4, subnam, timsub, nout, &info, 3L, 80L, 6L);
    if (info != 0) {
	goto L170;
    }

/*     Check that N <= LDA for the input values. */

    for (isub = 2; isub <= 4; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L10;
	}
	s_copy(cname, subnam + (isub - 1) * 6, 6L, 6L);
	atimck_(&c__2, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, 6L)
		;
	if (info > 0) {
	    io___7.ciunit = *nout;
	    s_wsfe(&io___7);
	    do_fio(&c__1, cname, 6L);
	    e_wsfe();
	    timsub[isub - 1] = FALSE_;
	}
L10:
	;
    }

/*     Do for each value of M: */

    i__1 = *nm;
    for (im = 1; im <= i__1; ++im) {

	m = mval[im];
	n = max(m,1);

/*        Time DPTTRF */

	if (timsub[0]) {
	    i__2 = n << 1;
	    dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	    ic = 0;
	    s1 = dsecnd_();
L20:
	    dpttrf_(&m, &a[1], &a[n + 1], &info);
	    s2 = dsecnd_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n << 1;
		dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		goto L20;
	    }

/*           Subtract the time used in DTIMMG. */

	    icl = 1;
	    s1 = dsecnd_();
L30:
	    s2 = dsecnd_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n << 1;
		dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		goto L30;
	    }

	    time = (time - untime) / (doublereal) ic;
	    ops = dopla_("DPTTRF", &m, &c__0, &c__0, &c__0, &c__0);
	    reslts[(im + (reslts_dim3 + 1) * reslts_dim2) * reslts_dim1 + 1] =
		     dmflop_(&ops, &time, &info);

	} else {
	    ic = 0;
	    i__2 = n << 1;
	    dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	}

/*        Generate another matrix and factor it using DPTTRF so   
          that the factored form can be used in timing the other   
          routines. */

	if (ic != 1) {
	    dpttrf_(&m, &a[1], &a[n + 1], &info);
	}

/*        Time DPTTRS */

	if (timsub[1]) {
	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		ldb = ldaval[ilda];
		i__3 = *nns;
		for (i = 1; i <= i__3; ++i) {
		    nrhs = nsval[i];
		    dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = dsecnd_();
L40:
		    dpttrs_(&m, &nrhs, &a[1], &a[n + 1], &b[1], &ldb, &info);
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L40;
		    }

/*                 Subtract the time used in DTIMMG. */

		    icl = 1;
		    s1 = dsecnd_();
L50:
		    s2 = dsecnd_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L50;
		    }

		    time = (time - untime) / (doublereal) ic;
		    ops = dopla_("DPTTRS", &m, &nrhs, &c__0, &c__0, &c__0)
			    ;
		    reslts[i + (im + (ilda + (reslts_dim3 << 1)) * 
			    reslts_dim2) * reslts_dim1] = dmflop_(&ops, &time,
			     &info);
/* L60: */
		}
/* L70: */
	    }
	}

	if (timsub[2]) {
	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		ldb = ldaval[ilda];
		i__3 = *nns;
		for (i = 1; i <= i__3; ++i) {
		    nrhs = nsval[i];
		    i__4 = n << 1;
		    dtimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
		    dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
		    ic = 0;
		    s1 = dsecnd_();
L80:
		    dptsv_(&m, &nrhs, &a[1], &a[n + 1], &b[1], &ldb, &info);
		    s2 = dsecnd_();
		    time = s2 - s1;
		    ++ic;
		    if (time < *timmin) {
			i__4 = n << 1;
			dtimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
			dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L80;
		    }

/*                 Subtract the time used in DTIMMG. */

		    icl = 1;
		    s1 = dsecnd_();
L90:
		    s2 = dsecnd_();
		    untime = s2 - s1;
		    ++icl;
		    if (icl <= ic) {
			i__4 = n << 1;
			dtimmg_(&c__13, &m, &m, &a[1], &i__4, &c__0, &c__0);
			dtimmg_(&c__0, &m, &nrhs, &b[1], &ldb, &c__0, &c__0);
			goto L90;
		    }

		    time = (time - untime) / (doublereal) ic;
		    ops = dopla_("DPTSV ", &m, &nrhs, &c__0, &c__0, &c__0)
			    ;
		    reslts[i + (im + (ilda + reslts_dim3 * 3) * reslts_dim2) *
			     reslts_dim1] = dmflop_(&ops, &time, &info);
/* L100: */
		}
/* L110: */
	    }
	}

	if (timsub[3]) {
	    i__2 = n << 1;
	    dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
	    dtimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
	    ic = 0;
	    s1 = dsecnd_();
L120:
	    dptsl_(&m, &a[1], &a[n + 1], &b[1]);
	    s2 = dsecnd_();
	    time = s2 - s1;
	    ++ic;
	    if (time < *timmin) {
		i__2 = n << 1;
		dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		dtimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L120;
	    }

/*           Subtract the time used in DTIMMG. */

	    icl = 1;
	    s1 = dsecnd_();
L130:
	    s2 = dsecnd_();
	    untime = s2 - s1;
	    ++icl;
	    if (icl <= ic) {
		i__2 = n << 1;
		dtimmg_(&c__13, &m, &m, &a[1], &i__2, &c__0, &c__0);
		dtimmg_(&c__0, &m, &c__1, &b[1], &n, &c__0, &c__0);
		goto L130;
	    }

	    time = (time - untime) / (doublereal) ic;
	    ops = dopla_("DPTSV ", &m, &c__1, &c__0, &c__0, &c__0);
	    reslts[(im + ((reslts_dim3 << 2) + 1) * reslts_dim2) * 
		    reslts_dim1 + 1] = dmflop_(&ops, &time, &info);
	}
/* L140: */
    }

/*     Print a table of results for each timed routine. */

    for (isub = 1; isub <= 4; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L160;
	}
	io___22.ciunit = *nout;
	s_wsfe(&io___22);
	do_fio(&c__1, subnam + (isub - 1) * 6, 6L);
	e_wsfe();
	if (*nlda > 1 && (timsub[1] || timsub[2])) {
	    i__1 = *nlda;
	    for (i = 1; i <= i__1; ++i) {
		io___23.ciunit = *nout;
		s_wsfe(&io___23);
		do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i], (ftnlen)sizeof(integer));
		e_wsfe();
/* L150: */
	    }
	}
	io___24.ciunit = *nout;
	s_wsle(&io___24);
	e_wsle();
	if (isub == 1) {
	    dprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts[
		    reslts_offset], ldr1, ldr2, nout, 1L, 1L);
	} else if (isub == 2) {
	    dprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &reslts[(
		    ((reslts_dim3 << 1) + 1) * reslts_dim2 + 1) * reslts_dim1 
		    + 1], ldr1, ldr2, nout, 4L, 1L);
	} else if (isub == 3) {
	    dprtbl_("NRHS", "N", nns, &nsval[1], nm, &mval[1], nlda, &reslts[(
		    (reslts_dim3 * 3 + 1) * reslts_dim2 + 1) * reslts_dim1 + 
		    1], ldr1, ldr2, nout, 4L, 1L);
	} else if (isub == 4) {
	    dprtbl_(" ", "N", &c__1, laval, nm, &mval[1], &c__1, &reslts[(((
		    reslts_dim3 << 2) + 1) * reslts_dim2 + 1) * reslts_dim1 + 
		    1], ldr1, ldr2, nout, 1L, 1L);
	}
L160:
	;
    }

L170:
    return 0;

/*     End of DTIMPT */

} /* dtimpt_ */

