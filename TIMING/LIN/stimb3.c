/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__1 = 1;
static integer c__0 = 0;
static real c_b47 = 1.f;

/* Subroutine */ int stimb3_(char *line, integer *nm, integer *mval, integer *
	nn, integer *nval, integer *nk, integer *kval, integer *nlda, integer 
	*ldaval, real *timmin, real *a, real *b, real *c, real *reslts, 
	integer *ldr1, integer *ldr2, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char names[6*6] = "SGEMM " "SSYMM " "SSYRK " "SSYR2K" "STRMM " 
	    "STRSM ";
    static char trans[1*2] = "N" "T";
    static char sides[1*2] = "L" "R";
    static char uplos[1*2] = "U" "L";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9995[] = "(/1x,\002SGEMM  with TRANSA = '\002,a1,\002', "
	    "TRANSB = '\002,a1,\002'\002)";
    static char fmt_9994[] = "(/1x,\002K = \002,i4,/)";
    static char fmt_9993[] = "(/1x,\002SSYMM  with SIDE = '\002,a1,\002', UP"
	    "LO = '\002,a1,\002'\002,/)";
    static char fmt_9992[] = "(/1x,a6,\002 with UPLO = '\002,a1,\002', TRANS"
	    " = '\002,a1,\002'\002,/)";
    static char fmt_9991[] = "(/1x,a6,\002 with SIDE = '\002,a1,\002', UPLO "
	    "= '\002,a1,\002',\002,\002 TRANS = '\002,a1,\002'\002,/)";
    static char fmt_9990[] = "(/////)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_offset, i__1, i__2, i__3, i__4;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer ilda;
    static char side[1];
    static integer imat, info;
    static char path[3];
    static real time;
    static integer isub;
    static char uplo[1];
    static integer i, k, m, n;
    static char cname[6];
    static integer iside;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int sgemm_(char *, char *, integer *, integer *, 
	    integer *, real *, real *, integer *, real *, integer *, real *, 
	    real *, integer *);
    static integer iuplo;
    extern /* Subroutine */ int strmm_(char *, char *, char *, char *, 
	    integer *, integer *, real *, real *, integer *, real *, integer *
	    );
    static real s1, s2;
    extern /* Subroutine */ int ssymm_(char *, char *, integer *, integer *, 
	    real *, real *, integer *, real *, integer *, real *, real *, 
	    integer *), strsm_(char *, char *, char *, char *,
	     integer *, integer *, real *, real *, integer *, real *, integer 
	    *), ssyrk_(char *, char *, 
	    integer *, integer *, real *, real *, integer *, real *, real *, 
	    integer *);
    extern doublereal sopbl3_(char *, integer *, integer *, integer *)
	    ;
    static integer ic, ik, im, in;
    extern /* Subroutine */ int ssyr2k_(char *, char *, integer *, integer *, 
	    real *, real *, integer *, real *, integer *, real *, real *, 
	    integer *), atimck_(integer *, char *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal second_(void);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    static char transa[1], transb[1];
    extern doublereal smflop_(real *, real *, integer *);
    static real untime;
    extern /* Subroutine */ int stimmg_(integer *, integer *, integer *, real 
	    *, integer *, integer *, integer *);
    static logical timsub[6];
    extern /* Subroutine */ int sprtbl_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    static integer lda, icl, ita, itb;
    static real ops;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___14 = { 0, 0, 0, fmt_9996, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9995, 0 };
    static cilist io___35 = { 0, 0, 0, fmt_9994, 0 };
    static cilist io___41 = { 0, 0, 0, fmt_9993, 0 };
    static cilist io___42 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___43 = { 0, 0, 0, fmt_9992, 0 };
    static cilist io___44 = { 0, 0, 0, fmt_9991, 0 };
    static cilist io___45 = { 0, 0, 0, fmt_9991, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_9990, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STIMB3 times the Level 3 BLAS routines.   

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
            The values of the matrix row dimension M.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix column dimension N.   

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of K.  K is used as the intermediate matrix   
            dimension for SGEMM (the product of an M x K matrix and a   
            K x N matrix) and as the dimension of the rank-K update in   
            SSYRK and SSYR2K.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) REAL   
            The minimum time a subroutine will be timed.   

    A       (workspace) REAL array, dimension (LDAMAX*NMAX)   
               where LDAMAX and NMAX are the maximum values permitted   
               for LDA and N.   

    B       (workspace) REAL array, dimension (LDAMAX*NMAX)   

    C       (workspace) REAL array, dimension (LDAMAX*NMAX)   

    RESLTS  (output) REAL array, dimension (LDR1,LDR2,NLDA)   
            The timing results for each subroutine over the relevant   
            values of M, N, K, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(1,NM,NK).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --mval;
    --nval;
    --kval;
    --ldaval;
    --a;
    --b;
    --c;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_offset = reslts_dim1 * (reslts_dim2 + 1) + 1;
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Single precision", 1L, 16L);
    s_copy(path + 1, "B3", 2L, 2L);
    atimin_(path, line, &c__6, names, timsub, nout, &info, 3L, line_len, 6L);
    if (info != 0) {
	goto L480;
    }

/*     Check that M <= LDA. */

    s_copy(cname, line, 6L, 6L);
    atimck_(&c__1, cname, nm, &mval[1], nlda, &ldaval[1], nout, &info, 6L);
    if (info > 0) {
	io___9.ciunit = *nout;
	s_wsfe(&io___9);
	do_fio(&c__1, cname, 6L);
	e_wsfe();
	goto L480;
    }

/*     Time each routine. */

    for (isub = 1; isub <= 6; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L470;
	}

/*        Print header. */

	s_copy(cname, names + (isub - 1) * 6, 6L, 6L);
	io___11.ciunit = *nout;
	s_wsfe(&io___11);
	do_fio(&c__1, cname, 6L);
	e_wsfe();
	if (*nlda == 1) {
	    io___12.ciunit = *nout;
	    s_wsfe(&io___12);
	    do_fio(&c__1, (char *)&ldaval[1], (ftnlen)sizeof(integer));
	    e_wsfe();
	} else {
	    i__1 = *nlda;
	    for (i = 1; i <= i__1; ++i) {
		io___14.ciunit = *nout;
		s_wsfe(&io___14);
		do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i], (ftnlen)sizeof(integer));
		e_wsfe();
/* L10: */
	    }
	}

/*        Time SGEMM */

	if (s_cmp(cname, "SGEMM ", 6L, 6L) == 0) {
	    for (ita = 1; ita <= 2; ++ita) {
		*(unsigned char *)transa = *(unsigned char *)&trans[ita - 1];
		for (itb = 1; itb <= 2; ++itb) {
		    *(unsigned char *)transb = *(unsigned char *)&trans[itb - 
			    1];
		    i__1 = *nk;
		    for (ik = 1; ik <= i__1; ++ik) {
			k = kval[ik];
			i__2 = *nlda;
			for (ilda = 1; ilda <= i__2; ++ilda) {
			    lda = ldaval[ilda];
			    i__3 = *nm;
			    for (im = 1; im <= i__3; ++im) {
				m = mval[im];
				i__4 = *nn;
				for (in = 1; in <= i__4; ++in) {
				    n = nval[in];
				    if (*(unsigned char *)transa == 'N') {
					stimmg_(&c__1, &m, &k, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					stimmg_(&c__1, &k, &m, &a[1], &lda, &
						c__0, &c__0);
				    }
				    if (*(unsigned char *)transb == 'N') {
					stimmg_(&c__0, &k, &n, &b[1], &lda, &
						c__0, &c__0);
				    } else {
					stimmg_(&c__0, &n, &k, &b[1], &lda, &
						c__0, &c__0);
				    }
				    stimmg_(&c__1, &m, &n, &c[1], &lda, &c__0,
					     &c__0);
				    ic = 0;
				    s1 = second_();
L20:
				    sgemm_(transa, transb, &m, &n, &k, &c_b47,
					     &a[1], &lda, &b[1], &lda, &c_b47,
					     &c[1], &lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					stimmg_(&c__1, &m, &n, &c[1], &lda, &
						c__0, &c__0);
					goto L20;
				    }

/*                             Subtract th
e time used in STIMMG. */

				    icl = 1;
				    s1 = second_();
L30:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					stimmg_(&c__1, &m, &n, &c[1], &lda, &
						c__0, &c__0);
					goto L30;
				    }

				    time = (time - untime) / (real) ic;
				    ops = sopbl3_(cname, &m, &n, &k);
				    reslts[im + (in + ilda * reslts_dim2) * 
					    reslts_dim1] = smflop_(&ops, &
					    time, &c__0);
/* L40: */
				}
/* L50: */
			    }
/* L60: */
			}
			if (ik == 1) {
			    io___34.ciunit = *nout;
			    s_wsfe(&io___34);
			    do_fio(&c__1, transa, 1L);
			    do_fio(&c__1, transb, 1L);
			    e_wsfe();
			}
			io___35.ciunit = *nout;
			s_wsfe(&io___35);
			do_fio(&c__1, (char *)&kval[ik], (ftnlen)sizeof(
				integer));
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, 1L, 
				1L);
/* L70: */
		    }
/* L80: */
		}
/* L90: */
	    }

	} else if (s_cmp(cname, "SSYMM ", 6L, 6L) == 0) {

/*           Time SSYMM */

	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 6;
		    } else {
			imat = -6;
		    }
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *nm;
			for (im = 1; im <= i__2; ++im) {
			    m = mval[im];
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				if (iside == 1) {
				    stimmg_(&imat, &m, &m, &a[1], &lda, &c__0,
					     &c__0);
				    stimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				} else {
				    stimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    stimmg_(&imat, &n, &n, &a[1], &lda, &c__0,
					     &c__0);
				}
				stimmg_(&c__1, &m, &n, &c[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L100:
				ssymm_(side, uplo, &m, &n, &c_b47, &a[1], &
					lda, &b[1], &lda, &c_b47, &c[1], &lda);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    stimmg_(&c__1, &m, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L100;
				}

/*                          Subtract the time 
used in STIMMG. */

				icl = 1;
				s1 = second_();
L110:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    stimmg_(&c__1, &m, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L110;
				}

				time = (time - untime) / (real) ic;
				i__4 = iside - 1;
				ops = sopbl3_(cname, &m, &n, &i__4);
				reslts[im + (in + ilda * reslts_dim2) * 
					reslts_dim1] = smflop_(&ops, &time, &
					c__0);
/* L120: */
			    }
/* L130: */
			}
/* L140: */
		    }
		    io___41.ciunit = *nout;
		    s_wsfe(&io___41);
		    do_fio(&c__1, side, 1L);
		    do_fio(&c__1, uplo, 1L);
		    e_wsfe();
		    sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
			    reslts[reslts_offset], ldr1, ldr2, nout, 1L, 1L);
/* L150: */
		}
/* L160: */
	    }

	} else if (s_cmp(cname, "SSYRK ", 6L, 6L) == 0) {

/*           Time SSYRK */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 6;
		} else {
		    imat = -6;
		}
		for (ita = 1; ita <= 2; ++ita) {
		    *(unsigned char *)transa = *(unsigned char *)&trans[ita - 
			    1];
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *nk;
			for (ik = 1; ik <= i__2; ++ik) {
			    k = kval[ik];
			    if (*(unsigned char *)transa == 'N') {
				stimmg_(&c__1, &n, &k, &a[1], &lda, &c__0, &
					c__0);
			    } else {
				stimmg_(&c__1, &k, &n, &a[1], &lda, &c__0, &
					c__0);
			    }
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				stimmg_(&imat, &n, &n, &c[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L170:
				ssyrk_(uplo, transa, &n, &k, &c_b47, &a[1], &
					lda, &c_b47, &c[1], &lda);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    stimmg_(&imat, &n, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L170;
				}

/*                          Subtract the time 
used in STIMMG. */

				icl = 1;
				s1 = second_();
L180:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    stimmg_(&imat, &n, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L180;
				}

				time = (time - untime) / (real) ic;
				ops = sopbl3_(cname, &n, &n, &k);
				reslts[ik + (in + ilda * reslts_dim2) * 
					reslts_dim1] = smflop_(&ops, &time, &
					c__0);
/* L190: */
			    }
/* L200: */
			}
/* L210: */
		    }
		    io___42.ciunit = *nout;
		    s_wsfe(&io___42);
		    do_fio(&c__1, cname, 6L);
		    do_fio(&c__1, uplo, 1L);
		    do_fio(&c__1, transa, 1L);
		    e_wsfe();
		    sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
			    reslts[reslts_offset], ldr1, ldr2, nout, 1L, 1L);
/* L220: */
		}
/* L230: */
	    }

	} else if (s_cmp(cname, "SSYR2K", 6L, 6L) == 0) {

/*           Time SSYR2K */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
		if (lsame_(uplo, "U")) {
		    imat = 6;
		} else {
		    imat = -6;
		}
		for (itb = 1; itb <= 2; ++itb) {
		    *(unsigned char *)transb = *(unsigned char *)&trans[itb - 
			    1];
		    i__1 = *nlda;
		    for (ilda = 1; ilda <= i__1; ++ilda) {
			lda = ldaval[ilda];
			i__2 = *nk;
			for (ik = 1; ik <= i__2; ++ik) {
			    k = kval[ik];
			    if (*(unsigned char *)transb == 'N') {
				stimmg_(&c__1, &n, &k, &a[1], &lda, &c__0, &
					c__0);
				stimmg_(&c__0, &n, &k, &b[1], &lda, &c__0, &
					c__0);
			    } else {
				stimmg_(&c__1, &k, &n, &a[1], &lda, &c__0, &
					c__0);
				stimmg_(&c__0, &k, &n, &b[1], &lda, &c__0, &
					c__0);
			    }
			    i__3 = *nn;
			    for (in = 1; in <= i__3; ++in) {
				n = nval[in];
				stimmg_(&imat, &n, &n, &c[1], &lda, &c__0, &
					c__0);
				ic = 0;
				s1 = second_();
L240:
				ssyr2k_(uplo, transb, &n, &k, &c_b47, &a[1], &
					lda, &b[1], &lda, &c_b47, &c[1], &lda);
				s2 = second_();
				time = s2 - s1;
				++ic;
				if (time < *timmin) {
				    stimmg_(&imat, &n, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L240;
				}

/*                          Subtract the time 
used in STIMMG. */

				icl = 1;
				s1 = second_();
L250:
				s2 = second_();
				untime = s2 - s1;
				++icl;
				if (icl <= ic) {
				    stimmg_(&imat, &n, &n, &c[1], &lda, &c__0,
					     &c__0);
				    goto L250;
				}

				time = (time - untime) / (real) ic;
				ops = sopbl3_(cname, &n, &n, &k);
				reslts[ik + (in + ilda * reslts_dim2) * 
					reslts_dim1] = smflop_(&ops, &time, &
					c__0);
/* L260: */
			    }
/* L270: */
			}
/* L280: */
		    }
		    io___43.ciunit = *nout;
		    s_wsfe(&io___43);
		    do_fio(&c__1, cname, 6L);
		    do_fio(&c__1, uplo, 1L);
		    do_fio(&c__1, transb, 1L);
		    e_wsfe();
		    sprtbl_("K", "N", nk, &kval[1], nn, &nval[1], nlda, &
			    reslts[reslts_offset], ldr1, ldr2, nout, 1L, 1L);
/* L290: */
		}
/* L300: */
	    }

	} else if (s_cmp(cname, "STRMM ", 6L, 6L) == 0) {

/*           Time STRMM */

	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 9;
		    } else {
			imat = -9;
		    }
		    for (ita = 1; ita <= 2; ++ita) {
			*(unsigned char *)transa = *(unsigned char *)&trans[
				ita - 1];
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nm;
			    for (im = 1; im <= i__2; ++im) {
				m = mval[im];
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    if (iside == 1) {
					stimmg_(&imat, &m, &m, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					stimmg_(&imat, &n, &n, &a[1], &lda, &
						c__0, &c__0);
				    }
				    stimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ic = 0;
				    s1 = second_();
L310:
				    strmm_(side, uplo, transa, "Non-unit", &m,
					     &n, &c_b47, &a[1], &lda, &b[1], &
					    lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					stimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L310;
				    }

/*                             Subtract th
e time used in STIMMG. */

				    icl = 1;
				    s1 = second_();
L320:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					stimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L320;
				    }

				    time = (time - untime) / (real) ic;
				    i__4 = iside - 1;
				    ops = sopbl3_(cname, &m, &n, &i__4);
				    reslts[im + (in + ilda * reslts_dim2) * 
					    reslts_dim1] = smflop_(&ops, &
					    time, &c__0);
/* L330: */
				}
/* L340: */
			    }
/* L350: */
			}
			io___44.ciunit = *nout;
			s_wsfe(&io___44);
			do_fio(&c__1, cname, 6L);
			do_fio(&c__1, side, 1L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, transa, 1L);
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, 1L, 
				1L);
/* L360: */
		    }
/* L370: */
		}
/* L380: */
	    }

	} else if (s_cmp(cname, "STRSM ", 6L, 6L) == 0) {

/*           Time STRSM */

	    for (iside = 1; iside <= 2; ++iside) {
		*(unsigned char *)side = *(unsigned char *)&sides[iside - 1];
		for (iuplo = 1; iuplo <= 2; ++iuplo) {
		    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 
			    1];
		    if (lsame_(uplo, "U")) {
			imat = 9;
		    } else {
			imat = -9;
		    }
		    for (ita = 1; ita <= 2; ++ita) {
			*(unsigned char *)transa = *(unsigned char *)&trans[
				ita - 1];
			i__1 = *nlda;
			for (ilda = 1; ilda <= i__1; ++ilda) {
			    lda = ldaval[ilda];
			    i__2 = *nm;
			    for (im = 1; im <= i__2; ++im) {
				m = mval[im];
				i__3 = *nn;
				for (in = 1; in <= i__3; ++in) {
				    n = nval[in];
				    if (iside == 1) {
					stimmg_(&imat, &m, &m, &a[1], &lda, &
						c__0, &c__0);
				    } else {
					stimmg_(&imat, &n, &n, &a[1], &lda, &
						c__0, &c__0);
				    }
				    stimmg_(&c__0, &m, &n, &b[1], &lda, &c__0,
					     &c__0);
				    ic = 0;
				    s1 = second_();
L390:
				    strsm_(side, uplo, transa, "Non-unit", &m,
					     &n, &c_b47, &a[1], &lda, &b[1], &
					    lda);
				    s2 = second_();
				    time = s2 - s1;
				    ++ic;
				    if (time < *timmin) {
					stimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L390;
				    }

/*                             Subtract th
e time used in STIMMG. */

				    icl = 1;
				    s1 = second_();
L400:
				    s2 = second_();
				    untime = s2 - s1;
				    ++icl;
				    if (icl <= ic) {
					stimmg_(&c__0, &m, &n, &b[1], &lda, &
						c__0, &c__0);
					goto L400;
				    }

				    time = (time - untime) / (real) ic;
				    i__4 = iside - 1;
				    ops = sopbl3_(cname, &m, &n, &i__4);
				    reslts[im + (in + ilda * reslts_dim2) * 
					    reslts_dim1] = smflop_(&ops, &
					    time, &c__0);
/* L410: */
				}
/* L420: */
			    }
/* L430: */
			}
			io___45.ciunit = *nout;
			s_wsfe(&io___45);
			do_fio(&c__1, cname, 6L);
			do_fio(&c__1, side, 1L);
			do_fio(&c__1, uplo, 1L);
			do_fio(&c__1, transa, 1L);
			e_wsfe();
			sprtbl_("M", "N", nm, &mval[1], nn, &nval[1], nlda, &
				reslts[reslts_offset], ldr1, ldr2, nout, 1L, 
				1L);
/* L440: */
		    }
/* L450: */
		}
/* L460: */
	    }
	}
	io___46.ciunit = *nout;
	s_wsfe(&io___46);
	e_wsfe();
L470:
	;
    }
L480:

    return 0;

/*     End of STIMB3 */

} /* stimb3_ */

