/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__0 = 0;
static integer c__1 = 1;

/* Subroutine */ int dtimpb_(char *line, integer *nn, integer *nval, integer *
	nk, integer *kval, integer *nns, integer *nsval, integer *nnb, 
	integer *nbval, integer *nlda, integer *ldaval, doublereal *timmin, 
	doublereal *a, doublereal *b, integer *iwork, doublereal *reslts, 
	integer *ldr1, integer *ldr2, integer *ldr3, integer *nout, ftnlen 
	line_len)
{
    /* Initialized data */

    static char uplos[1*2] = "U" "L";
    static char subnam[6*2] = "DPBTRF" "DPBTRS";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,a6,\002 with M =\002,i6,\002, UPLO = '\002"
	    ",a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6, i__7;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_cmp(char *, char *, ftnlen, ftnlen), s_wsle(cilist *), e_wsle(
	    void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static doublereal time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i, k, n;
    static char cname[6];
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern logical lsame_(char *, char *);
    static integer iuplo, i3;
    static doublereal s1, s2;
    static integer ic, nb, ik, in;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dpbtrf_(
	    char *, integer *, integer *, doublereal *, integer *, integer *), dtimmg_(integer *, integer *, integer *, doublereal *, 
	    integer *, integer *, integer *), dprtbl_(char *, char *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    integer *, integer *, integer *, ftnlen, ftnlen), dpbtrs_(char *, 
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, integer *, integer *), xlaenv_(integer *, 
	    integer *);
    static doublereal untime;
    static logical timsub[2];
    static integer lda, ldb, icl, inb, mat;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___33 = { 0, 0, 0, 0, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_9996, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DTIMPB times DPBTRF and -TRS.   

    Arguments   
    =========   

    LINE    (input) CHARACTER*80   
            The input line that requested this routine.  The first six   
            characters contain either the name of a subroutine or a   
            generic path name.  The remaining characters may be used to   
            specify the individual routines to be timed.  See ATIMIN for 
  
            a full description of the format of the input line.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix size N.   

    NK      (input) INTEGER   
            The number of values of K contained in the vector KVAL.   

    KVAL    (input) INTEGER array, dimension (NK)   
            The values of the band width K.   

    NNS     (input) INTEGER   
            The number of values of NRHS contained in the vector NSVAL.   

    NSVAL   (input) INTEGER array, dimension (NNS)   
            The values of the number of right hand sides NRHS.   

    NNB     (input) INTEGER   
            The number of values of NB contained in the vector NBVAL.   

    NBVAL   (input) INTEGER array, dimension (NNB)   
            The values of the blocksize NB.   

    NLDA    (input) INTEGER   
            The number of values of LDA contained in the vector LDAVAL.   

    LDAVAL  (input) INTEGER array, dimension (NLDA)   
            The values of the leading dimension of the array A.   

    TIMMIN  (input) DOUBLE PRECISION   
            The minimum time a subroutine will be timed.   

    A       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values permitted   
            for LDA and N.   

    B       (workspace) DOUBLE PRECISION array, dimension (LDAMAX*NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    RESLTS  (output) DOUBLE PRECISION array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N, K, NB, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(4,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NK).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --nval;
    --kval;
    --nsval;
    --nbval;
    --ldaval;
    --a;
    --b;
    --iwork;
    reslts_dim1 = *ldr1;
    reslts_dim2 = *ldr2;
    reslts_dim3 = *ldr3;
    reslts_offset = reslts_dim1 * (reslts_dim2 * (reslts_dim3 + 1) + 1) + 1;
    reslts -= reslts_offset;

    /* Function Body   

       Extract the timing request from the input line. */

    s_copy(path, "Double precision", 1L, 16L);
    s_copy(path + 1, "PB", 2L, 2L);
    atimin_(path, line, &c__2, subnam, timsub, nout, &info, 3L, 80L, 6L);
    if (info != 0) {
	goto L140;
    }

/*     Check that K+1 <= LDA for the input values. */

    s_copy(cname, line, 6L, 6L);
    atimck_(&c__0, cname, nk, &kval[1], nlda, &ldaval[1], nout, &info, 6L);
    if (info > 0) {
	io___7.ciunit = *nout;
	s_wsfe(&io___7);
	do_fio(&c__1, cname, 6L);
	e_wsfe();
	goto L140;
    }

/*     Do for each value of the matrix size N: */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];

/*        Do first for UPLO = 'U', then for UPLO = 'L' */

	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    *(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	    if (lsame_(uplo, "U")) {
		mat = 5;
	    } else {
		mat = -5;
	    }

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];
		i3 = (iuplo - 1) * *nlda + ilda;

/*              Do for each value of the band width K: */

		i__3 = *nk;
		for (ik = 1; ik <= i__3; ++ik) {
		    k = kval[ik];
/* Computing MAX   
   Computing MIN */
		    i__6 = k, i__7 = n - 1;
		    i__4 = 0, i__5 = min(i__6,i__7);
		    k = max(i__4,i__5);

/*                 Time DPBTRF */

		    if (timsub[0]) {

/*                    Do for each value of NB in NBVAL
.  Only DPBTRF is   
                      timed in this loop since the oth
er routines are   
                      independent of NB. */

			i__4 = *nnb;
			for (inb = 1; inb <= i__4; ++inb) {
			    nb = nbval[inb];
			    xlaenv_(&c__1, &nb);
			    dtimmg_(&mat, &n, &n, &a[1], &lda, &k, &k);
			    ic = 0;
			    s1 = dsecnd_();
L10:
			    dpbtrf_(uplo, &n, &k, &a[1], &lda, &info);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&mat, &n, &n, &a[1], &lda, &k, &k);
				goto L10;
			    }

/*                       Subtract the time used in
 DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L20:
			    dtimmg_(&mat, &n, &n, &a[1], &lda, &k, &k);
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				goto L20;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopla_("DPBTRF", &n, &n, &k, &k, &nb);
			    reslts[inb + (ik + (i3 + reslts_dim3) * 
				    reslts_dim2) * reslts_dim1] = dmflop_(&
				    ops, &time, &info);
/* L30: */
			}
		    } else {
			ic = 0;
			dtimmg_(&mat, &n, &n, &a[1], &lda, &k, &k);
		    }

/*                 Generate another matrix and factor it u
sing DPBTRF so   
                   that the factored form can be used in t
iming the other   
                   routines. */

		    nb = 1;
		    xlaenv_(&c__1, &nb);
		    if (ic != 1) {
			dpbtrf_(uplo, &n, &k, &a[1], &lda, &info);
		    }

/*                 Time DPBTRS */

		    if (timsub[1]) {
			i__4 = *nns;
			for (i = 1; i <= i__4; ++i) {
			    nrhs = nsval[i];
			    ldb = n;
			    dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    ic = 0;
			    s1 = dsecnd_();
L40:
			    dpbtrs_(uplo, &n, &k, &nrhs, &a[1], &lda, &b[1], &
				    ldb, &info);
			    s2 = dsecnd_();
			    time = s2 - s1;
			    ++ic;
			    if (time < *timmin) {
				dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, 
					&c__0);
				goto L40;
			    }

/*                       Subtract the time used in
 DTIMMG. */

			    icl = 1;
			    s1 = dsecnd_();
L50:
			    s2 = dsecnd_();
			    untime = s2 - s1;
			    ++icl;
			    if (icl <= ic) {
				dtimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, 
					&c__0);
				goto L50;
			    }

			    time = (time - untime) / (doublereal) ic;
			    ops = dopla_("DPBTRS", &n, &nrhs, &k, &k, &c__0);
			    reslts[i + (ik + (i3 + (reslts_dim3 << 1)) * 
				    reslts_dim2) * reslts_dim1] = dmflop_(&
				    ops, &time, &info);
/* L60: */
			}
		    }
/* L70: */
		}
/* L80: */
	    }
/* L90: */
	}

/*        Print tables of results for each timed routine. */

	for (isub = 1; isub <= 2; ++isub) {
	    if (! timsub[isub - 1]) {
		goto L120;
	    }

/*           Print header for routine names. */

	    if (in == 1 || s_cmp(cname, "DPB   ", 6L, 6L) == 0) {
		io___31.ciunit = *nout;
		s_wsfe(&io___31);
		do_fio(&c__1, subnam + (isub - 1) * 6, 6L);
		e_wsfe();
		if (*nlda > 1) {
		    i__2 = *nlda;
		    for (i = 1; i <= i__2; ++i) {
			io___32.ciunit = *nout;
			s_wsfe(&io___32);
			do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
			do_fio(&c__1, (char *)&ldaval[i], (ftnlen)sizeof(
				integer));
			e_wsfe();
/* L100: */
		    }
		}
	    }
	    io___33.ciunit = *nout;
	    s_wsle(&io___33);
	    e_wsle();
	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		io___34.ciunit = *nout;
		s_wsfe(&io___34);
		do_fio(&c__1, subnam + (isub - 1) * 6, 6L);
		do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
		do_fio(&c__1, uplos + (iuplo - 1), 1L);
		e_wsfe();
		i3 = (iuplo - 1) * *nlda + 1;
		if (isub == 1) {
		    dprtbl_("NB", "K", nnb, &nbval[1], nk, &kval[1], nlda, &
			    reslts[((i3 + reslts_dim3) * reslts_dim2 + 1) * 
			    reslts_dim1 + 1], ldr1, ldr2, nout, 2L, 1L);
		} else if (isub == 2) {
		    dprtbl_("NRHS", "K", nns, &nsval[1], nk, &kval[1], nlda, &
			    reslts[((i3 + (reslts_dim3 << 1)) * reslts_dim2 + 
			    1) * reslts_dim1 + 1], ldr1, ldr2, nout, 4L, 1L);
		}
/* L110: */
	    }
L120:
	    ;
	}
/* L130: */
    }

L140:
    return 0;

/*     End of DTIMPB */

} /* dtimpb_ */

