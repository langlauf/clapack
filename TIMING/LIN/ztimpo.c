/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int ztimpo_(char *line, integer *nn, integer *nval, integer *
	nns, integer *nsval, integer *nnb, integer *nbval, integer *nlda, 
	integer *ldaval, doublereal *timmin, doublecomplex *a, doublecomplex *
	b, integer *iwork, doublereal *reslts, integer *ldr1, integer *ldr2, 
	integer *ldr3, integer *nout, ftnlen line_len)
{
    /* Initialized data */

    static char uplos[1*2] = "U" "L";
    static char subnam[6*3] = "ZPOTRF" "ZPOTRS" "ZPOTRI";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002 timing run not attempted\002,/)";
    static char fmt_9998[] = "(/\002 *** Speed of \002,a6,\002 in megaflops "
	    "***\002)";
    static char fmt_9997[] = "(5x,\002line \002,i2,\002 with LDA = \002,i5)";
    static char fmt_9996[] = "(5x,a6,\002 with UPLO = '\002,a1,\002'\002,/)";

    /* System generated locals */
    integer reslts_dim1, reslts_dim2, reslts_dim3, reslts_offset, i__1, i__2, 
	    i__3;

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer ilda, info;
    static char path[3];
    static doublereal time;
    static integer isub, nrhs;
    static char uplo[1];
    static integer i, n;
    static char cname[6];
    extern doublereal dopla_(char *, integer *, integer *, integer *, integer 
	    *, integer *);
    extern logical lsame_(char *, char *);
    static integer iuplo, i3;
    static doublereal s1, s2;
    static integer ic, nb, in;
    extern doublereal dsecnd_(void);
    extern /* Subroutine */ int atimck_(integer *, char *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    extern doublereal dmflop_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int atimin_(char *, char *, integer *, char *, 
	    logical *, integer *, integer *, ftnlen, ftnlen, ftnlen), dprtbl_(
	    char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, integer *, ftnlen, 
	    ftnlen), xlaenv_(integer *, integer *);
    static doublereal untime;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static logical timsub[3];
    extern /* Subroutine */ int ztimmg_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, integer *, integer *), zpotrf_(char *,
	     integer *, doublecomplex *, integer *, integer *), 
	    zpotri_(char *, integer *, doublecomplex *, integer *, integer *), zpotrs_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, integer *);
    static integer lda, ldb, icl, inb, mat;
    static doublereal ops;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_9998, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___31 = { 0, 0, 0, 0, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_9996, 0 };



/*  -- LAPACK timing routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    ZTIMPO times ZPOTRF, -TRS, and -TRI.   

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

    A       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   
            where LDAMAX and NMAX are the maximum values permitted   
            for LDA and N.   

    B       (workspace) COMPLEX*16 array, dimension (LDAMAX*NMAX)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    RESLTS  (output) DOUBLE PRECISION array, dimension   
                     (LDR1,LDR2,LDR3,NSUBS)   
            The timing results for each subroutine over the relevant   
            values of N, NB, and LDA.   

    LDR1    (input) INTEGER   
            The first dimension of RESLTS.  LDR1 >= max(4,NNB).   

    LDR2    (input) INTEGER   
            The second dimension of RESLTS.  LDR2 >= max(1,NN).   

    LDR3    (input) INTEGER   
            The third dimension of RESLTS.  LDR3 >= max(1,2*NLDA).   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --nval;
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

    s_copy(path, "Zomplex precision", 1L, 17L);
    s_copy(path + 1, "PO", 2L, 2L);
    atimin_(path, line, &c__3, subnam, timsub, nout, &info, 3L, 80L, 6L);
    if (info != 0) {
	goto L150;
    }

/*     Check that N <= LDA for the input values. */

    s_copy(cname, line, 6L, 6L);
    atimck_(&c__2, cname, nn, &nval[1], nlda, &ldaval[1], nout, &info, 6L);
    if (info > 0) {
	io___7.ciunit = *nout;
	s_wsfe(&io___7);
	do_fio(&c__1, cname, 6L);
	e_wsfe();
	goto L150;
    }

/*     Do first for UPLO = 'U', then for UPLO = 'L' */

    for (iuplo = 1; iuplo <= 2; ++iuplo) {
	*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];
	if (lsame_(uplo, "U")) {
	    mat = 3;
	} else {
	    mat = -3;
	}

/*        Do for each value of N in NVAL. */

	i__1 = *nn;
	for (in = 1; in <= i__1; ++in) {
	    n = nval[in];

/*           Do for each value of LDA: */

	    i__2 = *nlda;
	    for (ilda = 1; ilda <= i__2; ++ilda) {
		lda = ldaval[ilda];
		i3 = (iuplo - 1) * *nlda + ilda;

/*              Do for each value of NB in NBVAL.  Only the bl
ocked   
                routines are timed in this loop since the othe
r routines   
                are independent of NB. */

		i__3 = *nnb;
		for (inb = 1; inb <= i__3; ++inb) {
		    nb = nbval[inb];
		    xlaenv_(&c__1, &nb);

/*                 Time ZPOTRF */

		    if (timsub[0]) {
			ztimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L10:
			zpotrf_(uplo, &n, &a[1], &lda, &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ztimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    goto L10;
			}

/*                    Subtract the time used in ZTIMMG
. */

			icl = 1;
			s1 = dsecnd_();
L20:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ztimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
			    goto L20;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("ZPOTRF", &n, &n, &c__0, &c__0, &nb);
			reslts[inb + (in + (i3 + reslts_dim3) * reslts_dim2) *
				 reslts_dim1] = dmflop_(&ops, &time, &info);

		    } else {
			ic = 0;
			ztimmg_(&mat, &n, &n, &a[1], &lda, &c__0, &c__0);
		    }

/*                 Generate another matrix and factor it u
sing ZPOTRF so   
                   that the factored form can be used in t
iming the other   
                   routines. */

		    if (ic != 1) {
			zpotrf_(uplo, &n, &a[1], &lda, &info);
		    }

/*                 Time ZPOTRI */

		    if (timsub[2]) {
			zlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda);
			ic = 0;
			s1 = dsecnd_();
L30:
			zpotri_(uplo, &n, &b[1], &lda, &info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    zlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda);
			    goto L30;
			}

/*                    Subtract the time used in ZLACPY
. */

			icl = 1;
			s1 = dsecnd_();
L40:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    zlacpy_(uplo, &n, &n, &a[1], &lda, &b[1], &lda);
			    goto L40;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("ZPOTRI", &n, &n, &c__0, &c__0, &nb);
			reslts[inb + (in + (i3 + reslts_dim3 * 3) * 
				reslts_dim2) * reslts_dim1] = dmflop_(&ops, &
				time, &info);
		    }
/* L50: */
		}

/*              Time ZPOTRS */

		if (timsub[1]) {
		    i__3 = *nns;
		    for (i = 1; i <= i__3; ++i) {
			nrhs = nsval[i];
			ldb = lda;
			ztimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &c__0);
			ic = 0;
			s1 = dsecnd_();
L60:
			zpotrs_(uplo, &n, &nrhs, &a[1], &lda, &b[1], &ldb, &
				info);
			s2 = dsecnd_();
			time = s2 - s1;
			++ic;
			if (time < *timmin) {
			    ztimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L60;
			}

/*                    Subtract the time used in ZTIMMG
. */

			icl = 1;
			s1 = dsecnd_();
L70:
			s2 = dsecnd_();
			untime = s2 - s1;
			++icl;
			if (icl <= ic) {
			    ztimmg_(&c__0, &n, &nrhs, &b[1], &ldb, &c__0, &
				    c__0);
			    goto L70;
			}

			time = (time - untime) / (doublereal) ic;
			ops = dopla_("ZPOTRS", &n, &nrhs, &c__0, &c__0, &c__0);
			reslts[i + (in + (i3 + (reslts_dim3 << 1)) * 
				reslts_dim2) * reslts_dim1] = dmflop_(&ops, &
				time, &info);
/* L80: */
		    }
		}
/* L90: */
	    }
/* L100: */
	}
/* L110: */
    }

/*     Print tables of results for each timed routine. */

    for (isub = 1; isub <= 3; ++isub) {
	if (! timsub[isub - 1]) {
	    goto L140;
	}
	io___29.ciunit = *nout;
	s_wsfe(&io___29);
	do_fio(&c__1, subnam + (isub - 1) * 6, 6L);
	e_wsfe();
	if (*nlda > 1) {
	    i__1 = *nlda;
	    for (i = 1; i <= i__1; ++i) {
		io___30.ciunit = *nout;
		s_wsfe(&io___30);
		do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&ldaval[i], (ftnlen)sizeof(integer));
		e_wsfe();
/* L120: */
	    }
	}
	io___31.ciunit = *nout;
	s_wsle(&io___31);
	e_wsle();
	for (iuplo = 1; iuplo <= 2; ++iuplo) {
	    io___32.ciunit = *nout;
	    s_wsfe(&io___32);
	    do_fio(&c__1, subnam + (isub - 1) * 6, 6L);
	    do_fio(&c__1, uplos + (iuplo - 1), 1L);
	    e_wsfe();
	    i3 = (iuplo - 1) * *nlda + 1;
	    if (isub == 1) {
		dprtbl_("NB", "N", nnb, &nbval[1], nn, &nval[1], nlda, &
			reslts[((i3 + reslts_dim3) * reslts_dim2 + 1) * 
			reslts_dim1 + 1], ldr1, ldr2, nout, 2L, 1L);
	    } else if (isub == 2) {
		dprtbl_("NRHS", "N", nns, &nsval[1], nn, &nval[1], nlda, &
			reslts[((i3 + (reslts_dim3 << 1)) * reslts_dim2 + 1) *
			 reslts_dim1 + 1], ldr1, ldr2, nout, 4L, 1L);
	    } else if (isub == 3) {
		dprtbl_("NB", "N", nnb, &nbval[1], nn, &nval[1], nlda, &
			reslts[((i3 + reslts_dim3 * 3) * reslts_dim2 + 1) * 
			reslts_dim1 + 1], ldr1, ldr2, nout, 2L, 1L);
	    }
/* L130: */
	}
L140:
	;
    }

L150:
    return 0;

/*     End of ZTIMPO */

} /* ztimpo_ */

