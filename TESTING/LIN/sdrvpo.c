/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer infot, nunit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__0 = 0;
static integer c_n1 = -1;
static real c_b51 = 0.f;

/* Subroutine */ int sdrvpo_(logical *dotype, integer *nn, integer *nval, 
	integer *nrhs, real *thresh, logical *tsterr, integer *nmax, real *a, 
	real *afac, real *asav, real *b, real *bsav, real *x, real *xact, 
	real *s, real *work, real *rwork, integer *iwork, integer *nout)
{
    /* Initialized data */

    static integer iseedy[4] = { 1988,1989,1990,1991 };
    static char uplos[1*2] = "U" "L";
    static char facts[1*3] = "F" "N" "E";
    static char equeds[1*2] = "N" "Y";

    /* Format strings */
    static char fmt_9999[] = "(1x,a6,\002, UPLO='\002,a1,\002', N =\002,i5"
	    ",\002, type \002,i1,\002, test(\002,i1,\002)=\002,g12.5)";
    static char fmt_9997[] = "(1x,a6,\002, FACT='\002,a1,\002', UPLO='\002,a"
	    "1,\002', N=\002,i5,\002, EQUED='\002,a1,\002', type \002,i1,\002"
	    ", test(\002,i1,\002) =\002,g12.5)";
    static char fmt_9998[] = "(1x,a6,\002, FACT='\002,a1,\002', UPLO='\002,a"
	    "1,\002', N=\002,i5,\002, type \002,i1,\002, test(\002,i1,\002)"
	    "=\002,g12.5)";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3, i__4, i__5[2];
    char ch__1[2];

    /* Builtin functions   
       Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static char fact[1];
    static integer ioff, mode;
    static real amax;
    static char path[3];
    static integer imat, info;
    static char dist[1], uplo[1], type[1];
    static integer nrun, i, k, n, ifact, nfail, iseed[4], nfact;
    extern logical lsame_(char *, char *);
    static char equed[1];
    static integer nbmin;
    static real rcond, roldc, scond;
    static integer nimat;
    extern doublereal sget06_(real *, real *);
    extern /* Subroutine */ int sget04_(integer *, integer *, real *, integer 
	    *, real *, integer *, real *, real *);
    static real anorm;
    static logical equil;
    extern /* Subroutine */ int spot01_(char *, integer *, real *, integer *, 
	    real *, integer *, real *, real *), spot02_(char *, 
	    integer *, integer *, real *, integer *, real *, integer *, real *
	    , integer *, real *, real *);
    static integer iuplo, izero, nerrs, k1;
    extern /* Subroutine */ int spot05_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , real *, real *, real *);
    static logical zerot;
    static char xtype[1];
    extern /* Subroutine */ int sposv_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *, integer *), slatb4_(char *, 
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    real *, integer *, real *, char *), 
	    aladhd_(integer *, char *);
    static integer nb, in, kl;
    extern /* Subroutine */ int alaerh_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static logical prefac;
    static integer ku, nt;
    static real rcondc;
    static logical nofact;
    static integer iequed;
    extern /* Subroutine */ int alasvm_(char *, integer *, integer *, integer 
	    *, integer *);
    static real cndnum, ainvnm;
    extern /* Subroutine */ int slacpy_(char *, integer *, integer *, real *, 
	    integer *, real *, integer *), slarhs_(char *, char *, 
	    char *, char *, integer *, integer *, integer *, integer *, 
	    integer *, real *, integer *, real *, integer *, real *, integer *
	    , integer *, integer *), slaset_(
	    char *, integer *, integer *, real *, real *, real *, integer *), xlaenv_(integer *, integer *), slatms_(integer *, 
	    integer *, char *, integer *, char *, real *, integer *, real *, 
	    real *, integer *, integer *, char *, real *, integer *, real *, 
	    integer *);
    extern doublereal slansy_(char *, char *, integer *, real *, integer *, 
	    real *);
    extern /* Subroutine */ int slaqsy_(char *, integer *, real *, integer *, 
	    real *, real *, real *, char *);
    static real result[6];
    extern /* Subroutine */ int spoequ_(integer *, real *, integer *, real *, 
	    real *, real *, integer *), spotrf_(char *, integer *, real *, 
	    integer *, integer *), spotri_(char *, integer *, real *, 
	    integer *, integer *), serrvx_(char *, integer *),
	     sposvx_(char *, char *, integer *, integer *, real *, integer *, 
	    real *, integer *, char *, real *, real *, integer *, real *, 
	    integer *, real *, real *, real *, real *, integer *, integer *);
    static integer lda;

    /* Fortran I/O blocks */
    static cilist io___48 = { 0, 0, 0, fmt_9999, 0 };
    static cilist io___51 = { 0, 0, 0, fmt_9997, 0 };
    static cilist io___52 = { 0, 0, 0, fmt_9998, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    SDRVPO tests the driver routines SPOSV and -SVX.   

    Arguments   
    =========   

    DOTYPE  (input) LOGICAL array, dimension (NTYPES)   
            The matrix types to be used for testing.  Matrices of type j 
  
            (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =   
            .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.   

    NN      (input) INTEGER   
            The number of values of N contained in the vector NVAL.   

    NVAL    (input) INTEGER array, dimension (NN)   
            The values of the matrix dimension N.   

    NRHS    (input) INTEGER   
            The number of right hand side vectors to be generated for   
            each linear system.   

    THRESH  (input) REAL   
            The threshold value for the test ratios.  A result is   
            included in the output file if RESULT >= THRESH.  To have   
            every test ratio printed, use THRESH = 0.   

    TSTERR  (input) LOGICAL   
            Flag that indicates whether error exits are to be tested.   

    NMAX    (input) INTEGER   
            The maximum value permitted for N, used in dimensioning the   
            work arrays.   

    A       (workspace) REAL array, dimension (NMAX*NMAX)   

    AFAC    (workspace) REAL array, dimension (NMAX*NMAX)   

    ASAV    (workspace) REAL array, dimension (NMAX*NMAX)   

    B       (workspace) REAL array, dimension (NMAX*NRHS)   

    BSAV    (workspace) REAL array, dimension (NMAX*NRHS)   

    X       (workspace) REAL array, dimension (NMAX*NRHS)   

    XACT    (workspace) REAL array, dimension (NMAX*NRHS)   

    S       (workspace) REAL array, dimension (NMAX)   

    WORK    (workspace) REAL array, dimension   
                        (NMAX*max(3,NRHS))   

    RWORK   (workspace) REAL array, dimension (NMAX+2*NRHS)   

    IWORK   (workspace) INTEGER array, dimension (NMAX)   

    NOUT    (input) INTEGER   
            The unit number for output.   

    ===================================================================== 
  

       Parameter adjustments */
    --iwork;
    --rwork;
    --work;
    --s;
    --xact;
    --x;
    --bsav;
    --b;
    --asav;
    --afac;
    --a;
    --nval;
    --dotype;

    /* Function Body   

       Initialize constants and the random number seed. */

    *(unsigned char *)path = 'S';
    s_copy(path + 1, "PO", 2L, 2L);
    nrun = 0;
    nfail = 0;
    nerrs = 0;
    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = iseedy[i - 1];
/* L10: */
    }

/*     Test the error exits */

    if (*tsterr) {
	serrvx_(path, nout);
    }
    infoc_1.infot = 0;

/*     Set the block size and minimum block size for testing. */

    nb = 1;
    nbmin = 2;
    xlaenv_(&c__1, &nb);
    xlaenv_(&c__2, &nbmin);

/*     Do for each value of N in NVAL */

    i__1 = *nn;
    for (in = 1; in <= i__1; ++in) {
	n = nval[in];
	lda = max(n,1);
	*(unsigned char *)xtype = 'N';
	nimat = 9;
	if (n <= 0) {
	    nimat = 1;
	}

	i__2 = nimat;
	for (imat = 1; imat <= i__2; ++imat) {

/*           Do the tests only if DOTYPE( IMAT ) is true. */

	    if (! dotype[imat]) {
		goto L130;
	    }

/*           Skip types 3, 4, or 5 if the matrix size is too small
. */

	    zerot = imat >= 3 && imat <= 5;
	    if (zerot && n < imat - 2) {
		goto L130;
	    }

/*           Do first for UPLO = 'U', then for UPLO = 'L' */

	    for (iuplo = 1; iuplo <= 2; ++iuplo) {
		*(unsigned char *)uplo = *(unsigned char *)&uplos[iuplo - 1];

/*              Set up parameters with SLATB4 and generate a t
est matrix   
                with SLATMS. */

		slatb4_(path, &imat, &n, &n, type, &kl, &ku, &anorm, &mode, &
			cndnum, dist);

		s_copy(srnamc_1.srnamt, "SLATMS", 6L, 6L);
		slatms_(&n, &n, dist, iseed, type, &rwork[1], &mode, &cndnum, 
			&anorm, &kl, &ku, uplo, &a[1], &lda, &work[1], &info);

/*              Check error code from SLATMS. */

		if (info != 0) {
		    alaerh_(path, "SLATMS", &info, &c__0, uplo, &n, &n, &c_n1,
			     &c_n1, &c_n1, &imat, &nfail, &nerrs, nout);
		    goto L120;
		}

/*              For types 3-5, zero one row and column of the 
matrix to   
                test that INFO is returned correctly. */

		if (zerot) {
		    if (imat == 3) {
			izero = 1;
		    } else if (imat == 4) {
			izero = n;
		    } else {
			izero = n / 2 + 1;
		    }
		    ioff = (izero - 1) * lda;

/*                 Set row and column IZERO of A to 0. */

		    if (iuplo == 1) {
			i__3 = izero - 1;
			for (i = 1; i <= i__3; ++i) {
			    a[ioff + i] = 0.f;
/* L20: */
			}
			ioff += izero;
			i__3 = n;
			for (i = izero; i <= i__3; ++i) {
			    a[ioff] = 0.f;
			    ioff += lda;
/* L30: */
			}
		    } else {
			ioff = izero;
			i__3 = izero - 1;
			for (i = 1; i <= i__3; ++i) {
			    a[ioff] = 0.f;
			    ioff += lda;
/* L40: */
			}
			ioff -= izero;
			i__3 = n;
			for (i = izero; i <= i__3; ++i) {
			    a[ioff + i] = 0.f;
/* L50: */
			}
		    }
		} else {
		    izero = 0;
		}

/*              Save a copy of the matrix A in ASAV. */

		slacpy_(uplo, &n, &n, &a[1], &lda, &asav[1], &lda);

		for (iequed = 1; iequed <= 2; ++iequed) {
		    *(unsigned char *)equed = *(unsigned char *)&equeds[
			    iequed - 1];
		    if (iequed == 1) {
			nfact = 3;
		    } else {
			nfact = 1;
		    }

		    i__3 = nfact;
		    for (ifact = 1; ifact <= i__3; ++ifact) {
			*(unsigned char *)fact = *(unsigned char *)&facts[
				ifact - 1];
			prefac = lsame_(fact, "F");
			nofact = lsame_(fact, "N");
			equil = lsame_(fact, "E");

			if (zerot) {
			    if (prefac) {
				goto L100;
			    }
			    rcondc = 0.f;

			} else if (! lsame_(fact, "N")) {

/*                       Compute the condition num
ber for comparison with   
                         the value returned by SPO
SVX (FACT = 'N' reuses   
                         the condition number from
 the previous iteration   
                         with FACT = 'F'). */

			    slacpy_(uplo, &n, &n, &asav[1], &lda, &afac[1], &
				    lda);
			    if (equil || iequed > 1) {

/*                          Compute row and co
lumn scale factors to   
                            equilibrate the ma
trix A. */

				spoequ_(&n, &afac[1], &lda, &s[1], &scond, &
					amax, &info);
				if (info == 0 && n > 0) {
				    if (iequed > 1) {
					scond = 0.f;
				    }

/*                             Equilibrate
 the matrix. */

				    slaqsy_(uplo, &n, &afac[1], &lda, &s[1], &
					    scond, &amax, equed);
				}
			    }

/*                       Save the condition number
 of the   
                         non-equilibrated system f
or use in SGET04. */

			    if (equil) {
				roldc = rcondc;
			    }

/*                       Compute the 1-norm of A. 
*/

			    anorm = slansy_("1", uplo, &n, &afac[1], &lda, &
				    rwork[1]);

/*                       Factor the matrix A. */

			    spotrf_(uplo, &n, &afac[1], &lda, &info);

/*                       Form the inverse of A. */

			    slacpy_(uplo, &n, &n, &afac[1], &lda, &a[1], &lda);
			    spotri_(uplo, &n, &a[1], &lda, &info);

/*                       Compute the 1-norm condit
ion number of A. */

			    ainvnm = slansy_("1", uplo, &n, &a[1], &lda, &
				    rwork[1]);
			    if (anorm <= 0.f || ainvnm <= 0.f) {
				rcondc = 1.f;
			    } else {
				rcondc = 1.f / anorm / ainvnm;
			    }
			}
/* L60:   

                      Restore the matrix A. */

			slacpy_(uplo, &n, &n, &asav[1], &lda, &a[1], &lda)
				;

/*                    Form an exact solution and set t
he right hand side. */

			s_copy(srnamc_1.srnamt, "SLARHS", 6L, 6L);
			slarhs_(path, xtype, uplo, " ", &n, &n, &kl, &ku, 
				nrhs, &a[1], &lda, &xact[1], &lda, &b[1], &
				lda, iseed, &info);
			*(unsigned char *)xtype = 'C';
			slacpy_("Full", &n, nrhs, &b[1], &lda, &bsav[1], &lda);

			if (nofact) {

/*                       --- Test SPOSV  ---   

                         Compute the L*L' or U'*U 
factorization of the   
                         matrix and solve the syst
em. */

			    slacpy_(uplo, &n, &n, &a[1], &lda, &afac[1], &lda);
			    slacpy_("Full", &n, nrhs, &b[1], &lda, &x[1], &
				    lda);

			    s_copy(srnamc_1.srnamt, "SPOSV ", 6L, 6L);
			    sposv_(uplo, &n, nrhs, &afac[1], &lda, &x[1], &
				    lda, &info);

/*                       Check error code from SPO
SV . */

			    if (info != izero) {
				alaerh_(path, "SPOSV ", &info, &izero, uplo, &
					n, &n, &c_n1, &c_n1, nrhs, &imat, &
					nfail, &nerrs, nout);
				goto L80;
			    } else if (info != 0) {
				goto L80;
			    }

/*                       Reconstruct matrix from f
actors and compute   
                         residual. */

			    spot01_(uplo, &n, &a[1], &lda, &afac[1], &lda, &
				    rwork[1], result);

/*                       Compute residual of the c
omputed solution. */

			    slacpy_("Full", &n, nrhs, &b[1], &lda, &work[1], &
				    lda);
			    spot02_(uplo, &n, nrhs, &a[1], &lda, &x[1], &lda, 
				    &work[1], &lda, &rwork[1], &result[1])
				    ;

/*                       Check solution from gener
ated exact solution. */

			    sget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda, &
				    rcondc, &result[2]);
			    nt = 3;

/*                       Print information about t
he tests that did not   
                         pass the threshold. */

			    i__4 = nt;
			    for (k = 1; k <= i__4; ++k) {
				if (result[k - 1] >= *thresh) {
				    if (nfail == 0 && nerrs == 0) {
					aladhd_(nout, path);
				    }
				    io___48.ciunit = *nout;
				    s_wsfe(&io___48);
				    do_fio(&c__1, "SPOSV ", 6L);
				    do_fio(&c__1, uplo, 1L);
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&imat, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(real));
				    e_wsfe();
				    ++nfail;
				}
/* L70: */
			    }
			    nrun += nt;
L80:
			    ;
			}

/*                    --- Test SPOSVX --- */

			if (! prefac) {
			    slaset_(uplo, &n, &n, &c_b51, &c_b51, &afac[1], &
				    lda);
			}
			slaset_("Full", &n, nrhs, &c_b51, &c_b51, &x[1], &lda);
			if (iequed > 1 && n > 0) {

/*                       Equilibrate the matrix if
 FACT='F' and   
                         EQUED='Y'. */

			    slaqsy_(uplo, &n, &a[1], &lda, &s[1], &scond, &
				    amax, equed);
			}

/*                    Solve the system and compute the
 condition number   
                      and error bounds using SPOSVX. 
*/

			s_copy(srnamc_1.srnamt, "SPOSVX", 6L, 6L);
			sposvx_(fact, uplo, &n, nrhs, &a[1], &lda, &afac[1], &
				lda, equed, &s[1], &b[1], &lda, &x[1], &lda, &
				rcond, &rwork[1], &rwork[*nrhs + 1], &work[1],
				 &iwork[1], &info);

/*                    Check the error code from SPOSVX
. */

			if (info != izero) {
/* Writing concatenation */
			    i__5[0] = 1, a__1[0] = fact;
			    i__5[1] = 1, a__1[1] = uplo;
			    s_cat(ch__1, a__1, i__5, &c__2, 2L);
			    alaerh_(path, "SPOSVX", &info, &izero, ch__1, &n, 
				    &n, &c_n1, &c_n1, nrhs, &imat, &nfail, &
				    nerrs, nout);
			    goto L100;
			}

			if (info == 0) {
			    if (! prefac) {

/*                          Reconstruct matrix
 from factors and compute   
                            residual. */

				spot01_(uplo, &n, &a[1], &lda, &afac[1], &lda,
					 &rwork[(*nrhs << 1) + 1], result)
					;
				k1 = 1;
			    } else {
				k1 = 2;
			    }

/*                       Compute residual of the c
omputed solution. */

			    slacpy_("Full", &n, nrhs, &bsav[1], &lda, &work[1]
				    , &lda);
			    spot02_(uplo, &n, nrhs, &asav[1], &lda, &x[1], &
				    lda, &work[1], &lda, &rwork[(*nrhs << 1) 
				    + 1], &result[1]);

/*                       Check solution from gener
ated exact solution. */

			    if (nofact || prefac && lsame_(equed, "N")
				    ) {
				sget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda,
					 &rcondc, &result[2]);
			    } else {
				sget04_(&n, nrhs, &x[1], &lda, &xact[1], &lda,
					 &roldc, &result[2]);
			    }

/*                       Check the error bounds fr
om iterative   
                         refinement. */

			    spot05_(uplo, &n, nrhs, &asav[1], &lda, &b[1], &
				    lda, &x[1], &lda, &xact[1], &lda, &rwork[
				    1], &rwork[*nrhs + 1], &result[3]);
			} else {
			    k1 = 6;
			}

/*                    Compare RCOND from SPOSVX with t
he computed value   
                      in RCONDC. */

			result[5] = sget06_(&rcond, &rcondc);

/*                    Print information about the test
s that did not pass   
                      the threshold. */

			for (k = k1; k <= 6; ++k) {
			    if (result[k - 1] >= *thresh) {
				if (nfail == 0 && nerrs == 0) {
				    aladhd_(nout, path);
				}
				if (prefac) {
				    io___51.ciunit = *nout;
				    s_wsfe(&io___51);
				    do_fio(&c__1, "SPOSVX", 6L);
				    do_fio(&c__1, fact, 1L);
				    do_fio(&c__1, uplo, 1L);
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, equed, 1L);
				    do_fio(&c__1, (char *)&imat, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(real));
				    e_wsfe();
				} else {
				    io___52.ciunit = *nout;
				    s_wsfe(&io___52);
				    do_fio(&c__1, "SPOSVX", 6L);
				    do_fio(&c__1, fact, 1L);
				    do_fio(&c__1, uplo, 1L);
				    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&imat, (ftnlen)
					    sizeof(integer));
				    do_fio(&c__1, (char *)&k, (ftnlen)sizeof(
					    integer));
				    do_fio(&c__1, (char *)&result[k - 1], (
					    ftnlen)sizeof(real));
				    e_wsfe();
				}
				++nfail;
			    }
/* L90: */
			}
			nrun = nrun + 7 - k1;
L100:
			;
		    }
/* L110: */
		}
L120:
		;
	    }
L130:
	    ;
	}
/* L140: */
    }

/*     Print a summary of the results. */

    alasvm_(path, nout, &nfail, &nrun, &nerrs);

    return 0;

/*     End of SDRVPO */

} /* sdrvpo_ */

