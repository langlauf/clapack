/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer nproc, nshift, maxb;
} cenvir_;

#define cenvir_1 cenvir_

struct {
    integer iparms[100];
} claenv_;

#define claenv_1 claenv_

struct {
    integer infot, nunit;
    logical ok, lerr;
} infoc_;

#define infoc_1 infoc_

struct {
    char srnamt[6];
} srnamc_;

#define srnamc_1 srnamc_

struct {
    integer selopt, seldim;
    logical selval[20];
    real selwr[20], selwi[20];
} sslct_;

#define sslct_1 sslct_

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__4 = 4;
static integer c__20 = 20;
static integer c__0 = 0;
static integer c__132 = 132;
static integer c__2 = 2;
static integer c__8 = 8;
static integer c__87781 = 87781;
static integer c__264 = 264;

/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static char intstr[10] = "0123456789";
    static integer ioldsd[4] = { 0,0,0,1 };

    /* Format strings */
    static char fmt_9987[] = "(\002 Tests of the Nonsymmetric Eigenvalue Pro"
	    "blem routines\002)";
    static char fmt_9986[] = "(\002 Tests of the Symmetric Eigenvalue Proble"
	    "m routines\002)";
    static char fmt_9985[] = "(\002 Tests of the Singular Value Decompositio"
	    "n routines\002)";
    static char fmt_9979[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Driver\002,/\002    SGEEV (eigenvalues and eigevectors)"
	    "\002)";
    static char fmt_9978[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Driver\002,/\002    SGEES (Schur form)\002)";
    static char fmt_9977[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Expert\002,\002 Driver\002,/\002    SGEEVX (eigenvalues, e"
	    "igenvectors and\002,\002 condition numbers)\002)";
    static char fmt_9976[] = "(/\002 Tests of the Nonsymmetric Eigenvalue Pr"
	    "oblem Expert\002,\002 Driver\002,/\002    SGEESX (Schur form and"
	    " condition\002,\002 numbers)\002)";
    static char fmt_9975[] = "(/\002 Tests of the Generalized Nonsymmetric E"
	    "igenvalue \002,\002Problem routines\002)";
    static char fmt_9974[] = "(\002 Tests of SSBTRD\002,/\002 (reduction of "
	    "a symmetric band \002,\002matrix to tridiagonal form)\002)";
    static char fmt_9967[] = "(\002 Tests of SGBBRD\002,/\002 (reduction of "
	    "a general band \002,\002matrix to real bidiagonal form)\002)";
    static char fmt_9971[] = "(/\002 Tests of the Generalized Linear Regress"
	    "ion Model \002,\002routines\002)";
    static char fmt_9970[] = "(/\002 Tests of the Generalized QR and RQ rout"
	    "ines\002)";
    static char fmt_9969[] = "(/\002 Tests of the Generalized Singular Valu"
	    "e\002,\002 Decomposition routines\002)";
    static char fmt_9968[] = "(/\002 Tests of the Linear Least Squares routi"
	    "nes\002)";
    static char fmt_9992[] = "(1x,a3,\002:  Unrecognized path name\002)";
    static char fmt_9972[] = "(/\002 LAPACK VERSION 2.0, released September "
	    "30, 1994 \002)";
    static char fmt_9984[] = "(/\002 The following parameter values will be "
	    "used:\002)";
    static char fmt_9989[] = "(\002 Invalid input value: \002,a6,\002=\002,i"
	    "6,\002; must be >=\002,i6)";
    static char fmt_9988[] = "(\002 Invalid input value: \002,a6,\002=\002,i"
	    "6,\002; must be <=\002,i6)";
    static char fmt_9983[] = "(4x,a6,10i6,/10x,10i6)";
    static char fmt_9981[] = "(\002 Relative machine \002,a,\002 is taken to"
	    " be\002,e16.6)";
    static char fmt_9982[] = "(/\002 Routines pass computational tests if te"
	    "st ratio is \002,\002less than\002,f8.2,/)";
    static char fmt_9999[] = "(/\002 Execution not attempted due to input er"
	    "rors\002)";
    static char fmt_9991[] = "(//\002 *** Invalid integer value in column"
	    " \002,i2,\002 of input\002,\002 line:\002,/a79)";
    static char fmt_9990[] = "(//1x,a3,\002 routines were not tested\002)";
    static char fmt_9998[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4,\002, NS =\002,i4,\002, MAXB =\002,i4)";
    static char fmt_9980[] = "(\002 *** Error code from \002,a6,\002 = \002,"
	    "i4)";
    static char fmt_9997[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4)";
    static char fmt_9995[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NX =\002,i4,\002, NRHS =\002,i4)";
    static char fmt_9973[] = "(/1x,71(\002-\002))";
    static char fmt_9996[] = "(//1x,a3,\002:  NB =\002,i4,\002, NBMIN =\002,"
	    "i4,\002, NS =\002,i4,\002, MAXB =\002,i4,\002, NBCOL =\002,i4)";
    static char fmt_9966[] = "(//1x,a3,\002:  NRHS =\002,i4)";
    static char fmt_9994[] = "(//\002 End of tests\002)";
    static char fmt_9993[] = "(\002 Total time used = \002,f12.2,\002 seco"
	    "nds\002,/)";

    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), e_wsfe(
	    void), s_rsle(cilist *), do_lio(integer *, integer *, char *, 
	    ftnlen), e_rsle(void), s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    static char line[80];
    static real taua[132];
    static integer info;
    static char path[3];
    static integer kval[20], lenp, mval[20], nval[20];
    static real taub[132];
    static integer pval[20], itmp, nrhs;
    static real work[87781], a[243936]	/* was [17424][14] */, b[87120]	/* 
	    was [17424][5] */, d[1584]	/* was [132][12] */;
    static integer i, k;
    static logical fatal;
    static real x[660];
    static integer iseed[4], nbcol[20], nbval[20], nbmin[20];
    static char vname[6];
    static integer newsd, nsval[20];
    static char c1[1], c3[3];
    static integer nxval[20], i1, iwork[20064];
    static real s1, s2;
    static integer ic, nk, nn;
    extern /* Subroutine */ int schkbb_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, logical *, integer *, integer *, 
	    real *, integer *, real *, integer *, real *, integer *, real *, 
	    real *, real *, integer *, real *, integer *, real *, integer *, 
	    real *, real *, integer *, real *, integer *), schkbd_(integer *, 
	    integer *, integer *, integer *, logical *, integer *, integer *, 
	    real *, real *, integer *, real *, real *, real *, real *, real *,
	     integer *, real *, real *, real *, integer *, real *, integer *, 
	    real *, real *, real *, integer *, integer *, integer *), schkec_(
	    real *, logical *, integer *, integer *), alareq_(char *, integer 
	    *, logical *, integer *, integer *, integer *), schkbk_(
	    integer *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int schkbl_(integer *, integer *), schkgg_(
	    integer *, integer *, integer *, logical *, integer *, real *, 
	    logical *, real *, integer *, real *, integer *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, integer *, real *,
	     real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, real *, integer *, logical *, real *, integer *), 
	    schkgk_(integer *, integer *);
    extern doublereal second_(void);
    extern /* Subroutine */ int schkgl_(integer *, integer *), schksb_(
	    integer *, integer *, integer *, integer *, integer *, logical *, 
	    integer *, real *, integer *, real *, integer *, real *, real *, 
	    real *, integer *, real *, integer *, real *, integer *);
    extern logical lsamen_(integer *, char *, char *);
    extern /* Subroutine */ int sckglm_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, real *, real *
	    , real *, real *, real *, real *, real *, integer *, integer *, 
	    integer *), serrbd_(char *, integer *), schkhs_(integer *,
	     integer *, integer *, logical *, integer *, real *, integer *, 
	    real *, integer *, real *, real *, real *, real *, integer *, 
	    real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, integer *, integer *, 
	    logical *, real *, integer *), scklse_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, real *, integer *, 
	    real *, real *, real *, real *, real *, real *, real *, integer *,
	     integer *, integer *), sdrvbd_(integer *, integer *, integer *, 
	    integer *, logical *, integer *, real *, real *, integer *, real *
	    , integer *, real *, integer *, real *, real *, real *, real *, 
	    real *, real *, real *, integer *, integer *, integer *), serred_(
	    char *, integer *);
    static integer mxbval[20];
    extern /* Subroutine */ int sckgqr_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, real *, 
	    integer *, real *, real *, real *, real *, real *, real *, real *,
	     real *, real *, real *, real *, real *, real *, integer *, 
	    integer *, integer *), serrgg_(char *, integer *), 
	    sdrvgg_(integer *, integer *, integer *, logical *, integer *, 
	    real *, real *, integer *, real *, integer *, real *, real *, 
	    real *, real *, real *, real *, integer *, real *, real *, real *,
	     real *, real *, real *, real *, real *, real *, real *, integer *
	    , real *, integer *);
    static logical tstdif;
    static real thresh;
    extern /* Subroutine */ int schkst_(integer *, integer *, integer *, 
	    logical *, integer *, real *, integer *, real *, integer *, real *
	    , real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, real *, real *, integer *, real *, real *, real *,
	     real *, real *, integer *, integer *, real *, integer *);
    static logical tstchk;
    static integer nparms;
    extern /* Subroutine */ int sckgsv_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, real *, integer *, real *, real *
	    , real *, real *, real *, real *, real *, real *, real *, real *, 
	    integer *, real *, real *, integer *, integer *, integer *), 
	    xlaenv_(integer *, integer *);
    static logical dotype[30], logwrk[132];
    static real thrshn;
    extern /* Subroutine */ int sdrves_(integer *, integer *, integer *, 
	    logical *, integer *, real *, integer *, real *, integer *, real *
	    , real *, real *, real *, real *, real *, real *, integer *, real 
	    *, real *, integer *, integer *, logical *, integer *), sdrvev_(
	    integer *, integer *, integer *, logical *, integer *, real *, 
	    integer *, real *, integer *, real *, real *, real *, real *, 
	    real *, real *, integer *, real *, integer *, real *, integer *, 
	    real *, real *, integer *, integer *, integer *), sdrvsg_(integer 
	    *, integer *, integer *, logical *, integer *, real *, integer *, 
	    real *, integer *, real *, integer *, real *, real *, integer *, 
	    real *, real *, real *, real *, real *, integer *, integer *, 
	    real *, integer *), serrhs_(char *, integer *);
    static real result[105];
    static integer maxtyp;
    static logical tsterr;
    static integer ntypes;
    static logical tstdrv;
    extern /* Subroutine */ int sdrvst_(integer *, integer *, integer *, 
	    logical *, integer *, real *, integer *, real *, integer *, real *
	    , real *, real *, real *, real *, real *, real *, real *, integer 
	    *, real *, real *, real *, real *, integer *, integer *, real *, 
	    integer *), serrst_(char *, integer *), sdrvsx_(integer *,
	     integer *, integer *, logical *, integer *, real *, integer *, 
	    integer *, real *, integer *, real *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, integer *, real *, real *,
	     real *, integer *, integer *, logical *, integer *), sdrvvx_(
	    integer *, integer *, integer *, logical *, integer *, real *, 
	    integer *, integer *, real *, integer *, real *, real *, real *, 
	    real *, real *, real *, integer *, real *, integer *, real *, 
	    integer *, real *, real *, real *, real *, real *, real *, real *,
	     real *, real *, real *, integer *, integer *, integer *);
    static logical sbb, glm, sbk, sbl, nep, lse, sgg, sep, sgk, gqr, ses, sgl,
	     ssb, sev, svd, gsv;
    static real eps;
    static logical ssx, svx;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 5, 1, "(A80)", 0 };
    static cilist io___26 = { 0, 6, 0, fmt_9987, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_9986, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_9985, 0 };
    static cilist io___29 = { 0, 6, 0, fmt_9979, 0 };
    static cilist io___30 = { 0, 6, 0, fmt_9978, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_9977, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_9976, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_9975, 0 };
    static cilist io___34 = { 0, 6, 0, fmt_9974, 0 };
    static cilist io___35 = { 0, 6, 0, fmt_9967, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_9971, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_9970, 0 };
    static cilist io___38 = { 0, 6, 0, fmt_9969, 0 };
    static cilist io___39 = { 0, 6, 0, fmt_9968, 0 };
    static cilist io___40 = { 0, 5, 0, 0, 0 };
    static cilist io___43 = { 0, 6, 0, fmt_9992, 0 };
    static cilist io___44 = { 0, 6, 0, fmt_9972, 0 };
    static cilist io___45 = { 0, 6, 0, fmt_9984, 0 };
    static cilist io___46 = { 0, 5, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___50 = { 0, 5, 0, 0, 0 };
    static cilist io___54 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___55 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___56 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___57 = { 0, 5, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___60 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___61 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___62 = { 0, 5, 0, 0, 0 };
    static cilist io___64 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___65 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___66 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___67 = { 0, 5, 0, 0, 0 };
    static cilist io___69 = { 0, 5, 0, 0, 0 };
    static cilist io___71 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___72 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___73 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___74 = { 0, 5, 0, 0, 0 };
    static cilist io___80 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___81 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___82 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___83 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___84 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___85 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___86 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___87 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___89 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___90 = { 0, 5, 0, 0, 0 };
    static cilist io___92 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___93 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___94 = { 0, 5, 0, 0, 0 };
    static cilist io___95 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___96 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___97 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___98 = { 0, 5, 0, 0, 0 };
    static cilist io___99 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___100 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___101 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___102 = { 0, 5, 0, 0, 0 };
    static cilist io___103 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___104 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___105 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___106 = { 0, 5, 0, 0, 0 };
    static cilist io___107 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___108 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___109 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___110 = { 0, 5, 0, 0, 0 };
    static cilist io___111 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___112 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___113 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___114 = { 0, 5, 0, 0, 0 };
    static cilist io___116 = { 0, 6, 0, fmt_9989, 0 };
    static cilist io___117 = { 0, 6, 0, fmt_9988, 0 };
    static cilist io___118 = { 0, 6, 0, fmt_9983, 0 };
    static cilist io___119 = { 0, 6, 0, 0, 0 };
    static cilist io___121 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___122 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___123 = { 0, 6, 0, fmt_9981, 0 };
    static cilist io___124 = { 0, 5, 0, 0, 0 };
    static cilist io___125 = { 0, 6, 0, fmt_9982, 0 };
    static cilist io___126 = { 0, 5, 0, 0, 0 };
    static cilist io___128 = { 0, 5, 0, 0, 0 };
    static cilist io___130 = { 0, 5, 0, 0, 0 };
    static cilist io___131 = { 0, 5, 0, 0, 0 };
    static cilist io___133 = { 0, 5, 0, 0, 0 };
    static cilist io___135 = { 0, 6, 0, fmt_9999, 0 };
    static cilist io___136 = { 0, 5, 1, "(A80)", 0 };
    static cilist io___145 = { 0, 6, 0, fmt_9991, 0 };
    static cilist io___146 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___149 = { 0, 6, 0, fmt_9998, 0 };
    static cilist io___157 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___158 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___159 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___160 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___161 = { 0, 6, 0, fmt_9997, 0 };
    static cilist io___162 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___164 = { 0, 6, 0, fmt_9995, 0 };
    static cilist io___165 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___166 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___167 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___168 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___169 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___170 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___171 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___172 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___173 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___174 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___175 = { 0, 6, 0, fmt_9990, 0 };
    static cilist io___176 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___177 = { 0, 6, 0, fmt_9973, 0 };
    static cilist io___178 = { 0, 6, 0, fmt_9996, 0 };
    static cilist io___181 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___182 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___183 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___184 = { 0, 6, 0, fmt_9966, 0 };
    static cilist io___185 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___188 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___191 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___192 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___193 = { 0, 6, 0, fmt_9980, 0 };
    static cilist io___194 = { 0, 6, 0, 0, 0 };
    static cilist io___195 = { 0, 6, 0, 0, 0 };
    static cilist io___196 = { 0, 6, 0, fmt_9992, 0 };
    static cilist io___197 = { 0, 6, 0, fmt_9994, 0 };
    static cilist io___199 = { 0, 6, 0, fmt_9993, 0 };



/*  -- LAPACK test routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   

    Purpose   
    =======   

    SCHKEE tests the REAL LAPACK subroutines for the matrix   
    eigenvalue problem.  The test paths in this version are   

    NEP (Nonsymmetric Eigenvalue Problem):   
        Test SGEHRD, SORGHR, SHSEQR, STREVC, SHSEIN, and SORMHR   

    SEP (Symmetric Eigenvalue Problem):   
        Test SSYTRD, SORGTR, SSTEQR, SSTERF, SSTEIN, SSTEDC,   
        and drivers SSYEV(X), SSBEV(X), SSPEV(X), SSTEV(X),   
                    SSYEVD,   SSBEVD,   SSPEVD,   SSTEVD   

    SVD (Singular Value Decomposition):   
        Test SGEBRD, SORGBR, and SBDSQR   
        and the driver routine SGESVD   

    SEV (Nonsymmetric Eigenvalue/eigenvector Driver):   
        Test SGEEV   

    SES (Nonsymmetric Schur form Driver):   
        Test SGEES   

    SVX (Nonsymmetric Eigenvalue/eigenvector Expert Driver):   
        Test SGEEVX   

    SSX (Nonsymmetric Schur form Expert Driver):   
        Test SGEESX   

    SGG (Generalized Nonsymmetric Eigenvalue Problem):   
        Test SGGHRD, SGGBAL, SGGBAK, SHGEQZ, and STGEVC   
        and the driver routines SGEGS and SGEGV   

    SSG (Symmetric Generalized Eigenvalue Problem):   
        Test SSYGST, SSYGV, SSPGST, SSPGV, SSBGST, and SSBGV   

    SSB (Symmetric Band Eigenvalue Problem):   
        Test SSBTRD   

    SBB (Band Singular Value Decomposition):   
        Test SGBBRD   

    SEC (Eigencondition estimation):   
        Test SLALN2, SLASY2, SLAEQU, SLAEXC, STRSYL, STREXC, STRSNA,   
        STRSEN, and SLAQTR   

    SBL (Balancing a general matrix)   
        Test SGEBAL   

    SBK (Back transformation on a balanced matrix)   
        Test SGEBAK   

    SGL (Balancing a matrix pair)   
        Test SGGBAL   

    SGK (Back transformation on a matrix pair)   
        Test SGGBAK   

    GLM (Generalized Linear Regression Model):   
        Tests SGGGLM   

    GQR (Generalized QR and RQ factorizations):   
        Tests SGGQRF and SGGRQF   

    GSV (Generalized Singular Value Decomposition):   
        Tests SGGSVD, SGGSVP, STGSJA, SLAGS2, SLAPLL, and SLAPMT   

    LSE (Constrained Linear Least Squares):   
        Tests SGGLSE   

    Each test path has a different set of inputs, but the data sets for   
    the driver routines xEV, xES, xVX, and xSX can be concatenated in a   
    single input file.  The first line of input should contain one of the 
  
    3-character path names in columns 1-3.  The number of remaining lines 
  
    depends on what is found on the first line.   

    The number of matrix types used in testing is often controllable from 
  
    the input file.  The number of matrix types for each path, and the   
    test routine that describes them, is as follows:   

    Path name(s)  Types    Test routine   

    SHS or NEP      21     SCHKHS   
    SST or SEP      21     SCHKST (routines)   
                    18     SDRVST (drivers)   
    SBD or SVD      16     SCHKBD (routines)   
                     5     SDRVBD (drivers)   
    SEV             21     SDRVEV   
    SES             21     SDRVES   
    SVX             21     SDRVVX   
    SSX             21     SDRVSX   
    SGG             26     SCHKGG (routines)   
                    26     SDRVGG (drivers)   
    SSG             21     SDRVSG   
    SSB             15     SCHKSB   
    SBB             15     SCHKBB   
    SEC              -     SCHKEC   
    SBL              -     SCHKBL   
    SBK              -     SCHKBK   
    SGL              -     SCHKGL   
    SGK              -     SCHKGK   
    GLM              8     SCKGLM   
    GQR              8     SCKGQR   
    GSV              8     SCKGSV   
    LSE              8     SCKLSE   

   -----------------------------------------------------------------------
   

    NEP input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, NX, NS, and   
             MAXB.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 7:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 8:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of shifts.   

    line 9:  MXBVAL, INTEGER array, dimension (NPARMS)   
             The values for MAXB, used in determining minimum blocksize. 
  

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.  To have all of the test   
             ratios printed, use THRESH = 0.0 .   

    line 11: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 11 was 2:   

    line 12: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 12-EOF:  The remaining lines occur in sets of 1 or 2 and allow 
  
             the user to specify the matrix types.  Each line contains   
             a 3-character path name in columns 1-3, and the number   
             of matrix types must be the first nonblank item in columns   
             4-80.  If the number of matrix types is at least 1 but is   
             less than the maximum number of possible types, a second   
             line will be read to get the numbers of the matrix types to 
  
             be used.  For example,   
    NEP 21   
             requests all of the matrix types for the nonsymmetric   
             eigenvalue problem, while   
    NEP  4   
    9 10 11 12   
             requests only matrices of type 9, 10, 11, and 12.   

             The valid 3-character path names are 'NEP' or 'SHS' for the 
  
             nonsymmetric eigenvalue routines.   

   -----------------------------------------------------------------------
   

    SEP or SSG input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, and NX.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 7:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 8:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 9:  TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines. 
  

    line 10: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines. 
  

    line 11: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 12: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 12 was 2:   

    line 13: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 13-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path names are 'SEP' or 'SST' for the   
             symmetric eigenvalue routines and driver routines, and   
             'SSG' for the routines for the symmetric generalized   
             eigenvalue problem.   

   -----------------------------------------------------------------------
   

    SVD input file:   

    line 2:  NN, INTEGER   
             Number of values of M and N.   

    line 3:  MVAL, INTEGER array, dimension (NN)   
             The values for the matrix row dimension M.   

    line 4:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix column dimension N.   

    line 5:  NPARMS, INTEGER   
             Number of values of the parameter NB, NBMIN, NX, and NRHS.   

    line 6:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 7:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for the minimum blocksize NBMIN.   

    line 8:  NXVAL, INTEGER array, dimension (NPARMS)   
             The values for the crossover point NX.   

    line 9:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of right hand sides NRHS.   

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 11: TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines. 
  

    line 12: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines. 
  

    line 13: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 14: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 14 was 2:   

    line 15: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 15-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path names are 'SVD' or 'SBD' for both the   
             SVD routines and the SVD driver routines.   

   -----------------------------------------------------------------------
   

    SEV and SES data files:   

    line 1:  'SEV' or 'SES' in columns 1 to 3.   

    line 2:  NSIZES, INTEGER   
             Number of sizes of matrices to use. Should be at least 0   
             and at most 20. If NSIZES = 0, no testing is done   
             (although the remaining  3 lines are still read).   

    line 3:  NN, INTEGER array, dimension(NSIZES)   
             Dimensions of matrices to be tested.   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   
             These integer parameters determine how blocking is done   
             (see ILAENV for details)   
             NB     : block size   
             NBMIN  : minimum block size   
             NX     : minimum dimension for blocking   
             NS     : number of shifts in xHSEQR   
             NBCOL  : minimum column dimension for blocking   

    line 5:  THRESH, REAL   
             The test threshold against which computed residuals are   
             compared. Should generally be in the range from 10. to 20.   
             If it is 0., all test case data will be printed.   

    line 6:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits.   

    line 7:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 7 was 2:   

    line 8:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9 and following:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'SEV' to test SGEEV, or   
             'SES' to test SGEES.   

   -----------------------------------------------------------------------
   

    The SVX data has two parts. The first part is identical to SEV,   
    and the second part consists of test matrices with precomputed   
    solutions.   

    line 1:  'SVX' in columns 1-3.   

    line 2:  NSIZES, INTEGER   
             If NSIZES = 0, no testing of randomly generated examples   
             is done, but any precomputed examples are tested.   

    line 3:  NN, INTEGER array, dimension(NSIZES)   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   

    line 5:  THRESH, REAL   

    line 6:  TSTERR, LOGICAL   

    line 7:  NEWSD, INTEGER   

    If line 7 was 2:   

    line 8:  INTEGER array, dimension (4)   

    lines 9 and following: The first line contains 'SVX' in columns 1-3   
             followed by the number of matrix types, possibly with   
             a second line to specify certain matrix types.   
             If the number of matrix types = 0, no testing of randomly   
             generated examples is done, but any precomputed examples   
             are tested.   

    remaining lines : Each matrix is stored on 1+2*N lines, where N is   
             its dimension. The first line contains the dimension (a   
             single integer). The next N lines contain the matrix, one   
             row per line. The last N lines correspond to each   
             eigenvalue. Each of these last N lines contains 4 real   
             values: the real part of the eigenvalue, the imaginary   
             part of the eigenvalue, the reciprocal condition number of   
             the eigenvalues, and the reciprocal condition number of the 
  
             eigenvector.  The end of data is indicated by dimension N=0. 
  
             Even if no data is to be tested, there must be at least one 
  
             line containing N=0.   

   -----------------------------------------------------------------------
   

    The SSX data is like SVX. The first part is identical to SEV, and the 
  
    second part consists of test matrices with precomputed solutions.   

    line 1:  'SSX' in columns 1-3.   

    line 2:  NSIZES, INTEGER   
             If NSIZES = 0, no testing of randomly generated examples   
             is done, but any precomputed examples are tested.   

    line 3:  NN, INTEGER array, dimension(NSIZES)   

    line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs   

    line 5:  THRESH, REAL   

    line 6:  TSTERR, LOGICAL   

    line 7:  NEWSD, INTEGER   

    If line 7 was 2:   

    line 8:  INTEGER array, dimension (4)   

    lines 9 and following: The first line contains 'SSX' in columns 1-3   
             followed by the number of matrix types, possibly with   
             a second line to specify certain matrix types.   
             If the number of matrix types = 0, no testing of randomly   
             generated examples is done, but any precomputed examples   
             are tested.   

    remaining lines : Each matrix is stored on 3+N lines, where N is its 
  
             dimension. The first line contains the dimension N and the   
             dimension M of an invariant subspace. The second line   
             contains M integers, identifying the eigenvalues in the   
             invariant subspace (by their position in a list of   
             eigenvalues ordered by increasing real part). The next N   
             lines contain the matrix. The last line contains the   
             reciprocal condition number for the average of the selected 
  
             eigenvalues, and the reciprocal condition number for the   
             corresponding right invariant subspace. The end of data is   
             indicated by a line containing N=0 and M=0. Even if no data 
  
             is to be tested, there must be at least one line containing 
  
             N=0 and M=0.   

   -----------------------------------------------------------------------
   

    SGG input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NPARMS, INTEGER   
             Number of values of the parameters NB, NBMIN, NS, MAXB, and 
  
             NBCOL.   

    line 5:  NBVAL, INTEGER array, dimension (NPARMS)   
             The values for the blocksize NB.   

    line 6:  NBMIN, INTEGER array, dimension (NPARMS)   
             The values for NBMIN, the minimum row dimension for blocks. 
  

    line 7:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of shifts.   

    line 8:  MXBVAL, INTEGER array, dimension (NPARMS)   
             The values for MAXB, used in determining minimum blocksize. 
  

    line 9:  NBCOL, INTEGER array, dimension (NPARMS)   
             The values for NBCOL, the minimum column dimension for   
             blocks.   

    line 10: THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 11: TSTCHK, LOGICAL   
             Flag indicating whether or not to test the LAPACK routines. 
  

    line 12: TSTDRV, LOGICAL   
             Flag indicating whether or not to test the driver routines. 
  

    line 13: TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 14: NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 14 was 2:   

    line 15: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 15-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'SGG' for the generalized   
             eigenvalue problem routines and driver routines.   

   -----------------------------------------------------------------------
   

    SSB input file:   

    line 2:  NN, INTEGER   
             Number of values of N.   

    line 3:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix dimension N.   

    line 4:  NK, INTEGER   
             Number of values of K.   

    line 5:  KVAL, INTEGER array, dimension (NK)   
             The values for the matrix dimension K.   

    line 6:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 7:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 7 was 2:   

    line 8:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 8-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'SSB'.   

   -----------------------------------------------------------------------
   

    SBB input file:   

    line 2:  NN, INTEGER   
             Number of values of M and N.   

    line 3:  MVAL, INTEGER array, dimension (NN)   
             The values for the matrix row dimension M.   

    line 4:  NVAL, INTEGER array, dimension (NN)   
             The values for the matrix column dimension N.   

    line 4:  NK, INTEGER   
             Number of values of K.   

    line 5:  KVAL, INTEGER array, dimension (NK)   
             The values for the matrix bandwidth K.   

    line 6:  NPARMS, INTEGER   
             Number of values of the parameter NRHS   

    line 7:  NSVAL, INTEGER array, dimension (NPARMS)   
             The values for the number of right hand sides NRHS.   

    line 8:  THRESH   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 9:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 9 was 2:   

    line 10: INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 10-EOF:  Lines specifying matrix types, as for SVD.   
             The 3-character path name is 'SBB'.   

   -----------------------------------------------------------------------
   

    SEC input file:   

    line  2: THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    lines  3-EOF:   

    Input for testing the eigencondition routines consists of a set of   
    specially constructed test cases and their solutions.  The data   
    format is not intended to be modified by the user.   

   -----------------------------------------------------------------------
   

    SBL and SBK input files:   

    line 1:  'SBL' in columns 1-3 to test SGEBAL, or 'SBK' in   
             columns 1-3 to test SGEBAK.   

    The remaining lines consist of specially constructed test cases.   

   -----------------------------------------------------------------------
   

    SGL and SGK input files:   

    line 1:  'SGL' in columns 1-3 to test SGGBAL, or 'SGK' in   
             columns 1-3 to test SGGBAK.   

    The remaining lines consist of specially constructed test cases.   

   -----------------------------------------------------------------------
   

    GLM data file:   

    line 1:  'GLM' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M (row dimension).   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P (row dimension).   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N (column dimension), note M <= N <= M+P.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GLM' for the generalized   
             linear regression model routines.   

   -----------------------------------------------------------------------
   

    GQR data file:   

    line 1:  'GQR' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M.   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P.   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GQR' for the generalized   
             QR and RQ routines.   

   -----------------------------------------------------------------------
   

    GSV data file:   

    line 1:  'GSV' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M (row dimension).   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P (row dimension).   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N (column dimension).   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GSV' for the generalized   
             SVD routines.   

   -----------------------------------------------------------------------
   

    LSE data file:   

    line 1:  'LSE' in columns 1 to 3.   

    line 2:  NN, INTEGER   
             Number of values of M, P, and N.   

    line 3:  MVAL, INTEGER array, dimension(NN)   
             Values of M.   

    line 4:  PVAL, INTEGER array, dimension(NN)   
             Values of P.   

    line 5:  NVAL, INTEGER array, dimension(NN)   
             Values of N, note P <= N <= P+M.   

    line 6:  THRESH, REAL   
             Threshold value for the test ratios.  Information will be   
             printed about each test for which the test ratio is greater 
  
             than or equal to the threshold.   

    line 7:  TSTERR, LOGICAL   
             Flag indicating whether or not to test the error exits for   
             the LAPACK routines and driver routines.   

    line 8:  NEWSD, INTEGER   
             A code indicating how to set the random number seed.   
             = 0:  Set the seed to a default value before each run   
             = 1:  Initialize the seed to a default value only before the 
  
                   first run   
             = 2:  Like 1, but use the seed values on the next line   

    If line 8 was 2:   

    line 9:  INTEGER array, dimension (4)   
             Four integer values for the random number seed.   

    lines 9-EOF:  Lines specifying matrix types, as for NEP.   
             The 3-character path name is 'GSV' for the generalized   
             SVD routines.   

   -----------------------------------------------------------------------
   

    NMAX is currently set to 132 and must be at least 12 for some of the 
  
    precomputed examples, and LWORK = NMAX*(5*NMAX+5)+1 in the parameter 
  
    statements below.  For SVD, we assume NRHS may be as big as N.  The   
    parameter NEED is set to 14 to allow for 14 N-by-N matrices for SGG. 
  

    ===================================================================== 
*/


    s1 = second_();
    fatal = FALSE_;
    infoc_1.nunit = 6;

/*     Return to here to read multiple sets of data */

L10:

/*     Read the first line and set the 3-character test path */

    i__1 = s_rsfe(&io___5);
    if (i__1 != 0) {
	goto L370;
    }
    i__1 = do_fio(&c__1, line, 80L);
    if (i__1 != 0) {
	goto L370;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L370;
    }
    s_copy(path, line, 3L, 3L);
    nep = lsamen_(&c__3, path, "NEP") || lsamen_(&c__3, path, "SHS");
    sep = lsamen_(&c__3, path, "SEP") || lsamen_(&c__3, path, "SST") || lsamen_(&c__3, path, "SSG");
    svd = lsamen_(&c__3, path, "SVD") || lsamen_(&c__3, path, "SBD");
    sev = lsamen_(&c__3, path, "SEV");
    ses = lsamen_(&c__3, path, "SES");
    svx = lsamen_(&c__3, path, "SVX");
    ssx = lsamen_(&c__3, path, "SSX");
    sgg = lsamen_(&c__3, path, "SGG");
    ssb = lsamen_(&c__3, path, "SSB");
    sbb = lsamen_(&c__3, path, "SBB");
    glm = lsamen_(&c__3, path, "GLM");
    gqr = lsamen_(&c__3, path, "GQR") || lsamen_(&c__3, path, "GRQ");
    gsv = lsamen_(&c__3, path, "GSV");
    lse = lsamen_(&c__3, path, "LSE");
    sbl = lsamen_(&c__3, path, "SBL");
    sbk = lsamen_(&c__3, path, "SBK");
    sgl = lsamen_(&c__3, path, "SGL");
    sgk = lsamen_(&c__3, path, "SGK");

/*     Report values of parameters. */

    if (s_cmp(path, "   ", 3L, 3L) == 0) {
	goto L10;
    } else if (nep) {
	s_wsfe(&io___26);
	e_wsfe();
    } else if (sep) {
	s_wsfe(&io___27);
	e_wsfe();
    } else if (svd) {
	s_wsfe(&io___28);
	e_wsfe();
    } else if (sev) {
	s_wsfe(&io___29);
	e_wsfe();
    } else if (ses) {
	s_wsfe(&io___30);
	e_wsfe();
    } else if (svx) {
	s_wsfe(&io___31);
	e_wsfe();
    } else if (ssx) {
	s_wsfe(&io___32);
	e_wsfe();
    } else if (sgg) {
	s_wsfe(&io___33);
	e_wsfe();
    } else if (ssb) {
	s_wsfe(&io___34);
	e_wsfe();
    } else if (sbb) {
	s_wsfe(&io___35);
	e_wsfe();
    } else if (glm) {
	s_wsfe(&io___36);
	e_wsfe();
    } else if (gqr) {
	s_wsfe(&io___37);
	e_wsfe();
    } else if (gsv) {
	s_wsfe(&io___38);
	e_wsfe();
    } else if (lse) {
	s_wsfe(&io___39);
	e_wsfe();
    } else if (sbl) {

/*        SGEBAL:  Balancing */

	schkbl_(&c__5, &c__6);
	goto L10;
    } else if (sbk) {

/*        SGEBAK:  Back transformation */

	schkbk_(&c__5, &c__6);
	goto L10;
    } else if (sgl) {

/*        SGGBAL:  Balancing */

	schkgl_(&c__5, &c__6);
	goto L10;
    } else if (sgk) {

/*        SGGBAK:  Back transformation */

	schkgk_(&c__5, &c__6);
	goto L10;
    } else if (lsamen_(&c__3, path, "SEC")) {

/*        SEC:  Eigencondition estimation */

	s_rsle(&io___40);
	do_lio(&c__4, &c__1, (char *)&thresh, (ftnlen)sizeof(real));
	e_rsle();
	tsterr = TRUE_;
	schkec_(&thresh, &tsterr, &c__5, &c__6);
	goto L10;
    } else {
	s_wsfe(&io___43);
	do_fio(&c__1, path, 3L);
	e_wsfe();
	goto L10;
    }
    s_wsfe(&io___44);
    e_wsfe();
    s_wsfe(&io___45);
    e_wsfe();

/*     Read the number of values of M, P, and N. */

    s_rsle(&io___46);
    do_lio(&c__3, &c__1, (char *)&nn, (ftnlen)sizeof(integer));
    e_rsle();
    if (nn < 1) {
	s_wsfe(&io___48);
	do_fio(&c__1, "   NN ", 6L);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    } else if (nn > 20) {
	s_wsfe(&io___49);
	do_fio(&c__1, "   NN ", 6L);
	do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&c__20, (ftnlen)sizeof(integer));
	e_wsfe();
	nn = 0;
	fatal = TRUE_;
    }

/*     Read the values of M */

    s_rsle(&io___50);
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	do_lio(&c__3, &c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
    }
    e_rsle();
    if (svd) {
	s_copy(vname, "    M ", 6L, 6L);
    } else {
	s_copy(vname, "    N ", 6L, 6L);
    }
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	if (mval[i - 1] < 0) {
	    s_wsfe(&io___54);
	    do_fio(&c__1, vname, 6L);
	    do_fio(&c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (mval[i - 1] > 132) {
	    s_wsfe(&io___55);
	    do_fio(&c__1, vname, 6L);
	    do_fio(&c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
/* L20: */
    }
    s_wsfe(&io___56);
    do_fio(&c__1, "M:    ", 6L);
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&mval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Read the values of P */

    if (glm || gqr || gsv || lse) {
	s_rsle(&io___57);
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    do_lio(&c__3, &c__1, (char *)&pval[i - 1], (ftnlen)sizeof(integer)
		    );
	}
	e_rsle();
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    if (pval[i - 1] < 0) {
		s_wsfe(&io___59);
		do_fio(&c__1, " P  ", 4L);
		do_fio(&c__1, (char *)&pval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (pval[i - 1] > 132) {
		s_wsfe(&io___60);
		do_fio(&c__1, " P  ", 4L);
		do_fio(&c__1, (char *)&pval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L30: */
	}
	s_wsfe(&io___61);
	do_fio(&c__1, "P:    ", 6L);
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&pval[i - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

/*     Read the values of N */

    if (svd || sbb || glm || gqr || gsv || lse) {
	s_rsle(&io___62);
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    do_lio(&c__3, &c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer)
		    );
	}
	e_rsle();
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    if (nval[i - 1] < 0) {
		s_wsfe(&io___64);
		do_fio(&c__1, "    N ", 6L);
		do_fio(&c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (nval[i - 1] > 132) {
		s_wsfe(&io___65);
		do_fio(&c__1, "    N ", 6L);
		do_fio(&c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L40: */
	}
    } else {
	i__1 = nn;
	for (i = 1; i <= i__1; ++i) {
	    nval[i - 1] = mval[i - 1];
/* L50: */
	}
    }
    s_wsfe(&io___66);
    do_fio(&c__1, "N:    ", 6L);
    i__1 = nn;
    for (i = 1; i <= i__1; ++i) {
	do_fio(&c__1, (char *)&nval[i - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();

/*     Read the number of values of K, followed by the values of K */

    if (ssb || sbb) {
	s_rsle(&io___67);
	do_lio(&c__3, &c__1, (char *)&nk, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___69);
	i__1 = nk;
	for (i = 1; i <= i__1; ++i) {
	    do_lio(&c__3, &c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer)
		    );
	}
	e_rsle();
	i__1 = nk;
	for (i = 1; i <= i__1; ++i) {
	    if (kval[i - 1] < 0) {
		s_wsfe(&io___71);
		do_fio(&c__1, "    K ", 6L);
		do_fio(&c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    } else if (kval[i - 1] > 132) {
		s_wsfe(&io___72);
		do_fio(&c__1, "    K ", 6L);
		do_fio(&c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		e_wsfe();
		fatal = TRUE_;
	    }
/* L60: */
	}
	s_wsfe(&io___73);
	do_fio(&c__1, "K:    ", 6L);
	i__1 = nk;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&kval[i - 1], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }

    if (sev || ses || svx || ssx) {

/*        For the nonsymmetric driver routines, only one set of   
          parameters is allowed. */

	s_rsle(&io___74);
	do_lio(&c__3, &c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	e_rsle();
	if (nbval[0] < 1) {
	    s_wsfe(&io___80);
	    do_fio(&c__1, "   NB ", 6L);
	    do_fio(&c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nbmin[0] < 1) {
	    s_wsfe(&io___81);
	    do_fio(&c__1, "NBMIN ", 6L);
	    do_fio(&c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nxval[0] < 1) {
	    s_wsfe(&io___82);
	    do_fio(&c__1, "   NX ", 6L);
	    do_fio(&c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (nsval[0] < 2) {
	    s_wsfe(&io___83);
	    do_fio(&c__1, "   NS ", 6L);
	    do_fio(&c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__2, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	} else if (mxbval[0] < 1) {
	    s_wsfe(&io___84);
	    do_fio(&c__1, " MAXB ", 6L);
	    do_fio(&c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    fatal = TRUE_;
	}
	xlaenv_(&c__1, nbval);
	xlaenv_(&c__2, nbmin);
	xlaenv_(&c__3, nxval);
	xlaenv_(&c__4, nsval);
	xlaenv_(&c__8, mxbval);
	s_wsfe(&io___85);
	do_fio(&c__1, "NB:   ", 6L);
	do_fio(&c__1, (char *)&nbval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___86);
	do_fio(&c__1, "NBMIN:", 6L);
	do_fio(&c__1, (char *)&nbmin[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___87);
	do_fio(&c__1, "NX:   ", 6L);
	do_fio(&c__1, (char *)&nxval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___88);
	do_fio(&c__1, "NS:   ", 6L);
	do_fio(&c__1, (char *)&nsval[0], (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___89);
	do_fio(&c__1, "MAXB: ", 6L);
	do_fio(&c__1, (char *)&mxbval[0], (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (! ssb && ! glm && ! gqr && ! gsv && ! lse) {

/*        For the other paths, the number of parameters can be varied 
  
          from the input file.  Read the number of parameter values. 
*/

	s_rsle(&io___90);
	do_lio(&c__3, &c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	e_rsle();
	if (nparms < 1) {
	    s_wsfe(&io___92);
	    do_fio(&c__1, "NPARMS", 6L);
	    do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__1, (ftnlen)sizeof(integer));
	    e_wsfe();
	    nparms = 0;
	    fatal = TRUE_;
	} else if (nparms > 20) {
	    s_wsfe(&io___93);
	    do_fio(&c__1, "NPARMS", 6L);
	    do_fio(&c__1, (char *)&nparms, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&c__20, (ftnlen)sizeof(integer));
	    e_wsfe();
	    nparms = 0;
	    fatal = TRUE_;
	}

/*        Read the values of NB */

	if (! sbb) {
	    s_rsle(&io___94);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (nbval[i - 1] < 0) {
		    s_wsfe(&io___95);
		    do_fio(&c__1, "   NB ", 6L);
		    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbval[i - 1] > 132) {
		    s_wsfe(&io___96);
		    do_fio(&c__1, "   NB ", 6L);
		    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L70: */
	    }
	    s_wsfe(&io___97);
	    do_fio(&c__1, "NB:   ", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	}

/*        Read the values of NBMIN */

	if (nep || sep || svd || sgg) {
	    s_rsle(&io___98);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (nbmin[i - 1] < 0) {
		    s_wsfe(&io___99);
		    do_fio(&c__1, "NBMIN ", 6L);
		    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbmin[i - 1] > 132) {
		    s_wsfe(&io___100);
		    do_fio(&c__1, "NBMIN ", 6L);
		    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L80: */
	    }
	    s_wsfe(&io___101);
	    do_fio(&c__1, "NBMIN:", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		nbmin[i - 1] = 1;
/* L90: */
	    }
	}

/*        Read the values of NX */

	if (nep || sep || svd) {
	    s_rsle(&io___102);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (nxval[i - 1] < 0) {
		    s_wsfe(&io___103);
		    do_fio(&c__1, "   NX ", 6L);
		    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nxval[i - 1] > 132) {
		    s_wsfe(&io___104);
		    do_fio(&c__1, "   NX ", 6L);
		    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L100: */
	    }
	    s_wsfe(&io___105);
	    do_fio(&c__1, "NX:   ", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		nxval[i - 1] = 1;
/* L110: */
	    }
	}

/*        Read the values of NSHIFT (if NEP or SGG) or NRHS (if SVD   
          or SBB). */

	if (nep || svd || sbb || sgg) {
	    s_rsle(&io___106);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (nsval[i - 1] < 0) {
		    s_wsfe(&io___107);
		    do_fio(&c__1, "   NS ", 6L);
		    do_fio(&c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nsval[i - 1] > 132) {
		    s_wsfe(&io___108);
		    do_fio(&c__1, "   NS ", 6L);
		    do_fio(&c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L120: */
	    }
	    s_wsfe(&io___109);
	    do_fio(&c__1, "NS:   ", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		nsval[i - 1] = 1;
/* L130: */
	    }
	}

/*        Read the values for MAXB. */

	if (nep || sgg) {
	    s_rsle(&io___110);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (mxbval[i - 1] < 0) {
		    s_wsfe(&io___111);
		    do_fio(&c__1, " MAXB ", 6L);
		    do_fio(&c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (mxbval[i - 1] > 132) {
		    s_wsfe(&io___112);
		    do_fio(&c__1, " MAXB ", 6L);
		    do_fio(&c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L140: */
	    }
	    s_wsfe(&io___113);
	    do_fio(&c__1, "MAXB: ", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(integer))
			;
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		mxbval[i - 1] = 1;
/* L150: */
	    }
	}

/*        Read the values for NBCOL. */

	if (sgg) {
	    s_rsle(&io___114);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_lio(&c__3, &c__1, (char *)&nbcol[i - 1], (ftnlen)sizeof(
			integer));
	    }
	    e_rsle();
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		if (nbcol[i - 1] < 0) {
		    s_wsfe(&io___116);
		    do_fio(&c__1, "NBCOL ", 6L);
		    do_fio(&c__1, (char *)&nbcol[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__0, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		} else if (nbcol[i - 1] > 132) {
		    s_wsfe(&io___117);
		    do_fio(&c__1, "NBCOL ", 6L);
		    do_fio(&c__1, (char *)&nbcol[i - 1], (ftnlen)sizeof(
			    integer));
		    do_fio(&c__1, (char *)&c__132, (ftnlen)sizeof(integer));
		    e_wsfe();
		    fatal = TRUE_;
		}
/* L160: */
	    }
	    s_wsfe(&io___118);
	    do_fio(&c__1, "NBCOL:", 6L);
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		do_fio(&c__1, (char *)&nbcol[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	} else {
	    i__1 = nparms;
	    for (i = 1; i <= i__1; ++i) {
		nbcol[i - 1] = 1;
/* L170: */
	    }
	}
    }

/*     Calculate and print the machine dependent constants. */

    s_wsle(&io___119);
    e_wsle();
    eps = slamch_("Underflow threshold");
    s_wsfe(&io___121);
    do_fio(&c__1, "underflow", 9L);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(real));
    e_wsfe();
    eps = slamch_("Overflow threshold");
    s_wsfe(&io___122);
    do_fio(&c__1, "overflow ", 9L);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(real));
    e_wsfe();
    eps = slamch_("Epsilon");
    s_wsfe(&io___123);
    do_fio(&c__1, "precision", 9L);
    do_fio(&c__1, (char *)&eps, (ftnlen)sizeof(real));
    e_wsfe();

/*     Read the threshold value for the test ratios. */

    s_rsle(&io___124);
    do_lio(&c__4, &c__1, (char *)&thresh, (ftnlen)sizeof(real));
    e_rsle();
    s_wsfe(&io___125);
    do_fio(&c__1, (char *)&thresh, (ftnlen)sizeof(real));
    e_wsfe();
    if (sep || svd || sgg) {

/*        Read the flag that indicates whether to test LAPACK routines
. */

	s_rsle(&io___126);
	do_lio(&c__8, &c__1, (char *)&tstchk, (ftnlen)sizeof(logical));
	e_rsle();

/*        Read the flag that indicates whether to test driver routines
. */

	s_rsle(&io___128);
	do_lio(&c__8, &c__1, (char *)&tstdrv, (ftnlen)sizeof(logical));
	e_rsle();
    }

/*     Read the flag that indicates whether to test the error exits. */

    s_rsle(&io___130);
    do_lio(&c__8, &c__1, (char *)&tsterr, (ftnlen)sizeof(logical));
    e_rsle();

/*     Read the code describing how to set the random number seed. */

    s_rsle(&io___131);
    do_lio(&c__3, &c__1, (char *)&newsd, (ftnlen)sizeof(integer));
    e_rsle();

/*     If NEWSD = 2, read another line with 4 integers for the seed. */

    if (newsd == 2) {
	s_rsle(&io___133);
	for (i = 1; i <= 4; ++i) {
	    do_lio(&c__3, &c__1, (char *)&ioldsd[i - 1], (ftnlen)sizeof(
		    integer));
	}
	e_rsle();
    }

    for (i = 1; i <= 4; ++i) {
	iseed[i - 1] = ioldsd[i - 1];
/* L180: */
    }

    if (fatal) {
	s_wsfe(&io___135);
	e_wsfe();
	s_stop("", 0L);
    }

/*     Read the input lines indicating the test path and its parameters. 
  
       The first three characters indicate the test path, and the number 
  
       of test matrix types must be the first nonblank item in columns   
       4-80. */

L190:
    i__1 = s_rsfe(&io___136);
    if (i__1 != 0) {
	goto L370;
    }
    i__1 = do_fio(&c__1, line, 80L);
    if (i__1 != 0) {
	goto L370;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L370;
    }
    s_copy(c3, line, 3L, 3L);
    lenp = i_len(line, 80L);
    i = 3;
    itmp = 0;
    i1 = 0;
L200:
    ++i;
    if (i > lenp) {
	if (i1 > 0) {
	    goto L230;
	} else {
	    ntypes = 30;
	    goto L230;
	}
    }
    if (*(unsigned char *)&line[i - 1] != ' ' && *(unsigned char *)&line[i - 
	    1] != ',') {
	i1 = i;
	*(unsigned char *)c1 = *(unsigned char *)&line[i1 - 1];

/*        Check that a valid integer was read */

	for (k = 1; k <= 10; ++k) {
	    if (*(unsigned char *)c1 == *(unsigned char *)&intstr[k - 1]) {
		ic = k - 1;
		goto L220;
	    }
/* L210: */
	}
	s_wsfe(&io___145);
	do_fio(&c__1, (char *)&i, (ftnlen)sizeof(integer));
	do_fio(&c__1, line, 80L);
	e_wsfe();
	goto L190;
L220:
	itmp = itmp * 10 + ic;
	goto L200;
    } else if (i1 > 0) {
	goto L230;
    } else {
	goto L200;
    }
L230:
    ntypes = itmp;

/*     Skip the tests if NTYPES is <= 0. */

    if (! (sev || ses || svx || ssx) && ntypes <= 0) {
	s_wsfe(&io___146);
	do_fio(&c__1, c3, 3L);
	e_wsfe();
	goto L190;
    }

/*     Reset the random number seed. */

    if (newsd == 0) {
	for (k = 1; k <= 4; ++k) {
	    iseed[k - 1] = ioldsd[k - 1];
/* L240: */
	}
    }

    if (lsamen_(&c__3, c3, "SHS") || lsamen_(&c__3, c3, "NEP")
	    ) {

/*        -------------------------------------   
          NEP:  Nonsymmetric Eigenvalue Problem   
          -------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point   
             NS    = number of shifts   
             MAXB  = minimum submatrix size */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tsterr) {
	    serrhs_("SHSEQR", &c__6);
	}
	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    xlaenv_(&c__1, &nbval[i - 1]);
	    xlaenv_(&c__2, &nbmin[i - 1]);
	    xlaenv_(&c__3, &nxval[i - 1]);
	    xlaenv_(&c__4, &nsval[i - 1]);
	    xlaenv_(&c__8, &mxbval[i - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L250: */
		}
	    }
	    s_wsfe(&io___149);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    schkhs_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, a, &
		    c__132, &a[17424], &a[34848], &a[52272], &a[69696], &
		    c__132, &a[87120], &a[104544], d, &d[132], &d[264], &d[
		    396], &a[121968], &a[139392], &a[156816], &a[174240], &a[
		    191664], &d[528], work, &c__87781, iwork, logwrk, result, 
		    &info);
	    if (info != 0) {
		s_wsfe(&io___157);
		do_fio(&c__1, "SCHKHS", 6L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L260: */
	}

    } else if (lsamen_(&c__3, c3, "SST") || lsamen_(&c__3, c3, "SEP")) {

/*        ----------------------------------   
          SEP:  Symmetric Eigenvalue Problem   
          ----------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point */

	maxtyp = 15;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tsterr) {
	    serrst_("SST", &c__6);
	}
	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    xlaenv_(&c__1, &nbval[i - 1]);
	    xlaenv_(&c__2, &nbmin[i - 1]);
	    xlaenv_(&c__3, &nxval[i - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L270: */
		}
	    }
	    s_wsfe(&io___158);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		schkst_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, a, 
			&c__132, &a[17424], d, &d[132], &d[264], &d[396], &d[
			528], &d[660], &d[792], &d[924], &d[1056], &d[1188], &
			d[1320], &a[34848], &c__132, &a[52272], &a[69696], &d[
			1452], &a[87120], work, &c__87781, iwork, result, &
			info);
		if (info != 0) {
		    s_wsfe(&io___159);
		    do_fio(&c__1, "SCHKST", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		sdrvst_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, a, 
			&c__132, &d[264], &d[396], &d[528], &d[660], &d[924], 
			&d[1056], &d[1188], &a[17424], &c__132, &a[34848], &d[
			1452], &a[52272], work, &c__87781, iwork, result, &
			info);
		if (info != 0) {
		    s_wsfe(&io___160);
		    do_fio(&c__1, "SDRVST", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L280: */
	}

    } else if (lsamen_(&c__3, c3, "SSG")) {

/*        ----------------------------------------------   
          SSG:  Symmetric Generalized Eigenvalue Problem   
          ----------------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    xlaenv_(&c__1, &nbval[i - 1]);
	    xlaenv_(&c__2, &nbmin[i - 1]);
	    xlaenv_(&c__3, &nxval[i - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L290: */
		}
	    }
	    s_wsfe(&io___161);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		sdrvsg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &c__6, a, 
			&c__132, &a[17424], &c__132, &d[264], &a[34848], &
			c__132, &a[52272], &a[69696], &a[87120], &a[104544], 
			work, &c__87781, iwork, result, &info);
		if (info != 0) {
		    s_wsfe(&io___162);
		    do_fio(&c__1, "SDRVSG", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L300: */
	}

    } else if (lsamen_(&c__3, c3, "SBD") || lsamen_(&c__3, c3, "SVD")) {

/*        ----------------------------------   
          SVD:  Singular Value Decomposition   
          ----------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NX    = crossover point   
             NRHS  = number of right hand sides */

	maxtyp = 16;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);

/*        Test the error exits */

	if (tsterr && tstchk) {
	    serrbd_("SBDSQR", &c__6);
	}
	if (tsterr && tstdrv) {
	    serred_("SBDSQR", &c__6);
	}

	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    nrhs = nsval[i - 1];
	    xlaenv_(&c__1, &nbval[i - 1]);
	    xlaenv_(&c__2, &nbmin[i - 1]);
	    xlaenv_(&c__3, &nxval[i - 1]);
	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L310: */
		}
	    }
	    s_wsfe(&io___164);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nxval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(integer));
	    e_wsfe();
	    if (tstchk) {
		schkbd_(&nn, mval, nval, &maxtyp, dotype, &nrhs, iseed, &
			thresh, a, &c__132, d, &d[132], &d[264], &d[396], &a[
			17424], &c__132, &a[34848], &a[52272], &a[69696], &
			c__132, &a[87120], &c__132, &a[104544], &a[121968], 
			work, &c__87781, &c__6, &info);
		if (info != 0) {
		    s_wsfe(&io___165);
		    do_fio(&c__1, "SCHKBD", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		sdrvbd_(&nn, mval, nval, &maxtyp, dotype, iseed, &thresh, a, &
			c__132, &a[17424], &c__132, &a[34848], &c__132, &a[
			52272], &a[69696], &a[87120], d, &d[132], &d[264], 
			work, &c__87781, &c__6, &info);
	    }
/* L320: */
	}

    } else if (lsamen_(&c__3, c3, "SEV")) {

/*        --------------------------------------------   
          SEV:  Nonsymmetric Eigenvalue Problem Driver   
                SGEEV (eigenvalues and eigenvectors)   
          -------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___166);
	    do_fio(&c__1, c3, 3L);
	    e_wsfe();
	} else {
	    if (tsterr) {
		serred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    sdrvev_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__6, a, &
		    c__132, &a[17424], d, &d[132], &d[264], &d[396], &a[34848]
		    , &c__132, &a[52272], &c__132, &a[69696], &c__132, result,
		     work, &c__87781, iwork, &info);
	    if (info != 0) {
		s_wsfe(&io___167);
		do_fio(&c__1, "SGEEV", 5L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___168);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "SES")) {

/*        --------------------------------------------   
          SES:  Nonsymmetric Eigenvalue Problem Driver   
                SGEES (Schur form)   
          -------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___169);
	    do_fio(&c__1, c3, 3L);
	    e_wsfe();
	} else {
	    if (tsterr) {
		serred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    sdrves_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__6, a, &
		    c__132, &a[17424], &a[34848], d, &d[132], &d[264], &d[396]
		    , &a[52272], &c__132, result, work, &c__87781, iwork, 
		    logwrk, &info);
	    if (info != 0) {
		s_wsfe(&io___170);
		do_fio(&c__1, "SGEES", 5L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___171);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "SVX")) {

/*        -----------------------------------------------------------
---   
          SVX:  Nonsymmetric Eigenvalue Problem Expert Driver   
                SGEEVX (eigenvalues, eigenvectors and condition number
s)   
          -----------------------------------------------------------
--- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___172);
	    do_fio(&c__1, c3, 3L);
	    e_wsfe();
	} else {
	    if (tsterr) {
		serred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    sdrvvx_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__5, &c__6, 
		    a, &c__132, &a[17424], d, &d[132], &d[264], &d[396], &a[
		    34848], &c__132, &a[52272], &c__132, &a[69696], &c__132, &
		    d[528], &d[660], &d[792], &d[924], &d[1056], &d[1188], &d[
		    1320], &d[1452], result, work, &c__87781, iwork, &info);
	    if (info != 0) {
		s_wsfe(&io___173);
		do_fio(&c__1, "SGEEVX", 6L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___174);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "SSX")) {

/*        ---------------------------------------------------   
          SSX:  Nonsymmetric Eigenvalue Problem Expert Driver   
                SGEESX (Schur form and condition numbers)   
          --------------------------------------------------- */

	maxtyp = 21;
	ntypes = min(maxtyp,ntypes);
	if (ntypes <= 0) {
	    s_wsfe(&io___175);
	    do_fio(&c__1, c3, 3L);
	    e_wsfe();
	} else {
	    if (tsterr) {
		serred_(c3, &c__6);
	    }
	    alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	    sdrvsx_(&nn, nval, &ntypes, dotype, iseed, &thresh, &c__5, &c__6, 
		    a, &c__132, &a[17424], &a[34848], d, &d[132], &d[264], &d[
		    396], &d[528], &d[660], &a[52272], &c__132, &a[69696], 
		    result, work, &c__87781, iwork, logwrk, &info);
	    if (info != 0) {
		s_wsfe(&io___176);
		do_fio(&c__1, "SGEESX", 6L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
	}
	s_wsfe(&io___177);
	e_wsfe();
	goto L10;

    } else if (lsamen_(&c__3, c3, "SGG")) {

/*        -------------------------------------------------   
          SGG:  Generalized Nonsymmetric Eigenvalue Problem   
          -------------------------------------------------   
          Vary the parameters   
             NB    = block size   
             NBMIN = minimum block size   
             NS    = number of shifts   
             MAXB  = minimum submatrix size   
             NBCOL = minimum column dimension for blocks */

	maxtyp = 26;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tstchk && tsterr) {
	    serrgg_(c3, &c__6);
	}
	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    xlaenv_(&c__1, &nbval[i - 1]);
	    xlaenv_(&c__2, &nbmin[i - 1]);
	    xlaenv_(&c__4, &nsval[i - 1]);
	    xlaenv_(&c__8, &mxbval[i - 1]);
	    xlaenv_(&c__5, &nbcol[i - 1]);

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L330: */
		}
	    }
	    s_wsfe(&io___178);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbmin[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nsval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&mxbval[i - 1], (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&nbcol[i - 1], (ftnlen)sizeof(integer));
	    e_wsfe();
	    tstdif = FALSE_;
	    thrshn = 10.f;
	    if (tstchk) {
		schkgg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &tstdif, &
			thrshn, &c__6, a, &c__132, &a[17424], &a[34848], &a[
			52272], &a[69696], &a[87120], &a[104544], &a[121968], 
			&a[139392], &c__132, &a[156816], &a[174240], &a[
			191664], d, &d[132], &d[264], &d[396], &d[528], &d[
			660], &a[209088], &a[226512], work, &c__87781, logwrk,
			 result, &info);
		if (info != 0) {
		    s_wsfe(&io___181);
		    do_fio(&c__1, "SCHKGG", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
	    if (tstdrv) {
		sdrvgg_(&nn, nval, &maxtyp, dotype, iseed, &thresh, &thrshn, &
			c__6, a, &c__132, &a[17424], &a[34848], &a[52272], &a[
			69696], &a[87120], &a[104544], &c__132, &a[121968], d,
			 &d[132], &d[264], &d[396], &d[528], &d[660], &a[
			209088], &a[226512], work, &c__87781, result, &info);
		if (info != 0) {
		    s_wsfe(&io___182);
		    do_fio(&c__1, "SDRVGG", 6L);
		    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		    e_wsfe();
		}
	    }
/* L340: */
	}

    } else if (lsamen_(&c__3, c3, "SSB")) {

/*        ------------------------------   
          SSB:  Symmetric Band Reduction   
          ------------------------------ */

	maxtyp = 15;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	if (tsterr) {
	    serrst_("SSB", &c__6);
	}
	schksb_(&nn, nval, &nk, kval, &maxtyp, dotype, iseed, &thresh, &c__6, 
		a, &c__132, d, &d[132], &a[17424], &c__132, work, &c__87781, 
		result, &info);
	if (info != 0) {
	    s_wsfe(&io___183);
	    do_fio(&c__1, "SCHKSB", 6L);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "SBB")) {

/*        ------------------------------   
          SBB:  General Band Reduction   
          ------------------------------ */

	maxtyp = 15;
	ntypes = min(maxtyp,ntypes);
	alareq_(c3, &ntypes, dotype, &maxtyp, &c__5, &c__6);
	i__1 = nparms;
	for (i = 1; i <= i__1; ++i) {
	    nrhs = nsval[i - 1];

	    if (newsd == 0) {
		for (k = 1; k <= 4; ++k) {
		    iseed[k - 1] = ioldsd[k - 1];
/* L350: */
		}
	    }
	    s_wsfe(&io___184);
	    do_fio(&c__1, c3, 3L);
	    do_fio(&c__1, (char *)&nrhs, (ftnlen)sizeof(integer));
	    e_wsfe();
	    schkbb_(&nn, mval, nval, &nk, kval, &maxtyp, dotype, &nrhs, iseed,
		     &thresh, &c__6, a, &c__132, &a[17424], &c__264, d, &d[
		    132], &a[52272], &c__132, &a[69696], &c__132, &a[87120], &
		    c__132, &a[104544], work, &c__87781, result, &info);
	    if (info != 0) {
		s_wsfe(&io___185);
		do_fio(&c__1, "SCHKBB", 6L);
		do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
		e_wsfe();
	    }
/* L360: */
	}

    } else if (lsamen_(&c__3, c3, "GLM")) {

/*        -----------------------------------------   
          GLM:  Generalized Linear Regression Model   
          ----------------------------------------- */

	if (tsterr) {
	    serrgg_("GLM", &c__6);
	}
	sckglm_(&nn, mval, pval, nval, &ntypes, iseed, &thresh, &c__132, a, &
		a[17424], b, &b[17424], x, work, d, &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___188);
	    do_fio(&c__1, "SCKGLM", 6L);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "GQR")) {

/*        ------------------------------------------   
          GQR:  Generalized QR and RQ factorizations   
          ------------------------------------------ */

	if (tsterr) {
	    serrgg_("GQR", &c__6);
	}
	sckgqr_(&nn, mval, &nn, pval, &nn, nval, &ntypes, iseed, &thresh, &
		c__132, a, &a[17424], &a[34848], &a[52272], taua, b, &b[17424]
		, &b[34848], &b[52272], &b[69696], taub, work, d, &c__5, &
		c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___191);
	    do_fio(&c__1, "SCKGQR", 6L);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "GSV")) {

/*        ----------------------------------------------   
          GSV:  Generalized Singular Value Decomposition   
          ---------------------------------------------- */

	if (tsterr) {
	    serrgg_("GSV", &c__6);
	}
	sckgsv_(&nn, mval, pval, nval, &ntypes, iseed, &thresh, &c__132, a, &
		a[17424], b, &b[17424], &a[34848], &b[34848], &a[52272], taua,
		 taub, &b[52272], iwork, work, d, &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___192);
	    do_fio(&c__1, "SCKGSV", 6L);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else if (lsamen_(&c__3, c3, "LSE")) {

/*        --------------------------------------   
          LSE:  Constrained Linear Least Squares   
          -------------------------------------- */

	if (tsterr) {
	    serrgg_("LSE", &c__6);
	}
	scklse_(&nn, mval, pval, nval, &ntypes, iseed, &thresh, &c__132, a, &
		a[17424], b, &b[17424], x, work, d, &c__5, &c__6, &info);
	if (info != 0) {
	    s_wsfe(&io___193);
	    do_fio(&c__1, "SCKLSE", 6L);
	    do_fio(&c__1, (char *)&info, (ftnlen)sizeof(integer));
	    e_wsfe();
	}

    } else {
	s_wsle(&io___194);
	e_wsle();
	s_wsle(&io___195);
	e_wsle();
	s_wsfe(&io___196);
	do_fio(&c__1, c3, 3L);
	e_wsfe();
    }
    goto L190;
L370:
    s_wsfe(&io___197);
    e_wsfe();
    s2 = second_();
    s_wsfe(&io___199);
    r__1 = s2 - s1;
    do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
    e_wsfe();


/*     End of SCHKEE */

    return 0;
} /* MAIN__   

   Main program alias */ int schkee_ () { MAIN__ (); return 0; }
