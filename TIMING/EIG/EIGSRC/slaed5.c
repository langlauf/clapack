/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real ops, itcnt;
} latime_;

#define latime_1 latime_

/* Subroutine */ int slaed5_(integer *i, real *d, real *z, real *delta, real *
	rho, real *dlam)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real temp, b, c, w, del, tau;


/*  -- LAPACK routine (instrumented to count operations, version 2.0) -- 
  
       Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab, 
  
       Courant Institute, NAG Ltd., and Rice University   
       September 30, 1994   

       Common block to return operation count and iteration count   
       ITCNT is unchanged, OPS is only incremented   

    Purpose   
    =======   

    This subroutine computes the I-th eigenvalue of a symmetric rank-one 
  
    modification of a 2-by-2 diagonal matrix   

               diag( D )  +  RHO *  Z * transpose(Z) .   

    The diagonal elements in the array D are assumed to satisfy   

               D(i) < D(j)  for  i < j .   

    We also assume RHO > 0 and that the Euclidean norm of the vector   
    Z is one.   

    Arguments   
    =========   

    I      (input) INTEGER   
           The index of the eigenvalue to be computed.  I = 1 or I = 2.   

    D      (input) REAL array, dimension (2)   
           The original eigenvalues.  We assume D(1) < D(2).   

    Z      (input) REAL array, dimension (2)   
           The components of the updating vector.   

    DELTA  (output) REAL array, dimension (2)   
           The vector DELTA contains the information necessary   
           to construct the eigenvectors.   

    RHO    (input) REAL   
           The scalar in the symmetric updating formula.   

    DLAM   (output) REAL   
           The computed lambda_I, the I-th updated eigenvalue.   

    ===================================================================== 
  


       Parameter adjustments */
    --delta;
    --z;
    --d;

    /* Function Body */
    del = d[2] - d[1];
    if (*i == 1) {
	w = *rho * 2.f * (z[2] * z[2] - z[1] * z[1]) / del + 1.f;
	if (w > 0.f) {
	    latime_1.ops += 33;
	    b = del + *rho * (z[1] * z[1] + z[2] * z[2]);
	    c = *rho * z[1] * z[1] * del;

/*           B > ZERO, always */

	    tau = c * 2.f / (b + sqrt((r__1 = b * b - c * 4.f, dabs(r__1))));
	    *dlam = d[1] + tau;
	    delta[1] = -(doublereal)z[1] / tau;
	    delta[2] = z[2] / (del - tau);
	} else {
	    latime_1.ops += 31;
	    b = -(doublereal)del + *rho * (z[1] * z[1] + z[2] * z[2]);
	    c = *rho * z[2] * z[2] * del;
	    if (b > 0.f) {
		tau = c * -2.f / (b + sqrt(b * b + c * 4.f));
	    } else {
		tau = (b - sqrt(b * b + c * 4.f)) / 2.f;
	    }
	    *dlam = d[2] + tau;
	    delta[1] = -(doublereal)z[1] / (del + tau);
	    delta[2] = -(doublereal)z[2] / tau;
	}
	temp = sqrt(delta[1] * delta[1] + delta[2] * delta[2]);
	delta[1] /= temp;
	delta[2] /= temp;
    } else {

/*     Now I=2 */

	latime_1.ops += 24;
	b = -(doublereal)del + *rho * (z[1] * z[1] + z[2] * z[2]);
	c = *rho * z[2] * z[2] * del;
	if (b > 0.f) {
	    tau = (b + sqrt(b * b + c * 4.f)) / 2.f;
	} else {
	    tau = c * 2.f / (-(doublereal)b + sqrt(b * b + c * 4.f));
	}
	*dlam = d[2] + tau;
	delta[1] = -(doublereal)z[1] / (del + tau);
	delta[2] = -(doublereal)z[2] / tau;
	temp = sqrt(delta[1] * delta[1] + delta[2] * delta[2]);
	delta[1] /= temp;
	delta[2] /= temp;
    }
    return 0;

/*     End OF SLAED5 */

} /* slaed5_ */

