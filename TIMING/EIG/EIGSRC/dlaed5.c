/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal ops, itcnt;
} latime_;

#define latime_1 latime_

/* Subroutine */ int dlaed5_(integer *i, doublereal *d, doublereal *z, 
	doublereal *delta, doublereal *rho, doublereal *dlam)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal temp, b, c, w, del, tau;


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

    D      (input) DOUBLE PRECISION array, dimension (2)   
           The original eigenvalues.  We assume D(1) < D(2).   

    Z      (input) DOUBLE PRECISION array, dimension (2)   
           The components of the updating vector.   

    DELTA  (output) DOUBLE PRECISION array, dimension (2)   
           The vector DELTA contains the information necessary   
           to construct the eigenvectors.   

    RHO    (input) DOUBLE PRECISION   
           The scalar in the symmetric updating formula.   

    DLAM   (output) DOUBLE PRECISION   
           The computed lambda_I, the I-th updated eigenvalue.   

    ===================================================================== 
  


       Parameter adjustments */
    --delta;
    --z;
    --d;

    /* Function Body */
    del = d[2] - d[1];
    if (*i == 1) {
	w = *rho * 2. * (z[2] * z[2] - z[1] * z[1]) / del + 1.;
	if (w > 0.) {
	    latime_1.ops += 33;
	    b = del + *rho * (z[1] * z[1] + z[2] * z[2]);
	    c = *rho * z[1] * z[1] * del;

/*           B > ZERO, always */

	    tau = c * 2. / (b + sqrt((d__1 = b * b - c * 4., abs(d__1))));
	    *dlam = d[1] + tau;
	    delta[1] = -z[1] / tau;
	    delta[2] = z[2] / (del - tau);
	} else {
	    latime_1.ops += 31;
	    b = -del + *rho * (z[1] * z[1] + z[2] * z[2]);
	    c = *rho * z[2] * z[2] * del;
	    if (b > 0.) {
		tau = c * -2. / (b + sqrt(b * b + c * 4.));
	    } else {
		tau = (b - sqrt(b * b + c * 4.)) / 2.;
	    }
	    *dlam = d[2] + tau;
	    delta[1] = -z[1] / (del + tau);
	    delta[2] = -z[2] / tau;
	}
	temp = sqrt(delta[1] * delta[1] + delta[2] * delta[2]);
	delta[1] /= temp;
	delta[2] /= temp;
    } else {

/*     Now I=2 */

	latime_1.ops += 24;
	b = -del + *rho * (z[1] * z[1] + z[2] * z[2]);
	c = *rho * z[2] * z[2] * del;
	if (b > 0.) {
	    tau = (b + sqrt(b * b + c * 4.)) / 2.;
	} else {
	    tau = c * 2. / (-b + sqrt(b * b + c * 4.));
	}
	*dlam = d[2] + tau;
	delta[1] = -z[1] / (del + tau);
	delta[2] = -z[2] / tau;
	temp = sqrt(delta[1] * delta[1] + delta[2] * delta[2]);
	delta[1] /= temp;
	delta[2] /= temp;
    }
    return 0;

/*     End OF DLAED5 */

} /* dlaed5_ */

