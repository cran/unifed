#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<R.h>
#include<Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_unifed(DllInfo* info) {
	R_registerRoutines(info, NULL, NULL, NULL, NULL);
	R_useDynamicSymbols(info, TRUE);
}

double unifed_kappa_one(double theta){
  
  if (fabs(theta) <= FLT_EPSILON)
    return 0;
  
  if ( theta <= 50)
    return log( ( exp(theta) - 1) / theta  );
  
  return theta - log (theta);  
}


SEXP unifed_kappa(SEXP theta){
  int n = length(theta);
  int i;
  double *ptheta,*pout;  
  SEXP out = PROTECT(allocVector(REALSXP,n));
  ptheta = REAL(theta);
  pout = REAL(out);

  for( i = 0; i < n; i++){
    pout[i] = unifed_kappa_one(ptheta[i]);
  }

  UNPROTECT(1);
  
  return out;
}


double unifed_kappa_prime(double theta,double tol){
  double return_value;
  if (fabs(theta) <= tol){
    return_value = 0.5;
  } else{
    return_value = -1/expm1(-theta) - 1/theta ;
    }
return return_value;
}

double unifed_kappa_double_prime(double theta,double tol){
  double return_value;
  if (fabs(theta) <= tol){
    return_value = 1./12.;
  }else{
    return_value = pow(1/theta,2)  - exp(-theta)/pow(expm1(-theta),2);
  }
  return return_value;
}


SEXP unifed_kappa_prime_inverse(SEXP mu_,SEXP tol_,SEXP maxit_,SEXP xinit_){

  double mu = asReal(mu_);
  double tol = asReal(tol_);
  int maxit = asInteger(maxit_);
  double xinit = asReal(xinit_);
    
  double found_solution = 0;
  double small_number=0;
  double y_prime;
  int iter=1;
  double old_x = xinit;
  double y;
  double new_x;

  if( mu < 0.1){
     mu = 1 - mu;
     small_number=1;
  }
   

  if ( fabs(mu - 0.5) <= 0.0001 )
    return ScalarReal(0);
  else{

    while(iter <= maxit){
      y = unifed_kappa_prime(old_x,tol) -mu;
      y_prime = unifed_kappa_double_prime(old_x,tol);
      new_x = old_x - y/y_prime;

      if ( fabs(new_x - old_x) <= tol * fabs(new_x)  ){
	found_solution = 1;
	break;
      }
      old_x = new_x;
      iter++;
    }

    if (found_solution){
      if( small_number )
	return ScalarReal(-new_x);
      else
	return ScalarReal(new_x);
    }
    else
      return  ScalarReal(NAN);    
  }
  
}


double kappa_prime_inverse(double mu, double tol, int maxit){

  double found_solution = 0;
  double y_prime;
  int iter=1;
  double small_number=0;
  double old_x = 0;
  double y;
  double new_x;

  if( mu < 0.1){
    mu = 1 - mu;
    small_number=1;
  }

  
  if ( fabs(mu - 0.5) <= 1e-5 )
    return 0;
  else{

    while(iter <= maxit){
      y = unifed_kappa_prime(old_x,tol) -mu;
      y_prime = unifed_kappa_double_prime(old_x,tol);
      new_x = old_x - y/y_prime;

      if ( fabs(new_x - old_x) <= tol * fabs(new_x)  ){
	found_solution = 1;
	break;
      }
      old_x = new_x;
      iter++;
    }

    if (found_solution){
      if (small_number)
	return -new_x;
      else
	return new_x;
    }
    else
      return  NAN;  
  }
  
}

double unit_deviance_one(double y,double mu, double tol, int maxit){
  double y_inv;
  double mu_inv;

  y_inv = kappa_prime_inverse(y,tol,maxit);
  mu_inv = kappa_prime_inverse(mu,tol,maxit);

  return 2 * ( y*( y_inv - mu_inv ) - unifed_kappa_one(y_inv) + unifed_kappa_one(mu_inv) );
}

SEXP unit_deviance(SEXP y_, SEXP mu_, SEXP tol_, SEXP maxit_){
  
  int n = length(y_);
  int i;
  double *py,*pmu,*pout;
  
  double tol=asReal(tol_);  
  double maxit=asInteger(maxit_);
  
  SEXP out = PROTECT(allocVector(REALSXP,n));

  py=REAL(y_);
  pmu=REAL(mu_);
  pout=REAL(out);

  for(i=0;i<n;i++)
    pout[i] = unit_deviance_one(py[i],pmu[i],tol,maxit) ;
    
  UNPROTECT(1);

  return out;
}
