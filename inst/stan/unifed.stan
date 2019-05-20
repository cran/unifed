real unifed_kappa(real theta){
    
  if( fabs(theta ) < 1e-6)
    return 0;

  if( theta <=50 )
    return log( expm1(theta) / theta  );

  return theta - log (theta);	
}

/**
 * Applies unifed_kappa to each element of a vector.
 * 
 * @param theta A vector.
 *
 * @return A vector whose elements are unifed_kappa applied to each
 * element of theta.
 */
vector unifed_kappa_v(vector theta){
  int N=num_elements(theta);
  vector[N] result;
  
  for(i in 1:N)
    result[i]=unifed_kappa(theta[i]);

  return result;
}

real unifed_kappa_prime(real theta){
  real tol=1e-7;
  real return_value;
  if (fabs(theta) <= machine_precision()){
    return_value = 0.5;
  } else{
    return_value = -1/expm1(-theta) - 1/theta ;
    }
return return_value;
}

real unifed_kappa_double_prime(real theta){
  real tol=1e-7;
  real return_value;
  if (fabs(theta) <= tol){
    return_value = 1./12.;
  }else{
    if(theta > -100)
      return_value = pow(1/theta,2)  - exp(-theta)/pow(expm1(-theta),2);
    else
      return_value = pow(1/theta,2);
  }
  return return_value;
}
  
real unifed_lpdf(real x,real theta){
  if( x < 0 || x > 1  )
    reject("x must be between 0 and 1");
  return x * theta - unifed_kappa(theta);
}

real unifed_quantile(real p,real theta){
  if( p<0 || p>1)
    reject("p must be between 0 and 1");
  if( fabs(theta) < 1e-6)
    return p;
  else
    return log(p * ( exp(theta) -1 ) + 1 ) / theta;
}

real unifed_rng(real theta){
  return unifed_quantile(uniform_rng(0,1),theta);
}

real unifed_lcdf(real x,real theta){
  
  real tol = 1e-7;
  
  if( x <= 0)
    return negative_infinity();
  if( x >= 1 )
    return 0;

  if( fabs(theta) < tol )
    return log(x);

  if( theta <=50 )
    return log( expm1(x*theta) / expm1(theta)  );
  
  return  (x-1) * theta + log1m_exp( - theta * x) - log1m_exp( - theta); 
  
}



real unifed_kappa_prime_inverse(real mu){
  
  real tol=1e-7;
  int maxit=10000000;
  real found_solution = 0;
  real y_prime;
  int iter=1;

  real old_x = 0;
  real y;
  real new_x;


  if ( fabs(mu - 0.5) <= 1e-5 )
    return 0;

  else{
    
    while(iter <= maxit){
      y = unifed_kappa_prime(old_x) - mu;
      y_prime = unifed_kappa_double_prime(old_x);
      new_x = old_x - y/y_prime;

      if ( fabs(new_x - old_x) <= tol * fabs(new_x)  ){
	found_solution = 1;
	break;
      }
      old_x = new_x;
      iter+=1;
    }

    if (found_solution){
	return new_x;
    }
    else
      return  not_a_number();
  }
}

real unifed_unit_deviance(real y,real mu){
  real y_inv=unifed_kappa_prime_inverse(y);
  real mu_inv=unifed_kappa_prime_inverse(mu);

  return 2 * ( y*( y_inv - mu_inv ) - unifed_kappa(y_inv) + unifed_kappa(mu_inv) );
  
}

/**
 * Applies unifed_kappa to each element of a vector.
 * 
 * @param theta A vector.
 *
 * @return A vector whose elements are unifed_kappa applied to each
 * element of theta.
 */
vector unifed_kappa_prime_inverse_v(vector mu){
  int N=num_elements(mu);
  vector[N] result;
  for(i in 1:N){
    result[i] = unifed_kappa_prime_inverse(mu[i]);
  }
  return result;
}

/**
 * Increases the log probability accumulator with the likelihood
 * function of a unifed GLM.
 *
 * @param y vector of observed responses
 * 
 * @param theta vector containing the cenonical parameters of each
 * class of the glm.
 *
 * @param weights vector with the number of observations in each class.
 *
 */
void unifed_glm_lp(vector y, vector theta, vector weights){
  target += dot_product(weights,(y .* theta  - unifed_kappa_v(theta)));
}
