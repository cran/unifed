functions{
#include unifed.stan
}

data{
  int<lower=1> M; //Rows in the design matrix
  int<lower=1> P; //Columns in the design matrix
  matrix[M,P] X;
  vector<lower=0,upper=1>[M] y;
  int<lower=1> ws[M]; //Number of observations in each class
}


parameters{
  vector[P] beta;
}

transformed parameters{
  vector[M] theta;
  theta = X*beta;   
}


model{ 
  beta ~ normal(0,20);
  unifed_glm_lp(y, theta , to_vector(ws));      
}

generated quantities{
  vector[M] replicated_samples=rep_vector(0,M);
  vector[M] mu;
  
  for(i in 1:M){
    int Nobs;
    Nobs=ws[i];
    for(n in 1:Nobs ){
      replicated_samples[i]+=unifed_rng(theta[i]);
    }
    replicated_samples[i]/=Nobs;
    mu[i]=unifed_kappa_prime(theta[i]);
  }}
