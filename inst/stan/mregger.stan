//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// J indicates number of instruments
// The input data is a vector 'y' of length 'N'.

data {
    int<lower=0> n; // number of instruments 
    vector[n] ybeta; // instrument outcome associations
    vector[n] xbeta; // instrument exposure associations
    vector[n] weights; // indicating weights
    real<lower=-1,upper=1> rho;  // correlation to fit
    int<lower=1, upper=4> prior;
}

transformed data{
    vector[2] tau; // individual standard deviation
    vector[2] mu; // individual mean
    for (i in 1:2){
    tau[i] = 10;
    mu[i] = 0;
  }
}

parameters {
    // intercept and noise sd
    real intercept;
    real sigma;
    real estimate;
    vector[2] eta;
}

// Covariance matrix
transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = square(tau[1]);
  Sigma[2,2] = square(tau[2]);
  Sigma[1,2] = rho * tau[1] * tau[2];
  Sigma[2,1] = Sigma[1,2];
}

model {
    // priors
    
    // Non-informative prior
    if (prior == 1){
      intercept ~ normal(0,10);
      estimate ~ normal(0,100);
      sigma ~ uniform(1,10);
	  
      // model
      ybeta ~ normal(intercept*weights + xbeta*estimate, sigma);
    }
    // Weakly informative prior
    else if (prior == 2){
      intercept ~ normal(0,10);
      estimate ~ normal(0,10);
      sigma ~ uniform(1,10);
	  
      // model
      ybeta ~ normal(intercept*weights + xbeta*estimate, sigma);
    }
    // Pseudo-horseshoe prior
    else if (prior == 3){
      intercept ~ normal(0,10);
      estimate ~ cauchy(0,1);
      sigma ~ inv_gamma(0.5,0.5);
	  
      // model
      ybeta ~ normal(intercept*weights + xbeta*estimate, sigma);
    }
    // joint prior
    else {
      eta ~ multi_normal(mu,Sigma);
      sigma ~ uniform(1,10);

      // model
      ybeta ~ normal(eta[1]*weights+xbeta*eta[2], sigma);
    }
}
