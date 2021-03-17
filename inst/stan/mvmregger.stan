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
    int<lower=0> d; // number of phenotypes
    vector[n] ybeta; // instrument outcome associations
    matrix[n,d] xbeta; // instrument exposure associations
    vector[n] weights; // indicating weights
    real<lower=-1,upper=1> rho;  // correlation to fit
    int<lower=1, upper=4> prior; // option for choice of prior distribution
}


parameters {
    // intercept and noise sd
    real intercept;
    vector[d] estimate;
    real sigma;
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

}
