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
    int<lower=1, upper=3> prior;  // prior options
}

parameters {
    // intercept and noise sd
    //real intercept;
    real sigma;
    real estimate;
}

model {
    
    ybeta ~ normal(xbeta*estimate, sigma);
    // priors
    // Non-informative prior
    if (prior == 1){
      estimate ~ normal(0,100);
      sigma ~ uniform(1,10);
    }
    // Weakly informative prior
    else if (prior == 2){
      estimate ~ normal(0,10);
      sigma ~ uniform(1,10);
    }
    // Pseudo-horseshoe prior
    else {
      estimate ~ cauchy(0,1);
      sigma ~ inv_gamma(0.5,0.5);
    }
}
