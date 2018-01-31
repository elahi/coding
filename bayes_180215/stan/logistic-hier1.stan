// logistic-hier1.stan
data {
  int<lower=0> N;             // number of observations
  int<lower=0,upper=1> y[N];  // set the (binary) dependent variable as bounded between 0 and 1
  vector[N] x1;               // Nation
  vector[N] x2;               // Distance to closure
  vector[N] x3;               // Vessel length
}
parameters {
  real alpha;
  real b1;
  real b2;
  real b3;
  real<lower=0> sigmap;       // to allow for sampling variance
  real logodds[N]; 
}
model {
  alpha ~ normal(0,10);       // prior on intercept
  b1 ~ normal(0,10);          // prior on coefficient
  b2 ~ normal(0,10);
  b3 ~ normal(0,10);
  sigmap ~ chi_square(2);

  for(i in 1:N)
    logodds[i] = inv_logit(alpha + b1*x1[i] + b2*x2[i] + b3*x3[i]);  // get predicted log odds
    p[i] ~ normal(logodds[i], sigmap);                                // sample from predicted log odds
    y[i] ~ bernoulli(p[i]);                                           // sample bernoulli
}

