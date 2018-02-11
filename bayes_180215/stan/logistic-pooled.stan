// logistic-pooled.stan
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
}
model {
  alpha ~ normal(0,10);       // prior on intercept
  b1 ~ normal(0,10);          // prior on coefficient
  b2 ~ normal(0,10);
  b3 ~ normal(0,10);
  y ~ bernoulli_logit(alpha + b1*x1 + b2*x2 + b3*x3);
}

