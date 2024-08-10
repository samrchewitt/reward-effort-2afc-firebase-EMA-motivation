// parameter recovery version, which constrains parameter values to a reasonable range & prevent local minima 



// hierarchical item response theory model to get latent state motivation 

// subjective value free parameters (reward & eff sens) are function of trait-apathy score (+effect of apathy), an individual-by-time offset & state (+state effect)

// this is why we refer to it as a joint model parameter estimation embeds the influence of subjective measures (and therefore our hypotheses/expectations)

// the effects are quantified by 4 beta weights (state, trait, reward, effort)



data {

  int nTaskTimes; // N game timepoints indicator

  

  int nStateTimes; // n State timepoints indicator

  

  int nPpts; // number of subjects

  

  int nTrials_max; // number of trials in the game

  
  

  array[nPpts, nTaskTimes] int nT_ppts; // number of trials for each game, in case of any incompletes

  

  // game data over nTaskTimes 

  

  array[nPpts, nTaskTimes, nTrials_max] real rew1;

  

  array[nPpts, nTaskTimes, nTrials_max] real eff1;

  

  array[nPpts, nTaskTimes, nTrials_max] real rew2;

  

  array[nPpts, nTaskTimes, nTrials_max] real eff2;

  

  // choices made by participants

  

  array[nPpts, nTaskTimes, nTrials_max] int<lower=0, upper=1> choice01;

  

  // matrix indicating missing data 

  

  array[nPpts, nTaskTimes, nTrials_max] int<lower=0, upper=1> is_missing;

  

  // self-report data over nStateTimes

  

  int<lower=1> nItems; // number of items per participant

  

  int<lower=1> N; // total number of observations

  

  array[N] int<lower=1, upper=nPpts> jj; // person id for observation [n, time t]

  

  array[N] int<lower=1, upper=nItems> ii; // item id for observation n, time t 

  

  array[N, nStateTimes] int<lower=0, upper=1> Y; // binary response for observations n; y in {0 ... m_i}

  

  array[nPpts, nStateTimes] int<lower=0, upper=1> is_missing_state; // matrix of missing data for states

  

  array[nPpts, nTaskTimes] int<lower=1, upper=nStateTimes> ss; // task timepoint indices within nStateTimes array 

  

  // apathy 

  

  array[nPpts] real apathyCFA;

}

parameters {

  // covariance matrices for each parameter

  

  cholesky_factor_corr[nTaskTimes] R_chol_rewSens;

  

  cholesky_factor_corr[nTaskTimes] R_chol_effSens;

  

  cholesky_factor_corr[nStateTimes] R_chol_theta;

  

  // // group means at each timepoint (removed and use the trait-apathy estimate instead)

  

  vector[nStateTimes] mu_theta;

  

  // group sd at each timepoint 

  

  vector<lower=0, upper=10>[nTaskTimes] sigma_rewSens;

  

  vector<lower=0, upper=10>[nTaskTimes] sigma_effSens;

  

  vector<lower=0, upper=10>[nStateTimes] sigma_theta;

  

  // matrix at individual level 

  

  matrix<lower=-10, upper=10>[nTaskTimes, nPpts] rewSens_raw;

  

  matrix<lower=-10, upper=10>[nTaskTimes, nPpts] effSens_raw;

  

  matrix[nStateTimes, nPpts] theta_raw;

  

  // group level beta weights for effect of state

  

  //real beta_eff;

  

  real beta_rew;

  

  real beta_eff;

  

  // trait modulations 

  

  real beta_trait_rew;

  

  real beta_trait_eff;

  

  // IRT item parameters

  

  matrix<lower=0>[nItems, nStateTimes] delta;

}

transformed parameters {

  // participant level transformed 

  

  matrix[nPpts, nTaskTimes] rewSens;

  

  matrix[nPpts, nTaskTimes] effSens;

  

  matrix[nPpts, nStateTimes] theta;

  

  matrix[nPpts, nStateTimes] thetaZ; // person centred latent state

  

  // probability of endorsing each self-report item 

  

  matrix<lower=0, upper=1>[N, nStateTimes] prob;

  

  // participant level offsets - note dims = time x person 

  

  matrix[nTaskTimes, nPpts] rewSens_tilde;

  

  matrix[nTaskTimes, nPpts] effSens_tilde;

  

  matrix[nStateTimes, nPpts] theta_tilde;

  

  //////////individual level state & decision-making task parameters////////////////

  

  // construct individual offsets (for non-centered parameterization)

  

  rewSens_tilde = diag_pre_multiply(sigma_rewSens, R_chol_rewSens)

                  * rewSens_raw;

  

  effSens_tilde = diag_pre_multiply(sigma_effSens, R_chol_effSens)

                  * effSens_raw;

  

  theta_tilde = diag_pre_multiply(sigma_theta, R_chol_theta) * theta_raw;

  

  // loop participants

  

  for (p in 1 : nPpts) {

    // initialise mean and sd of state theta

    

    real mean_theta;

    

    real sd_theta;

    

    // for each self-report timepoint, define theta as group mean for that timepoint + participant offset at that time

    

    for (t in 1 : nStateTimes) {

      if (is_missing_state[p, t] == 0) {

        theta[p, t] = mu_theta[t] + theta_tilde[t, p];

      } else {

        // if data is missing, assign group mean (for that timepoint only)

        

        theta[p, t] = mu_theta[t];

      }

    }

    

    // dissociate state from mean through a z-score [thetaZ] (within-person scaling)

    

    mean_theta = mean(theta[p,  : ]);

    

    sd_theta = sd(theta[p,  : ]);

    

    thetaZ[p,  : ] = (theta[p,  : ] - mean_theta) / sd_theta;

    

    // now loop the decision-making timepoints

    

    for (t in 1 : nTaskTimes) {

      if (is_missing[p, t, 1] == 0) {

        // allow theta to influnce task parameters if desired, and quantify with beta weight 

        

        rewSens[p, t] = beta_trait_rew * apathyCFA[p] + rewSens_tilde[t, p]

                        + beta_rew * thetaZ[p, ss[p, t]];

        

        effSens[p, t] = beta_trait_eff * apathyCFA[p] + effSens_tilde[t, p]

                        + beta_eff * thetaZ[p, ss[p, t]];

      } else {

        // if missing don't apply beta weights

        

        rewSens[p, t] = beta_trait_rew * apathyCFA[p];

        

        effSens[p, t] = beta_trait_eff * apathyCFA[p];

      }

    }

  }

  

  // SELF_REPORT MODEL // 

  

  // use theta and delta to generate a probability of endorsing each option (the 1-parameter IRT model)

  

  // 1 parameter is slightly confusing because delta is also a parameter but at the item level (not subject)

  

  for (t in 1 : nStateTimes) {

    vector[N] theta_delta_diff;

    

    for (n in 1 : N) {

      theta_delta_diff[n] = theta[jj[n], t] - delta[ii[n], t];

    }

    

    prob[ : , t] = inv_logit(theta_delta_diff);

  }

}

model {

  // weakly informative, generic priors 

  

  R_chol_rewSens ~ lkj_corr_cholesky(1);

  

  R_chol_effSens ~ lkj_corr_cholesky(1);

  

  R_chol_theta ~ lkj_corr_cholesky(1);

  

  // state 

  

  beta_rew ~ normal(0, 1);

  

  beta_eff ~ normal(0, 1);

  

  // trait modulations

  

  beta_trait_rew ~ normal(0, 1);

  

  beta_trait_eff ~ normal(0, 1);

  

  //beta_eff ~ normal(0, 1);

  

  mu_theta ~ normal(0, 1);

  

  sigma_rewSens ~ cauchy(0, 1);

  

  sigma_effSens ~ cauchy(0, 1);

  

  sigma_theta ~ cauchy(0, 1);

  

  to_vector(rewSens_raw) ~ normal(0, 1);

  

  to_vector(effSens_raw) ~ normal(0, 1);

  

  to_vector(theta_raw) ~ normal(0, 1);

  

  to_vector(delta) ~ normal(0, 1);

  

  // get choice values and fit to data 

  

  for (p in 1 : nPpts) {

    for (t in 1 : nTaskTimes) {

      if (is_missing[p, t, 1] == 0) {

        vector[nT_ppts[p, t]] v1;

        

        vector[nT_ppts[p, t]] v2;

        

        v1 = rewSens[p, t] * to_vector(rew1[p, t,  : ])

             - effSens[p, t] * to_vector(eff1[p, t,  : ]);

        

        v2 = rewSens[p, t] * to_vector(rew2[p, t,  : ])

             - effSens[p, t] * to_vector(eff2[p, t,  : ]);

        

        choice01[p, t,  : ] ~ bernoulli_logit(v2 - v1);

      }

    }

  }

  

  // self-report model: fit the probability of endorsement to the categorical endorsements 

  

  for (n in 1 : N) {

    for (t in 1 : nStateTimes) {

      Y[n, t] ~ bernoulli(prob[n, t]);

    }

  }

}

generated quantities {

  // log likelihood 

  

  array[N, nStateTimes] real loglik_y; // for state endorsements

  

  array[N, nStateTimes] int<lower=0, upper=1> y_rep; // posterior replicated state endorsements

  

  array[nPpts, nTaskTimes, nTrials_max] real post_pred_t; // posterior predictive dist for choices

  

  array[nPpts, nTaskTimes, nTrials_max] real log_lik; // log likelihood for each choice

  

  array[nPpts, nTaskTimes] real sum_log_lik; // sumLL for each game 

  

  // test-retest correlations (don't generate to save N parameters)

  

  corr_matrix[nTaskTimes] R_rewSens;

  

  corr_matrix[nTaskTimes] R_effSens;

  

  corr_matrix[nStateTimes] R_theta;

  

  // reconstruct correlation matrix from cholesky factors

  

  R_rewSens = R_chol_rewSens * R_chol_rewSens';

  

  R_effSens = R_chol_effSens * R_chol_effSens';

  

  R_theta = R_chol_theta * R_chol_theta';

  

  // generate likelihood and model predicted responses

  

  // self report: binary response from probability of endorsement

  

  for (n in 1 : N) {

    for (t in 1 : nStateTimes) {

      // initialise loglik array

      

      loglik_y[n, t] = 0;

      

      // log likelihood for the self-report response (only if data is available)

      

      if (is_missing_state[jj[n], ii[t]] == 0) {

        // note that you need jj and ii to index participants because of Y structure

        

        loglik_y[n, t] = bernoulli_lpmf(Y[n, t] | prob[n, t]);

      }

      

      y_rep[n, t] = bernoulli_rng(prob[n, t]);

    }

  }

  

  // add mean loglik state for missing states 

  

  for (n in 1 : N) {

    for (t in 1 : nStateTimes) {

      // log likelihood for the self-report response

      

      if (is_missing_state[jj[n], ii[t]] == 1) {

        loglik_y[n, t] = mean(loglik_y[ : , ii[t]]);

      }

    }

  }

  

  // generate posterior predictions and get likelihood

  

  for (p in 1 : nPpts) {

    for (t in 1 : nTaskTimes) {

      // define vectors of choice values at each timepoint

      

      vector[nTrials_max] v1;

      

      vector[nTrials_max] v2;

      

      // initialise post_pred_t array for missing value detectio

      

      post_pred_t[p, t,  : ] = rep_array(0.0, nTrials_max);

      

      // choice values for option 1 and 2 as linear discount 

      

      v1 = rewSens[p, t] * to_vector(rew1[p, t,  : ])

           - effSens[p, t] * to_vector(eff1[p, t,  : ]);

      

      v2 = rewSens[p, t] * to_vector(rew2[p, t,  : ])

           - effSens[p, t] * to_vector(eff2[p, t,  : ]);

      

      // only for data that is available, generate posterior predicted choices       

      

      if (is_missing[p, t, 1] == 0) {

        post_pred_t[p, t,  : ] = bernoulli_rng(inv_logit(v2 - v1));

      } // end available data clause 

      

      // unvectorised likelihood (bernoulli_logit_lpmf does not work vectorised?)

      

      for (trial in 1 : nTrials_max) {

        // initialise LL 

        

        log_lik[p, t, trial] = 0;

        

        // estimate log likelihood on real data trials only, don't on missing data since the choice01 is set to 1 and is not real data 

        

        if (is_missing[p, t, 1] == 0) {

          log_lik[p, t, trial] = bernoulli_logit_lpmf(choice01[p, t, trial] | v2[trial]

                                                                    - v1[trial]);

        }

      } // end of trials loop

    } // end of task / game loop

  } // end of subject loop

  

  // for missing data add the mean LL 

  

  for (p in 1 : nPpts) {

    for (t in 1 : nTaskTimes) {

      if (is_missing[p, t, 1] == 1) {

        for (trial in 1 : nTrials_max) {

          // mean LL for each trial

          

          real mean_LL;

          

          // get mean over all timepoints for that trial

          

          mean_LL = mean(to_matrix(log_lik[ : ,  : , trial])); // Calculate the mean log-likelihood for each trial

          

          log_lik[p, t, trial] = mean_LL;

        } // end of trial loop

      } // end of timepoint loop

      

      // vectorized sum log likelihood by task (nPpts x nTasks)

      

      sum_log_lik[p, t] = sum(to_vector(log_lik[p, t,  : ]));

    } // end of sessions loop

  } // end of subject loop

}

// end of generated params 


