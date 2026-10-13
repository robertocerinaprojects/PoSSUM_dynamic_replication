#include functions.stan

data{
  


  // DIMENSIONS OF THE DATA
  int<lower = 1> N; // number of observations
  int<lower = 2> J;
  array[N] int<lower=1, upper=J> V; // vote choice
  
  // identify obs which have a level 2 area 
  int<lower = 1> yes_area_N;
  array[yes_area_N] int<lower = 1> yes_area;
  int<lower = 1> no_area_N;
  array[no_area_N] int<lower = 1> no_area;



  // SPATIAL-COMPONENT DATA
  array[N] int area_id; // index of areas in the observed data
  int<lower=1> area_N; // no. of spatial units
  int<lower=1> kappa; // no. of separate inner-connected groups
  array[kappa] int group_size; // observational units per group
  array[area_N] int group_idx; // index of observations, ordered by group
  
  int<lower=1> N_edges; // number of adjacency instances
  array[N_edges] int<lower=1, upper=area_N> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=area_N> node2; // node1[i] < node2[i]
  array[area_N] int<lower=1, upper=kappa> comp_id; // ids of groups by areas
  
  vector[kappa] inv_sqrt_scaling_factor; // BYM2 scale factor, with singletons 



  // FIXED EFFECTS 
  array[J] int<lower=1> P;
  int<lower=1> P_sum ;
  int<lower=1> P_max ;
  matrix[N, P_sum] Z; // state- level covariate matrix
  array[J, P_max] int<lower = 1> Z_id; // state- level covariate matrix

  // AREA-IND INTERACTION EFFECTS 
  array[J] int<lower=1> P_INT;
  int<lower=1> P_INT_sum ;
  int<lower=1> P_INT_max ;
  matrix[N, P_INT_sum] X; 
  array[J, P_INT_max] int<lower = 1> X_id; 
  int<lower = 1> INT_N;
  array[INT_N] int<lower = 1> INT; 


  // DATA INDIVIDIAL-LEVEL EFFECTS
  array[N] int gen_id; // gender level id
  int<lower=1> gen_N; // number of gender levels 

  array[N] int eth_id; // ethnicity level id
  int<lower=1> eth_N; // number of ethnicity levels 

  array[N] int inc_id; // income level id
  int<lower=1> inc_N; // number of income levels 

  array[N] int age_id; // age level id
  int<lower=1> age_N; // number of age levels 

  array[N] int v20_id; // 2020 vote level id
  int<lower=1> v20_N; // number of 2020 vote levels 


  // DAY RANDOM EFFECT
  array[N] int poll_id; // identifier for temporally slated poll
  int<lower=1> poll_N; // number of polls


}

transformed data {

  //SPATIAL HYPER-PARAMETERS
  int<lower=0,upper=1> has_phi=1; // turn phi on to include unstructured random effect in BYM2 spec.

}

parameters {



  // CHOICE BASELINE
  vector[J-1] beta_s; // baseline rate of support



  // SPATIAL MODEL PARAMETERS
  matrix[area_N,J-1] phi; // unstructured area effect
  matrix[area_N,J-1] psi; // spatial (ICAR) area effect
  vector<lower=0,upper = 1>[J-1] omega; // mixing parameter for structured/unstructured area effects
  real<lower=0> lambda_sd; // sd for structured area effects



  // PARAMS INDIVIDIAL-LEVEL EFFECTS
  matrix[gen_N,J-1] xi_gen;
  vector<lower=0>[J-1] xi_gen_sd; 

  matrix[eth_N,J-1] xi_eth;
  vector<lower=0>[J-1] xi_eth_sd;

  matrix[inc_N,J-1] xi_inc;
  vector<lower=0>[J-1] xi_inc_sd;

  matrix[age_N,J-1] xi_age;
  vector<lower=0>[J-1] xi_age_sd; 

  matrix[v20_N,J-1] xi_v20;
  vector<lower=0>[J-1] xi_v20_sd; 


  // PARAMS FOR DTE EFFECTS
  matrix[poll_N,J-1] delta;
  vector<lower=0>[J-1] delta_sd; 


  // CONTEXT EFFECTS
  matrix[P_max,J-1] eta_s; 
  matrix[P_INT_max,J-1] xi_eta; 
  vector<lower=0>[J-1] xi_eta_sd; 


  // NO-AREA EFFECT
  vector[J-1] gamma_s; 



}


transformed parameters {



  // // // EXPECTED PREFERENCE LEVEL 
  matrix[N,J] mu_s; 



  // SPATIAL MODEL 
  matrix[area_N,J-1] lambda_s;



  // TRANSFORMED PARAMS INDIVIDIAL-LEVEL EFFECTS
  matrix[gen_N,J-1] xi_gen_s;
  matrix[eth_N,J-1] xi_eth_s;
  matrix[inc_N,J-1] xi_inc_s;
  matrix[age_N,J-1] xi_age_s;
  matrix[v20_N,J-1] xi_v20_s;  

  // AREA-IND INTERACTIONS
  matrix[P_INT_max,J-1] xi_eta_s; 

  // TRANSFORMED PARAMS DTE EFFETCS
  matrix[poll_N,J-1] delta_s;


  // LINEAR PREDICTOR
  // set first level of the nominal outcome to 0 - baseline category
  mu_s[1:N,1] = rep_vector(0,N);


  // start counting effects from the 2nd level of the nominal outcome
  for(j in 2:J){

    xi_eta_s[:,j-1] = xi_eta[:,j-1] * xi_eta_sd[j-1];

    xi_gen_s[:,j-1] = xi_gen[:,j-1] * xi_gen_sd[j-1];
    xi_eth_s[:,j-1] = xi_eth[:,j-1] * xi_eth_sd[j-1];
    xi_inc_s[:,j-1] = xi_inc[:,j-1] * xi_inc_sd[j-1];
    xi_age_s[:,j-1] = xi_age[:,j-1] * xi_age_sd[j-1];
    xi_v20_s[:,j-1] = xi_v20[:,j-1] * xi_v20_sd[j-1];

    delta_s[:,j-1] = delta[:,j-1] * delta_sd[j-1];

    lambda_s[:,j-1] = convolve_bym2(psi[:,j-1], phi[:,j-1], lambda_sd, area_N, kappa, group_size, group_idx, omega[j-1], inv_sqrt_scaling_factor);



    mu_s[yes_area,j] = 
      
      beta_s[j-1] + 
      
      delta_s[poll_id[yes_area],j-1] + 
      
      lambda_s[area_id[yes_area],j-1] +
      
      Z[yes_area,Z_id[j,1:P[j]]]*eta_s[1:P[j],j-1] + 

      xi_gen_s[gen_id[yes_area],j-1] + 
      xi_eth_s[eth_id[yes_area],j-1] + 
      xi_inc_s[inc_id[yes_area],j-1] + 
      xi_age_s[age_id[yes_area],j-1] + 
      xi_v20_s[v20_id[yes_area],j-1] ;



    mu_s[no_area,j] = 

      beta_s[j-1] + 

      delta_s[poll_id[no_area],j-1] + 

      gamma_s[j-1] + 

      xi_gen_s[gen_id[no_area],j-1] + 
      xi_eth_s[eth_id[no_area],j-1] + 
      xi_inc_s[inc_id[no_area],j-1] + 
      xi_age_s[age_id[no_area],j-1] + 
      xi_v20_s[v20_id[no_area],j-1] ;



  }


  for(j in INT){

      mu_s[yes_area,j+1] = 
        mu_s[yes_area,j+1] +
        X[yes_area,X_id[j+1,1:P_INT[j+1]]]*xi_eta_s[1:P_INT[j+1],j];

    }


}

model {



  // BASELINE PREFERENCES
  beta_s ~ std_normal();



  // SPATIAL PRIORS
  for(j in 1:(J-1)) {
    psi[,j] ~ icar_normal(lambda_sd,node1, node2, kappa, group_size, group_idx, has_phi);
    phi[,j] ~ std_normal();
    omega[j] ~ beta(0.5,0.5);
  }
  lambda_sd ~ std_normal();



  // NO-AREA FIXED EFFECT PRIOR
  gamma_s ~ std_normal();


  
  // CONTEXT FIXED EFFECT PRIORS
  to_vector(eta_s) ~ std_normal();


  to_vector(xi_eta) ~ std_normal();
  xi_eta_sd ~ std_normal();


  
  // GENDER EFFECT
  to_vector(xi_gen) ~ std_normal();
  xi_gen_sd ~ std_normal();

  // ETHNICITY EFFECT
  to_vector(xi_eth) ~ std_normal();
  xi_eth_sd ~ std_normal();

  // PAST VOTE EFFECTS
  to_vector(xi_v20) ~ std_normal();
  xi_v20_sd ~ std_normal();



  // AGE SWINGS IN PREFERENCES
  for(j in 1:(J-1)){
    sum(xi_age[,j]) ~ normal(0, 0.01 * age_N); // sum-to-0 constraint
      for(i in 2:age_N){ 
        xi_age[i,j] ~ normal(xi_age[i-1,j],1); 
    }
  }
  xi_age_sd ~ std_normal();

  

  // INCOME SWINGS IN PREFERENCES
  for(j in 1:(J-1)){
    sum(xi_inc[,j]) ~ normal(0, 0.01 * inc_N); // sum-to-0 constraint
      for(i in 2:inc_N){ 
        xi_inc[i,j] ~ normal(xi_inc[i-1,j],1); 
    }
  }
  xi_inc_sd ~ std_normal();



  // DAILY SWINGS IN PREFERENCES
  for(j in 1:(J-1)){
    sum(delta[,j]) ~ normal(0, 0.01 * poll_N); // sum-to-0 constraint
      for(i in 2:poll_N){ 
        delta[i,j] ~ normal(delta[i-1,j],1); 
    }
  }
  delta_sd ~ std_normal();



  //PREFERENCE LIKELIHOOD
  for (i in 1:N) V[i] ~ categorical_logit(mu_s[i,1:J]');
}
