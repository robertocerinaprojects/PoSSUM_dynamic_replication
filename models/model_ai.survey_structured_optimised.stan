// Equivalent Gaussian-prior parameterization plus cached data-only selections.
// The original priors, likelihood and posterior distribution are preserved.
// psi, xi_age and xi_inc remain available under their original output names.
// Sampling coordinates change to independent standard-normal raw effects.
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

  array[N] int inc_id; // education level id
  int<lower=1> inc_N; // number of education levels 

  array[N] int age_id; // age level id
  int<lower=1> age_N; // number of age levels 

  array[N] int v20_id; // 2020 vote level id
  int<lower=1> v20_N; // number of 2020 vote levels 


}

transformed data {

  // Cache data-only selections once, instead of rebuilding them at every
  // log-density/gradient evaluation. The active slices have the original
  // dimensions and order; padding is never included in a matrix product.
  array[yes_area_N] int area_yes = area_id[yes_area];
  array[yes_area_N] int gen_yes = gen_id[yes_area];
  array[yes_area_N] int eth_yes = eth_id[yes_area];
  array[yes_area_N] int inc_yes = inc_id[yes_area];
  array[yes_area_N] int age_yes = age_id[yes_area];
  array[yes_area_N] int v20_yes = v20_id[yes_area];
  array[no_area_N] int gen_no = gen_id[no_area];
  array[no_area_N] int eth_no = eth_id[no_area];
  array[no_area_N] int inc_no = inc_id[no_area];
  array[no_area_N] int age_no = age_id[no_area];
  array[no_area_N] int v20_no = v20_id[no_area];
  array[J] matrix[yes_area_N, P_max] Z_yes_cache;
  array[J] matrix[yes_area_N, P_INT_max] X_yes_cache;

  // The original spatial and ordered-effect priors are Gaussian, including
  // their soft sum-to-zero constraints. Build their exact precision matrices.
  matrix[area_N, area_N] Q_psi = rep_matrix(0, area_N, area_N);
  matrix[age_N, age_N] Q_age = rep_matrix(inv_square(0.01 * age_N), age_N, age_N);
  matrix[inc_N, inc_N] Q_inc = rep_matrix(inv_square(0.01 * inc_N), inc_N, inc_N);
  matrix[area_N, area_N] psi_transform;
  matrix[age_N, age_N] age_transform;
  matrix[inc_N, inc_N] inc_transform;
  int group_pos = 1;

  // Each neighbour difference contributes (psi[node1] - psi[node2])^2.
  for (e in 1:N_edges) {
    Q_psi[node1[e], node1[e]] += 1;
    Q_psi[node2[e], node2[e]] += 1;
    Q_psi[node1[e], node2[e]] -= 1;
    Q_psi[node2[e], node1[e]] -= 1;
  }
  // Preserve every component's original Normal(0, 0.001 * group_size)
  // constraint on its sum. This also preserves the singleton priors.
  for (g in 1:kappa) {
    for (a in group_pos:(group_pos + group_size[g] - 1)) {
      for (b in group_pos:(group_pos + group_size[g] - 1)) {
        Q_psi[group_idx[a], group_idx[b]] += inv_square(0.001 * group_size[g]);
      }
    }
    group_pos += group_size[g];
  }
  // Preserve the age and income adjacent-level Normal(previous, 1) priors.
  if (age_N > 1) {
    for (i in 2:age_N) {
      Q_age[i, i] += 1;
      Q_age[i - 1, i - 1] += 1;
      Q_age[i, i - 1] -= 1;
      Q_age[i - 1, i] -= 1;
    }
  }
  if (inc_N > 1) {
    for (i in 2:inc_N) {
      Q_inc[i, i] += 1;
      Q_inc[i - 1, i - 1] += 1;
      Q_inc[i - 1, i] -= 1;
      Q_inc[i, i - 1] -= 1;
    }
  }
  // If Q = L * L', then effect = inverse(L') * raw has precision Q.
  // These transforms depend only on data and are calculated once per run.
  psi_transform = mdivide_left_tri_low(cholesky_decompose(Q_psi),
                    diag_matrix(rep_vector(1.0, area_N)))';
  age_transform = mdivide_left_tri_low(cholesky_decompose(Q_age),
                    diag_matrix(rep_vector(1.0, age_N)))';
  inc_transform = mdivide_left_tri_low(cholesky_decompose(Q_inc),
                    diag_matrix(rep_vector(1.0, inc_N)))';

  for (j in 1:J) {
    Z_yes_cache[j] = rep_matrix(0, yes_area_N, P_max);
    X_yes_cache[j] = rep_matrix(0, yes_area_N, P_INT_max);
  }
  // D is the reference outcome: its unused predictor block stays unused.
  for (j in 2:J) {
    Z_yes_cache[j, :, 1:P[j]] = Z[yes_area, Z_id[j, 1:P[j]]];
  }
  for (j in INT) {
    X_yes_cache[j + 1, :, 1:P_INT[j + 1]] =
      X[yes_area, X_id[j + 1, 1:P_INT[j + 1]]];
  }

}

parameters {



  // CHOICE BASELINE
  vector[J-1] beta_s; // baseline rate of support



  // SPATIAL MODEL PARAMETERS
  matrix[area_N,J-1] phi; // unstructured area effect
  matrix[area_N,J-1] psi_raw; // whitened spatial (ICAR) coordinates
  vector<lower=0,upper = 1>[J-1] omega; // mixing parameter for structured/unstructured area effects
  real<lower=0> lambda_sd; // sd for structured area effects



  // PARAMS INDIVIDIAL-LEVEL EFFECTS
  matrix[gen_N,J-1] xi_gen;
  vector<lower=0>[J-1] xi_gen_sd; 

  matrix[eth_N,J-1] xi_eth;
  vector<lower=0>[J-1] xi_eth_sd;

  matrix[inc_N,J-1] xi_inc_raw;
  vector<lower=0>[J-1] xi_inc_sd;

  matrix[age_N,J-1] xi_age_raw;
  vector<lower=0>[J-1] xi_age_sd; 

  matrix[v20_N,J-1] xi_v20;
  vector<lower=0>[J-1] xi_v20_sd; 


  // CONTEXT EFFECTS
  matrix[P_max,J-1] eta_s; 
  matrix[P_INT_max,J-1] xi_eta; 
  vector<lower=0>[J-1] xi_eta_sd; 


  // NO-AREA EFFECT
  vector[J-1] gamma_s; 



}


transformed parameters {

  // Reconstruct the original effects. Their joint prior distribution is
  // exactly the original Gaussian prior; no sum constraint is tightened.
  matrix[area_N, J-1] psi = psi_transform * psi_raw;
  matrix[inc_N, J-1] xi_inc = inc_transform * xi_inc_raw;
  matrix[age_N, J-1] xi_age = age_transform * xi_age_raw;



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

    lambda_s[:,j-1] = convolve_bym2(psi[:,j-1], phi[:,j-1], lambda_sd, area_N, kappa, group_size, group_idx, omega[j-1], inv_sqrt_scaling_factor);



    mu_s[yes_area,j] = 
      
      beta_s[j-1] + 
            
      lambda_s[area_yes,j-1] +
      
      Z_yes_cache[j, :, 1:P[j]]*eta_s[1:P[j],j-1] + 

      xi_gen_s[gen_yes,j-1] + 
      xi_eth_s[eth_yes,j-1] + 
      xi_inc_s[inc_yes,j-1] + 
      xi_age_s[age_yes,j-1] + 
      xi_v20_s[v20_yes,j-1] ;



    mu_s[no_area,j] = 

      beta_s[j-1] + 

      gamma_s[j-1] + 

      xi_gen_s[gen_no,j-1] + 
      xi_eth_s[eth_no,j-1] + 
      xi_inc_s[inc_no,j-1] + 
      xi_age_s[age_no,j-1] + 
      xi_v20_s[v20_no,j-1] ;



  }


  for(j in INT){

      mu_s[yes_area,j+1] = 
        mu_s[yes_area,j+1] +
        X_yes_cache[j+1, :, 1:P_INT[j+1]]*xi_eta_s[1:P_INT[j+1],j];

    }


}

model {



  // BASELINE PREFERENCES
  beta_s ~ std_normal();



  // SPATIAL PRIORS
  for(j in 1:(J-1)) {
    psi_raw[,j] ~ std_normal();
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
  to_vector(xi_age_raw) ~ std_normal();
  xi_age_sd ~ std_normal();

  

  // INCOME SWINGS IN PREFERENCES
  to_vector(xi_inc_raw) ~ std_normal();
  xi_inc_sd ~ std_normal();





  //PREFERENCE LIKELIHOOD
  for (i in 1:N) V[i] ~ categorical_logit(mu_s[i,1:J]');
}

