# 10. 4. 1 Ntzoufras CPO
set.seed(459736)
library(rjags)
library(mnormt)
source('./cpo_joint_log/ar_me_joint/draws_v2.R') #saved in draws_posterior

load('./cpo_joint_log/industrial_artisanal_matrix_nonas_arme.RData')


# last one Bosnia
#-------------------

source('./functions/suma_logaritmica_de_elementos.R') #function



# 1st calculate ppo_i 10.3.4

log_cpo_i <- function(datamI, datamA, draws, i, log = TRUE) { 
  
  X <- matrix(c(rep(1, dim(datamI)[2]), 0 : (dim(datamI)[2] - 1)), nrow = 2, byrow = T)
  Z <- matrix(c(rep(1, dim(datamI)[2]), 0 : (dim(datamI)[2] - 1)), nrow = 2, byrow = T)
  
  
  
  log_likelihood <- c()
  for(l in 1 : length(draws[, 1])){
    
    SigmaI <- matrix(data = NA, nrow = 34, ncol = 34)
    SigmaA <- matrix(data = NA, nrow = 34, ncol = 34)
    for(t in 1 : 34){
      for(s in 1 : 34){
        SigmaI[t, s] <- draws[l, 5]^abs(t - s) * draws[l, 53]^2 *
          sum(draws[l, 5]^(0 : (min(s, t) - 1)))
        
        SigmaA[t, s] <- draws[l, 6]^abs(t - s) * draws[l, 54]^2 *
          sum(draws[l, 6]^(0 : (min(s, t) - 1)))
      }
    }
    
    SigmaI <- SigmaI + diag(34) * draws[l, 7]^2
    SigmaA <- SigmaA + diag(34) * draws[l, 8]^2
    
    log_likelihood[l] <- dmnorm(x = datamI[i,], 
                                mean = as.vector(t(X) %*% matrix(c(draws[l, 1], draws[l, 3]))) + 
                                  as.vector(t(Z) %*% matrix(c(draws[l, 8 + i], draws[l, 30 + i]))), 
                                varcov = SigmaI, log = log) +
      dmnorm(x = datamA[i,], 
             mean = as.vector(t(X) %*% matrix(c(draws[l, 2], draws[l, 4]))) + 
               as.vector(t(Z) %*% matrix(c(draws[l, 19 + i], draws[l, 41 + i]))), 
             varcov = SigmaA, log = log)
  }
  # print(log_likelihood)
  # log_cpo <- - log_likelihood
  inv_loglik <- - log_likelihood
  # return(inv_loglik)
  
  mean_inv_loglik <- - log(dim(draws)[1]) + suma_logaritmica_de_elementos(inv_loglik)
  
  log_cpo <- - mean_inv_loglik
  
  return(log_cpo)
}

log_cpo <- c()
for(s in 1 : dim(data_artisanal)[1]){
  log_cpo[s] <- log_cpo_i(datamI =  log(data_industrial), 
                          datamA =  log(data_artisanal), 
                          draws =  posterior_draws, 
                          i = s)
}

log_cpo_arme_joint <- log_cpo
save(log_cpo_arme_joint, file = './cpo_joint_log/ar_me_joint/log_cpo_arme_joint2.RData')

