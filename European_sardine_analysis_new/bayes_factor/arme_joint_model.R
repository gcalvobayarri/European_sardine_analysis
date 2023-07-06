# parece que pasa algo con log_likeod, quiza hay que probar 1 iteracion
load('./cpo_joint_log/industrial_artisanal_matrix_nonas_arme.RData')
library("bridgesampling")
library("BayesFactor")
library("R2jags")
library("rstan")
library("sm")
# install.packages("mvtnorm")
library(mvtnorm)
library(mnormt)


#-------------------------------------------------------------------------------
# fit models
#-------------------------------------------------------------------------------
set.seed(72188)

jags_H3 <- jags(data = list(T = 34, Y1 = log(data_industrial), 
                            Y2 = log(data_artisanal)),
                parameters.to.save = c( "beta01", "beta02", "beta11", "beta12", 
                                        "b0", "b1", "b0Albania", "b0Bosnia", 
                                        "b1Albania", "b1Bosnia", "sigma", "sigma0", 
                                        "sigma1", "rho0", "rho1", "w1", "w2", 
                                        "rho", "sigmau"),
                model.file = "./models/autoregressive_and_me_joint.txt", 
                n.chains = 3,
                n.iter = 5000000, n.burnin = 1000000, n.thin = 500)

#-------------------------------------------------------------------------------
# specify unnormalized log posterior functions
#-------------------------------------------------------------------------------

log_posterior_H3 <- function(pars, data) {
  
  beta0I <- pars["beta01"]           # extract parameter
  beta0A <- pars["beta02"]           # extract parameter
  beta1I <- pars["beta11"]           # extract parameter
  beta1A <- pars["beta12"]           # extract parameter
  sigmaI <- pars["sigma[1]"]         # extract parameter
  sigmaA <- pars["sigma[2]"]         # extract parameter
  sigma0I <- pars["sigma0[1]"]       # extract parameter
  sigma0A <- pars["sigma0[2]"]       # extract parameter
  sigma1I <- pars["sigma1[1]"]       # extract parameter
  sigma1A <- pars["sigma1[2]"]       # extract parameter
  sigmauI <- pars["sigmau[1]"]       # extract parameter
  sigmauA <- pars["sigmau[2]"]       # extract parameter
  rho0 <- pars["rho0"]               # extract parameter
  rho1 <- pars["rho1"]               # extract parameter
  rhoI <- pars["rho[1]"]               # extract parameter
  rhoA <- pars["rho[2]"]               # extract parameter
  
  b0I <- c()
  b0A <- c()
  b1I <- c()
  b1A <- c()
  for(n in 1 :10){
    b0I[n] <- pars[paste('b0[',n,',1]', sep = '')]
    b0A[n] <- pars[paste('b0[',n,',2]', sep = '')]
    
    b1I[n] <- pars[paste('b1[',n,',1]', sep = '')]
    b1A[n ] <- pars[paste('b1[',n,',2]', sep = '')]
  }
  
  b0Al <- pars["b0Albania"]
  b0Bo <- pars["b0Bosnia"]
  
  b1Al <- pars["b1Albania"]
  b1Bo <- pars["b1Bosnia"]
  
  wI <- matrix(data = NA, nrow = 11, ncol = 34)
  wA <- matrix(data = NA, nrow = 11, ncol = 34)
  for(n in 1 : 11){
    for(t in 1 : 34){
      wI[n, t] <- pars[paste('w1[',n,',',t,']', sep = '')]
      wA[n, t] <- pars[paste('w2[',n,',',t,']', sep = '')]
    }
  }
  
  # prior
  prior <- dunif(sigmaI, 0, 100, log = T) + dunif(sigmaA, 0, 100, log = T)+
    dnorm(beta0I, 0, 10, log = T) + dnorm(beta0A, 0, 10, log = T) +
    dnorm(beta1I, 0, 10, log = T) + dnorm(beta1A, 0, 10, log = T) +
    dunif(sigma0I, 0, 100, log = T) + dunif(sigma0A, 0, 100, log = T) +
    dunif(sigma1I, 0, 100, log = T) + dunif(sigma1A, 0, 100, log = T) +
    dunif(sigmauI, 0, 100, log = T) + dunif(sigmauA, 0, 100, log = T) +
    dunif(rho0, -1, 1, log = T) + dunif(rho1, -1, 1, log = T) +
    dunif(rhoI, -1, 1, log = T) + dunif(rhoA, -1, 1, log = T)
  
  # random effects
  r_effects <- 0
  for(n in 1 : 10){
    r_effects <- r_effects + dmvnorm(x = cbind(b0I[n], b0A[n]), rep(0,2), 
                                     sigma = matrix(data = c(sigma0I^2,
                                                             rho0*sigma0I*sigma0A,
                                                             rho0*sigma0I*sigma0A,
                                                             sigma0A^2), 
                                                    nrow = 2,
                                                    byrow = T),
                                     log = T)+
      dmvnorm(x = cbind(b1I[n], b1A[n]), rep(0,2), 
              sigma = matrix(data = c(sigma1I^2,
                                      rho1*sigma1I*sigma1A,
                                      rho1*sigma1I*sigma1A,
                                      sigma1A^2), 
                             nrow = 2,
                             byrow = T),
              log = T)
  }
  r_effects <- r_effects + dnorm(b0Al, 0, sd=sigma0I, log = T) + 
    dnorm(b1Al, 0, sd=sigma1I, log = T) + # Albania
    dnorm(b0Bo, 0, sd=sigma0A, log = T) + 
    dnorm(b1Bo, 0, sd=sigma1A, log = T) # Bosnia
  
  for(n in 1 : 11){
    r_effects <- r_effects + dnorm(wI[n, 1], 0, sd=sigmauI, log = T) +
      dnorm(wA[n, 1], 0, sd=sigmauA, log = T)
    for(t in 2 : 34){
      r_effects <- r_effects + dnorm(wI[n, t], rhoI * wI[n, t-1], sd=sigmauI, log = T) +
        dnorm(wA[n, t], rhoA * wA[n, t-1], sd=sigmauA, log = T)
    }
  }
  
  # likelihood
  log_likelihood <- 0
  for(n in 1 : 10){
    X <- matrix(c(rep(1, 34), 0:33), ncol = 2, byrow = F)
    Z <- matrix(c(rep(1, 34), 0:33), ncol = 2, byrow = F)
    
    log_likelihood <- log_likelihood + 
      dmnorm(data$yI[n,], 
             mean = as.vector((X) %*% t(t(c(beta0I, beta1I)))) +
               as.vector(Z %*% t(t(c(b0I[n], b1I[n])))) + as.vector(wI[n,]),
             varcov = diag(sigmaI^2, 34),
             log = T) +
      dmnorm(data$yA[n,], 
             mean = as.vector((X) %*% t(t(c(beta0A, beta1A)))) +
               as.vector(Z %*% t(t(c(b0A[n], b1A[n])))) + as.vector(wA[n,]),
             varcov = diag(sigmaA^2, 34),
             log = T)
    
    # log_likelihood <- log_likelihood + 
    #   sum(dnorm(data$yI[n,], 
    #         mean = beta0I + b0I[n] + (beta1I + b1I[n]) * (0:33),
    #         sd = sigmaI,
    #         log = T)) +
    #   sum(dnorm(data$yA[n,], 
    #         mean = beta0A + b0A[n] + (beta1A + b1A[n]) * (0:33),
    #         sd = sigmaA,
    #         log = T))
  }
  log_likelihood <- log_likelihood +
    dmnorm(data$yI[11,], 
           mean = as.vector((X) %*% t(t(c(beta0I, beta1I)))) +
             as.vector(Z %*% t(t(c(b0Al, b1Al)))) + wI[11,],
           varcov = diag(sigmaI^2, 34),
           log = T) +
    dmnorm(data$yA[11,], 
           mean = as.vector((X) %*% t(t(c(beta0A, beta1A)))) +
             as.vector(Z %*% t(t(c(b0Bo, b1Bo)))) + wA[11,],
           varcov = diag(sigmaA^2, 34),
           log = T)
  
  out <- prior + r_effects + log_likelihood
  
  return(out)
  
}

#-------------------------------------------------------------------------------
# specify lower and upper bounds for the parameters
#-------------------------------------------------------------------------------

dimnames(jags_H3$BUGSoutput$sims.array)[[3]][-49]

lb_H3 <- rep(-Inf, 808)
ub_H3 <- rep(Inf, 808)
names(lb_H3) <- names(ub_H3) <- 
  dimnames(jags_H3$BUGSoutput$sims.array)[[3]][-49]
lb_H3[["sigma[1]"]] <- 0
ub_H3[["sigma[1]"]] <- 100
lb_H3[["sigma0[1]"]] <- 0
ub_H3[["sigma0[1]"]] <- 100
lb_H3[["sigma[2]"]] <- 0
ub_H3[["sigma[2]"]] <- 100
lb_H3[["sigma0[2]"]] <- 0
ub_H3[["sigma0[2]"]] <- 100
lb_H3[["sigma1[1]"]] <- 0
ub_H3[["sigma1[1]"]] <- 100
lb_H3[["sigma1[2]"]] <- 0
ub_H3[["sigma1[2]"]] <- 100

lb_H3[["rho0"]] <- -1
ub_H3[["rho0"]] <- 1
lb_H3[["rho1"]] <- -1
ub_H3[["rho1"]] <- 1

lb_H3[["rho[1]"]] <- -1
ub_H3[["rho[1]"]] <- 1
lb_H3[["rho[2]"]] <- -1
ub_H3[["rho[2]"]] <- 1


#-------------------------------------------------------------------------------
# compute log marginal likelihoods
#-------------------------------------------------------------------------------

bridge_H3 <- bridge_sampler(samples = jags_H3,
                            log_posterior = log_posterior_H3,
                            data = list(yI = log(data_industrial), 
                                        yA = log(data_artisanal)),
                            lb = lb_H3, ub = ub_H3, verbose = T, 
                            method = 'warp3', repetitions = 10)
summary(bridge_H3)

marginal_likelihood_H3 <- bridge_H3$logml

save(marginal_likelihood_H3, 
     file = './bayes_factor/marginal_likelihood_H3.RData')

