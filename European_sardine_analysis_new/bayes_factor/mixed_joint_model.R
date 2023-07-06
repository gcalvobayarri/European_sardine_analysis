# source('./analysis_from_1985/data/matricial_data.R') # NAs in artisanal
load('./cpo_joint_log/industrial_artisanal_matrix_nonas_mixed.RData')
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

set.seed(12345)

jags_H1 <- jags(data = list(T = 34, Y1 = log(data_industrial), 
                            Y2 = log(data_artisanal)),
                parameters.to.save = c( "beta01", "beta02", "beta11", "beta12", 
                                        "b0", "b1", "b0Albania", "b0Bosnia", 
                                        "b1Albania", "b1Bosnia", "sigma", "sigma0", 
                                        "sigma1", "rho0", "rho1"),
                model.file = "./models/mixed_joint.txt", 
                n.chains = 3,
                n.iter = 5000000, n.burnin = 1000000, n.thin = 500)

#-------------------------------------------------------------------------------
# specify unnormalized log posterior functions
#-------------------------------------------------------------------------------

log_posterior_H1 <- function(pars, data) {
  
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
  rho0 <- pars["rho0"]               # extract parameter
  rho1 <- pars["rho1"]               # extract parameter
  
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
  
  # prior
  prior <- dunif(sigmaI, 0, 100, log = T) + dunif(sigmaA, 0, 100, log = T)+
    dnorm(beta0I, 0, 10, log = T) + dnorm(beta0A, 0, 10, log = T) +
    dnorm(beta1I, 0, 10, log = T) + dnorm(beta1A, 0, 10, log = T) +
    dunif(sigma0I, 0, 100, log = T) + dunif(sigma0A, 0, 100, log = T) +
    dunif(sigma1I, 0, 100, log = T) + dunif(sigma1A, 0, 100, log = T) +
    dunif(rho0, -1, 1, log = T) + dunif(rho1, -1, 1, log = T)
  
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
  
  # likelihood
  log_likelihood <- 0
  for(n in 1 : 10){
    X <- matrix(c(rep(1, 34), 0:33), ncol = 2, byrow = F)
    Z <- matrix(c(rep(1, 34), 0:33), ncol = 2, byrow = F)
    
    log_likelihood <- log_likelihood + 
      dmnorm(data$yI[n,], 
             mean = as.vector((X) %*% t(t(c(beta0I, beta1I)))) +
               as.vector(Z %*% t(t(c(b0I[n], b1I[n])))),
             varcov = diag(sigmaI^2, 34),
             log = T) +
      dmnorm(data$yA[n,], 
             mean = as.vector((X) %*% t(t(c(beta0A, beta1A)))) +
               as.vector(Z %*% t(t(c(b0A[n], b1A[n])))),
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
           as.vector(Z %*% t(t(c(b0Al, b1Al)))),
         varcov = diag(sigmaI^2, 34),
         log = T) +
    dmnorm(data$yA[11,], 
           mean = as.vector((X) %*% t(t(c(beta0A, beta1A)))) +
             as.vector(Z %*% t(t(c(b0Bo, b1Bo)))),
           varcov = diag(sigmaA^2, 34),
           log = T)
  
  out <- prior + r_effects + log_likelihood
  
  return(out)
  
}

#-------------------------------------------------------------------------------
# specify lower and upper bounds for the parameters
#-------------------------------------------------------------------------------

dimnames(jags_H1$BUGSoutput$sims.array)[[3]][-49]

lb_H1 <- rep(-Inf, 56)
ub_H1 <- rep(Inf, 56)
names(lb_H1) <- names(ub_H1) <- 
  dimnames(jags_H1$BUGSoutput$sims.array)[[3]][-49]
lb_H1[["sigma[1]"]] <- 0
ub_H1[["sigma[1]"]] <- 100
lb_H1[["sigma0[1]"]] <- 0
ub_H1[["sigma0[1]"]] <- 100
lb_H1[["sigma[2]"]] <- 0
ub_H1[["sigma[2]"]] <- 100
lb_H1[["sigma0[2]"]] <- 0
ub_H1[["sigma0[2]"]] <- 100
lb_H1[["sigma1[1]"]] <- 0
ub_H1[["sigma1[1]"]] <- 100
lb_H1[["sigma1[2]"]] <- 0
ub_H1[["sigma1[2]"]] <- 100

lb_H1[["rho0"]] <- -1
ub_H1[["rho0"]] <- 1
lb_H1[["rho1"]] <- -1
ub_H1[["rho1"]] <- 1


#-------------------------------------------------------------------------------
# compute log marginal likelihoods
#-------------------------------------------------------------------------------

bridge_H1 <- bridge_sampler(samples = jags_H1,
                            log_posterior = log_posterior_H1,
                            data = list(yI = log(data_industrial), 
                                                      yA = log(data_artisanal)),
                            lb = lb_H1, ub = ub_H1, verbose = T, 
                            method = 'warp3', repetitions = 10)
summary(bridge_H1)

marginal_likelihood_H1 <- bridge_H1$logml

save(marginal_likelihood_H1, 
     file = './bayes_factor/marginal_likelihood_H1.RData')
