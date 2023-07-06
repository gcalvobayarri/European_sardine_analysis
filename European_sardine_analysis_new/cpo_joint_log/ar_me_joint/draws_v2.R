library(rjags)
library(MCMCvis)

load('./results/res_ar1_me_jointlog.RData')

I <- length(unlist(rsamps_ar1_me_joint_tonnes[1][, 1]))
C <- 3
n <- 10 #+1

# summary(rsamps_ar1_me_joint_tonnes)$statistics

b0I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b0A <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1A <- matrix(data = NA, nrow = n + 1, ncol = C * I)



beta0I <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                      params = "beta01", ISB = F)))
beta0A <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                      params = "beta02", ISB = F)))

beta1I <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                      params = "beta11", ISB = F)))
beta1A <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                      params = "beta12", ISB = F)))

rhoI <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                    params = paste('rho[', 1, ']', sep = ''), 
                                    ISB = F)))
rhoA <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                    params = paste('rho[', 2, ']', sep = ''), 
                                    ISB = F)))

sigmaI <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                      params = paste('sigma[', 1, ']', sep = ''), 
                                      ISB = F)))
sigmaA <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                      params = paste('sigma[', 2, ']', sep = ''), 
                                      ISB = F)))

sigmauI <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                      params = paste('sigmau[', 1, ']', sep = ''), 
                                      ISB = F)))
sigmauA <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                      params = paste('sigmau[', 2, ']', sep = ''), 
                                      ISB = F)))

for(i in 1 : n){
  b0I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b0","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b0A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b0","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  b1I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b1","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b1A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b1","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  
}

# Albania
b0I[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                         params = 'b0Albania', 
                                         ISB = F)))
#Bosnia
b0A[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                         params = 'b0Bosnia', 
                                         ISB = F)))
#Albania
b1I[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                         params = 'b1Albania', 
                                         ISB = F)))
#Bosnia
b1A[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                         params = 'b1Bosnia', 
                                         ISB = F)))

posterior_draws <- data.frame(beta0I, beta0A, beta1I, beta1A, rhoI, rhoA,
                              sigmaI, sigmaA, b0I = t(b0I), 
                              b0A = t(b0A), b1I = t(b1I), b1A = t(b1A), 
                              sigmauI, sigmauA)

# b0I columns 9 - 19 (19 Albania)
# b0A columns 20 - 30 (30 Bosnia)
# b1I columns 31 - 41 (41 Albania)
# b1A columns 42 - 52 (52 Bosnia)
rm(beta0I, beta0A, beta1I, beta1A, rhoI, rhoA, sigmaI, sigmaA, sigmauI, sigmauA, 
   b0I, b0A, b1I, b1A, C, i, I, n, t, rsamps_ar1_me_joint_tonnes)
