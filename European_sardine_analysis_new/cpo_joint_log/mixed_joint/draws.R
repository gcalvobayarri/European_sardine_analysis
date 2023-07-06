library(rjags)
library(MCMCvis)

load('./results/res_mixed_jointlog.RData')

I <- length(unlist(rsamps_mixed_joint[1][, 1]))
C <- 3
n <- 10 #+1

# summary(rsamps_mixed_joint)$statistics

b0I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b0A <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1A <- matrix(data = NA, nrow = n + 1, ncol = C * I)


beta0I <- as.vector(unlist(MCMCchains(rsamps_mixed_joint,
                                      params = "beta01", ISB = F)))
beta0A <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                      params = "beta02", ISB = F)))

beta1I <- as.vector(unlist(MCMCchains(rsamps_mixed_joint,
                                      params = "beta11", ISB = F)))
beta1A <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                      params = "beta12", ISB = F)))

sigmaI <- as.vector(unlist(MCMCchains(rsamps_mixed_joint,
                                      params = paste('sigma[', 1, ']', sep = ''), 
                                      ISB = F)))
sigmaA <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                      params = paste('sigma[', 2, ']', sep = ''), 
                                      ISB = F)))

for(i in 1 : n){
  b0I[i, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                          params = paste("b0","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b0A[i, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                          params = paste("b0","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  b1I[i, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                          params = paste("b1","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b1A[i, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                          params = paste("b1","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  
}

# Albania
b0I[11, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                         params = 'b0Albania', 
                                         ISB = F)))
#Bosnia
b0A[11, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                         params = 'b0Bosnia', 
                                         ISB = F)))
#Albania
b1I[11, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                         params = 'b1Albania', 
                                         ISB = F)))
#Bosnia
b1A[11, ] <- as.vector(unlist(MCMCchains(rsamps_mixed_joint, 
                                         params = 'b1Bosnia', 
                                         ISB = F)))



posterior_draws <- data.frame(beta0I, beta0A, beta1I, beta1A,
                              sigmaI, sigmaA, b0I = t(b0I), 
                              b0A = t(b0A), b1I = t(b1I), b1A = t(b1A))

# b0I columns 7 - 17 (17 Albania)
# b0A columns 18 - 28 (28 Bosnia)
# b1I columns 29 - 39 (39 Albania)
# b1A columns 40 - 50 (50 Bosnia)
rm(beta0I, beta0A, beta1I, beta1A, sigmaI, sigmaA, b0I, b0A, b1I, 
   b1A, C, i, I, n, 
   rsamps_mixed_joint)
