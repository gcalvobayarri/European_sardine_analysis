# Computing missing data ar model
set.seed(928955)
# 1. Loading datata----------------
load('./data/industrial_artisanal_tonnes.RData')

# matrix
N <- 11
Y <- 34

# Industrial
BBB <- as.character(mediterranean_countries_industrial_tonnes$country)
BBB <- as.factor(BBB)
country_id <- as.numeric(BBB)


mediterranean_countries_industrial_tonnes <- 
  data.frame(mediterranean_countries_industrial_tonnes, country_id)
rm(BBB, country_id)

data_industrial <- matrix(rep(NA, N*Y), nrow = N)
for( i in 1 : length(mediterranean_countries_industrial_tonnes[,1])){ 
  data_industrial[mediterranean_countries_industrial_tonnes[i, 4], 
                  mediterranean_countries_industrial_tonnes[i, 3] - 1984] <- 
    mediterranean_countries_industrial_tonnes[i, 1]
}

data_industrial <- rbind(data_industrial[2 : 11,], data_industrial[1,])
# last one Albania


# Artisanal
BBB <- as.character(mediterranean_countries_artisanal_tonnes$country)
BBB <- as.factor(BBB)
country_id <- as.numeric(BBB)

mediterranean_countries_artisanal_tonnes <- 
  data.frame(mediterranean_countries_artisanal_tonnes, country_id)
rm(BBB, country_id)

data_artisanal <- matrix(rep(NA, N*Y), nrow = N)
for( i in 1 : length(mediterranean_countries_artisanal_tonnes[,1])){ 
  data_artisanal[mediterranean_countries_artisanal_tonnes[i, 4], 
                 mediterranean_countries_artisanal_tonnes[i, 3] - 1984] <- 
    mediterranean_countries_artisanal_tonnes[i, 1]
}

data_artisanal <- rbind(data_artisanal[c(1, 3 : 11), ], 
                        data_artisanal[2, ])

rm(mediterranean_countries_artisanal_tonnes, 
   mediterranean_countries_industrial_tonnes, i
   )


# 2. Loading posterior distribution-----------------------

library(rjags)
library(MCMCvis)

load('./results/res_autoregressive_jointv2log.RData')

I <- length(unlist(rsamps_ar1_joint_tonnesv2[1][, 1]))
C <- 3
n <- 10 #+1

# summary(rsamps_ar1_joint_tonnesv2)$statistics

b0I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b0A <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1I <- matrix(data = NA, nrow = n + 1, ncol = C * I)
b1A <- matrix(data = NA, nrow = n + 1, ncol = C * I)

beta0I <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2,
                                      params = "beta01", ISB = F)))
beta0A <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                      params = "beta02", ISB = F)))

beta1I <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2,
                                      params = "beta11", ISB = F)))
beta1A <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                      params = "beta12", ISB = F)))

rhoI <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2,
                                    params = paste('rho[', 1, ']', sep = ''), 
                                    ISB = F)))
rhoA <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                    params = paste('rho[', 2, ']', sep = ''), 
                                    ISB = F)))

sigmaI <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2,
                                      params = paste('sigma[', 1, ']', sep = ''), 
                                      ISB = F)))
sigmaA <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                      params = paste('sigma[', 2, ']', sep = ''), 
                                      ISB = F)))

for(i in 1 : n){
  b0I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                          params = paste("b0","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b0A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                          params = paste("b0","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  b1I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                          params = paste("b1","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b1A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                          params = paste("b1","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  
}

# Albania
b0I[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                         params = 'b0Albania', 
                                         ISB = F)))
#Bosnia
b0A[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                         params = 'b0Bosnia', 
                                         ISB = F)))
#Albania
b1I[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                         params = 'b1Albania', 
                                         ISB = F)))
#Bosnia
b1A[11, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_joint_tonnesv2, 
                                         params = 'b1Bosnia', 
                                         ISB = F)))

# 3. Computing missing data (posterior mean)--------------------------

for(i in 1 : N){
  # Industrial
  if(is.na(data_industrial[i, 1])){
    data_industrial[i, 1] <- exp(rnorm(1,
      mean = mean(beta0I) + mean(b0I[i,]), sd = mean(sigmaI)))
  }
  
  # Artisanal
  if(is.na(data_artisanal[i, 1])){
    data_artisanal[i, 1] <- exp(rnorm(1,
      mean = mean(beta0A) + mean(b0A[i,]), sd = mean(sigmaA)))
  }
  
  for(t in 1 : (Y-1)){
    # Industrial
    if(is.na(data_industrial[i, t + 1])){
      data_industrial[i, t +  1] <- exp(rnorm(1,
        mean = mean(beta0I) + mean(b0I[i,]) + (mean(beta1I) + mean(b1I[i,])) * t + mean(rhoI) * 
            (log(data_industrial[i, t]) - (mean(beta0I) + mean(b0I[i,]) + (mean(beta1I) + mean(b1I[i,])) * (t - 1))), 
        sd = mean(sigmaI)))
    }
    
    # Artisanal
    if(is.na(data_artisanal[i, t + 1])){
      data_artisanal[i, t +  1] <- exp(rnorm(1,
        mean = mean(beta0A) + mean(b0A[i,]) + (mean(beta1A) + mean(b1A[i,])) * t + mean(rhoA) * 
            (log(data_artisanal[i, t]) - (mean(beta0A) + mean(b0A[i,]) + (mean(beta1A) + mean(b1A[i,])) * (t - 1))), 
        sd = mean(sigmaA)))
    }
    
  }
}

save(data_industrial, data_artisanal, 
     file = './cpo_joint_log/industrial_artisanal_matrix_nonas_ar.RData')
