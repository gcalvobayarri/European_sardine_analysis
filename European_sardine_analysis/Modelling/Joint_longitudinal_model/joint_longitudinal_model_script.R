# Joint longitudinal model

# 1. Loading data and packages--------

load("./Data/matrices/data_industrial_fishing.RData")
load("./Data/matrices/data_artisanal_fishing.RData")
library(mcmcplots)
library(rjags)

# 2. Model--------------

X1 <- rbind(log(data_industrial_fishing[c(-1, -10),]), 
            log(data_industrial_fishing[c(1, 10),])) # + Albania & Spain 

X2 <- rbind(log(data_artisanal_fishing[-2,]), 
            log(data_artisanal_fishing[2,])) # + bosnia herzegovina

data_joint_model <- list(T = 45, Y1 = X1, Y2 =X2, N = 9)

inits <- function() {
  list( beta01 = runif(1, -7, 9), beta02 = runif(1, -4, 6), 
        sigma = runif(1, 0, 10), I01_sigma = runif(1, 0, 10), 
        I02_sigma = runif(1, 0, 10), T11_sigma = runif(1, 0, 10), 
        T12_sigma = runif(1, 0, 10))
}

parameters <- c("beta01", "beta02", "b0", "b1", "sigma", "I01_sigma", 
                "I02_sigma", "T11_sigma", "T12_sigma", "nu_0", 
                "nu_1", "b0Albania", "b0Espana", "b0Bosnia", 
                "b1Albania", "b1Espana", "b1Bosnia")


result <- jags.model("./Modelling/Joint_longitudinal_model/joint_longitudinal_model_jags.txt", 
                     data_joint_model, inits, n.chains = 3,
                     n.adapt = 0)

update(result, n.iter = 600000)


rsamps_joint_model2_dependent_effects_12_paises <- coda.samples(result, 
              parameters, n.iter = 1000000, n.burnin = 0, thin = 1000)


mcmcplot(rsamps_joint_model2_dependent_effects_12_paises)

summary(rsamps_joint_model2_dependent_effects_12_paises)

save(rsamps_joint_model2_dependent_effects_12_paises, 
file = "./Modelling/Joint_longitudinal_model/jags_iterations/rsamps_joint_model2_dependent_effects_12_paises2.RData")
