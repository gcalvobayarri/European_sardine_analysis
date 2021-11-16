# Longitudinal model

# 1. Loading data and packages---------

load("./Data/matrices/data_annual_mediterranean_tonnes.RData")
library(mcmcplots)
library(rjags)

# 2. Model-------------------------

data <- list(
  N=12, T=45, Y = log(data_annual_mediterranean_tonnes)
)

inits <- list( beta0 = runif(1, -10, 10), 
              sigma = runif(1, 0, 10), 
              I0_sigma = runif(1, 0, 10), 
              T1_sigma = runif(1, 0, 10))


parameters <- c("beta0", "b0", "b1", "sigma", "I0_sigma", "T1_sigma")


result <- jags.model("./Modelling/Longitudinal_model/longitudinal_model_jags.txt",
                     data, inits, n.chains = 3,
                     n.adapt = 0)

update(result, n.iter = 50000)


rsamps_random_trends_global <- coda.samples(result, parameters, 
                                            n.iter = 1000000, thin = 1000)

mcmcplot(rsamps_random_trends_global)

summary(rsamps_random_trends_global)


save(rsamps_random_trends_global,
file = "./Modelling/Longitudinal_model/jags_iterations/rsamps_random_trends_global2.RData")
