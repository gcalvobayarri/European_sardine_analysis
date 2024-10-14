source('./data/matricial_data.R')
set.seed(422795)

# jags model----------------

library(runjags)
library(mcmcplots)
data_model_tonnes <- list(T = Y, Y1 = log(data_industrial), 
                          Y2 = log(data_artisanal))
inits_ar1 <- function() {
  list( beta01 = runif(1, 4, 8), beta02 = runif(1, 4, 8),
        sigma = runif(2, 0, 5), rho = runif(2, -0.99, 0.99))
}
parameters_ar1 <- c( "beta01", "beta02", "beta11", "beta12", "b0", "b1", "b0Albania", "b0Bosnia", 
                     "b1Albania", "b1Bosnia", "sigma", "sigma0", 
                     "sigma1", "rho", "rho0", "rho1")

runJagsOut <- run.jags( method="parallel" ,
                        model="./models/autoregressive_joint_v2.txt" ,
                        monitor=parameters_ar1 ,
                        data=data_model_tonnes ,
                        inits=inits_ar1 ,
                        n.chains=3 ,
                        adapt=0 ,
                        burnin=1000000 ,
                        sample=1000 ,
                        thin=5000 ,
                        summarise=FALSE ,
                        plots=FALSE )
rsamps_ar1_joint_tonnesv2 <- as.mcmc.list( runJagsOut )

mcmcplot(rsamps_ar1_joint_tonnesv2)
summary(rsamps_ar1_joint_tonnesv2)$statistics[-c(5:48),]
summary(rsamps_ar1_joint_tonnesv2)$quantiles[-c(5:48),]
#dic_ar_joint_annual_tonnes <- dic.samples(results, n.iter = 100000, thin = 100)

save(rsamps_ar1_joint_tonnesv2, 
     file = './results/res_autoregressive_jointv2log.RData')
#load('./results/res_autoregressive_jointv2log.RData')
