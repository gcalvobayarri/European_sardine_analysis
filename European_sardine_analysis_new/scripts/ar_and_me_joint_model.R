source('./data/matricial_data.R')
set.seed(6693346)

# jags model----------------

library(runjags)
library(mcmcplots)
data_model_tonnes <- list(T = Y, Y1 = log(data_industrial), 
                          Y2 = log(data_artisanal) )
inits_ar1 <- function() {
  list( beta01 = runif(1, 4, 8), beta02 = runif(1, 4, 8),
        sigma = runif(2, 0, 5), sigmau = runif(2, 0, 5), 
        sigma0 = runif(2, 0, 5), sigma1 = runif(2, 0, 5),
        rho = runif(2, -0.99, 0.99), rho0 = runif(1, -0.99, 0.99),
        rho1 = runif(1, -0.99, 0.99))
}
parameters_ar1 <- c( "beta01", "beta02", "beta11", "beta12", "b0", "b1", 
                     "b0Albania", "b0Bosnia", 
                     "b1Albania", "b1Bosnia", "sigma", "sigmau", "sigma0", 
                     "sigma1", "rho", "rho0", "rho1", 'w1', 'w2')

runJagsOut <- run.jags( method="parallel" ,
                        model="./models/autoregressive_and_me_joint.txt" ,
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
rsamps_ar1_me_joint_tonnes <- as.mcmc.list( runJagsOut )

mcmcplot(rsamps_ar1_me_joint_tonnes)
summary(rsamps_ar1_me_joint_tonnes)$statistics[-c(5 : 48, 61:808),]
summary(rsamps_ar1_me_joint_tonnes)$quantiles[-c(5 : 48, 61:808),]

save(rsamps_ar1_me_joint_tonnes, 
     file = './results/res_ar1_me_jointlog.RData')
#load('./results/res_ar1_me_jointlog.RData')

