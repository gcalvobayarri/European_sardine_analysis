# DIC for the models
# This process is slow since can not be parallelised

# 1. Preparing data------------
source('./data/matricial_data.R')

library(runjags)
library(mcmcplots)
data_model_tonnes <- list(T = Y, Y1 = log(data_industrial), 
                          Y2 = log(data_artisanal))

parameters <- c('dic', 'deviance')


# 2. Computation of DIC---------------

# 2. 1. Mixed model---------------------
set.seed(48409)
runJagsOut <- run.jags( method="rjags" ,
                        model="./models/mixed_joint.txt" ,
                        monitor=parameters ,
                        data=data_model_tonnes ,
                        # inits=inits_ar1 ,
                        n.chains=3 ,
                        adapt=0 ,
                        burnin=1000000 ,
                        sample=1000 ,
                        thin=5000 ,
                        summarise=FALSE ,
                        plots=FALSE )

dic_joint_mixed <- extract(runJagsOut, 'dic')

save(dic_joint_mixed, 
     file = './results/dic_joint_mixed.RData')

# 2. 2. AR model---------------------
set.seed(53289)
runJagsOut <- run.jags( method="rjags" ,
                        model="./models/autoregressive_joint_v2.txt" ,
                        monitor=parameters ,
                        data=data_model_tonnes ,
                        # inits=inits_ar1 ,
                        n.chains=3 ,
                        adapt=0 ,
                        burnin=1000000 ,
                        sample=1000 ,
                        thin=5000 ,
                        summarise=FALSE ,
                        plots=FALSE )

dic_joint_ar <- extract(runJagsOut, 'dic')

save(dic_joint_ar, 
     file = './results/dic_joint_ar.RData')

# 2. 3. ARME model---------------------
set.seed(279903)
runJagsOut <- run.jags( method="rjags" ,
                        model="./models/autoregressive_and_me_joint.txt" ,
                        monitor=parameters ,
                        data=data_model_tonnes ,
                        # inits=inits_ar1 ,
                        n.chains=3 ,
                        adapt=0 ,
                        burnin=1000000 ,
                        sample=1000 ,
                        thin=5000 ,
                        summarise=FALSE ,
                        plots=FALSE )

dic_joint_arme <- extract(runJagsOut, 'dic')

save(dic_joint_arme, 
     file = './results/dic_joint_arme.RData')
