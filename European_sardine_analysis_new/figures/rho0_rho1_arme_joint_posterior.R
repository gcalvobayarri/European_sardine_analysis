#(not included in the manuscript) This figure shows the positive correlation 
# between the random effects of the artisanal response variable and the 
# industrial response variable.

# 1. Loading data and packages-------------

load('./results/res_ar1_me_jointlog.RData')

library(rjags)
library(ggplot2)
library(MCMCvis)

rho0 <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                    params = paste('rho0',
                                                   sep = ''), ISB = F)))
# sum(rho0>0)/I

rho1 <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                    params = paste('rho1',
                                                   sep = ''), ISB = F)))
# length(rho1[rho1>0])/15000

# 2. Plot------------------
rho0 <- data.frame(x = rho0)

rho1 <- data.frame(x = rho1)


# rho0
gg=ggplot(rho0, aes(x=x)) +
  geom_density(size=1.5, alpha=.4)+ 
  theme_minimal(base_size = 20, base_line_size = 15/20) +
  xlim(-1, 1) + ylim(0, 1.5) +
  labs(x="", y = "") 

dat = ggplot_build(gg)$data[[1]]

gg +geom_area(data = subset(dat, x > 0), aes(x = x, y = y), fill = "purple",
              alpha = 0.4)

length(which(rho0>0)) / length(rho0[,1]) #prob of rho0 to be positive 0.882

#rho1
gg=ggplot(rho1, aes(x=x)) +
  geom_density(size=1.5, alpha=.4)+ 
  theme_minimal(base_size = 20, base_line_size = 15/20) +
  xlim(-1, 1) + ylim(0, 1.5) +
  labs(x="", y = "") 

dat = ggplot_build(gg)$data[[1]]

gg +geom_area(data = subset(dat, x > 0), aes(x = x, y = y), fill = "purple",
              alpha = 0.4)

length(which(rho1>0)) / length(rho1[,1]) #prob of rho1 to be positive 0.848
