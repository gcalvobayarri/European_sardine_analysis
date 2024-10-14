
# 1. Loading data and package-------------

load('./results/res_ar1_me_jointlog.RData')

library(rjags)
library(ggplot2)
library(MCMCvis)

beta11 <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                    params = paste('beta11',
                                                   sep = ''), ISB = F)))
# sum(beta11<0)/length(beta11)

beta12 <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes,
                                    params = paste('beta12',
                                                   sep = ''), ISB = F)))
# sum(beta12<0)/length(beta12)

# 2. Plot------------------
beta1I <- data.frame(x = beta11)

beta1A <- data.frame(x = beta12)


# beta1I
gg=ggplot(beta1I, aes(x=x)) +
  geom_density(size=1.5, alpha=.4)+ 
  theme_minimal(base_size = 20, base_line_size = 15/20) +
  xlim(-.15, .1) + ylim(0, 20) +
  labs(x="", y = "") 

dat = ggplot_build(gg)$data[[1]]

gg +geom_area(data = subset(dat, x < 0), aes(x = x, y = y), fill = "purple",
              alpha = 0.4)

length(which(beta1I<0)) / length(beta1I[,1]) #prob of beta1I to be negative 0.912

# beta1A
gg=ggplot(beta1A, aes(x=x)) +
  geom_density(size=1.5, alpha=.4)+ 
  theme_minimal(base_size = 20, base_line_size = 15/20) +
  xlim(-.15, .1) + ylim(0, 20) +
  labs(x="", y = "") 

dat = ggplot_build(gg)$data[[1]]

gg +geom_area(data = subset(dat, x < 0), aes(x = x, y = y), fill = "purple",
              alpha = 0.4)

length(which(beta1A<0)) / length(beta1A[,1]) #prob of beta1A to be negative 0.857
