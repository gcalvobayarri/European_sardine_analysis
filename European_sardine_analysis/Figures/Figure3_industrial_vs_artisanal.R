# Industrial vs. Artisanal fishing

# 1. Loading data and package-------------

load("./Modelling/Joint_longitudinal_model/jags_iterations/rsamps_joint_model2_dependent_effects_12_paises.RData")
library(rjags)
library(ggplot2)

# if you ran joint_longitudinal_model_script.R completely:
# load("./Modelling/Joint_longitudinal_model/jags_iterations/rsamps_joint_model2_dependent_effects_12_paises2.RData")

# 2. Random effects------------------
N <- 9
T <- 45
C <- 3
I <- 1000

# Industrial_fishing
b0_industrial <- matrix(rep(NA, (N + 2) * I * C), nrow = N + 2)
b1_industrial <- matrix(rep(NA, (N + 2) * I * C), nrow = N + 2)
for(c in 1 : C){
  for(n in 1 : N){
    for(i in 1 : I){
      b0_industrial[n, (c-1) * I + i] <- unlist(rsamps_joint_model2_dependent_effects_12_paises[c][i,4 + n])
      b1_industrial[n, (c-1) * I + i] <- unlist(rsamps_joint_model2_dependent_effects_12_paises[c][i,4 + 2 * N + 3 + n])
    }
  }
}

# Artisanal fishing
b0_artisanal <- matrix(rep(NA, (N + 1) * I * C), nrow = N + 1)
b1_artisanal <- matrix(rep(NA, (N + 1) * I * C), nrow = N + 1)
for(c in 1 : C){
  for(n in 1 : N){
    for(i in 1 : I){
      b0_artisanal[n, (c-1) * I + i] <- unlist(rsamps_joint_model2_dependent_effects_12_paises[c][i,4 + N + n])
      b1_artisanal[n, (c-1) * I + i] <- unlist(rsamps_joint_model2_dependent_effects_12_paises[c][i,4 + 3 * N + 3 + n])
    }
  }
}

# data frames
df_intercepts_industrial_artisanal <- data.frame(
  intercepts_industrial = c(mean(b0_industrial[1,]), 
                            mean(b0_industrial[2,]), mean(b0_industrial[3,]), 
                            mean(b0_industrial[4,]), mean(b0_industrial[5,]), 
                            mean(b0_industrial[6,]), mean(b0_industrial[7,]), 
                            mean(b0_industrial[8,]), mean(b0_industrial[9,])),
  
  intercepts_artisanal = c(mean(b0_artisanal[1,]), 
                           mean(b0_artisanal[2,]), mean(b0_artisanal[3,]), 
                           mean(b0_artisanal[4,]), mean(b0_artisanal[5,]), 
                           mean(b0_artisanal[6,]), mean(b0_artisanal[7,]), 
                           mean(b0_artisanal[8,]), mean(b0_artisanal[9,])),
  
  names = c('Algeria','Croatia','France','Grece','Italy','Montenegro','Morocco','Slovenia','Turkey'))

df_trends_industrial_artisanal <- data.frame(
  trends_industrial = c(mean(b1_industrial[1,]), 
                        mean(b1_industrial[2,]), mean(b1_industrial[3,]), 
                        mean(b1_industrial[4,]), mean(b1_industrial[5,]), 
                        mean(b1_industrial[6,]), mean(b1_industrial[7,]), 
                        mean(b1_industrial[8,]), mean(b1_industrial[9,])),
  
  trends_artisanal = c(mean(b1_artisanal[1,]), 
                       mean(b1_artisanal[2,]), mean(b1_artisanal[3,]), 
                       mean(b1_artisanal[4,]), mean(b1_artisanal[5,]), 
                       mean(b1_artisanal[6,]), mean(b1_artisanal[7,]), 
                       mean(b1_artisanal[8,]), mean(b1_artisanal[9,])),
  
  names = c('Algeria','Croatia','France','Grece','Italy','Montenegro','Morocco','Slovenia','Turkey'))

# 3. Plots--------------
ggplot(df_intercepts_industrial_artisanal, aes(x = intercepts_artisanal, y = intercepts_industrial))+
  labs(x = 'Artisanal',y = 'Industrial')+
  geom_point(size = 2) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_label(aes(label = names), data = df_intercepts_industrial_artisanal, nudge_y = 0.25, alpha = 0.5)+
  theme_minimal()+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))

ggplot(df_trends_industrial_artisanal, aes(x = trends_artisanal, y = trends_industrial))+
  labs(x = 'Artisanal',y = 'Industrial')+
  geom_point(size = 2)+
  coord_cartesian(xlim = c(-0.10, 0.10), ylim = c(-0.10, 0.10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_label(aes(label = names), data = df_trends_industrial_artisanal, nudge_y = 0.006, alpha = 0.5)+
  theme_minimal()+
  theme(axis.line = element_line(color="black", size = 0.5))
