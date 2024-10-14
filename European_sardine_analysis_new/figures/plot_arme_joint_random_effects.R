# Industrial vs. Artisanal fishing

# 1. Loading data and package-------------

load('./results/res_ar1_me_jointlog.RData')
library(rjags)
library(ggplot2)


# 2. Random effects------------------
N <- 10
T <- 34
I <- 3000

# Industrial fishing
b0I <- matrix(rep(NA, N * I), nrow = N)
b1I <- matrix(rep(NA, N * I), nrow = N)

# Artisanal fishing
b0A <- matrix(rep(NA, N * I), nrow = N)
b1A <- matrix(rep(NA, N * I), nrow = N)


for(i in 1 : N){
  b0I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b0","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b0A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b0","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  b1I[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b1","[",i,",", 1,"]", sep = ''), 
                                          ISB = F)))
  b1A[i, ] <- as.vector(unlist(MCMCchains(rsamps_ar1_me_joint_tonnes, 
                                          params = paste("b1","[",i,",", 2,"]", sep = ''), 
                                          ISB = F)))
  
}

# data frames
df_intercepts_industrial_artisanal <- data.frame(
  intercepts_industrial = c(mean(b0I[1,]), 
                            mean(b0I[2,]), mean(b0I[3,]), 
                            mean(b0I[4,]), mean(b0I[5,]), 
                            mean(b0I[6,]), mean(b0I[7,]), 
                            mean(b0I[8,]), mean(b0I[9,]), mean(b0I[10,])),
  
  intercepts_artisanal = c(mean(b0A[1,]), 
                           mean(b0A[2,]), mean(b0A[3,]), 
                           mean(b0A[4,]), mean(b0A[5,]), 
                           mean(b0A[6,]), mean(b0A[7,]), 
                           mean(b0A[8,]), mean(b0A[9,]), mean(b0A[10,])),
  
  Countries = c('Algeria','Croatia','France','Grece','Italy','Montenegro','Morocco',
            'Slovenia', 'Spain', 'Turkey'))

df_trends_industrial_artisanal <- data.frame(
  trends_industrial = c(mean(b1I[1,]), 
                        mean(b1I[2,]), mean(b1I[3,]), 
                        mean(b1I[4,]), mean(b1I[5,]), 
                        mean(b1I[6,]), mean(b1I[7,]), 
                        mean(b1I[8,]), mean(b1I[9,]), mean(b1I[10,])),
  
  trends_artisanal = c(mean(b1A[1,]), 
                       mean(b1A[2,]), mean(b1A[3,]), 
                       mean(b1A[4,]), mean(b1A[5,]), 
                       mean(b1A[6,]), mean(b1A[7,]), 
                       mean(b1A[8,]), mean(b1A[9,]), mean(b1A[10,])),
  
  Countries = c('Algeria','Croatia','France','Grece','Italy','Montenegro','Morocco',
            'Slovenia', 'Spain', 'Turkey'))

# 3. Plots--------------
lm_intercepts <- lm(intercepts_industrial~intercepts_artisanal, 
                    data = df_intercepts_industrial_artisanal)

ggplot(df_intercepts_industrial_artisanal, aes(x = intercepts_artisanal, 
                                               y = intercepts_industrial,
                                               colour = Countries))+
  labs(x = 'Artisanal',y = 'Industrial')+
  geom_point(size = 4) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))+
  geom_abline(intercept = lm_intercepts$coefficients[1], 
              slope = lm_intercepts$coefficients[2], linetype = "dashed") + # estimates from lm()
  # geom_label(aes(label = names), data = df_intercepts_industrial_artisanal, nudge_y = 0.25, alpha = 0.5)+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))

lm_trends <- lm(trends_industrial~trends_artisanal, 
                data = df_trends_industrial_artisanal)
ggplot(df_trends_industrial_artisanal, aes(x = trends_artisanal, 
                                           y = trends_industrial,
                                           colour = Countries))+
  labs(x = 'Artisanal',y = 'Industrial')+
  geom_point(size = 4)+
  coord_cartesian(xlim = c(-0.10, 0.10), ylim = c(-0.10, 0.10)) +
  geom_abline(intercept = lm_trends$coefficients[1], 
              slope = lm_trends$coefficients[2], linetype = "dashed")+ # estimates from lm()
  # geom_label(aes(label = names), data = df_trends_industrial_artisanal, nudge_y = 0.006, alpha = 0.5)+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5))
