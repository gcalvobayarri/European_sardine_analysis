# Interval plots

# 1. Loading data and package-------------

load("./Modelling/Longitudinal_model/jags_iterations/rsamps_random_trends_global.RData")
library(rjags)
library(ggplot2)

# if you ran longitudinal_model_script.R completely:
# load("./Modelling/Longitudinal_model/jags_iterations/rsamps_random_trends_global2.RData")

# 2. Random effects------------------
N <- 12
T <- 45
C <- 3
I <- 1000

b0 <- matrix(rep(NA, N * I * C), nrow = N)
b1 <- matrix(rep(NA, N * I * C), nrow = N)
for(c in 1 : C){
  for(n in 1 : N){
    for(i in 1 : I){
      b0[n, (c-1) * I + i] <- unlist(rsamps_random_trends_global[c][i,2 + n])
      b1[n, (c-1) * I + i] <- unlist(rsamps_random_trends_global[c][i,2 + N + n])
    }
  }
}

# random intercepts
U <- c(); L <- c(); M <- c()

b0_sort <- matrix(rep(NA, N * I * C), nrow = N)
for(n in 1 : N){
  b0_sort[n,] <- sort(b0[n,])
  U[n] <- b0_sort[n, 2925]
  L[n] <- b0_sort[n, 75]
  M[n] <- mean(b0_sort[n,])
}
df_intercepts <- data.frame(x = c('Albania','Algeria','B&H','Croatia',
                                  'France','Grece','Italy','Montenegro',
                                  'Morocco','Slovenia','Spain','Turkey'),
                            M = M,
                            L = L,
                            U = U)

# random slopes
U <- c(); L <- c(); M <- c()

b1_sort <- matrix(rep(NA, N * I * C), nrow = N)
for(n in 1 : N){
  b1_sort[n,] <- sort(b1[n,])
  U[n] <- b1_sort[n, 2925]
  L[n] <- b1_sort[n, 75]
  M[n] <- mean(b1_sort[n,])
}
df_trends <- data.frame(x = c('Albania','Algeria','B&H','Croatia',
                              'France','Grece','Italy','Montenegro',
                              'Morocco','Slovenia','Spain','Turkey'),
                        M = M,
                        L = L,
                        U = U)


# 3. Plots-------

ggplot(df_intercepts, aes(x = x, y = M)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = U, ymin = L))+
  labs(y = expression(b[0][i]))+
  coord_cartesian(ylim = c(-13,7))+
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, -0.9, 0, "cm"))


ggplot(df_trends, aes(x = x, y = M)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = U, ymin = L))+
  labs(y = expression(b[1][i]))+
  coord_cartesian(ylim = c(-0.1,0.12))+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, -0.9, 0, "cm"))
