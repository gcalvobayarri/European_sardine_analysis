# Spaghetti plots

# 1. Loading data and package-----------

load("./Data/mediterranean_countries_annual.RData")
load("./Data/mediterranean_countries_Industrial.RData")
load("./Data/mediterranean_countries_Artisanal.RData")
library(ggplot2)

# 2. Plots-------------

# Total fishing
ggplot(mediterranean_countries_annual, aes(year, log(tonnes),
      colour = Countries)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Logarithm of tonnes")+
  coord_cartesian(ylim = c(-3,13))+
  theme(legend.position = "right")+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5))+ 
  theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 18),
    plot.margin = margin(0.5, -0.2, -0.9, 0, "cm") )

# Industrial fishing
ggplot(mediterranean_countries_Industrial, aes(year, log(tonnes), colour = Countries)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(-3,13))+
  theme(legend.position = "right")+
  labs(x="", y="Logarithm of tonnes")+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5)) + theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 18),
    plot.margin = margin(0.5, -0.2, -0.9, 0, "cm") )

# Artisanal fishing
ggplot(mediterranean_countries_Artisanal, aes(year, log(tonnes), colour = Countries)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(-3,13))+
  theme(legend.position = "right")+
  labs(x="", y="Logarithm of tonnes")+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(axis.line = element_line(color="black", size = 0.5)) + theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 18),
    plot.margin = margin(0.5, -0.2, -0.9, 0, "cm") )
