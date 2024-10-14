load('./data/industrial_artisanal_tonnes.RData')
load('./data/annual_tonnes.RData')

# spaghettis----------------
library(ggplot2)

# industrial
ggplot(mediterranean_countries_industrial_tonnes, aes(year, tonnes / 1000,
                                                  colour = country)) +
  geom_point(size = .7) +
  geom_line(size = 2) +
  labs(x="", y="Thousands of tonnes")+
  coord_cartesian(ylim = c(0,150))+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(legend.position = "right")+
  theme(axis.line = element_line(color="black", size = 0.5))+
  theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 16),
    plot.margin = margin(0.5, -0.2, 0, 0, "cm") )+ 
  scale_colour_manual(values = c("Albania" = "#F8766D",
                                 "Algeria" = "#DE8C00",
                                 "Croatia" = "#7CAE00",
                                 "France" = "#00BA38",
                                 "Greece" = "#00C08B",
                                 "Italy" = "#00BFC4",
                                 "Montenegro" = "#00B4F0",
                                 "Morocco" = "#619CFF",
                                 "Slovenia" = "#C77CFF",
                                 "Spain" = "#F564E3",
                                 "Turkey" = "#FF64B0"))



# artisanal
ggplot(mediterranean_countries_artisanal_tonnes, aes(year, tonnes / 1000,
                                                      colour = country)) +
  geom_point(size = .7) +
  geom_line(size = 2) +
  labs(x="", y="Thousands of tonnes")+
  coord_cartesian(ylim = c(0,150))+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(legend.position = "right")+
  theme(axis.line = element_line(color="black", size = 0.5))+
  theme(
    legend.title = element_text( size = 18),
    legend.text = element_text( size = 16),
    plot.margin = margin(0.5, -0.2, 0, 0, "cm") )+ 
  scale_colour_manual(values = c("Algeria" = "#DE8C00",
                                 "B&H" = "#B79F00",
                                 "Croatia" = "#7CAE00",
                                 "France" = "#00BA38",
                                 "Greece" = "#00C08B",
                                 "Italy" = "#00BFC4",
                                 "Montenegro" = "#00B4F0",
                                 "Morocco" = "#619CFF",
                                 "Slovenia" = "#C77CFF",
                                 "Spain" = "#F564E3",
                                 "Turkey" = "#FF64B0"))

# annual total serie--------------------
ann <- sqldf('select SUM(tonnes), year FROM mediterranean_countries_annual_tonnes GROUP BY year')

ann$tonnes <- ann$`SUM(tonnes)`
ann <- ann[, -1]


ggplot(ann, aes(year, tonnes/1000)) +
  geom_point(size = .7) +
  geom_line(size = 2) +
  labs(x="", y="Thousands of tonnes")+
  coord_cartesian(ylim = c(150,400))+
  theme_minimal(base_size = 20, base_line_size = 15/20)+
  theme(legend.position = "right")+
  theme(axis.line = element_line(color="black", size = 0.5))
