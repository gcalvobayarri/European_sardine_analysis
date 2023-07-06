# matricial data:
# Transform the info from thedata frame into matricial format

load('./data/industrial_artisanal_tonnes.RData')

# matrix-----------
N <- 11
Y <- 34

# Industrial
BBB <- as.character(mediterranean_countries_industrial_tonnes$country)
BBB <- as.factor(BBB)
country_id <- as.numeric(BBB)

mediterranean_countries_industrial_tonnes <- 
  data.frame(mediterranean_countries_industrial_tonnes, country_id)
rm(BBB, country_id)

data_industrial <- matrix(rep(NA, N*Y), nrow = N)
for( i in 1 : length(mediterranean_countries_industrial_tonnes[,1])){ 
  data_industrial[mediterranean_countries_industrial_tonnes[i, 4], 
                  mediterranean_countries_industrial_tonnes[i, 3] - 1984] <- 
    mediterranean_countries_industrial_tonnes[i, 1]
}

data_industrial <- rbind(data_industrial[2 : 11,], data_industrial[1,])
# last one Albania


# Artisanal
BBB <- as.character(mediterranean_countries_artisanal_tonnes$country)
BBB <- as.factor(BBB)
country_id <- as.numeric(BBB)

mediterranean_countries_artisanal_tonnes <- 
  data.frame(mediterranean_countries_artisanal_tonnes, country_id)
rm(BBB, country_id)

data_artisanal <- matrix(rep(NA, N*Y), nrow = N)
for( i in 1 : length(mediterranean_countries_artisanal_tonnes[,1])){ 
  data_artisanal[mediterranean_countries_artisanal_tonnes[i, 4], 
                 mediterranean_countries_artisanal_tonnes[i, 3] - 1984] <- 
    mediterranean_countries_artisanal_tonnes[i, 1]
}

data_artisanal <- rbind(data_artisanal[c(1, 3 : 11), ], 
                        data_artisanal[2, ])

rm(mediterranean_countries_artisanal_tonnes, 
   mediterranean_countries_industrial_tonnes, i)
# save(data_industrial, data_artisanal, file = 'matrices.RData')
