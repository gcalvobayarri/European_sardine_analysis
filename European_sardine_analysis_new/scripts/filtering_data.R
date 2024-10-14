# 1. Filtering data

library(sqldf)
# Mediterranean fisheries:
d <- read.csv(file = './data/SAU LME 26 v48-0.csv')
levels(as.factor(d$scientific_name))

# European sardines:
# industrial
mediterranean_countries_industrial_tonnes <- 
  sqldf("SELECT SUM(tonnes), fishing_entity,
  year FROM d WHERE year > 1984 AND scientific_name IN ('Sardina pilchardus') 
  AND fishing_sector IN ('Industrial')  GROUP BY fishing_entity, year")
names(mediterranean_countries_industrial_tonnes) <- 
  c('tonnes', 'country', 'year')

# Remove Portugal in 2013 7 tonnes......
mediterranean_countries_industrial_tonnes <- 
  mediterranean_countries_industrial_tonnes[-273,]


# artisanal
mediterranean_countries_artisanal_tonnes <- 
  sqldf("SELECT SUM(tonnes), fishing_entity,
  year FROM d WHERE year > 1984 AND scientific_name IN ('Sardina pilchardus') 
  AND fishing_sector IN ('Artisanal')  GROUP BY fishing_entity, year")
names(mediterranean_countries_artisanal_tonnes) <- 
  c('tonnes', 'country', 'year')

mediterranean_countries_artisanal_tonnes$country[
  mediterranean_countries_artisanal_tonnes$country == "Bosnia & Herzegovina"] <- 'B&H'

# annual serie
mediterranean_countries_annual_tonnes <- 
  sqldf("SELECT SUM(tonnes), fishing_entity,  
  year FROM d WHERE year > 1984 AND scientific_name IN ('Sardina pilchardus') 
        GROUP BY fishing_entity, year")
names(mediterranean_countries_annual_tonnes) <- 
  c('tonnes', 'country', 'year')

mediterranean_countries_annual_tonnes$country[
  mediterranean_countries_annual_tonnes$country == "Bosnia & Herzegovina"] <- 'B&H'

# Remove Portugal in 2013 7 tonnes......
mediterranean_countries_annual_tonnes <- mediterranean_countries_annual_tonnes[-307,]




# 2. Saving data

save(mediterranean_countries_industrial_tonnes, 
     mediterranean_countries_artisanal_tonnes,
     file = './data/industrial_artisanal_tonnes.RData')

save(mediterranean_countries_annual_tonnes, 
     file = './data/annual_tonnes.RData')