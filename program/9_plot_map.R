#*
#*This file plot the properties relative to sfha
#*Author: Wei Guo
#*Last edit: 9/4/2024
#*

rm(list=ls())

library(data.table)
library(RODBC) 
library(fst) 
library(utils)
library(plyr)
library(dplyr)
library(haven)
library(modelsummary)
library(wru)
library(tidycensus)
library(future)
library(stringr)
library(wordcloud)
library(tm)
library(tidytext)
library(Matrix)
library(maxLik)
library(tictoc)
library(foreach)
library(doParallel)
library(bigmemory )
library(mapview )

sf_use_s2(FALSE)


path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data/Property data")
output_folder <<- paste0(path,"/Data/Output")
program_folder <<- paste0(path,"/Program/R")



# plot the properties relative to sfha
setwd(data_folder)
meri_data <- read_dta("property_sales_2010_2022.dta") %>%
  as.data.table()
# add race info
meri_data[,id := 1:.N]
prop_name <- meri_data[,c("id","buyer1lastname")]
prop_name[grepl("EQUITIES|LLC|TRUSTEE|HOLDINGS|MANAGERS|REALTY|
                ASSOCIATES|STREET|OWNERS|PROPERTY|CORP|ASSOCI|INC|
                LIMITED|AMERICAN|FAMILY|CHURCH|VILLAGE|TRUST|
                PLACE|OF|AVENUE|HOUSING|RECREATION|PARKS|associates|
                properties|owner|city|dept|texas|harris|galveston|HOLDING|agent|real|owners|
                management|preservation|department|gregory|estate|property|
                equities|assoc|partners|environmental|associat|authority|
                transportatio|university|busines|blvd|residential|
                capital|land|court|center|apartments|building|garden|community|
                trustees",buyer1lastname), buyer1lastname:=NA ]
prop_name[buyer1lastname=="",buyer1lastname:=NA  ]
prop_name[, surname := buyer1lastname]
prop_name <- prop_name[!is.na(surname)]
prop_race <- predict_race(voter.file = prop_name,
                          year = 2020,
                          surname.only = T)
prop_race <- prop_race %>% 
  select(id, surname:pred.oth)
meri_data <- merge(meri_data, prop_race, by = "id", all.x = T)
rm(prop_name, prop_race)

# load sfha map
setwd( paste0(path,"/Data/harrissfha/harrissfha"))
sfha <- st_read("Floodplain_100yr.shp")
sfha <- st_zm(sfha)
sfha <- st_union(sfha)

# obtain property locations
prop <- meri_data %>%
  select(parcelpointid, sfha, v6, lat) %>%
  unique %>%
  subset(!is.na(v6))
prop <- st_as_sf(x = prop,
                 coords = c("v6","lat"),
                 crs = st_crs(4326))

# figure on average sale price
mapview(sfha %>% st_as_sf)


