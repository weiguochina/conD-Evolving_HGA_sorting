#*
#*This file generates IV for price in the second stage
#*Author: Wei Guo
#*Last edit: 1/12/2024
#*

rm(list=ls())

library(data.table)
library(RODBC) 
library(fst) 
library(utils)
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
library(BiocParallel)
library(xtable)
library(fixest)
library(tigris)
library(sf)


path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data/Property data")
first_stage_folder <<- paste0(path,"/Data/S1_year_result")
program_folder <<- paste0(path,"/Program/R")
output_folder <<- paste0(path, "/Output/Table")

# sink(paste0("/S1_summ_log.txt"))


######
# obtain linkage between market id, census tract, and year-month

setwd(data_folder)
meri_data <- read_fst("choice_tract_year_2010_2022.fst",
                      as.data.table = T) 

# redefine neighborhood by year-month
setorder(meri_data, year)
meri_data[, neighborhood := paste0(year,"_",censustract,"_",luxury,"_",sfha)]
meri_data[, market := paste0(year)] # one market is a year
meri_data[, prop_type := paste0(censustract,"_",luxury,"_",sfha)] # property type is defined by location, housing type, and flood risk
neigh_idx <- expand.grid(unique(meri_data$market),
                         unique(meri_data$prop_type)) %>%
  arrange(Var1)
neigh_idx <- paste0(neigh_idx$Var1, "_", neigh_idx$Var2)
prop_idx <- unique(meri_data$prop_type)
mkt_idx <- unique(meri_data$market)
meri_data[, neigh_id := match(neighborhood,neigh_idx)]
meri_data[, prop_id := match(prop_type,prop_idx)]
meri_data[, mkt_id := match(market,mkt_idx)]

mkt_yearmon <- unique(meri_data[,c("mkt_id","market")])
neigh_tract <- unique(meri_data[, c("neigh_id","luxury", "censustract")])

######
# Generate IVs, for each census tract and property type (luxury), 
# the average among all surrounding census tracts of:
# 1 average distance to a park
# 2 average distance to a beach
# 2 average distance to a beach
# weighted by the number of housing transactions in that census tracts


# for each census tract, find surrounding census tracts
tracts <- tracts("TX")
inersection <- st_touches(tracts,tracts)
tracts_touches <- data.frame(v1 = unlist(inersection), v2 = rep(seq(length(inersection)), lengths(inersection)))
tracts_df <- tracts %>% 
  as.data.table %>%
  select(-geometry)
tracts_df[, censustract := GEOID]
tracts_df[, v1 := 1:.N]
# tracts_df[, censustract_from := censustract]
tracts_df1 <- tracts_df[, list(v1, censustract)]
tracts_touches <- merge(tracts_touches, tracts_df1, by="v1", all.x=T)
tracts_df[, v2 := 1:.N]
tracts_df[, censustract_touch := censustract]
tracts_df1 <- tracts_df[, list(v2, censustract_touch)]
tracts_touches <- merge(tracts_touches, tracts_df1, by="v2", all.x=T)
tracts_touches <- tracts_touches %>% select(-v1, -v2)
tracts_touches <- as.data.table(tracts_touches)
tracts_touches[, censustract := as.numeric(censustract)]

# generate a correspondence linkage from census tract and property type, to their
# surrounding census tracts with the sanem property type
tract_proptype <- unique(neigh_tract[,c("luxury", "censustract")])
tracts_touches1 <- rbind(tracts_touches, tracts_touches)
tracts_touches1[, seq := sequence(.N), by = list(censustract,censustract_touch)]
tracts_touches1[, luxury := F]
tracts_touches1[seq==2, luxury := T]
tract_proptype <- merge(tract_proptype, tracts_touches1, by=c("censustract","luxury"), all.x=T)
tract_proptype[, seq := sequence(.N), by = list(censustract,luxury)]

# merge the characteristics of properties in surrounding census tracts
tract_proptype[, censustract_from := censustract]
tract_proptype[, censustract := as.numeric(censustract_touch)]
tract_proptype <- merge(tract_proptype, meri_data, by=c("censustract","luxury"), all.x=T, allow.cartesian=TRUE)
tract_proptype <- tract_proptype[!is.na(saleamount_r)]

# summarise and generate the IVs for each census tract and property type combination
iv <- setDT(tract_proptype)[,
                            list(mean_saleamount_r = mean(saleamount_r),
                                 mean_age = mean(age, na.rm=T),
                                 mean_fireplace = mean(fireplace, na.rm=T),
                                 mean_pool = mean(pool, na.rm=T),
                                 mean_AC = mean(AC, na.rm=T),
                                 mean_heating_central = mean(heating_central, na.rm=T),
                                 mean_garage = mean(garage, na.rm=T),
                                 mean_bathrooms = mean(bathrooms, na.rm=T),
                                 mean_rooms = mean(rooms, na.rm=T),
                                 mean_acres_r = mean(acres_r, na.rm=T),
                                 mean_stories = mean(stories, na.rm=T),
                                 mean_airport_dist = mean(airport_dist),
                                 mean_nhd_dist = mean(nhd_dist),
                                 mean_npl_dist = mean(npl_dist),
                                 mean_rivers_dist = mean(rivers_dist),
                                 mean_road_dist = mean(road_dist),
                                 mean_sfha_dist = mean(sfha_dist),
                                 mean_parks_dist = mean(parks_dist),
                                 mean_pred.whi = mean(pred.whi),
                                 mean_pred.bla = mean(pred.bla),
                                 mean_pred.his = mean(pred.his),
                                 mean_pred.asi = mean(pred.asi)),
                            by = list(censustract_from,luxury)]
iv[, censustract := censustract_from]

# export the iv data
setwd(paste0(path,"/Data"))
write.csv(iv, "IV_year.csv")
