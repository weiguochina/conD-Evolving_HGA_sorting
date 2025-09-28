#*
#*This file initializes the property transaction data for sorting model
#*Author: Wei Guo
#*Last edit: 10/3/2023
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

path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data/Property data")
output_folder <<- paste0(path,"/Data/Output")
program_folder <<- paste0(path,"/Program/R")

######
# Load and initialize the data made by Meri

setwd(data_folder)
meri_data <- read_dta("property_sales_2010_2022.dta") %>%
  as.data.table()

# select only useful variables
meri_data <- meri_data %>%
  select(clip:blockgroup,buildinggrosssquarefeet_r,parcellevellongitude,
         parcellevellatitude,buyer1fullname:buyer1firstnameandmiddleinitial) 

# trim the data to remove outliers
meri_data <- meri_data[ !is.na(saleamount_r) ]
meri_data <- meri_data[ saleamount_r <= quantile(meri_data$saleamount_r, 0.99, na.rm= T) ]
meri_data <- meri_data[ saleamount_r >= quantile(meri_data$saleamount_r, 0.01, na.rm= T) ]
meri_data <- meri_data[ buildinggrosssquarefeet_r <= quantile(meri_data$buildinggrosssquarefeet_r, 0.99, na.rm= T) ]
meri_data <- meri_data[ acres_r  <= quantile(meri_data$acres_r , 0.99, na.rm= T) ]
meri_data <- meri_data[ stories <= quantile(meri_data$stories, 0.99, na.rm= T) ]
meri_data <- meri_data[ bathrooms <= quantile(meri_data$bathrooms, 0.99, na.rm= T) ]
meri_data <- meri_data[ rooms <= quantile(meri_data$rooms, 0.99, na.rm= T) ]
write_dta(meri_data, "")

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
                          surname.year = 2020,
                          surname.only = T)
# 49429 (8.2%) individuals' last names were not matched.
prop_race <- prop_race %>% 
  select(id, surname:pred.oth)

meri_data <- merge(meri_data, prop_race, by = "id", all.x = T) 
# remove columns without race info
meri_data <- meri_data %>% subset(!is.na(pred.whi))

# generate summary statistics for Meri's data, repectively
meri_summ <- datasummary( year+month+saleamount_r+fireplace+pool+AC+heating_central+
                            foundation_slab+garage+age+bathrooms+rooms+acres_r+buildinggrosssquarefeet_r+stories+building_quality+
                            building_traditional+homestead+mobile_home+
                            sfha+hazard+harvey_inundation+ike_inunda+
                            pred.whi+pred.bla+pred.his+pred.asi+pred.oth
                          ~N+Mean+SD+Min+P25+P50+P75+Max,
                          data = meri_data,
                          fmt = '%.2f',
                          output = "meri_summ.csv",
                          title = 'Summary Statistics for Property Transactions (Meri)') 

# generate unique neighborhood identifier 
# census_tract + year + luxuriousness (building sqft > 2000) + sfha
meri_data[, luxury := buildinggrosssquarefeet_r >= 2400]
meri_data[, neighborhood := paste0(censustract,"_",year,"_",luxury,"_",sfha)]
meri_data[, neigh_id := as.numeric(as.factor(neighborhood))]
meri_data <- meri_data %>% arrange(neigh_id)

# save neighborhood matrix
meri_neigh <- meri_data[, c("neigh_id","neighborhood","censustract","year","luxury","sfha")] %>%
  arrange(neigh_id) %>%
  unique()
write_fst(meri_neigh, "final_sample_2010_2022.fst")

# save the choice matrix
write_fst(meri_data, "choice_tract_year_2010_2022.fst")





