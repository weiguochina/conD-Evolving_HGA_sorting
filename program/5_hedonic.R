#*
#*This file performs the hedonic analysis
#*Author: Wei Guo
#*Last edit: 4/5/2024
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
library(glue)


path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data/Property data")
first_stage_folder <<- paste0(path,"/Data/quaterr_distbin3")
program_folder <<- paste0(path,"/Program/R")
output_folder <<- paste0(path, "/Output/Table")
figure_folder <<- paste0(path, "/Output/Figure")

# sink(paste0("/S1_summ_log.txt"))



######
# obtain linkage between market id and year-month

setwd(data_folder)
meri_data <- read_fst("choice_tract_year_2010_2022.fst",
                      as.data.table = T) 
meri_data[, iid := 1:.N]
meri_data <- meri_data[year <= 2021]


# redefine neighborhood by year quarter
meri_data[, quarter := ceiling(as.numeric(month) / 3)]
setorder(meri_data, year, quarter)
meri_data[, neighborhood := paste0(year,"_",quarter,"_",censustract,"_",luxury,"_",sfha)]
meri_data[, market := paste0(year,"_",quarter)] # one market is a year quarter
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

# generate indicator
race_columns <- meri_data %>%
  dplyr::select(pred.whi:pred.oth)
race_indic <- colnames(race_columns)[apply(race_columns,1,which.max)]
meri_data[, race := race_indic]
meri_data[, is_white := race=="pred.whi"]
meri_data[, is_black := race=="pred.bla"]
meri_data[, is_hisp := race=="pred.his"]
meri_data[, is_asi := race=="pred.asi"]


mkt_yearmon <- unique(meri_data[,c("mkt_id","market")])


# define distance indicators
meri_data[, sfha_dist_0_500 := sfha_dist<=500 & sfha_dist>0 ]
meri_data[, sfha_dist_500_1000 := sfha_dist<=1000 & sfha_dist>500 ]
meri_data[, sfha_dist_1000_2000 := sfha_dist<=2000 & sfha_dist>1000]


# compare house qualities by distance from sfha
mod1 <- feols(as.formula("saleamount_r ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
             data = meri_data,
             cluster = "censustract")
mod2 <- feols(as.formula("rooms ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod3 <- feols(as.formula("bathrooms ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod4 <- feols(as.formula("age ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod5 <- feols(as.formula("stories ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod6 <- feols(as.formula("buildinggrosssquarefeet_r ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod7 <- feols(as.formula("pool  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod8 <- feols(as.formula("foundation_slab  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")
mod9 <- feols(as.formula("mobile_home  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract"),
              data = meri_data,
              cluster = "censustract")

mod <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9)
names(mod) = c(glue("saleamount_r mean ={round(mean(meri_data$saleamount_r, na.rm=T), digits=2)}"),
               glue("rooms mean ={round(mean(meri_data$rooms, na.rm=T), digits=2)}") , 
               glue("bathrooms mean ={round(mean(meri_data$bathrooms, na.rm=T), digits=2)}"), 
               glue("age mean ={round(mean(meri_data$age, na.rm=T), digits=2)}"), 
               glue("stories mean ={round(mean(meri_data$stories, na.rm=T), digits=2)}"),
               glue("buildinggrosssquarefeet_r mean ={round(mean(meri_data$buildinggrosssquarefeet_r, na.rm=T), digits=2)}"),
               glue("pool mean ={round(mean(meri_data$pool, na.rm=T), digits=2)}"), 
               glue("foundation_slab mean ={round(mean(meri_data$foundation_slab, na.rm=T), digits=2)}"), 
               glue("mobile_home mean ={round(mean(meri_data$mobile_home, na.rm=T), digits=2)}"))
setwd(output_folder)
modelsummary(mod,
             output = "house_diff_sfha.html",
             title = glue("House Difference by Distance from SFHA"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)



# neighborhood amenities
mod1 <- feols(as.formula("road_dist ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod2 <- feols(as.formula("airport_dist ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod3 <- feols(as.formula("npl_dist ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod4 <- feols(as.formula("nhd_dist ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod5 <- feols(as.formula("rivers_dist ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod <- list(mod1, mod2, mod3, mod4, mod5)
names(mod) = c(glue("road_dist mean ={round(mean(meri_data$road_dist, na.rm=T), digits=2)}"),
               glue("airport_dist mean ={round(mean(meri_data$airport_dist, na.rm=T), digits=2)}") , 
               glue("npl_dist mean ={round(mean(meri_data$npl_dist, na.rm=T), digits=2)}"), 
               glue("nhd_dist mean ={round(mean(meri_data$nhd_dist, na.rm=T), digits=2)}"), 
               glue("rivers_dist mean ={round(mean(meri_data$rivers_dist, na.rm=T), digits=2)}"))
setwd(output_folder)
modelsummary(mod,
             output = "amnt_diff_sfha.html",
             title = glue("Amenity Difference by Distance from SFHA"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)


# Race amenities

mod1 <- feols(as.formula("pred.bla ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod2 <- feols(as.formula("pred.his ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod3 <- feols(as.formula("pred.whi ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod4 <- feols(as.formula("pred.asi ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod <- list(mod1, mod2, mod3, mod4)
names(mod) = c(glue("pred.bla mean ={round(mean(meri_data$pred.bla, na.rm=T), digits=2)}"),
               glue("pred.his mean ={round(mean(meri_data$pred.his, na.rm=T), digits=2)}") , 
               glue("pred.whi mean ={round(mean(meri_data$pred.whi, na.rm=T), digits=2)}"), 
               glue("pred.asi mean ={round(mean(meri_data$pred.asi, na.rm=T), digits=2)}"))
setwd(output_folder)
modelsummary(mod,
             output = "race_diff_sfha.html",
             title = glue("Race Difference by Distance from SFHA"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)




# exposure to previous disasters

mod1 <- feols(as.formula("hazard  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod2 <- feols(as.formula("harvey_inundation ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod3 <- feols(as.formula("harveyclaims_dist  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod4 <- feols(as.formula("harvey_claims_payout  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod51 <- feols(as.formula("ikeclaims_dist  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod5 <- feols(as.formula("ike_claims_count  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod6 <- feols(as.formula("ike_claims_payout   ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod7 <- feols(as.formula("ike_inunda  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod8 <- feols(as.formula("ikeclaims_dist  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod9 <- feols(as.formula("allclaims_dist   ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod10 <- feols(as.formula("hist_claims_count   ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod11 <- feols(as.formula("hist_claims_payout  ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 | censustract "),
              data = meri_data,
              cluster = "censustract")
mod <- list(mod1, mod2, mod3, mod4, mod51, mod5, mod6, mod7, mod8, mod9, mod10, mod11)
names(mod) = c(glue("hazard mean ={round(mean(meri_data$hazard, na.rm=T), digits=2)}"),
               glue("harvey_inundation mean ={round(mean(meri_data$harvey_inundation, na.rm=T), digits=2)}") , 
               glue("harveyclaims_dist mean ={round(mean(meri_data$harveyclaims_dist, na.rm=T), digits=2)}"), 
               glue("harvey_claims_payout mean ={round(mean(meri_data$harvey_claims_payout, na.rm=T), digits=2)}"), 
               glue("ikeclaims_dist mean ={round(mean(meri_data$ikeclaims_dist, na.rm=T), digits=2)}"), 
               glue("ike_claims_count mean ={round(mean(meri_data$ike_claims_count, na.rm=T), digits=2)}"), 
               glue("ike_claims_payout mean ={round(mean(meri_data$ike_claims_payout, na.rm=T), digits=2)}"), 
               glue("ike_inunda mean ={round(mean(meri_data$ike_inunda, na.rm=T), digits=2)}"), 
               glue("ikeclaims_dist mean ={round(mean(meri_data$ikeclaims_dist, na.rm=T), digits=2)}"), 
               glue("allclaims_dist mean ={round(mean(meri_data$allclaims_dist, na.rm=T), digits=2)}"), 
               glue("hist_claims_count mean ={round(mean(meri_data$hist_claims_count, na.rm=T), digits=2)}"), 
               glue("hist_claims_payout mean ={round(mean(meri_data$hist_claims_payout, na.rm=T), digits=2)}"))
setwd(output_folder)
modelsummary(mod,
             output = "haz_diff_sfha.html",
             title = glue("Exposure to Historical Hazard by Distance from SFHA"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)



# hedonic analysis starts here
hdn0 <- feols(as.formula("ln_saleamount ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000 "),
              data = meri_data,
              cluster = "censustract+year")
hdn1 <- feols(as.formula("ln_saleamount ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000  | 
                        censustract^year^month"),
             data = meri_data,
             cluster = "censustract+year")
hdn2 <- feols(as.formula("ln_saleamount ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000  + rooms + bathrooms +
 age+ acres_r + stories + buildinggrosssquarefeet_r   | 
                        censustract^year^month"),
             data = meri_data,
             cluster = "censustract+year")
hdn3 <- feols(as.formula("ln_saleamount ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000  + rooms + bathrooms +
 age+ acres_r + stories + buildinggrosssquarefeet_r + rivers_dist + road_dist + airport_dist + npl_dist + nhd_dist | 
                        censustract^year^month"),
             data = meri_data,
             cluster = "censustract+year")
hdn4 <- feols(as.formula("ln_saleamount ~  sfha + sfha_dist_0_500 + sfha_dist_500_1000  + sfha_dist_1000_2000  + rooms + bathrooms +
 age+ acres_r + stories + buildinggrosssquarefeet_r + rivers_dist + road_dist + airport_dist + npl_dist + nhd_dist + 
 pred.bla + pred.his + pred.whi + pred.asi  | 
                        censustract^year^month"),
             data = meri_data,
             cluster = "censustract+year")
mod <- list(hdn0, hdn1, hdn2, hdn3, hdn4)
setwd(output_folder)
modelsummary(mod,
             output = "hdn_sfha.html",
             title = glue("Hedonic Analysis on Log(Sales Price) by Distance from SFHA"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)

