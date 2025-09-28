#*
#*This file performs the first stage analysis estimating heterogeneity parameters and mean utilities
#*Author: Wei Guo
#*Last edit: 12/15/2023
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


path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data/Property data")
output_folder <<- paste0(path,"/Data/quaterr_distbin3")
program_folder <<- paste0(path,"/Program/R")
first_stage_yearmonth_folder <<- paste0(path,"/Data/S1_result")





######
# obtain linkage between market id and year-month (for year-month results)

setwd(data_folder)
meri_data <- read_fst("choice_tract_year_2010_2022.fst",
                      as.data.table = T) 
meri_data[, iid := 1:.N]
meri_data <- meri_data[year <= 2021]

# redefine neighborhood by year-month
setorder(meri_data, year, month)
meri_data[, neighborhood := paste0(year,"_",month,"_",censustract,"_",luxury,"_",sfha)]
meri_data[, market := paste0(year,"_",month)] # one market is a year month
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

######
# summarize first stage estimate

setwd(first_stage_yearmonth_folder)

# first_stage_files <- list.files(first_stage_folder, pattern = ".rds")
first_stage_files <- paste0("s1_results_",1:144, ".rds")

neigh_estimate <- data.frame()
heter_estimate <- data.frame()
s1_size <- c()
s1_loglik <- c()
s1_mkt <- c()

for(i in seq_len(length(first_stage_files))){
  s1.results_sample <- readRDS(first_stage_files[i])
  mkt_i <- sub(".rds","",sub("s1_results_","",first_stage_files[i]))
  table_s1 <- summary(s1.results_sample)
  table_estimate_i <-table_s1$estimate
  # if the final estimates have infinite standard errors, use the last estimate before convergence
  if(mean(table_estimate_i[,2])==Inf){
    table_estimate_i[,1] <- s1.results_sample$last.step[["theta0"]]
    table_hess_i <- attributes(s1.results_sample$last.step$f0)[["hessian"]]
    table_estimate_i[,2] <- sqrt(diag(solve(-table_hess_i)))
    table_estimate_i[,3] <- table_estimate_i[,1]/table_estimate_i[,2]
    table_estimate_i[,4] <-  2*pt(abs(table_estimate_i[,3]), 
                                  nrow(s1.results_sample$gradientObs), 
                                  lower=FALSE)
  }
  heter_estimate_i <- table_estimate_i[grepl(":",rownames(table_estimate_i)),]
  rownames(heter_estimate_i) <- paste0(rownames(heter_estimate_i),"_",mkt_i)
  heter_estimate <- rbind(heter_estimate, heter_estimate_i)
  neigh_estimate <- rbind(neigh_estimate, table_estimate_i[!grepl(":",rownames(table_estimate_i)),])
  s1_size <- c(s1_size, nrow(s1.results_sample[["gradientObs"]]))
  s1_loglik <- c(s1_loglik, table_s1[["loglik"]])
  s1_mkt <- c(s1_mkt, mkt_i)
}


#####
# link the original data with heterogeneity and mean utility coefficients


# obtain short-listed variables from meri_data on market occurrence
meri_short <- meri_data %>%
  dplyr::select(iid, year,month,censustract,mkt_id,prop_id,saleamount_r, ln_saleamount,market,neigh_id,luxury,sfha,ln_sfha_dist,rooms,
                bathrooms,buildinggrosssquarefeet_r,harvey_inundation,harveyclaims_dist,harvey_claims_payout,
                npl_dist, parks_dist, nhd_dist,
                prop_type, pred.bla,pred.his) %>%
  mutate(mkt_id := as.character(mkt_id))%>%
  mutate(neigh_id := as.character(neigh_id))


# process the heterogeneity coefficient table
heter_estimate1 <- cbind(heter_estimate, rownames(heter_estimate)) %>%
  as.data.table()
heter_estimate1[, heter_var := sub('_[^_]*$', '',`rownames(heter_estimate)`)]
heter_estimate1[, mkt_id := sub(".*_", "", `rownames(heter_estimate)`)]
heter_estimate1[, `rownames(heter_estimate)` := NULL]
heter_estimate1 <- heter_estimate1 %>%
  reshape(idvar = "mkt_id", timevar = "heter_var", direction = "wide")

# link the original data with heterogeneity coefficients
meri_short <- merge(meri_short, heter_estimate1, by="mkt_id", all.x=T)


# process the mean utility coefficient table
neigh_estimate1 <- cbind(neigh_estimate, rownames(neigh_estimate)) %>%
  as.data.table()
neigh_estimate1[, neigh_id := `rownames(neigh_estimate)`]
neigh_estimate1[, `rownames(neigh_estimate)` := NULL]

# link the original data with mean utility coefficients
meri_short <- merge(meri_short, neigh_estimate1, by="neigh_id", all.x=T)


######
# Perform the first stage analysis on the quarterly level

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

meri_data[, saleamount_r := saleamount_r/1000]

# add the year-month result
meri_data <- merge(meri_data, meri_short[,c(3,26:45)], by="iid", all.x=T)

# generate race indicator
race_columns <- meri_data %>%
  dplyr::select(pred.whi:pred.oth)
race_indic <- colnames(race_columns)[apply(race_columns,1,which.max)]
meri_data[, race := race_indic]
meri_data[, is_white := race=="pred.whi"]
meri_data[, is_black := race=="pred.bla"]
meri_data[, is_hispanic := race=="pred.his"]
meri_data[, is_asian := race=="pred.asi"]

# define distance indicators
meri_data[, sfha_dist_0_500 := sfha_dist<=500 & sfha_dist>0 ]
meri_data[, sfha_dist_500_1000 := sfha_dist<=1000 & sfha_dist>500 ]
meri_data[, sfha_dist_1000_2000 := sfha_dist<=2000 & sfha_dist>1000]


setwd(program_folder)
source("first_stage.R")
source("first_stage_foreach_test.R")
source("first_stage_foreach_year.R")

# meri_data <- meri_data[1:50000,]
meri_data <- as.data.frame(meri_data)
X.vars <- c("sfha","sfha_dist_0_500","sfha_dist_500_1000","sfha_dist_1000_2000")
Z.vars <- c("pred.asi", "pred.bla","pred.his")
Alt.var = "neigh_id"


for(mkt_i in 1:length(mkt_idx)){
  
  dir.create(file.path(output_folder), showWarnings = FALSE)
  setwd(output_folder)
  
  # sink the log file
  sink(paste0("S1_",mkt_i,".txt"))
  
  guess <- setDT(meri_data[meri_data$mkt_id==mkt_i,c("Estimate", "neigh_id")])[,
    list(Estimate = mean(Estimate, na.rm=T)),
    by = neigh_id
  ] 
  guess <- c(guess$Estimate[2:nrow(guess)])
  guess[is.na(guess)] <- 0
  guess <- c(guess,rep(0,length(X.vars)*length(Z.vars)))
  
  tic(mkt_idx[mkt_i])
  s1.results_sample <- first_stage_foreach_year(code_name = Alt.var,
                                           X_names = X.vars,
                                           Z_names = Z.vars,
                                           initials = guess,
                                           chunk_size = 125,
                                           data = meri_data[meri_data$mkt_id==mkt_i,],
                                           print_detail = 3)
  toc()
  # save work environment
  saveRDS(s1.results_sample, paste0("s1_results_",mkt_i,".rds"))
  
  # print S1 summary results
  summary(s1.results_sample)
  
  # unsink log file
  sink()
  
  print(mkt_i)
}
