#*
#*This file performs the first stage analysis estimating heterogeneity parameters and mean utilities
#*Author: Wei Guo
#*Last edit: 5/15/2024
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
first_stage_folder <<- paste0(path,"/Data/quaterr_distbin")
program_folder <<- paste0(path,"/Program/R")
output_folder <<- paste0(path, "/Output/Table")
figure_folder <<- paste0(path, "/Output/Figure")

# sink(paste0("/S1_summ_log.txt"))



# using 100 draws 
Ndraws = 100

# number of draws in the standard error calculation of the WTP
Ndraws_wtp <- 500000


#########
## function to compute the WTP

s2_WTP <- function(Ndraws_mone = Ndraws_wtp, 
                   seed = 1, 
                   s2_estimate, 
                   mean_salesprice = mean_salesprice_23, 
                   outlier = 0.1, 
                   output_name){

  # IV results for white 
  set.seed(seed)
  white_iv <- lapply(1:Ndraws_mone, function(x){
    as.data.table(t(rnorm(nrow(s2_estimate), s2_estimate$Estimate, sqrt(nrow(meri_short))*s2_estimate$`Std. Error`)))
  })
  white_iv <- rbindlist(white_iv)
  gc()
  colnames(white_iv) <- rownames(s2_estimate)
  iv_avr <- copy(white_iv) # create a copy to avoid unintential update of white_iv
  white_iv[, elasticity := -mean_salesprice*fit_saleamount]
  elasticity <- white_iv$elasticity
  lower <- quantile(elasticity, outlier)
  upper <- quantile(elasticity, 1-outlier)
  elasticity <- elasticity[elasticity<upper & elasticity>lower]
  whi_mone <- lapply(names(white_iv)[2:nrow(s2_estimate)], function(x){
    sfha_mone <- -white_iv[,get(x)]/s2_estimate$Estimate[1]  #white_iv[,get("fit_saleamount")] 
    lower <- quantile(sfha_mone, outlier)
    upper <- quantile(sfha_mone, 1-outlier)
    sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
    return(sfha_mone)
  })
  whi_mone <- Reduce("cbind",whi_mone)
  # whi_mone <- cbind(elasticity, whi_mone)
  whi_mone <- as.data.table(whi_mone)
  white_wtp <- data.table(
    varname = c("Own Elasticity",names(white_iv)[2:nrow(s2_estimate)]),
    Estimate = c(-s2_estimate$Estimate[1]*mean_salesprice,
                 as.numeric(whi_mone[, lapply(.SD, mean)])),
    `Std.Error` = c(-s2_estimate$`Std. Error`[1]*mean_salesprice,
                    as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(nrow(meri_short))))
  )
  white_wtp[, "t.stat" := Estimate/`Std.Error`]
  white_wtp[, "p.value" := 2*pt(abs(`t.stat`), nrow(meri_short), lower=FALSE)]
  rm(white_iv)
  gc()
  
  
  
  # IV results for Black 
  white_iv <- copy(iv_avr)
  diff_coef <- heter_table[grepl("bla",rownames(heter_table)),]
  # replace price coefficient to zero if it is not significant
  if(diff_coef[1,4]>=0.01){
    diff_coef[1,1]=0
    diff_coef[1,2]=0
  }
  set.seed(seed)
  diff <- lapply(1:Ndraws_mone, function(x){
    as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, sqrt(nrow(meri_short))*diff_coef$`Std. Error`)))
  })
  diff <- rbindlist(diff)
  gc()
  black_iv <- white_iv
  for(xi in 1:5 ) {
    xx <- c("fit_saleamount","^(?=.*sfha)(?!.*sfha_).*$","sfha_dist_0_500","sfha_dist_500_1000","sfha_dist_500_1000")[xi]
    idx <- names(diff)[xi]
    for(x in names(white_iv)[grepl(xx,names(white_iv), perl = TRUE)]){
      a <-  white_iv[,get(x)] + diff[,get(idx)]
      black_iv[, (x) := a]
    }
  }
  black_iv[, elasticity := -mean_salesprice*fit_saleamount]
  elasticity <- black_iv$elasticity
  lower <- quantile(elasticity, outlier)
  upper <- quantile(elasticity, 1-outlier)
  elasticity <- elasticity[elasticity<upper & elasticity>lower]
  bla_mone <- lapply(names(black_iv)[2:nrow(s2_estimate)], function(x){
    sfha_mone <- -black_iv[,get(x)]/(s2_estimate$Estimate[1] + diff_coef$Estimate[1])  #white_iv[,get("fit_saleamount")] 
    lower <- quantile(sfha_mone, outlier)
    upper <- quantile(sfha_mone, 1-outlier)
    sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
    return(sfha_mone)
  })
  bla_mone <- Reduce("cbind",bla_mone)
  # bla_mone <- cbind(elasticity, bla_mone)
  bla_mone <- as.data.table(bla_mone)
  bla_WTP <- data.table(
    varname = c("Own Elasticity",names(black_iv)[2:nrow(s2_estimate)]),
    Estimate = as.numeric(c(-(s2_estimate$Estimate[1] + diff_coef$Estimate[1])*mean_salesprice,
                          bla_mone[, lapply(.SD, mean)])),
    `Std.Error` = as.numeric(c(sd(elasticity)/sqrt(nrow(meri_short)),
                             bla_mone[, lapply(.SD, sd)]/sqrt(nrow(meri_short))))
  )
  bla_WTP[, "t.stat" := Estimate/`Std.Error`]
  bla_WTP[, "p.value" := 2*pt(abs(`t.stat`), nrow(meri_short), lower=FALSE)]
  rm(white_iv)
  gc()

  
  
  # IV results for Hispanic 
  white_iv <- copy(iv_avr)
  diff_coef <- heter_table[grepl("his",rownames(heter_table)),]
  # replace price coefficient to zero if it is not significant
  if(diff_coef[1,4]>=0.01){
    diff_coef[1,1]=0
    diff_coef[1,2]=0
  }
  set.seed(seed)
  diff <- lapply(1:Ndraws_mone, function(x){
    as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, sqrt(nrow(meri_short))*diff_coef$`Std. Error`)))
  })
  diff <- rbindlist(diff)
  hisp_iv <- white_iv
  for(xi in 1:5 ) {
    xx <- c("fit_saleamount","^(?=.*sfha)(?!.*sfha_).*$","sfha_dist_0_500","sfha_dist_500_1000","sfha_dist_500_1000")[xi]
    idx <- names(diff)[xi]
    for(x in names(white_iv)[grepl(xx,names(white_iv), perl = TRUE)]){
      a <-  white_iv[,get(x)] + diff[,get(idx)]
      hisp_iv[, (x) := a]
    }
  }
  hisp_iv[, elasticity := -mean_salesprice*fit_saleamount]
  elasticity <- hisp_iv$elasticity
  lower <- quantile(elasticity, outlier)
  upper <- quantile(elasticity, 1-outlier)
  elasticity <- elasticity[elasticity<upper & elasticity>lower]
  hisp_mone <- lapply(names(hisp_iv)[2:nrow(s2_estimate)], function(x){
    sfha_mone <- -hisp_iv[,get(x)]/(s2_estimate$Estimate[1] + diff_coef$Estimate[1])
    lower <- quantile(sfha_mone, outlier)
    upper <- quantile(sfha_mone, 1-outlier)
    sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
    return(sfha_mone)
  })
  hisp_mone <- Reduce("cbind",hisp_mone)
  # hisp_mone <- cbind(elasticity, hisp_mone)
  hisp_mone <- as.data.table(hisp_mone)
  hisp_wtp <- data.table(
    varname = c("Own Elasticity",names(hisp_iv)[2:nrow(s2_estimate)]),
    Estimate = as.numeric(c(-(s2_estimate$Estimate[1] + diff_coef$Estimate[1])*mean_salesprice,
                            hisp_mone[, lapply(.SD, mean)])),
    `Std.Error` = as.numeric(c(sd(elasticity)/sqrt(nrow(meri_short)),
                               hisp_mone[, lapply(.SD, sd)]/sqrt(nrow(meri_short))))
  )
  hisp_wtp[, "t.stat" := Estimate/`Std.Error`]
  hisp_wtp[, "p.value" := 2*pt(abs(`t.stat`), nrow(meri_short), lower=FALSE)]
  rm(white_iv)
  gc()
  
  
  # IV results for Asian 
  white_iv <- copy(iv_avr)
  diff_coef <- heter_table[grepl("asi",rownames(heter_table)),]
  # replace price coefficient to zero if it is not significant
  if(diff_coef[1,4]>=0.01){
    diff_coef[1,1]=0
    diff_coef[1,2]=0
  }
  set.seed(seed)
  diff <- lapply(1:Ndraws_mone, function(x){
    as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, sqrt(nrow(meri_short))*diff_coef$`Std. Error`)))
  })
  diff <- rbindlist(diff)
  asia_iv <- white_iv
  for(xi in 1:5 ) {
    xx <- c("fit_saleamount","^(?=.*sfha)(?!.*sfha_).*$","sfha_dist_0_500","sfha_dist_500_1000","sfha_dist_500_1000")[xi]
    idx <- names(diff)[xi]
    for(x in names(white_iv)[grepl(xx,names(white_iv), perl = TRUE)]){
      a <-  white_iv[,get(x)] + diff[,get(idx)]
      asia_iv[, (x) := a]
    }
  }
  asia_iv[, elasticity := -mean_salesprice*fit_saleamount]
  elasticity <- asia_iv$elasticity
  lower <- quantile(elasticity, outlier)
  upper <- quantile(elasticity, 1-outlier)
  elasticity <- elasticity[elasticity<upper & elasticity>lower]
  asi_mone <- lapply(names(asia_iv)[2:nrow(s2_estimate)], function(x){
    sfha_mone <- -asia_iv[,get(x)]/(s2_estimate$Estimate[1] + diff_coef$Estimate[1])
    lower <- quantile(sfha_mone, outlier)
    upper <- quantile(sfha_mone, 1-outlier)
    sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
    return(sfha_mone)
  })
  asi_mone <- Reduce("cbind",asi_mone)
  # asi_mone <- cbind(elasticity, asi_mone)
  asi_mone <- as.data.table(asi_mone)
  asi_wtp <- data.table(
    varname = c("Own Elasticity",names(white_iv)[2:nrow(s2_estimate)]),
    Estimate = as.numeric(c(-(s2_estimate$Estimate[1] + diff_coef$Estimate[1])*mean_salesprice,
                            asi_mone[, lapply(.SD, mean)])),
    `Std.Error` = as.numeric(c(sd(elasticity)/sqrt(nrow(meri_short)),
                               asi_mone[, lapply(.SD, sd)]/sqrt(nrow(meri_short))))
  )
  asi_wtp[, "t.stat" := Estimate/`Std.Error`]
  asi_wtp[, "p.value" := 2*pt(abs(`t.stat`), nrow(meri_short), lower=FALSE)]

  
  # combine the WTP results
  WTP <- cbind(white_wtp, bla_WTP, hisp_wtp, asi_wtp)
  
  # output the coefficient table
  setwd(output_folder)
  cap <- as.character(glue("Stage 2 IV for White, Black, Hispanic, and Asian.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
  heter_xtable <- xtable(WTP, caption=cap,
                         label = "",
                         digits = 4)
  print(heter_xtable, 
        file=paste0(output_name,".html"), type="html",
        include.rownames = F
  )
  
  rm(white_iv, black_iv, hisp_iv, asia_iv, diff)
  gc()
  
}



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

######
# summarize first stage estimate

setwd(first_stage_folder)

# first_stage_files <- list.files(first_stage_folder, pattern = ".rds")
first_stage_files <- paste0("s1_results_",1:48, ".rds")

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
  # dplyr::select(year,month, quarter,censustract,mkt_id,prop_id,saleamount_r, ln_saleamount,market,neigh_id,luxury,sfha,ln_sfha_dist,rooms,
  #               bathrooms,buildinggrosssquarefeet_r,harvey_inundation,harveyclaims_dist,harvey_claims_payout,sfha_dist,
  #               npl_dist, parks_dist, nhd_dist,
  #               prop_type, is_white:pred.asi, pred.whi:pred.oth) %>%
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

# find the number of transactions of each market
meri_short[, mkt_id_N := .N, by = list(mkt_id)]



######
# Synthenize the mean utility across different markets

# find the mean utlity for each market (year)
meri_short[, mean_u := abs(mean(Estimate, na.rm=T)), by = market]

# find the mean house price for each market (year)
meri_short[, mean_p := mean(saleamount_r, na.rm=T), by = market]
meri_short[, mean_p := mean_p / mean(mean_p)]

# ensure the mean of each utility unit is consistent
meri_short[, `Estimate.pred.bla:saleamount_r` := `Estimate.pred.bla:saleamount_r`/ mean_u]
meri_short[, `Std. error.pred.bla:saleamount_r` := `Std. error.pred.bla:saleamount_r`/ mean_u ]
meri_short[, `Estimate.pred.his:saleamount_r` := `Estimate.pred.his:saleamount_r`/ mean_u]
meri_short[, `Std. error.pred.his:saleamount_r` := `Std. error.pred.his:saleamount_r`/ mean_u]
meri_short[, `Estimate.pred.asi:saleamount_r` := `Estimate.pred.asi:saleamount_r`/ mean_u]
meri_short[, `Std. error.pred.asi:saleamount_r` := `Std. error.pred.asi:saleamount_r`/ mean_u]
meri_short[, `Estimate.pred.bla:sfha` := `Estimate.pred.bla:sfha`/ mean_u] 
meri_short[, `Std. error.pred.bla:sfha` := `Std. error.pred.bla:sfha`/ mean_u]
meri_short[, `Estimate.pred.his:sfha` := `Estimate.pred.his:sfha`/ mean_u]
meri_short[, `Std. error.pred.his:sfha` := `Std. error.pred.his:sfha`/ mean_u]
meri_short[, `Estimate.pred.asi:sfha` := `Estimate.pred.asi:sfha`/ mean_u]
meri_short[, `Std. error.pred.asi:sfha` := `Std. error.pred.asi:sfha`/ mean_u]

meri_short[, `Estimate.pred.bla:sfha_dist_0_500` := `Estimate.pred.bla:sfha_dist_0_500`/ mean_u] 
meri_short[, `Std. error.pred.bla:sfha_dist_0_500` := `Std. error.pred.bla:sfha_dist_0_500`/ mean_u]
meri_short[, `Estimate.pred.his:sfha_dist_0_500` := `Estimate.pred.his:sfha_dist_0_500`/ mean_u]
meri_short[, `Std. error.pred.his:sfha_dist_0_500` := `Std. error.pred.his:sfha_dist_0_500`/ mean_u]
meri_short[, `Estimate.pred.asi:sfha_dist_0_500` := `Estimate.pred.asi:sfha_dist_0_500`/ mean_u]
meri_short[, `Std. error.pred.asi:sfha_dist_0_500` := `Std. error.pred.asi:sfha_dist_0_500`/ mean_u]

meri_short[, `Estimate.pred.bla:sfha_dist_1000_2000` := `Estimate.pred.bla:sfha_dist_1000_2000`/ mean_u] 
meri_short[, `Std. error.pred.bla:sfha_dist_1000_2000` := `Std. error.pred.bla:sfha_dist_1000_2000`/ mean_u]
meri_short[, `Estimate.pred.his:sfha_dist_1000_2000` := `Estimate.pred.his:sfha_dist_1000_2000`/ mean_u]
meri_short[, `Std. error.pred.his:sfha_dist_1000_2000` := `Std. error.pred.his:sfha_dist_1000_2000`/ mean_u]
meri_short[, `Estimate.pred.asi:sfha_dist_1000_2000` := `Estimate.pred.asi:sfha_dist_1000_2000`/ mean_u]
meri_short[, `Std. error.pred.asi:sfha_dist_1000_2000` := `Std. error.pred.asi:sfha_dist_1000_2000`/ mean_u]

meri_short[, `Estimate.pred.bla:sfha_dist_500_1000` := `Estimate.pred.bla:sfha_dist_500_1000`/ mean_u] 
meri_short[, `Std. error.pred.bla:sfha_dist_500_1000` := `Std. error.pred.bla:sfha_dist_500_1000`/ mean_u]
meri_short[, `Estimate.pred.his:sfha_dist_500_1000` := `Estimate.pred.his:sfha_dist_500_1000`/ mean_u]
meri_short[, `Std. error.pred.his:sfha_dist_500_1000` := `Std. error.pred.his:sfha_dist_500_1000`/ mean_u]
meri_short[, `Estimate.pred.asi:sfha_dist_500_1000` := `Estimate.pred.asi:sfha_dist_500_1000`/ mean_u]
meri_short[, `Std. error.pred.asi:sfha_dist ` := `Std. error.pred.asi:sfha_dist_500_1000`/ mean_u]

meri_short[, Estimate := Estimate/mean_u]
meri_short[, `Std. error` := `Std. error`/mean_u]

# replace the baseline utility with 0
meri_short[is.na(Estimate), Estimate := 0]
meri_short[is.na(`Std. error`), `Std. error` := 0]



######
# load IV

setwd(paste0(path,"/Data"))
iv <- fread("IV.csv")
iv[, censustract := as.numeric(censustract_from)]
meri_short <- merge(meri_short, iv, by = c("censustract", "luxury"), all.x = T)

# define distance indicators
meri_short[, sfha_dist_0_500 := sfha_dist<=500 & sfha_dist>0 ]
meri_short[, sfha_dist_500_1000 := sfha_dist<=1000 & sfha_dist>500 ]
meri_short[, sfha_dist_1000_2000 := sfha_dist<=2000 & sfha_dist>1000]


######
# compute the average heterogeneity coefficients for all period

set.seed(1)


heter_draws <- lapply(1:Ndraws, function(x){
  heter_x <- meri_short %>% select(`Estimate.pred.bla:saleamount_r`,`Std. error.pred.bla:saleamount_r`,
                                   `Estimate.pred.his:saleamount_r`,`Std. error.pred.his:saleamount_r`,
                                   `Estimate.pred.asi:saleamount_r`,`Std. error.pred.asi:saleamount_r`,
                                   `Estimate.pred.bla:sfha`, `Std. error.pred.bla:sfha`,
                                   `Estimate.pred.his:sfha`, `Std. error.pred.his:sfha`,
                                   `Estimate.pred.asi:sfha`, `Std. error.pred.asi:sfha`,
                                   `Estimate.pred.bla:sfha_dist_500_1000`, `Std. error.pred.bla:sfha_dist_500_1000`,
                                   `Estimate.pred.his:sfha_dist_500_1000`, `Std. error.pred.his:sfha_dist_500_1000`,
                                   `Estimate.pred.asi:sfha_dist_500_1000`, `Std. error.pred.asi:sfha_dist_500_1000`,
                                   `Estimate.pred.bla:sfha_dist_0_500`, `Std. error.pred.bla:sfha_dist_0_500`,
                                   `Estimate.pred.his:sfha_dist_0_500`, `Std. error.pred.his:sfha_dist_0_500`,
                                   `Estimate.pred.asi:sfha_dist_0_500`, `Std. error.pred.asi:sfha_dist_0_500`,
                                   `Estimate.pred.bla:sfha_dist_1000_2000`, `Std. error.pred.bla:sfha_dist_1000_2000`,
                                   `Estimate.pred.his:sfha_dist_1000_2000`, `Std. error.pred.his:sfha_dist_1000_2000`,
                                   `Estimate.pred.asi:sfha_dist_1000_2000`, `Std. error.pred.asi:sfha_dist_1000_2000`,
                                   mkt_id_N) %>% 
    as.data.table()
  heter_x[, `pred.bla:saleamount_ran` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:saleamount_r`,abs(heter_x$`Std. error.pred.bla:saleamount_r`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.his:saleamount_ran` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:saleamount_r`,abs(heter_x$`Std. error.pred.his:saleamount_r`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.asi:saleamount_ran` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.asi:saleamount_r`,abs(heter_x$`Std. error.pred.asi:saleamount_r`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.bla:sfha_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:sfha`,abs(heter_x$`Std. error.pred.bla:sfha`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.his:sfha_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:sfha`,abs(heter_x$`Std. error.pred.his:sfha`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.asi:sfha_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.asi:sfha`,abs(heter_x$`Std. error.pred.asi:sfha`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.bla:sfha_dist_0_500_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:sfha_dist_0_500`,abs(heter_x$`Std. error.pred.bla:sfha_dist_0_500`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.his:sfha_dist_0_500_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:sfha_dist_0_500`,abs(heter_x$`Std. error.pred.his:sfha_dist_0_500`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.asi:sfha_dist_0_500_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.asi:sfha_dist_0_500`,abs(heter_x$`Std. error.pred.asi:sfha_dist_0_500`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.bla:sfha_dist_500_1000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:sfha_dist_500_1000`,abs(heter_x$`Std. error.pred.bla:sfha_dist_500_1000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.his:sfha_dist_500_1000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:sfha_dist_500_1000`,abs(heter_x$`Std. error.pred.his:sfha_dist_500_1000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.asi:sfha_dist_500_1000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.asi:sfha_dist_500_1000`,abs(heter_x$`Std. error.pred.asi:sfha_dist_500_1000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.bla:sfha_dist_1000_2000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:sfha_dist_1000_2000`,abs(heter_x$`Std. error.pred.bla:sfha_dist_1000_2000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.his:sfha_dist_1000_2000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:sfha_dist_1000_2000`,abs(heter_x$`Std. error.pred.his:sfha_dist_1000_2000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, `pred.asi:sfha_dist_1000_2000_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.asi:sfha_dist_1000_2000`,abs(heter_x$`Std. error.pred.asi:sfha_dist_1000_2000`*sqrt(heter_x$mkt_id_N)))]
  heter_x[, id := x]
  return(heter_x %>% select(id, `pred.bla:saleamount_ran`:`pred.asi:sfha_dist_1000_2000_r`) %>%
           cbind(meri_short %>% select(mkt_id, year, quarter, censustract)))
})
gc()
heter_draws <- rbindlist(heter_draws)
names(heter_draws) <- gsub(":","__",names(heter_draws))

heter_table <- lapply(names(heter_draws)[2:16], function(x){
  a <- feols(
    as.formula(paste0(x,"~1")  ),
    data = heter_draws,
    cluster = "censustract + mkt_id")
  summary(a)
  table <- data.table(a$coeftable)
  rm(a)
  gc()
  return(table)
})
heter_table <- rbindlist(heter_table)
heter_table <- as.data.frame(heter_table)
rownames(heter_table) <- names(heter_draws)[2:16]

# output the heterogeneity coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 1 Estimate: Heterogeneity Coefficients. ({Ndraws} draws)"))
heter_xtable <- xtable(heter_table, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202405_s1_hetero.html", type="html",
      include.rownames = T
)

gc()



######
# hetero coefficient by exposure to Harvey


# generate market index by year

heter_table_year <- lapply(1:12, function(xx){
  
  
  heter_xx <- heter_draws[mkt_id %in% as.character(1:48)[(xx*4-3):(xx*4)]]
  
  table <- lapply(names(heter_draws)[2:16], function(x){
    a <- feols(
      as.formula(paste0(x,"~1")  ),
      data = heter_xx,
      cluster = "censustract + mkt_id")
    summary(a)
    table <- data.table(a$coeftable)
    rm(a)
    gc()
    return(table)
  })
  
  table <- rbindlist(table)
  
  table <- as.data.frame(table)
  rownames(table) <- paste0(names(heter_draws)[2:16],"_",xx+2009)
  
  return(table)
})

heter_table_year_full <- rbindlist(heter_table_year)

# output the heterogeneity coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 1 Estimate by Year: Heterogeneity Coefficients. ({Ndraws} draws)"))
heter_xtable <- xtable(heter_table_year_full, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202405_s1_hetero_year.html", type="html",
      include.rownames = T
)
gc()



######
# hetero coefficient by year


# generate market index by year

heter_table_year <- lapply(1:12, function(xx){
  
  
  heter_xx <- heter_draws[mkt_id %in% as.character(1:48)[(xx*4-3):(xx*4)]]
  
  table <- lapply(names(heter_draws)[2:16], function(x){
    a <- feols(
      as.formula(paste0(x,"~1")  ),
      data = heter_xx,
      cluster = "censustract + mkt_id")
    summary(a)
    table <- data.table(a$coeftable)
    rm(a)
    gc()
    return(table)
  })
  
  table <- rbindlist(table)
  
  table <- as.data.frame(table)
  rownames(table) <- paste0(names(heter_draws)[2:16],"_",xx+2009)
  
  return(table)
})

heter_table_year_full <- rbindlist(heter_table_year)

# output the heterogeneity coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 1 Estimate by Year: Heterogeneity Coefficients. ({Ndraws} draws)"))
heter_xtable <- xtable(heter_table_year_full, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202405_s1_hetero_year.html", type="html",
      include.rownames = T
)
gc()


######
# summary statistics


setwd(output_folder)
datasummary(saleamount_r + sfha + sfha_dist_0_500 + sfha_dist_500_1000 + sfha_dist_1000_2000 + 
              sfha_dist + rooms + luxury + bathrooms + buildinggrosssquarefeet_r +
              npl_dist + parks_dist + nhd_dist  + pred.his + pred.bla + 
              pred.asi + pred.whi + pred.his + pred.bla + pred.asi ~ N + Mean + SD + Min + Max,
            data = meri_short %>% mutate(luxury = as.numeric(luxury),
                                         sfha_dist_0_500 = as.numeric(sfha_dist_0_500),
                                         sfha_dist_500_1000 = as.numeric(sfha_dist_500_1000),
                                         sfha_dist_1000_2000 = as.numeric(sfha_dist_1000_2000),
                                         pred.his = as.numeric(pred.his),
                                          pred.bla = as.numeric(pred.bla),
                                          pred.asi = as.numeric(pred.asi)),
            output = "summ.html")


######
# generate the second stage results

set.seed(1)

# meri_short1[, Estimate_r := rnorm(nrow(meri_short1),meri_short1$`Estimate`,meri_short1$`Std. error`)]
meri_short[, Estimate_r := Estimate]
meri_short[, saleamount := saleamount_r/1000]


# compute the second stage result for every month

s2_iv_avr <- feols(as.formula("Estimate_r ~  sfha + sfha_dist_0_500 + 
sfha_dist_500_1000  + sfha_dist_1000_2000 + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                    data = meri_short,
                    cluster = "censustract + mkt_id")
stage1_stat <- fitstat(s2_iv_hazard, ~ ivf1 + ivwald1 + ivf2 + ivwald2, cluster = "censustract+mkt_id")
# s2_iv_year <- feols(as.formula("Estimate_r ~  i(year,sfha) + i(year,sfha_dist_0_500) + 
# i(year,sfha_dist_500_1000)  +i(year,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
#   age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
#                           saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
#                data = meri_short,
#                cluster = "censustract + mkt_id")
s2_iv_hazard <- feols(as.formula("Estimate_r ~  i(hazard,sfha) + i(hazard,sfha_dist_0_500) + 
i(hazard,sfha_dist_500_1000)  +i(hazard,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                      data = meri_short %>% mutate(hazard = hazard>0, afterharvey = as.numeric(mkt_id)>31),
                      cluster = "censustract + mkt_id")
s2_iv_harvey <- feols(as.formula("Estimate_r ~  i(harvey,sfha) + i(harvey,sfha_dist_0_500) + 
i(harvey,sfha_dist_500_1000)  +i(harvey,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                    data = meri_short %>% mutate(harvey = harvey_inundation>0, afterharvey = as.numeric(mkt_id)>31),
                    cluster = "censustract + mkt_id")
s2_iv_harvey_treatd <- feols(as.formula("Estimate_r ~  i(harvey*afterharvey,sfha) + i(harvey*afterharvey,sfha_dist_0_500) + 
i(harvey*afterharvey,sfha_dist_500_1000)  +i(harvey*afterharvey,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                      data = meri_short %>% mutate(harvey = harvey_inundation>0, afterharvey = as.numeric(mkt_id)>31),
                      cluster = "censustract + mkt_id")
s2_iv_harvey_claims <- feols(as.formula("Estimate_r ~  i(harvey_claimed,sfha) + i(harvey_claimed,sfha_dist_0_500) + 
i(harvey_claimed,sfha_dist_500_1000)  +i(harvey_claimed,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                      data = meri_short %>% mutate(harvey_claimed = harvey_claims_payout>0, afterharvey = as.numeric(mkt_id)>31),
                      cluster = "censustract + mkt_id")
s2_iv_harvey_claims_treatd <- feols(as.formula("Estimate_r ~  i(harvey_claimed*afterharvey,sfha) + i(harvey_claimed*afterharvey,sfha_dist_0_500) + 
i(harvey_claimed*afterharvey,sfha_dist_500_1000)  +i(harvey_claimed*afterharvey,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                             data = meri_short %>% mutate(harvey_claimed = harvey_claims_payout>0, afterharvey = as.numeric(mkt_id)>31),
                             cluster = "censustract + mkt_id")
s2_iv_ike <- feols(as.formula("Estimate_r ~  i(ike,sfha) + i(ike,sfha_dist_0_500) + 
i(ike,sfha_dist_500_1000)  +i(ike,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                      data = meri_short %>% mutate(ike = ike_inunda>0, afterharvey = as.numeric(mkt_id)>31),
                      cluster = "censustract + mkt_id")
s2_iv_ike_claims <- feols(as.formula("Estimate_r ~  i(ike_claimed,sfha) + i(ike_claimed,sfha_dist_0_500) + 
i(ike_claimed,sfha_dist_500_1000)  +i(ike_claimed,sfha_dist_1000_2000) + luxury + rooms + bathrooms +
  age+ acres_r + stories + building_traditional + mobile_home + homestead  | censustract^year^month | 
                          saleamount ~ mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist + mean_road_dist  + mean_parks_dist"),
                   data = meri_short %>% mutate(ike_claimed = ike_claims_count>0, afterharvey = as.numeric(mkt_id)>31),
                   cluster = "censustract + mkt_id")

mod <- list(s2_iv_avr, s2_iv_hazard, s2_iv_harvey, s2_iv_harvey_treatd, s2_iv_harvey_claims, s2_iv_harvey_claims_treatd, 
            s2_iv_ike, s2_iv_ike_claims)  

setwd(output_folder)
modelsummary(mod,
             output = "202405_s2_diverse.html",
             title = glue("Stage 2 IV Estimate. (mean.utility = {round(mean(meri_short$Estimate_r, na.rm=T), digits=3)}), F-state = {round(stage1_stat[['ivf1::saleamount']][['stat']],2)}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             stars_note = T,
             coef_omit = "^(?!.*sfha|.*saleamount)",
             gof_omit='Within|BIC|AIC|Sigma|Pseudo|Log|RMSE',
             fmt = '%.3g',
             booktabs = T, escape = FALSE)




######
# generate the second stage results on average


mean_salesprice_23 <- 285189/1000 # use the  the average house value in Harris County at the end of 2023

s2_iv_estimate <- summary(s2_iv_avr)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_avr")

s2_iv_estimate <- summary(s2_iv_hazard)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_hazard")

s2_iv_estimate <- summary(s2_iv_harvey)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_harvey")

s2_iv_estimate <- summary(s2_iv_harvey_treatd)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_harvey_treated")

s2_iv_estimate <- summary(s2_iv_harvey_claims)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_harvey_claims")

s2_iv_estimate <- summary(s2_iv_harvey_claims_treatd)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_harvey_claims_treated")

s2_iv_estimate <- summary(s2_iv_ike)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_ike")

s2_iv_estimate <- summary(s2_iv_ike_claims)[["coeftable"]] %>% as.data.frame()
s2_WTP(s2_estimate = s2_iv_estimate, output_name = "202405_s2WTP_ike_claims")
######
# generate results with heterogeneity values


s2_ols_estimate <- summary(s2_ols1)[["coeftable"]] %>%
  as.data.frame()
s2_iv_estimate <- summary(s2_iv)[["coeftable"]] %>%
  as.data.frame()







######
# generate results with heterogeneity values
# before Harvey


s2_iv_estimate <- summary(s2_iv_before)[["coeftable"]] %>%
  as.data.frame()

Ndraws_mone <- 10000

mean_salesprice <- mean(meri_short$saleamount)

# IV results for white 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for White. Before Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_white_before.html", type="html",
      include.rownames = F
)


# IV results for Black 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_before_harvey[grepl("bla",rownames(heter_table_before_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Black. Before Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_black_before.html", type="html",
      include.rownames = F
)


# IV results for Hispanic 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_before_harvey[grepl("his",rownames(heter_table_before_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
# manually set the price coefficient as zero
diff_coef[1,1] <- 0
diff_coef[1,2] <- 0
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Hispanic. Before Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin2_mone_IV_hisp_before.html", type="html",
      include.rownames = F
)



# IV results for Asian 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_before_harvey[grepl("asi",rownames(heter_table_before_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Asian. Before Harvey
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_asi_before.html", type="html",
      include.rownames = F
)



######
# generate results with heterogeneity values
# after Harvey


s2_iv_estimate <- summary(s2_iv_after)[["coeftable"]] %>%
  as.data.frame()

Ndraws_mone <- 10000

mean_salesprice <- mean(meri_short$saleamount)

# IV results for white 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for White. after Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_white_after.html", type="html",
      include.rownames = F
)


# IV results for Black 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_after_harvey[grepl("bla",rownames(heter_table_after_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Black. after Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_black_after.html", type="html",
      include.rownames = F
)


# IV results for Hispanic 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_after_harvey[grepl("his",rownames(heter_table_after_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
# manually set the price coefficient as zero
diff_coef[1,1] <- 0
diff_coef[1,2] <- 0
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Hispanic. after Harvey.
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin2_mone_IV_hisp_after.html", type="html",
      include.rownames = F
)



# IV results for Asian 
set.seed(1)
white_iv <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
})
white_iv <- Reduce("rbind",white_iv)
colnames(white_iv) <- rownames(s2_iv_estimate)
diff_coef <- heter_table_after_harvey[grepl("asi",rownames(heter_table_after_harvey)),]
# replace price coefficient to zero if it is not significant
if(diff_coef[1,4]>=0.01){
  diff_coef[1,1]=0
  diff_coef[1,2]=0
}
set.seed(1)
diff <- lapply(1:Ndraws_mone, function(x){
  as.data.table(t(rnorm(nrow(diff_coef), diff_coef$Estimate, diff_coef$`Std. Error`)))
})
diff <- Reduce("rbind",diff)
white_iv[,1:5] <- white_iv[,1:5] + diff
white_iv[, elasticity := -mean_salesprice*fit_saleamount]
elasticity <- white_iv$elasticity
lower <- quantile(elasticity, 0.001)
upper <- quantile(elasticity, 0.999)
elasticity <- elasticity[elasticity<upper & elasticity>lower]
whi_mone <- lapply(names(white_iv)[2:8], function(x){
  sfha_mone <- -white_iv[,get(x)]/white_iv[,get("fit_saleamount")] 
  lower <- quantile(sfha_mone, 0.001)
  upper <- quantile(sfha_mone, 0.999)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  return(sfha_mone)
})
whi_mone <- Reduce("cbind",whi_mone)
whi_mone <- cbind(elasticity, whi_mone)
whi_mone <- as.data.table(whi_mone)
white_iv <- data.table(
  varname = c("Own Elasticity",names(white_iv)[2:8]),
  Estimate = as.numeric(whi_mone[, lapply(.SD, mean)]),
  `Std.Error` = as.numeric(whi_mone[, lapply(.SD, sd)]/sqrt(Ndraws))
)
white_iv[, "t.stat" := Estimate/`Std.Error`]
white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws_mone, lower=FALSE)]
# output the coefficient table
setwd(output_folder)
cap <- as.character(glue("Stage 2 IV for Asian. after Harvey
                         Dependent Variable: WTP in Monetary Value ($1000). ({Ndraws_mone} draws)"))
heter_xtable <- xtable(white_iv, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="202404_s2_dist_bin_mone_IV_asi_after.html", type="html",
      include.rownames = F
)


######
# find how the WTP changes over time

meri_short1 <- meri_short1 %>%
  arrange(year)
meri_short1[, yearmonth := paste0(year,"_", month)]
time_range <- meri_short1$year %>% unique


white_year_result <- data.frame()
black_year_result <- data.frame()
hisp_year_result <- data.frame()


for(i in time_range){
  
  meri_short_i <- meri_short[year==i]
  
  # obtain stage 1 result for year i
  set.seed(1)
  heter_draws_i <- lapply(1:Ndraws, function(x){
    heter_x <- meri_short_i %>% select(`Estimate.pred.bla:saleamount_r`,`Std. error.pred.bla:saleamount_r`,
                                       `Estimate.pred.his:saleamount_r`,`Std. error.pred.his:saleamount_r`,
                                       `Estimate.pred.bla:sfha`, `Std. error.pred.bla:sfha`,
                                       `Estimate.pred.his:sfha`, `Std. error.pred.his:sfha`) %>% 
      as.data.table()
    heter_x[, `pred.bla:saleamount_ran` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:saleamount_r`,heter_x$`Std. error.pred.bla:saleamount_r`*sqrt(nrow(heter_x)))]
    heter_x[, `pred.his:saleamount_ran` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:saleamount_r`,heter_x$`Std. error.pred.his:saleamount_r`*sqrt(nrow(heter_x)))]
    heter_x[, `pred.bla:sfha_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.bla:sfha`,heter_x$`Std. error.pred.bla:sfha`*sqrt(nrow(heter_x)))]
    heter_x[, `pred.his:sfha_r` := rnorm(nrow(heter_x),heter_x$`Estimate.pred.his:sfha`,heter_x$`Std. error.pred.his:sfha`*sqrt(nrow(heter_x)))]
    heter_x[, `pred.bla:saleamount_mean` := mean(`pred.bla:saleamount_ran`, na.rm = T)]
    heter_x[, `pred.his:saleamount_mean` := mean(`pred.his:saleamount_ran`, na.rm = T)]
    heter_x[, `pred.bla:sfha_mean` := mean(`pred.bla:sfha_r`, na.rm = T)]
    heter_x[, `pred.his:sfha_mean` := mean(`pred.his:sfha_r`, na.rm = T)]
    heter_x[, id := x]
    return(heter_x %>% select(id, `pred.bla:saleamount_ran`:`pred.his:sfha_mean`))
  })
  gc()
  heter_draws_i <- rbindlist(heter_draws_i)
  # remove outliers for each random draw vector of coefficients
  lower <- quantile(heter_draws_i$`pred.bla:saleamount_ran`, 0.01)
  upper <- quantile(heter_draws_i$`pred.bla:saleamount_ran`, 0.99)
  pred.bla.saleamount_ran <- heter_draws_i$`pred.bla:saleamount_ran`[ heter_draws_i$`pred.bla:saleamount_ran`<upper &
                                                                        heter_draws_i$`pred.bla:saleamount_ran`>lower]
  lower <- quantile(heter_draws_i$`pred.his:saleamount_ran`, 0.01)
  upper <- quantile(heter_draws_i$`pred.his:saleamount_ran`, 0.99)
  pred.his.saleamount_ran <- heter_draws_i$`pred.his:saleamount_ran`[ heter_draws_i$`pred.his:saleamount_ran`<upper &
                                                                        heter_draws_i$`pred.his:saleamount_ran`>lower]
  lower <- quantile(heter_draws_i$`pred.bla:sfha_r`, 0.01)
  upper <- quantile(heter_draws_i$`pred.bla:sfha_r`, 0.99)
  pred.bla.sfha_r <- heter_draws_i$`pred.bla:sfha_r`[ heter_draws_i$`pred.bla:sfha_r`<upper &
                                                        heter_draws_i$`pred.bla:sfha_r`>lower]
  lower <- quantile(heter_draws_i$`pred.his:sfha_r`, 0.01)
  upper <- quantile(heter_draws_i$`pred.his:sfha_r`, 0.99)
  pred.his.sfha_r <- heter_draws_i$`pred.his:sfha_r`[ heter_draws_i$`pred.his:sfha_r`<upper &
                                                        heter_draws_i$`pred.his:sfha_r`>lower]
  # generate the heterogeneity coefficient table
  heter_table_i <- data.table(`Estimate-pred.bla:saleamount`=mean(pred.bla.saleamount_ran, na.rm=T),
                              `Std.error-pred.bla:saleamount`=sd(pred.bla.saleamount_ran, na.rm=T)/sqrt(nrow(meri_short)),
                              `Estimate-pred.his:saleamount`=mean(pred.his.saleamount_ran, na.rm=T),
                              `Std.error-pred.his:saleamount`=sd(pred.his.saleamount_ran, na.rm=T)/sqrt(nrow(meri_short)),
                              `Estimate-pred.bla:sfha`=mean(pred.bla.sfha_r, na.rm=T),
                              `Std.error-pred.bla:sfha`=sd(pred.bla.sfha_r, na.rm=T)/sqrt(nrow(meri_short)),
                              `Estimate-pred.his:sfha`=mean(pred.his.sfha_r, na.rm=T),
                              `Std.error-pred.his:sfha`=sd(pred.his.sfha_r, na.rm=T)/sqrt(nrow(meri_short)))
  heter_table_i <- heter_table_i %>% melt(variable.name = "var")
  heter_table_i[, type := sub("\\-.*", "", var)]
  heter_table_i[, varname := sub(".*\\-", "", var)]
  heter_table_i[, var := NULL]
  heter_table_i <- heter_table_i %>% 
    reshape(idvar = "varname", timevar = "type", direction = "wide")
  colnames(heter_table_i) <- c("heter_var", "Estimate","Std.Error")
  heter_table_i[, "t.stat" := Estimate/`Std.Error`]
  heter_table_i[, "p.value" := 2*pt(abs(`t.stat`), Ndraws, lower=FALSE)]
  
  
  
  
  # obtain stage 2 iv result for year i
  s2_iv_i <- feols(as.formula("Estimate_r ~  sfha + ln_sfha_dist + luxury + rooms + bathrooms  | year^month+censustract | 
                          saleamount ~  mean_airport_dist +  mean_nhd_dist + mean_npl_dist + mean_rivers_dist +  mean_parks_dist + 
                          mean_road_dist "),
                   data = meri_short1[year==i],
                   cluster = "censustract")
  s2_iv_estimate <- summary(s2_iv_i)[["coeftable"]] %>%
    as.data.frame()
  
  
  
  # obtain draws for iv coefficients
  set.seed(1)
  base_iv <- lapply(1:Ndraws, function(x){
    as.data.table(t(rnorm(nrow(s2_iv_estimate), s2_iv_estimate$Estimate, s2_iv_estimate$`Std. Error`)))
  })
  base_iv <- Reduce("rbind",base_iv)
  
  
  
  # generate WTP for white
  white_iv <- base_iv
  colnames(white_iv) <- rownames(s2_ols_estimate)
  mean_salesprice <- mean(meri_short1$saleamount)
  white_iv[, elasticity := -mean_salesprice*saleamount]
  elasticity <- white_iv$elasticity
  lower <- quantile(elasticity, 0.01)
  upper <- quantile(elasticity, 0.99)
  elasticity <- white_iv$elasticity
  white_iv[, sfha_mone := -sfha/saleamount]
  sfha_mone <- white_iv$sfha_mone
  lower <- quantile(sfha_mone, 0.01)
  upper <- quantile(sfha_mone, 0.99)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  white_iv[, ln_sfha_dist_mone := -ln_sfha_dist/saleamount]
  ln_sfha_dist_mone <- white_iv$ln_sfha_dist_mone
  lower <- quantile(ln_sfha_dist_mone, 0.01)
  upper <- quantile(ln_sfha_dist_mone, 0.99)
  ln_sfha_dist_mone <- ln_sfha_dist_mone[ln_sfha_dist_mone<upper & ln_sfha_dist_mone>lower]
  white_iv[, luxuryTRUE_mone := -luxuryTRUE/saleamount]
  luxuryTRUE_mone <- white_iv$luxuryTRUE_mone
  lower <- quantile(luxuryTRUE_mone, 0.01)
  upper <- quantile(luxuryTRUE_mone, 0.99)
  luxuryTRUE_mone <- luxuryTRUE_mone[luxuryTRUE_mone<upper & luxuryTRUE_mone>lower]
  white_iv[, rooms_mone := -rooms/saleamount]
  rooms_mone <- white_iv$rooms_mone
  lower <- quantile(rooms_mone, 0.01)
  upper <- quantile(rooms_mone, 0.99)
  rooms_mone <- rooms_mone[rooms_mone<upper & rooms_mone>lower]
  white_iv[, bathrooms_mone := -bathrooms/saleamount]
  bathrooms_mone <- white_iv$bathrooms_mone
  lower <- quantile(bathrooms_mone, 0.01)
  upper <- quantile(bathrooms_mone, 0.99)
  bathrooms_mone <- bathrooms_mone[bathrooms_mone<upper & bathrooms_mone>lower]
  white_iv <- data.table(
    varname = c("elasticity","sfha","ln_sfha_dist","luxuryTRUE","rooms","bathrooms"),
    Estimate = c(mean(elasticity),mean(sfha_mone),mean(ln_sfha_dist_mone),mean(luxuryTRUE_mone),
                 mean(rooms_mone),mean(bathrooms_mone)),
    `Std.Error` = c(sd(elasticity)/sqrt(Ndraws),
                    sd(sfha_mone)/sqrt(Ndraws),sd(ln_sfha_dist_mone)/sqrt(Ndraws),
                    sd(luxuryTRUE_mone)/sqrt(Ndraws),sd(rooms_mone)/sqrt(Ndraws),
                    sd(bathrooms_mone)/sqrt(Ndraws))
  )
  white_iv[, "t.stat" := Estimate/`Std.Error`]
  white_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws, lower=FALSE)]
  white_iv[, year := i]
  white_year_result <- rbind(white_year_result, white_iv)
  
  
  
  # generate WTP for black
  set.seed(1)
  black_saleamount_diff <- lapply(1:Ndraws, function(x){
    as.data.table(t(rnorm(1, heter_table_i$Estimate[1], heter_table_i$Std.Error[1])))
  })
  black_saleamount_diff <- Reduce("rbind",black_saleamount_diff)
  set.seed(1)
  black_sfha_diff <- lapply(1:Ndraws, function(x){
    as.data.table(t(rnorm(1, heter_table_i$Estimate[3], heter_table_i$Std.Error[3])))
  })
  black_sfha_diff <- Reduce("rbind",black_sfha_diff)
  black_iv <- base_iv
  black_iv$V1 <- black_iv$V1  + black_saleamount_diff$V1
  black_iv$V2 <- black_iv$V2  + black_sfha_diff$V1
  colnames(black_iv) <- rownames(s2_ols_estimate)
  mean_salesprice <- mean(meri_short1$saleamount)
  black_iv[, elasticity := -mean_salesprice*saleamount]
  elasticity <- black_iv$elasticity
  lower <- quantile(elasticity, 0.01)
  upper <- quantile(elasticity, 0.99)
  elasticity <- black_iv$elasticity
  black_iv[, sfha_mone := -sfha/saleamount]
  sfha_mone <- black_iv$sfha_mone
  lower <- quantile(sfha_mone, 0.01)
  upper <- quantile(sfha_mone, 0.99)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  black_iv[, ln_sfha_dist_mone := -ln_sfha_dist/saleamount]
  ln_sfha_dist_mone <- black_iv$ln_sfha_dist_mone
  lower <- quantile(ln_sfha_dist_mone, 0.01)
  upper <- quantile(ln_sfha_dist_mone, 0.99)
  ln_sfha_dist_mone <- ln_sfha_dist_mone[ln_sfha_dist_mone<upper & ln_sfha_dist_mone>lower]
  black_iv[, luxuryTRUE_mone := -luxuryTRUE/saleamount]
  luxuryTRUE_mone <- black_iv$luxuryTRUE_mone
  lower <- quantile(luxuryTRUE_mone, 0.01)
  upper <- quantile(luxuryTRUE_mone, 0.99)
  luxuryTRUE_mone <- luxuryTRUE_mone[luxuryTRUE_mone<upper & luxuryTRUE_mone>lower]
  black_iv[, rooms_mone := -rooms/saleamount]
  rooms_mone <- black_iv$rooms_mone
  lower <- quantile(rooms_mone, 0.01)
  upper <- quantile(rooms_mone, 0.99)
  rooms_mone <- rooms_mone[rooms_mone<upper & rooms_mone>lower]
  black_iv[, bathrooms_mone := -bathrooms/saleamount]
  bathrooms_mone <- black_iv$bathrooms_mone
  lower <- quantile(bathrooms_mone, 0.01)
  upper <- quantile(bathrooms_mone, 0.99)
  bathrooms_mone <- bathrooms_mone[bathrooms_mone<upper & bathrooms_mone>lower]
  black_iv <- data.table(
    varname = c("elasticity","sfha","ln_sfha_dist","luxuryTRUE","rooms","bathrooms"),
    Estimate = c(mean(elasticity),mean(sfha_mone),mean(ln_sfha_dist_mone),mean(luxuryTRUE_mone),
                 mean(rooms_mone),mean(bathrooms_mone)),
    `Std.Error` = c(sd(elasticity)/sqrt(Ndraws),
                    sd(sfha_mone)/sqrt(Ndraws),sd(ln_sfha_dist_mone)/sqrt(Ndraws),
                    sd(luxuryTRUE_mone)/sqrt(Ndraws),sd(rooms_mone)/sqrt(Ndraws),
                    sd(bathrooms_mone)/sqrt(Ndraws))
  )
  black_iv[, "t.stat" := Estimate/`Std.Error`]
  black_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws, lower=FALSE)]
  black_iv[, year := i]
  black_year_result <- rbind(black_year_result, black_iv)
  
  
  
  # generate WTP for hispanic
  set.seed(1)
  black_saleamount_diff <- lapply(1:Ndraws, function(x){
    as.data.table(t(rnorm(1, heter_table_i$Estimate[2], heter_table_i$Std.Error[2])))
  })
  black_saleamount_diff <- Reduce("rbind",black_saleamount_diff)
  set.seed(1)
  black_sfha_diff <- lapply(1:Ndraws, function(x){
    as.data.table(t(rnorm(1, heter_table_i$Estimate[4], heter_table_i$Std.Error[4])))
  })
  black_sfha_diff <- Reduce("rbind",black_sfha_diff)
  black_iv <- base_iv
  black_iv$V1 <- black_iv$V1  + black_saleamount_diff$V1
  black_iv$V2 <- black_iv$V2  + black_sfha_diff$V1
  colnames(black_iv) <- rownames(s2_ols_estimate)
  mean_salesprice <- mean(meri_short1$saleamount)
  black_iv[, elasticity := -mean_salesprice*saleamount]
  elasticity <- black_iv$elasticity
  lower <- quantile(elasticity, 0.01)
  upper <- quantile(elasticity, 0.99)
  elasticity <- black_iv$elasticity
  black_iv[, sfha_mone := -sfha/saleamount]
  sfha_mone <- black_iv$sfha_mone
  lower <- quantile(sfha_mone, 0.01)
  upper <- quantile(sfha_mone, 0.99)
  sfha_mone <- sfha_mone[sfha_mone<upper & sfha_mone>lower]
  black_iv[, ln_sfha_dist_mone := -ln_sfha_dist/saleamount]
  ln_sfha_dist_mone <- black_iv$ln_sfha_dist_mone
  lower <- quantile(ln_sfha_dist_mone, 0.01)
  upper <- quantile(ln_sfha_dist_mone, 0.99)
  ln_sfha_dist_mone <- ln_sfha_dist_mone[ln_sfha_dist_mone<upper & ln_sfha_dist_mone>lower]
  black_iv[, luxuryTRUE_mone := -luxuryTRUE/saleamount]
  luxuryTRUE_mone <- black_iv$luxuryTRUE_mone
  lower <- quantile(luxuryTRUE_mone, 0.01)
  upper <- quantile(luxuryTRUE_mone, 0.99)
  luxuryTRUE_mone <- luxuryTRUE_mone[luxuryTRUE_mone<upper & luxuryTRUE_mone>lower]
  black_iv[, rooms_mone := -rooms/saleamount]
  rooms_mone <- black_iv$rooms_mone
  lower <- quantile(rooms_mone, 0.01)
  upper <- quantile(rooms_mone, 0.99)
  rooms_mone <- rooms_mone[rooms_mone<upper & rooms_mone>lower]
  black_iv[, bathrooms_mone := -bathrooms/saleamount]
  bathrooms_mone <- black_iv$bathrooms_mone
  lower <- quantile(bathrooms_mone, 0.01)
  upper <- quantile(bathrooms_mone, 0.99)
  bathrooms_mone <- bathrooms_mone[bathrooms_mone<upper & bathrooms_mone>lower]
  black_iv <- data.table(
    varname = c("elasticity","sfha","ln_sfha_dist","luxuryTRUE","rooms","bathrooms"),
    Estimate = c(mean(elasticity),mean(sfha_mone),mean(ln_sfha_dist_mone),mean(luxuryTRUE_mone),
                 mean(rooms_mone),mean(bathrooms_mone)),
    `Std.Error` = c(sd(elasticity)/sqrt(Ndraws),
                    sd(sfha_mone)/sqrt(Ndraws),sd(ln_sfha_dist_mone)/sqrt(Ndraws),
                    sd(luxuryTRUE_mone)/sqrt(Ndraws),sd(rooms_mone)/sqrt(Ndraws),
                    sd(bathrooms_mone)/sqrt(Ndraws))
  )
  black_iv[, "t.stat" := Estimate/`Std.Error`]
  black_iv[, "p.value" := 2*pt(abs(`t.stat`), Ndraws, lower=FALSE)]
  black_iv[, year := i]
  hisp_year_result <- rbind(hisp_year_result, black_iv)
  
}

# generate the number of observations by year
number_year <-  meri_short$year %>% as.factor %>% summary 
number_year = data.frame(year = names(number_year),
                         number = number_year) 


# plot the WTP for sfha of white and black population by year
white_year_result_sfha <- white_year_result[year<2022 & varname=="sfha"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
white_year_result_sfha[, Race := "White/Hispanic"]
black_year_result_sfha <- black_year_result[year<2022 & varname=="sfha"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
black_year_result_sfha[, Race := "Black"]
sfha_result_year <- rbind(white_year_result_sfha, black_year_result_sfha)
sfha_result_year[, lower := Estimate - 1.96*Std.Error]
sfha_result_year[, upper := Estimate + 1.96*Std.Error]
figure <- ggplot(data=sfha_result_year, aes(x = year,group = Race)) +
  geom_line(aes( y = -Estimate, color = Race)) +
  geom_ribbon(aes(ymin = -lower, ymax = -upper, fill = Race), alpha = 0.2) +
  theme_void() +
  labs(title = "Lifetime WTP ($1000) for Avoding SFHA",
       x = "Year",
       y = "Coefficient and 95% CI") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(color = "black"),  # Customize x axis text
        axis.text.y = element_text(color = "black"),  # Customize y axis text
        axis.ticks = element_line(color = "black"),  # Customize axis ticks
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))  # Set length of ticks
setwd(figure_folder)
ggsave("WTP_sfha_year_race.png", plot = figure, scale = 1.2)



# plot the WTP for distance from sfha of white and black population by year
white_year_result_sfha <- white_year_result[year<2022 & varname=="ln_sfha_dist"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
white_year_result_sfha[, Race := "White/Hispanic"]
black_year_result_sfha <- black_year_result[year<2022 & varname=="ln_sfha_dist"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
black_year_result_sfha[, Race := "Black"]
sfha_result_year <- rbind(white_year_result_sfha, black_year_result_sfha)
sfha_result_year[, lower := Estimate - 1.96*Std.Error]
sfha_result_year[, upper := Estimate + 1.96*Std.Error]
figure <- ggplot(data=sfha_result_year, aes(x = year,group = Race)) +
  geom_line(aes( y = Estimate, color = Race)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Race), alpha = 0.2) +
  theme_void() +
  labs(title = "Lifetime WTP ($1000) for Distance from SFHA (ln_sfha_dist)",
       x = "Year",
       y = "Coefficient and 95% CI") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(color = "black"),  # Customize x axis text
        axis.text.y = element_text(color = "black"),  # Customize y axis text
        axis.ticks = element_line(color = "black"),  # Customize axis ticks
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))  # Set length of ticks
setwd(figure_folder)
ggsave("WTP_ln_sfha_dist_year_race.png", plot = figure, scale = 1.2)




# plot the price elasticity of white and black population by year
white_year_result_sfha <- white_year_result[year<2022 & varname=="elasticity"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
white_year_result_sfha[, Race := "White/Hispanic"]
black_year_result_sfha <- black_year_result[year<2022 & varname=="elasticity"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
black_year_result_sfha[, Race := "Black"]
sfha_result_year <- rbind(white_year_result_sfha, black_year_result_sfha)
sfha_result_year[, lower := Estimate - 1.96*Std.Error]
sfha_result_year[, upper := Estimate + 1.96*Std.Error]
figure <- ggplot(data=sfha_result_year, aes(x = year,group = Race)) +
  geom_line(aes( y = Estimate, color = Race)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Race), alpha = 0.2) +
  theme_void() +
  labs(title = "Price elasticity for demand of housing",
       x = "Year",
       y = "Coefficient and 95% CI") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(color = "black"),  # Customize x axis text
        axis.text.y = element_text(color = "black"),  # Customize y axis text
        axis.ticks = element_line(color = "black"),  # Customize axis ticks
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))  # Set length of ticks
setwd(figure_folder)
ggsave("elasticity_year_race.png", plot = figure, scale = 1.2)



# plot the WTP for luxury house of white and black population by year
white_year_result_sfha <- white_year_result[year<2022 & varname=="luxuryTRUE"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
white_year_result_sfha[, Race := "White/Hispanic"]
black_year_result_sfha <- black_year_result[year<2022 & varname=="luxuryTRUE"] %>%
  mutate(year = as.character(year)) %>%
  merge(number_year, by="year", all.x=T)
black_year_result_sfha[, Race := "Black"]
sfha_result_year <- rbind(white_year_result_sfha, black_year_result_sfha)
sfha_result_year[, lower := Estimate - 1.96*Std.Error]
sfha_result_year[, upper := Estimate + 1.96*Std.Error]
figure <- ggplot(data=sfha_result_year, aes(x = year,group = Race)) +
  geom_line(aes( y = Estimate, color = Race)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Race), alpha = 0.2) +
  theme_void() +
  labs(title = "Lifetime WTP ($1000) for Luxury Residence",
       x = "Year",
       y = "Coefficient and 95% CI") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(color = "black"),  # Customize x axis text
        axis.text.y = element_text(color = "black"),  # Customize y axis text
        axis.ticks = element_line(color = "black"),  # Customize axis ticks
        axis.ticks.length = unit(0.15, "cm"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"))  # Set length of ticks
setwd(figure_folder)
ggsave("WTP_luxury_year_race.png", plot = figure, scale = 1.2)

