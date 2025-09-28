#*
#*This file aggregate welfare change in each climate scenario
#*Author: Wei Guo
#*Last edit: 6/5/2024
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
library(sf)
library(future.apply)

sf_use_s2(FALSE)

path <<-  dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

data_folder <<-  paste0(path,"/Data")
program_folder <<- paste0(path,"/Program/R")
output_folder <<- paste0(path, "/Output/Table")
figure_folder <<- paste0(path, "/Output/Figure")

# sink(paste0("/S1_summ_log.txt"))

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}


##############
# generate distance indicators for each scenario

# load the asmt data, the distance to sfha, and the distance to the floodmap in each scenario
setwd(paste0(data_folder,"/texas_a_m_galveston_property_basic_res2_300000423089087_20220916_085147_data"))
asmt <- fread("texas_a_m_galveston_property_basic_res2_300000423089087_20220916_085147_data.txt") 
asmt <- asmt %>%
  select(CLIP, `OWNER 1 LAST NAME`, `PARCEL LEVEL LATITUDE`, `PARCEL LEVEL LONGITUDE`,
         `OWNER 1 CORPORATE INDICATOR`, `FIPS CODE`, `MARKET TOTAL VALUE`)
names(asmt) <- c("clip", "last_name", "lat", "long", "corp", "fips", "mkt_value")
asmt <- asmt[!is.na(lat)]
asmt_sf <- st_as_sf(x = asmt,
                    coords = c("long", "lat"),
                    crs = st_crs(4326))

# find the race info of each home owner
asmt_name <- asmt[!last_name=="", c("clip", "last_name", "fips")]
names(asmt_name) <- c("clip","surname", "county")
asmt_name[, county := str_sub(county,3,5)]
asmt_name[, state := "TX"]
asmt_race <- predict_race(voter.file = asmt_name,
                           year = "2020",
                           surname.only = F,
                           census.geo = "county",
                          impute.missing = F,
                          retry = 3
)
# 88438 (7.4%) individuals' last names were not matched.
asmt_sf <- asmt_sf %>%
  as.data.table() %>%
  merge(asmt_race %>% select(clip, pred.whi:pred.oth), by = "clip", all.x = T) %>%
  st_as_sf()

# replace missing race info with the average level
avr.whi <- mean(asmt_race$pred.whi, na.rm=T)
avr.bla <- mean(asmt_race$pred.bla, na.rm=T)
avr.his <- mean(asmt_race$pred.his, na.rm=T)
avr.asi <- mean(asmt_race$pred.asi, na.rm=T)
avr.oth <- mean(asmt_race$pred.oth, na.rm=T)
asmt_sf <- as.data.table(asmt_sf)
asmt_sf[ is.na(pred.whi), pred.whi := avr.whi]
asmt_sf[ is.na(pred.bla), pred.bla := avr.bla]
asmt_sf[ is.na(pred.his), pred.his := avr.his]
asmt_sf[ is.na(pred.asi), pred.asi := avr.asi]
asmt_sf[ is.na(pred.oth), pred.oth := avr.oth]
asmt_sf <- st_as_sf(asmt_sf)
rm(asmt_race, asmt_name)


# load the joint data with sfha
setwd(paste0(data_folder,"/asmt_prop"))
asmt_sfha <- read_fst("asmt_sfha.fst", as.data.table = T)
asmt_sfha[is.na(sfha), sfha := 0]


# load floodplain data
setwd(paste0(data_folder,"/Floodplain_shapefile_status"))
floodplain <- st_read("fl_map.shp")

# load sfha
setwd(paste0(data_folder, "/harrissfha/harrissfha"))
sfha <- read_sf("Floodplain_100yr.shp")
sfha <- st_union(sfha)
sfha <- st_as_sf(sfha) %>% st_zm


# # generate a map with the worst future scenarios and sfha
# map <- mapview(floodplain[5,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[5]) + 
#   mapview(floodplain[6,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[6])+ 
#   mapview(floodplain[11,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[11])+ 
#   mapview(floodplain[12,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[12])+ 
#   mapview(floodplain[17,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[17])+ 
#   mapview(floodplain[20,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[20])+ 
#   mapview(floodplain[22,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[22])+ 
#   mapview(floodplain[24,] %>% st_simplify(dTolerance = 100), layer = floodplain$floodmap[24])+
#   mapview(sfha,col.regions = "red")
# setwd(paste0(data_folder,"/Floodplain_shapefile_status"))
# mapshot(map, 
#         url = paste0("sfha_floodmap.html"))


# load flood map coverage data
setwd(paste0(data_folder))
asmt_floodmap <- read_fst("asmt_floodmap_corrected.fst", as.data.table = T)
names(asmt_floodmap)[8] <- "coastal"
asmt_floodmap1 <- read_fst("asmt_floodmap.fst", as.data.table = T)
asmt_floodmap <- asmt_floodmap %>%
  mutate(clip = NULL) %>% 
 cbind(asmt_floodmap1 %>% select(clip, 
                                  inun_CanESM_Present_barrier:dist_HadGEM6_Present_no_barrier))


# add sfha and floodmap info to the full asmt data
asmt_sf <- asmt_sf %>%
  as.data.table() %>%
  merge(asmt_sfha, by = c("clip", "last_name",   "corp",  "fips", "mkt_value"), all.x = T) %>%
  merge(asmt_floodmap %>%
          select(-last_name, -corp, -fips, -mkt_value, -sfha, -sfha_dist), by = c("clip"), all.x = T)
rm(asmt_sfha, asmt_floodmap)

# create the distance indicators for each scenario
sce <- c("sfha",names(asmt_sf)[grepl("inun_",  names(asmt_sf))])
dist <- lapply(sce, function(x){
  inun <- asmt_sf[, get(x)]
  if(x=="sfha"){name <- "sfha"}else{
    name <- gsub("inun_","",x)
  }
  if(x=="sfha"){x <- "sfha_dist"}else{
    x <- gsub("inun_","dist_",x)
  }
  dist <- asmt_sf[, get(x)]
  dist_500 <- dist<=500 & dist>=0
  dist_500_1000 <- (dist>500)&(dist<=1000)
  dist_1000_2000 <- (dist>1000)&(dist<=2000)
  df <- data.table(inun = inun, dist = dist, dist_500 = dist_500,
                   dist_500_1000 = dist_500_1000, dist_1000_2000 = dist_1000_2000)
  names(df) <- paste0(name,"_",names(df))
  return(df)
})
dist_df <- Reduce("cbind",dist)
dist_df$coastal <- asmt_sf$coastal
dist_df$clip <- asmt_sf$clip
dist_df$fips <- asmt_sf$fips
dist_df$mkt_value <- asmt_sf$mkt_value
setwd(paste0(data_folder))
write_fst(dist_df, "floodplain_dist_indi_corrected.fst")

# add race info
setwd(paste0(data_folder))
dist_df <- read_fst("floodplain_dist_indi_corrected.fst", as.data.table = T)
dist_df <- dist_df %>%
  merge(asmt_sf %>% as.data.table %>% select(clip, pred.whi:pred.oth),
        by = "clip", all.x = T)
setwd(paste0(data_folder))
write_fst(dist_df, "floodplain_dist_indi_corrected.fst")




##############
# summary statistics in each scenario

setwd(paste0(data_folder))
dist_df <- read_fst("floodplain_dist_indi_corrected.fst", as.data.table = T)

# generate summary statistics of inundation status and distance to sfha for coastal areas
dist_df[is.na(coastal), coastal := 0]
dist_df_0 <- dist_df[coastal==0] %>% select(clip, fips, mkt_value,pred.whi:pred.oth,coastal,sfha_inun:sfha_dist_1000_2000)
logical_cols <- sapply(dist_df_0, is.logical)
dist_df_0[, (names(dist_df_0)[logical_cols]) := 
            lapply(.SD, as.numeric), .SDcols = logical_cols]
setwd(paste0(data_folder))
datasummary(All(dist_df_0)~N+Mean+SD+Min+Max,
            data = dist_df_0,
            title = "Non-Coastal Area",
            output = 'non_coastal_sfha_corrected.html')

# generate summary statistics of inundation status and distance to sfha and each flood plain
dist_df_1 <- dist_df[coastal==1] %>% select(clip, fips, mkt_value,pred.whi:pred.oth,coastal,sfha_inun:HadGEM6_Present_no_barrier_dist_1000_2000)
logical_cols <- sapply(dist_df_1, is.logical)
dist_df_1[, (names(dist_df_1)[logical_cols]) := 
            lapply(.SD, as.numeric), .SDcols = logical_cols]
setwd(paste0(data_folder))
datasummary(All(dist_df_1)~N+Mean+SD+Min+Max,
            data = dist_df_1,
            title = "Coastal Area",
            output = 'coastal_floodplain_corrected.html')



# generate the summary statistics weighted by probability of each race

setwd(paste0(data_folder))
dist_df <- read_fst("floodplain_dist_indi_corrected.fst", as.data.table = T)
dist_df[is.na(coastal), coastal := 0]

# classify other race as white in the WTP calculation
for(i in c("whi","bla","his","asi")){
  dist_df_0 <- dist_df[coastal==0] %>% select(clip,pred.whi:pred.oth,sfha_inun:sfha_dist_1000_2000)
  dist_df_0[, pred.whi := (pred.whi + pred.oth)]
  dist_df_0[, sfha_inun := sfha_inun==1]
  dist_df_1 <- dist_df[coastal==1] %>% select(clip,pred.whi:pred.oth,sfha_inun:HadGEM6_Present_no_barrier_dist_1000_2000)
  dist_df_1[, pred.whi := (pred.whi + pred.oth)]
  dist_df_1[, sfha_inun := sfha_inun==1]

  race_col <- paste0("pred.",i)
  logical_cols <- names(dist_df_0)[sapply(dist_df_0, is.logical)]
  dist_df_0[, (logical_cols) := 
              lapply(.SD, function(x) as.numeric(x) * get(race_col)), 
            .SDcols = logical_cols]  
  datasummary(All(dist_df_0)~N+Mean+SD+Min+Max,
              data = dist_df_0,
              title = paste0("Non-Coastal Area, weighted by ", race_col),
              output = paste0('corrected_non_coastal_sfha_',i,'.html'))
  
  logical_cols <- names(dist_df_1)[sapply(dist_df_1, is.logical)]
  dist_df_1[, (logical_cols) := 
              lapply(.SD, function(x) as.numeric(x) * get(race_col)), 
            .SDcols = logical_cols]  
  datasummary(All(dist_df_1)~N+Mean+SD+Min+Max,
              data = dist_df_1,
              title = paste0("Coastal Area, weighted by ", race_col),
              output = paste0('corrected_coastal_sfha_',i,'.html'))
}


##############
# WTP aggregation


setwd(paste0(data_folder))
dist_df <- read_fst("floodplain_dist_indi_corrected.fst", as.data.table = T)
dist_df[is.na(coastal), coastal := 0]
inun_cols <- grep("_inun$", names(dist_df), value = TRUE)
for (inun_col in inun_cols) {
  dist_col <- sub("_inun$", "_dist_500", inun_col)
  if (dist_col %in% names(dist_df)) {
    dist_df[get(inun_col) == 1, (dist_col) := TRUE]
  }
}

# load coefficient table
setwd(dirname(output_folder))
coef <- readxl::read_xlsx("Results_202405.xlsx", sheet = "WTP_for_estimate") %>%
  as.data.table()
rownames(coef) <- coef$varname

# non-coastal area
dist_df_0 <- dist_df[coastal==0] %>% select(clip,pred.whi:pred.oth,sfha_inun:sfha_dist_1000_2000)
dist_df_0[, pred.whi := (pred.whi + pred.oth)]
dist_df_0[, sfha_inun := sfha_inun==1]
non_coast_WTP <- data.frame(
  sfha_inun_pop = as.numeric(dist_df_0[sfha_inun==T,2:5][, lapply(.SD, sum)]),
  sfha_inun_WTP = as.numeric(coef[varname=="sfha",2:5]) *
    as.numeric(dist_df_0[sfha_inun==T,2:5][, lapply(.SD, sum)]),
  sfha_0_500_pop = as.numeric(dist_df_0[sfha_dist_500==T,2:5][, lapply(.SD, sum)]),
  sfha_0_500_WTP = as.numeric(coef[varname=="sfha_dist_0_500TRUE",2:5]) *
    as.numeric(dist_df_0[sfha_dist_500==T,2:5][, lapply(.SD, sum)]),
  sfha_500_1000_pop = as.numeric(dist_df_0[sfha_dist_500_1000==T,2:5][, lapply(.SD, sum)]),
  sfha_500_1000_WTP = as.numeric(coef[varname=="sfha_dist_500_1000TRUE",2:5]) *
    as.numeric(dist_df_0[sfha_dist_500_1000==T,2:5][, lapply(.SD, sum)]),
  sfha_1000_2000_pop = as.numeric(dist_df_0[sfha_dist_1000_2000==T,2:5][, lapply(.SD, sum)]),
  sfha_1000_2000_WTP = as.numeric(coef[varname=="sfha_dist_1000_2000TRUE",2:5]) *
    as.numeric(dist_df_0[sfha_dist_1000_2000==T,2:5][, lapply(.SD, sum)]),
  sfha_2000_out_pop = as.numeric(dist_df_0[sfha_inun==F & sfha_dist_500==F & sfha_dist_500_1000==F  &
                                  sfha_dist_1000_2000==F,2:5][, lapply(.SD, sum)])
)
non_coast_WTP <- as.data.table(non_coast_WTP)
non_coast_WTP[, agg_WTP := rowSums(.SD), .SDcols = patterns("_WTP")]
rownames(non_coast_WTP) <- names(coef)[2:5]
non_coast_WTP1 <- as_tibble(t(non_coast_WTP), .name_repair = "minimal") %>%
  as.data.frame()
colnames(non_coast_WTP1) <- rownames(non_coast_WTP)
rownames(non_coast_WTP1) <- colnames(non_coast_WTP)
setwd(output_folder)
cap <- as.character(glue("Non-Coastal Areas"))
heter_xtable <- xtable(non_coast_WTP1, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="corrected_non_cstl_wtp.html", type="html",
      include.rownames = T
)


# coastal area, present scenario
dist_df_1 <- dist_df[coastal==1] %>% select(clip,pred.whi:pred.oth,sfha_inun:HadGEM6_Present_no_barrier_dist_1000_2000)
dist_df_1[, pred.whi := (pred.whi + pred.oth)]
dist_df_1[, sfha_inun := sfha_inun==1]
pre_scn <- names(dist_df_1)[grepl("_Present",names(dist_df_1))]
pre_scn <- c("sfha",sub("(_inun|_dist).*", "", pre_scn) %>% unique)
coast_WTP <- data.frame()
for(i in pre_scn){
  WTP_i <- data.frame(
    sfha_inun_pop = as.numeric(dist_df_1[get(paste0(i,"_inun"))==T,2:5][, lapply(.SD, sum)]),
    sfha_inun_WTP = as.numeric(coef[varname=="sfha",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_inun"))==T,2:5][, lapply(.SD, sum)]),
    sfha_0_500_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_500"))==T,2:5][, lapply(.SD, sum)]),
    sfha_0_500_WTP = as.numeric(coef[varname=="sfha_dist_0_500TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_500"))==T,2:5][, lapply(.SD, sum)]),
    sfha_500_1000_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_500_1000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_500_1000_WTP = as.numeric(coef[varname=="sfha_dist_500_1000TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_500_1000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_1000_2000_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_1000_2000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_1000_2000_WTP = as.numeric(coef[varname=="sfha_dist_1000_2000TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_1000_2000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_2000_out_pop = as.numeric(dist_df_1[get(paste0(i,"_inun"))==F & get(paste0(i,"_dist_500"))==F & get(paste0(i,"_dist_500_1000"))==F  &
                                               get(paste0(i,"_dist_1000_2000"))==F,2:5][, lapply(.SD, sum)])
  )
  names(WTP_i) <- gsub("sfha",i,names(WTP_i))
  WTP_i <- as.data.table(WTP_i)
  WTP_i[, agg_WTP := rowSums(.SD), .SDcols = patterns("_WTP")]
  names(WTP_i)[ncol(WTP_i)] <- paste0(i,"_agg_WTP")
  rownames(WTP_i) <- names(coef)[2:5]
  WTP_i1 <- as_tibble(t(WTP_i), .name_repair = "minimal") %>%
    as.data.frame()
  colnames(WTP_i1) <- rownames(WTP_i)
  rownames(WTP_i1) <- colnames(WTP_i)
  setwd(output_folder)
  cap <- as.character(glue("Coastal Areas {i}"))
  heter_xtable <- xtable(WTP_i1, caption=cap,
                         label = "",
                         digits = 4)
  print(heter_xtable, file=paste0("cstl_",i,"_wtp.html"), type="html",
        include.rownames = T)
  coast_WTP <- rbind(coast_WTP, as.data.frame(WTP_i1))
}
setwd(output_folder)
cap <- as.character(glue("Coastal Areas Present Scenarios"))
heter_xtable <- xtable(coast_WTP, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="corrected_cstl_pre_wtp.html", type="html",
      include.rownames = T
)



# coastal area, future scenario
dist_df_1 <- dist_df[coastal==1] %>% select(clip,pred.whi:pred.oth,sfha_inun:HadGEM6_Present_no_barrier_dist_1000_2000)
dist_df_1[, pred.whi := (pred.whi + pred.oth)]
dist_df_1[, sfha_inun := sfha_inun==1]
fut_scn <- names(dist_df_1)[grepl("_SLR",names(dist_df_1))]
fut_scn <- sub("(_inun|_dist).*", "", fut_scn) %>% unique
coast_WTP <- data.frame()
for(i in fut_scn){
  WTP_i <- data.frame(
    sfha_inun_pop = as.numeric(dist_df_1[get(paste0(i,"_inun"))==T,2:5][, lapply(.SD, sum)]),
    sfha_inun_WTP = as.numeric(coef[varname=="sfha",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_inun"))==T,2:5][, lapply(.SD, sum)]),
    sfha_0_500_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_500"))==T,2:5][, lapply(.SD, sum)]),
    sfha_0_500_WTP = as.numeric(coef[varname=="sfha_dist_0_500TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_500"))==T,2:5][, lapply(.SD, sum)]),
    sfha_500_1000_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_500_1000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_500_1000_WTP = as.numeric(coef[varname=="sfha_dist_500_1000TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_500_1000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_1000_2000_pop = as.numeric(dist_df_1[get(paste0(i,"_dist_1000_2000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_1000_2000_WTP = as.numeric(coef[varname=="sfha_dist_1000_2000TRUE",2:5]) *
      as.numeric(dist_df_1[get(paste0(i,"_dist_1000_2000"))==T,2:5][, lapply(.SD, sum)]),
    sfha_2000_out_pop = as.numeric(dist_df_1[get(paste0(i,"_inun"))==F & get(paste0(i,"_dist_500"))==F & get(paste0(i,"_dist_500_1000"))==F  &
                                               get(paste0(i,"_dist_1000_2000"))==F,2:5][, lapply(.SD, sum)])
  )
  names(WTP_i) <- gsub("sfha",i,names(WTP_i))
  WTP_i <- as.data.table(WTP_i)
  WTP_i[, agg_WTP := rowSums(.SD), .SDcols = patterns("_WTP")]
  names(WTP_i)[ncol(WTP_i)] <- paste0(i,"_agg_WTP")
  rownames(WTP_i) <- names(coef)[2:5]
  WTP_i1 <- as_tibble(t(WTP_i), .name_repair = "minimal") %>%
    as.data.frame()
  colnames(WTP_i1) <- rownames(WTP_i)
  rownames(WTP_i1) <- colnames(WTP_i)
  setwd(output_folder)
  cap <- as.character(glue("Coastal Areas {i}"))
  heter_xtable <- xtable(WTP_i1, caption=cap,
                         label = "",
                         digits = 4)
  print(heter_xtable, file=paste0("corrected_cstl_",i,"_wtp.html"), type="html",
        include.rownames = T)
  coast_WTP <- rbind(coast_WTP, as.data.frame(WTP_i1))
}
setwd(output_folder)
cap <- as.character(glue("Coastal Areas Future SLR Scenarios"))
heter_xtable <- xtable(coast_WTP, caption=cap,
                       label = "",
                       digits = 4)
print(heter_xtable, file="corrected_cstl_slr_wtp.html", type="html",
      include.rownames = T
)

gc()
