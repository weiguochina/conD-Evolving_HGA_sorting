#*
#*This file merges the sfha and floodplain with the assessment data
#*Author: Wei Guo
#*Last edit: 5/28/2024
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



######
# load data

# assessment data

asmt <- fread("asmt.txt") 
asmt <- asmt %>%
  select(CLIP, `OWNER 1 LAST NAME`, `PARCEL LEVEL LATITUDE`, `PARCEL LEVEL LONGITUDE`,
         `OWNER 1 CORPORATE INDICATOR`, `FIPS CODE`, `MARKET TOTAL VALUE`)
names(asmt) <- c("clip", "last_name", "lat", "long", "corp", "fips", "mkt_value")
asmt <- asmt[!is.na(lat)]
asmt_sf <- st_as_sf(x = asmt,
                    coords = c("long", "lat"),
                    crs = st_crs(4326))

# load sfha
setwd(paste0(data_folder, "/harrissfha/harrissfha"))
sfha <- read_sf("Floodplain_100yr.shp")
sfha <- st_union(sfha)


# for each property, find whether it is inside sfha
asmt_sf <- as.data.table(asmt_sf)
asmt_sf[, int := c( sf::st_intersects( geometry, sfha ))]
asmt_sf1 <- asmt_sf[ , list( int = unlist(int) ) , by = list(clip) ]
asmt_sf[, int := NULL]
setkey(asmt_sf,"clip")
setkey(asmt_sf1,"clip")
asmt_sf <- merge(asmt_sf, asmt_sf1, all.x=T)
rm(asmt_sf1)
asmt_sf[, sfha := int]
asmt_sf[, int := NULL]


# for each property, find distance to sfha (<500, 500-1000, 1000-2000, >2000)
setwd(paste0(data_folder, "/harrissfha/harrissfha"))
sfha <- read_sf("Floodplain_100yr.shp") %>%
  st_buffer(0) %>%
  st_make_valid() %>%
  st_transform(  st_crs(4087))  %>%
  st_union()
asmt_sf <- asmt_sf%>%
  st_as_sf %>%
  st_transform(  st_crs(4087))
sfha <- sfha %>% st_crop(st_bbox(st_as_sf(asmt_sf)))



# find the distance from each property to sfha
# use paralleling to do the segmentation
options(future.globals.maxSize = 1e9)
plan(multisession, workers = 10) # adjust the number of workers by your #threads and memory
dist <- future_lapply(seq_len(ceiling(nrow(asmt_sf)/100)), function(x){
  x1 <- (x-1)*100 + 1
  x2 <- min(nrow(asmt_sf), 100*x)
  st_distance(asmt_sf[x1:x2,], sfha)
})
sfha_dist <- Reduce(c, dist)
asmt_sf$sfha_dist <- sfha_dist
rm(sfha_dist)

# save the asmt_sf data
setwd(paste0(data_folder,"/asmt_prop"))
write_fst(asmt_sf %>% as.data.table() %>% select(-geometry),
         "asmt_sfha.fst")


##############
# reload the asmt data and it merged with sfha

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

setwd(paste0(data_folder,"/asmt_prop"))
asmt_sfha <- read_fst("asmt_sfha.fst", as.data.table = T)
asmt_sfha[is.na(sfha), sfha := 0]

asmt_sf <- asmt_sf %>%
  as.data.table() %>%
  merge(asmt_sfha, by = c("clip", "last_name",   "corp",  "fips", "mkt_value"), all.x = T)
rm(asmt_sfha)

asmt_sf <- asmt_sf %>%
  st_as_sf %>%
  st_transform(  st_crs(4087))

asmt_sf_full <- asmt_sf

# find the parcels within 2km of the possible flood area
setwd(paste0(data_folder,"/Floodplain_shapefile_status/Floodplain_shapefile_status/"))
pre_scn <- list.files(path = getwd(), 
                      pattern = "\\.shp$",
                      recursive = T,
                      full.names = T)
pre_names <- list.files(path = getwd(), 
                      pattern = "\\.shp$",
                      recursive = T,
                      full.names = F) %>%
  str_extract("(?<=/)[^/]*(?=\\.shp)")
fl_map <- lapply(seq_len(length(pre_scn)), function(x){
  a <- read_sf(pre_scn[x]) %>%
    st_buffer(dist = as.numeric(grid_dist), nQuadSegs = 1) %>%
    st_transform(4087) %>%
    st_buffer(2000)%>% 
    st_union() %>%
    st_as_sf 
  a$floodmap <- pre_names[x]
  return(a)
})
fl_map <- rbindlist(fl_map)
setwd(paste0(data_folder,"/Floodplain_shapefile_status"))
st_write(fl_map, "fl_map.shp")
fl_map <- fl_map %>%
  st_as_sf() %>%
  st_union
fl_map <- fl_map %>%
  st_as_sf() 
fl_map$flood <- 1
fl_map_union <- fl_map


asmt_sf <- asmt_sf_full %>% 
  st_join(fl_map)
asmt_sf <- asmt_sf %>% subset(flood==1)
setwd(paste0(data_folder,"/Floodplain_shapefile_status"))
st_write(asmt_sf, "asmt_inside_floodmap_union.shp")


##############
## join with floodplain data

# present climate

setwd(paste0(data_folder,"/Floodplain_shapefile_status/Floodplain_shapefile_status/Present_climate"))
pre_scn <- list.dirs(path = getwd(), full.names = T)
pre_scn <- pre_scn[-1]

# find the distance between grids
shp_file <- list.files(path = pre_scn[1], pattern = "\\.shp$", full.names = T)
fl_map <- read_sf(shp_file)
fl_map <- st_zm(fl_map)
grid_dist <- sqrt(sum(c(diff(rbind(st_bbox(fl_map[1,]), 
                                   st_bbox(fl_map[3,])))[1:2])^2))/sqrt(2)

# loop over scenarios
for(x in pre_scn){
  
  # load the flood map
  shp_file <- list.files(path = x, pattern = "\\.shp$", full.names = T)
  fl_map <- read_sf(shp_file) %>%
    st_buffer(dist = as.numeric(grid_dist), nQuadSegs = 1) %>%
    st_transform(  st_crs(4087)) 
  shp_name <- list.files(path = x, pattern = "\\.shp$", full.names = F) %>%
    gsub(pattern = ".shp",replacement = "")
  
  # find the inundated area
  # fl_map %>% st_as_sf %>% group_by(Field3) %>% summarise() %>% st_simplify(dTolerance = 50) %>% mapview
  inun_map <- st_union(fl_map %>% subset(Field3==1)) %>% st_as_sf
  
  # find properties inundated in this scenario
  asmt_sf <- as.data.table(asmt_sf)
  asmt_sf[, int := c( sf::st_intersects( geometry, inun_map ))]
  asmt_sf1 <- asmt_sf[ , list( int = unlist(int) ) , by = list(clip) ]
  asmt_sf[, int := NULL]
  setkey(asmt_sf,"clip")
  setkey(asmt_sf1,"clip")
  asmt_sf <- merge(asmt_sf, asmt_sf1, all.x=T)
  rm(asmt_sf1)
  asmt_sf[is.na(int), int := 0]
  names(asmt_sf)[ncol(asmt_sf)] <- paste0("inun_", shp_name)
  
  # find the distance to the flood map in this scenario
  options(future.globals.maxSize = 1e9)
  asmt_sf <- st_as_sf(asmt_sf)
  tic()
  plan(multisession, workers = 10) #   adjust the number of workers by your #threads and memory
  dist <- future_lapply(seq_len(ceiling(nrow(asmt_sf)/200)), function(x){
    x1 <- (x-1)*200 + 1
    x2 <- min(nrow(asmt_sf), 200*x)
    data.table(st_distance(asmt_sf[x1:x2,], inun_map) %>% as.numeric)
  })
  plan(sequential)
  toc()
  sfha_dist <- rbindlist(dist)
  asmt_sf$floopmap_dist <- sfha_dist$V1
  names(asmt_sf)[ncol(asmt_sf)] <- paste0("dist_", shp_name)
  rm(sfha_dist, dist)
}




# future climate scenarios

setwd(paste0(data_folder,"/Floodplain_shapefile_status/Floodplain_shapefile_status/Future_climate"))
fut_scn <- list.dirs(path = getwd(), full.names = T, recursive = F)

for(i in fut_scn){
  
  setwd(i)
  pre_scn <- list.dirs(path = getwd(), full.names = T, recursive = F)

  # loop over scenarios
  for(x in pre_scn){
    
    # load the flood map
    shp_file <- list.files(path = x, pattern = "\\.shp$", full.names = T)
    fl_map <- read_sf(shp_file) %>%
      st_buffer(dist = as.numeric(grid_dist), nQuadSegs = 1) %>%
      st_transform(  st_crs(4087)) 
    shp_name <- list.files(path = x, pattern = "\\.shp$", full.names = F) %>%
      gsub(pattern = ".shp",replacement = "")
    
    # find the inundated area
    # fl_map %>% st_as_sf %>% group_by(Field3) %>% summarise() %>% st_simplify(dTolerance = 50) %>% mapview
    inun_map <- st_union(fl_map %>% subset(Field3==1)) %>% st_as_sf
    
    # find properties inundated in this scenario
    asmt_sf <- as.data.table(asmt_sf)
    asmt_sf[, int := c( sf::st_intersects( geometry, inun_map ))]
    asmt_sf1 <- asmt_sf[ , list( int = unlist(int) ) , by = list(clip) ]
    asmt_sf[, int := NULL]
    setkey(asmt_sf,"clip")
    setkey(asmt_sf1,"clip")
    asmt_sf <- merge(asmt_sf, asmt_sf1, all.x=T)
    rm(asmt_sf1)
    asmt_sf[is.na(int), int := 0]
    names(asmt_sf)[ncol(asmt_sf)] <- paste0("inun_", shp_name)
    
    # find the distance to the flood map in this scenario
    options(future.globals.maxSize = 1e9)
    asmt_sf <- st_as_sf(asmt_sf)
    tic()
    plan(multisession, workers = 10) #   adjust the number of workers by your #threads and memory
    dist <- future_lapply(seq_len(ceiling(nrow(asmt_sf)/200)), function(x){
      x1 <- (x-1)*200 + 1
      x2 <- min(nrow(asmt_sf), 200*x)
      data.table(st_distance(asmt_sf[x1:x2,], inun_map) %>% as.numeric)
    })
    plan(sequential)
    toc()
    sfha_dist <- rbindlist(dist)
    asmt_sf$floopmap_dist <- sfha_dist$V1
    names(asmt_sf)[ncol(asmt_sf)] <- paste0("dist_", shp_name)
    rm(sfha_dist, dist)
  }
}


#######################################
# save the merged asmt data
asmt_flood <- asmt_sf %>%
  as.data.table() %>%
  select(-geometry)
setwd(data_folder)
write_fst(asmt_flood, "asmt_floodmap.fst")



#######################################
# correct future scenario calculation
# as 1w/SLR + 2w/SLR - 2w/oSLR

setwd(paste0(data_folder,"/Floodplain_shapefile_status"))
asmt_sf <- st_read("asmt_inside_floodmap_union.shp")

# future climate scenarios

setwd(paste0(data_folder,"/Floodplain_shapefile_status/Floodplain_shapefile_status/Future_climate"))
fut_scn <- list.dirs(path = getwd(), full.names = T, recursive = F)

for(i in fut_scn){
  
  setwd(i)
  slr_scn <- list.dirs(path = getwd(), full.names = T, recursive = F)
  
  
  # loop over scenarios
  for(x in slr_scn){
    
    # load misclassified as being submerged properties
    setwd(paste0(data_folder,"/Floodplain_shapefile_status/Floodplain_shapefile_status/Present_climate/",
                 gsub("_","",basename(i)),"_Present_",
                 ifelse(grepl("no_barrier",x),"no_barrier","barrier")
                 ))
    shp_file <- list.files(pattern = "\\.shp$", full.names = T)
    submerged_model_bias <-  read_sf(shp_file) %>%
      st_buffer(dist = as.numeric(grid_dist), nQuadSegs = 1) %>%
      st_transform(  st_crs(4087)) 
    submerged_model_bias <- st_union(submerged_model_bias %>% subset(Field3==2)) %>% st_as_sf
    
    # load the flood map
    shp_file <- list.files(path = x, pattern = "\\.shp$", full.names = T)
    fl_map <- read_sf(shp_file) %>%
      st_buffer(dist = as.numeric(grid_dist), nQuadSegs = 1) %>%
      st_transform(  st_crs(4087)) 
    shp_name <- list.files(path = x, pattern = "\\.shp$", full.names = F) %>%
      gsub(pattern = ".shp",replacement = "")
    
    
    # find the inundated area
    # fl_map %>% st_as_sf %>% group_by(Field3) %>% summarise() %>% st_simplify(dTolerance = 50) %>% mapview
    inun_map <- st_union(fl_map %>% subset(Field3%in%c(1,2))) %>% st_as_sf %>%
      st_difference(submerged_model_bias)

    # find properties inundated in this scenario
    asmt_sf <- as.data.table(asmt_sf)
    asmt_sf[, int := c( sf::st_intersects( geometry, inun_map ))]
    asmt_sf1 <- asmt_sf[ , list( int = unlist(int) ) , by = list(clip) ]
    asmt_sf[, int := NULL]
    setkey(asmt_sf,"clip")
    setkey(asmt_sf1,"clip")
    asmt_sf <- merge(asmt_sf, asmt_sf1, all.x=T)
    rm(asmt_sf1)
    asmt_sf[is.na(int), int := 0]
    names(asmt_sf)[ncol(asmt_sf)] <- paste0("inun_corrected_", shp_name)
    
    # find the distance to the flood map in this scenario
    options(future.globals.maxSize = 1e9)
    asmt_sf <- st_as_sf(asmt_sf)
    tic()
    plan(multisession, workers = 10) #   adjust the number of workers by your #threads and memory
    dist <- future_lapply(seq_len(ceiling(nrow(asmt_sf)/200)), function(x){
      x1 <- (x-1)*200 + 1
      x2 <- min(nrow(asmt_sf), 200*x)
      data.table(st_distance(asmt_sf[x1:x2,], inun_map) %>% as.numeric)
    })
    plan(sequential)
    toc()
    sfha_dist <- rbindlist(dist)
    asmt_sf$floopmap_dist <- sfha_dist$V1
    names(asmt_sf)[ncol(asmt_sf)] <- paste0("dist_corrected_", shp_name)
    rm(sfha_dist, dist)
  }
}

asmt_flood <- asmt_sf %>%
  as.data.table() %>%
  select(-geometry)
setwd(data_folder)
write_fst(asmt_flood, "asmt_floodmap_corrected.fst")

