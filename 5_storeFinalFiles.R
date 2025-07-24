

### store final files, round them

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(terra)
library(sf)

library(dplyr)
library(tidyverse)

library(tidyterra)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1. read files -----

yearsIn <- 1990:2023


cntry_info <- read_csv("input/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code))



r_adm0 <- rast(paste0('results/rast_gnic_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_adm1 <- rast(paste0('results/rast_gnic_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 


p_adm0 <- st_read(paste0('results/polyg_gnic_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm0 <- vect(paste0('results/polyg_gnic_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm1 <- st_read(paste0('results/vect_gnic_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm1 <- vect(paste0('results/vect_gnic_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))


#### 2. write rasters ----


# create folder if not exist

if (dir.exists('results_final/')) {
  
} else {
  dir.create('results_final/')  
}


terra::writeRaster(round(r_adm0,0),paste0('results_final/rast_adm0_gni_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm1,0),paste0('results_final/rast_adm1_gni_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW", overwrite=TRUE)



#### 3. round and write polygons ----


v_gni_pc_adm0 <- v_adm0 %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 0))) %>% 
  mutate(across(all_of('slope'), ~ round(., 5))) %>% 
  select(-GID_nmbr) %>% 
  select(iso3, Country, cntry_id, everything())

v_gni_pc_adm1 <- v_adm1 %>% 
  mutate(across(all_of(as.character(yearsIn)), ~ round(., 0))) %>% 
  mutate(across(all_of('slope'), ~ round(., 5))) %>% 
  left_join(v_gni_pc_adm0 %>% as_tibble() %>% select(iso3, Country, cntry_id)) %>% 
  select(iso3, Country,cntry_id, region, admID, GDLCODE, everything()) %>% 
  select(-estimate, -p.value) #%>%
  #set_names(c('iso3', 'Country', 'cntry_id', 'region', 'admID', 'GDLCODE', 'slope', paste0(1990:2021)))

writeVector(v_gni_pc_adm0,paste0('results_final/polyg_adm0_gni_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gni_pc_adm1,paste0('results_final/polyg_adm1_gni_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)



#### 4.  write tabulated results ----

t_adm0 <- as_tibble(v_gni_pc_adm0)
t_adm1 <- as_tibble(v_gni_pc_adm1)



write_csv(t_adm0, 'results_final/tabulated_adm0_gni_perCapita.csv')
write_csv(t_adm1, 'results_final/tabulated_adm1_gni_perCapita.csv')
