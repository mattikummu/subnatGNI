
## prepare adm1 level HDI components

library(openxlsx)

library(sf)

library(zoo)

library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### 1. preparations ------

# 1.1 set time steps ------
timestep <- c(seq(1990, 2022))
step <- c(seq(1,length(timestep)))

### 1.2 load cntry_info


cntry_info <- read_csv("input/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code))

cntry_info_temp <- cntry_info %>% 
  rename(country = country_name) %>% 
  select(country,iso_code)

### 2. read admin 1 data -----


HDI_data <- read_csv('input/SHDI-SGDI-Total 8.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, region, year, GDLCODE, level,  gnic) 


HDI_data_filled <- HDI_data 


# gadm_410-levels.gpkg available from https://gadm.org 

adm0_GADM_data <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  st_drop_geometry() %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>%  # kosovo to correct iso3
  rename(iso3 = GID_0)




### 3  interpolate HDI component adm1 dataset ----------------
# variableName = 'cci'

source('functions/f_hdi_Adm1_interp.R')


### run function -----------------------------------------------------

# variables: lifexp; gnic;  esch;  msch 


st_hdiComponent_gnic <- f_hdi_Adm1_interp('gnic') 


adm1_ratioAdm1Adm0_interp <- st_hdiComponent_gnic %>% 
  left_join(HDI_data %>% select(GDLCODE, region) %>% distinct())


write_csv(adm1_ratioAdm1Adm0_interp, 'results/adm1_ratioAdm1Adm0_interp.csv')



