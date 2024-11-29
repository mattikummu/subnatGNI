
# prepare national data for HDI components

library(openxlsx)
library(zoo)
library(broom)

library(sf)
library(terra)

library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### 1. preparations ------

# 1.1 set time steps ------
timestep <- c(seq(1990, 2021))
step <- c(seq(1,length(timestep)))

### 1.2 load cntry_info
cntry_info <- read_csv("input/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code))


#### 2 national datasets----

# UN national database
# https://hdr.undp.org/data-center/documentation-and-downloads
UN_HDI_db <- read_csv('input/HDR23-24_Composite_indices_complete_time_series.csv') %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3))


# SHDI national data
SHDI_data <- read_csv('input/SHDI-SGDI-Total 7.0.csv') %>% 
  rename(iso3 = iso_code) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% # kosovo to correct iso3
  filter(region == 'Total')  # only national data


#### 2.2 income ----



# GNI per capita, PPP (constant 2017 international $)

# first years of Nauru scaled with Tuvalu, as extrapolation resulted negative values
NRU_gnipc <- as_tibble(read.xlsx("input/nauru_gnic.xlsx", startRow = 1)) %>% 
  filter(iso3 == "NRU") %>% 
  select(iso3, country, paste0('gnipc_',1990:2021))

UN_HDI_gnipc <- UN_HDI_db %>% 
  select(iso3, country, paste0('gnipc_',1990:2021)) %>% 
  filter(nchar(iso3) == 3) %>% # filter out regional values
  filter(!if_all(contains('gnipc'), is.na)) %>% # exclude rows without data
  filter(!iso3 == "NRU") %>% 
  bind_rows(NRU_gnipc)



PRK_gnipc <-  as_tibble(read.xlsx("input/gnic_PRK_PRI.xlsx")) %>% 
  filter(iso_code == 'PRK') %>% 
  select(-c(GDLCODE, level, region, continent)) %>% 
  rename(iso3 = iso_code) %>% 
  set_names('iso3', 'country', paste0('gnipc_',1990:2019))%>% 
  select(iso3, country, everything())


KSV_gnipc <-  as_tibble(read.xlsx("input/kosovo_gni.xlsx", startRow = 3)) %>% 
  filter(source == 'filled') %>% 
  select(-source) %>% 
  set_names('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code', paste0('X',1990:2023))




# GNI per capita, PPP (constant 2021 international $)

WB_data_gnipc <- read.csv('input/API_NY/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_2051334.csv', skip = 4) %>% 
  as_tibble() %>% 
  filter(!Country.Code == 'XKX') %>% 
  bind_rows(KSV_gnipc) %>% 
  rename(iso3 = Country.Code) %>% 
  rename(country = Country.Name) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
  select(iso3, country, paste0('X',1990:2021)) %>% 
  set_names('iso3', 'country', paste0('gnipc_',1990:2021)) %>% 
  filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
  filter(!if_all(contains('gnipc_'), is.na))  %>%  # exclude rows without data
  filter(!iso3=='PRK') %>% 
  bind_rows(PRK_gnipc)


indexValue <- as_tibble(readxl::read_xls("input/annual-index-value_annual-percent-change.xls", skip = 3)) %>%
  set_names(c('year', 'index', 'change'))  %>%
  filter(year %in% c(2017,2021))

index2017_2021 <- indexValue$index[1] /  indexValue$index[2]

WB_data_gnipc_Scaled <- WB_data_gnipc %>%
  mutate_if(is.numeric, ~.*!!index2017_2021)
  

SHDI_data_gnipc <- SHDI_data %>% 
  select(iso3, country, year, gnic) %>% 
  pivot_wider( names_from = year, values_from = gnic) %>% 
  set_names('iso3', 'country', paste0('gnipc_',1990:2021)) %>% 
  filter(!if_all(contains('gnipc_'), is.na))  # exclude rows without data


# combine UN and WB and SHDI
UN_HDI_gnipc_combWB <- UN_HDI_gnipc %>% 
  bind_rows(WB_data_gnipc_Scaled %>% filter(!iso3 %in% UN_HDI_gnipc$iso3)) 

UN_HDI_gnipc_combWB_SHDI <-UN_HDI_gnipc_combWB %>% 
  bind_rows(SHDI_data_gnipc %>% filter(!iso3 %in% UN_HDI_gnipc_combWB$iso3)) %>% 
  set_names('iso3', 'country', paste0(1990:2021)) %>% 
  pivot_longer(-c(iso3, country), names_to = 'year', values_to = 'gnic')




##### 3 combine all ----

adm0_comb <- UN_HDI_gnipc_combWB_SHDI %>% 
  select(iso3, year, gnic) %>% 
  left_join(cntry_info[,1:2] %>% rename(iso3 = iso_code)) %>% 
  rename(country = country_name) %>% 
  select(iso3, country, everything()) 



#### 4. some modifications ----

### extrapolation returns negative values for msch for Somalia and Eritrea and Bhutan
# and very high values for esch

# let's use the last available year for previous years for these countries


### for some territories data in subnational data, let's add those still

HDI_data <- read_csv('input/SHDI-SGDI-Total 7.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, GDLCODE, level, gnic)


HDI_polyg <- read_sf('data_gis/GDL Shapefiles V6.1/shdi2022_World_large.shp') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  # some iso_codes missing; let's fill those
  mutate(iso3fill = substr(gdlcode,start = 1, stop=3)) %>% 
  mutate(iso_code = ifelse(is.na(iso_code), iso3fill, iso_code))

HDI_nogeom <- HDI_polyg %>% 
  st_drop_geometry()

HDI_nogeom_dist <- HDI_nogeom %>% 
  distinct(gdlcode)

HDI_data_filt <- HDI_data %>% 
  filter(!GDLCODE %in% HDI_nogeom_dist$gdlcode) %>% 
  filter(!level == 'National') %>% 
  arrange(GDLCODE)

# these territories of France there is data but those are not in the shape file; 
# let's add them to the national data

# FRAr125 = Guadeloupe (GLP)
# FRAr126 = Martinique (MTQ)
# FRAr128 = Reunion (REU)


HDI_data_filt_distinct <- HDI_data_filt %>% 
  distinct(GDLCODE, iso3)

HDI_data_toAdm0 <- HDI_data %>% 
  filter(GDLCODE %in% c('FRAr125','FRAr126','FRAr128')) %>%
  mutate(country = ifelse(GDLCODE == 'FRAr125', 'Guadeloupe',
                          ifelse(GDLCODE == 'FRAr126', 'Martinique',
                                 'Reunion' ))) %>% 
  mutate(iso3 = ifelse(GDLCODE == 'FRAr125', 'GLP',
                       ifelse(GDLCODE == 'FRAr126', 'MTQ',
                              'REU' ))) %>% 
  select(-c(GDLCODE, level)) %>% 
  mutate(year = as.character(year))

adm0_comb_filled <- adm0_comb %>% 
  bind_rows(HDI_data_toAdm0) %>% 
  distinct(iso3, year, .keep_all = T)



## check for which indicators there is no data

adm0_combSummary <- adm0_comb_filled %>% 
  #select(iso3, lifexp) %>% 
  group_by(iso3) %>% 
  summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
  #summarise_all(funs(sum(!is.na(.)))) %>% 
  left_join(adm0_comb_filled[,1:2] %>% distinct(), by = 'iso3') %>% 
  mutate(sumOfn = rowSums(select_if(., is.numeric), na.rm = TRUE)) 



#### 5. interpolation and extrapolation  -----

source('functions/f_interpAdm0.R')

source('functions/f_Extrapol_adm0.R')

source('functions/f_interpExtrap_adm0.R')


### 5.2 apply functions ----

# sf_cntryGIS <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0') %>% 
#   st_simplify(dTolerance = 0.05, preserveTopology = T)

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  # GADM file available from https://gadm.org 
  v_cntryGIS <- terra::simplifyGeom(vect('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0'))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  terra::writeVector(v_cntryGIS, 'data_GIS/v_cntryGISsimplif.gpkg')
  terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
}




# indicators:  lifexp  gnic  esch  msch gdi

gnic_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'gnic')

adm0_comb_interpExtrap <- gnic_interpExtrap


if (dir.exists('results/')) {
  
} else {
  dir.create('results/')  
}


write_csv(adm0_comb_interpExtrap, 'results/adm0_comb_interpExtrap.csv')





