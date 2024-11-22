
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
timestep <- c(seq(1990, 2021))
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

  
GDI_data <- read_csv('input/GDL-Subnational-GDI-data.csv') %>% 
  rename(iso3 = ISO_Code, country = Country, level = Level) %>% 
  select(-c(Continent, Region )) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  pivot_longer(-c(country, iso3,  level, GDLCODE), names_to = 'year', values_to = 'gdi') %>% 
  select(iso3, year, GDLCODE, level, gdi) %>% 
  mutate(year = as.numeric(year))

# region codes between GDL and CCI data
CCI_key <- readxl::read_excel('../GIS_data_common/subnat_corruption/GDL_SCI Regional Key.xlsx') %>% 
  rename(iso3 = ISO_Code) %>% 
  select(iso3, GDLCODE, GDLCODE_SCI) 
  

CCI_data <- readxl::read_excel('../GIS_data_common/subnat_corruption/SUBCPI and SUBCCI Dataset.xlsx') %>% 
  rename(iso3 = iso) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% # kosovo to correct iso3
  mutate(level = ifelse(is.na(region),'National','Subnat')) %>% 
  #filter(!is.na(region)) %>%  # only national data
  rename(cci = SUBCCI) %>% 
  rename(GDLCODE_SCI = GDLcode) %>% 
  select(iso3, year, GDLCODE_SCI, level, cci)

# CCI_data_inGDLcode <- CCI_key %>% 
#   left_join()

HDI_data <- read_csv('input/SHDI-SGDI-Total 7.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, GDLCODE, level, lifexp,  gnic,  esch,  msch) %>% 
  left_join(GDI_data) %>% 
  # CCI uses for some countries different division, let's join the CCI_key for that
  left_join(CCI_key) %>% 
  left_join(CCI_data %>% select(-level))




# some subnat data missing, let's use national value for those

GDI_adm0 <- read_csv('results/tabulated_gdi_adm0.csv') %>% 
  select(-c(GID_nmbr,  Country, slope, cntry_id, geom)) %>% 
  pivot_longer(-iso3, names_to = "year", values_to = 'gdi_adm0') %>% 
  mutate(year = as.numeric(year))

GDI_data_national <- GDI_data %>% 
  filter(level == "National") %>% 
  rename(gdi_adm0 = gdi) %>% 
  select(-level, -GDLCODE) %>% 
  ## add adm0 data if missing
  bind_rows(GDI_adm0 %>% filter(!iso3 %in% unique(GDI_data$iso3)))



CCI_adm0 <- read_csv('results/tabulated_cci_adm0.csv') %>% 
  select(-c(GID_nmbr,  Country, slope, cntry_id)) %>% 
  pivot_longer(-iso3, names_to = "year", values_to = 'cci_adm0') %>% 
  mutate(year = as.numeric(year))



CCI_data_national <- CCI_data %>% 
  filter(level == "National") %>% 
  rename(cci_adm0 = cci) %>% 
  select(-level, -GDLCODE_SCI) %>% 
  ## add adm0 data if missing
  bind_rows(CCI_adm0 %>% filter(!iso3 %in% unique(CCI_data$iso3)))


HDI_data_filled <- HDI_data %>% 
  left_join(GDI_data_national) %>% 
  mutate(gdi = ifelse(is.na(gdi), gdi_adm0, gdi)) %>% 
  left_join(CCI_data_national) %>% 
  mutate(cci = ifelse(is.na(cci), cci_adm0, cci)) %>% 
  # let's scale CCI from 0 to 5, instead from -2.5 to 2.5 so that ratio can be calculated
  mutate(cci = cci + 2.5) %>% 
  select(-c(GDLCODE_SCI, gdi_adm0, cci_adm0))




adm0_GADM_data <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  st_drop_geometry() %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>%  # kosovo to correct iso3
  rename(iso3 = GID_0)




### 3  interpolate HDI component adm1 dataset ----------------
# variableName = 'cci'

source('functions/f_hdi_Adm1_interp.R')


### run function -----------------------------------------------------

# variables: lifexp; gnic;  esch;  msch 

st_hdiComponent_lifexp <- f_hdi_Adm1_interp('lifexp')
st_hdiComponent_gnic <- f_hdi_Adm1_interp('gnic') 
st_hdiComponent_esch <- f_hdi_Adm1_interp('esch') 
st_hdiComponent_msch <- f_hdi_Adm1_interp('msch') 
st_hdiComponent_gdi <- f_hdi_Adm1_interp('gdi') 
st_hdiComponent_cci <- f_hdi_Adm1_interp('cci') 

adm1_ratioAdm1Adm0_interp <- st_hdiComponent_lifexp %>% 
  full_join(st_hdiComponent_gnic) %>% 
  full_join(st_hdiComponent_esch) %>% 
  full_join(st_hdiComponent_msch) %>% 
  full_join(st_hdiComponent_gdi) %>% 
  full_join(st_hdiComponent_cci) 


write_csv(adm1_ratioAdm1Adm0_interp, 'results/adm1_ratioAdm1Adm0_interp.csv')



