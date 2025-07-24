

### adm0 data to raster and polygon

# prepare national data for HDI components

library(openxlsx)
library(zoo)
library(broom)
library(mblm)

library(sf)
library(terra)

library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### 1. preparations ------

# 1.1 set time steps ------
timestep <- c(seq(1990, 2023))
step <- c(seq(1,length(timestep)))

### 1.2 load cntry_info
cntry_info <- read_csv("input/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code))


#### 2. put data to gpkg (and slope to raster) -----

# select indicator : lifexp  gnic  esch  msch gdi

indicName = 'gnic'

adm0_comb_interpExtrap <- read.csv( 'results/adm0_comb_interpExtrap.csv') %>% 
  select(iso3, year, !!indicName) %>% 
  as_tibble()

n_cntry <- adm0_comb_interpExtrap %>% 
  distinct(iso3)

## adm0 polyg

# some of the adm1 levels are divided to those that are officially in a country and those that are 
# on conflict zones (between CHN, IND and PAK)
# let's unite them with those that we have data for


adm0_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
  rename(iso3 = GID_0) # %>% 


adm0_gadm_old <- read_sf('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>% 
  rename(iso3 = GID_0) %>% 
  rename(COUNTRY = NAME_0) %>% 
  filter(iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC'))

sf_adm0_polyg_diss <- adm0_polyg %>% 
  filter(!iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC')) %>% 
  bind_rows(adm0_gadm_old)


# join cntry info
adm0_polyg_final <- sf_adm0_polyg_diss %>% 
  #st_drop_geometry() %>% 
  rename(Country = COUNTRY) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO','KSV',iso3)) %>% 
  filter(iso3 != 'ALA' & iso3 != 'XCA' & iso3 != 'ATA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed), Antarctica
  #filter(iso3 != 'ESH') %>%  # drop Western Sahara; in GDL part of Morocco
  mutate(GID_nmbr = paste0(iso3,'t')) %>% 
  left_join(cntry_info[,c(2,4)] %>% rename(iso3 = iso_code)) %>% 
  mutate(GID_nmbr = cntry_id) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3,GID_nmbr, geom)


temp <- adm0_polyg_final %>% 
  st_drop_geometry()





# 3. create adm0  raster -----------------------------------------------------

if (file.exists('data_gis/gdp_Adm0_raster_5arcmin.tif')){
  # load it
  r_gdp_adm0_polyg_5arcmin <- rast('data_gis/gdp_Adm0_raster_5arcmin.tif')
} else { 
  # create it
  
  #create ref raster
  # ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  # rasterise to 1 arc min resolutions
  
  r_gdp_adm0_polyg_1arcmin <-  rasterize(adm0_polyg_final,ref_raster_1arcmin,field="GID_nmbr")
  
  
  # aggregate to 5 arc-min
  r_gdp_adm0_polyg_5arcmin <- terra::aggregate(r_gdp_adm0_polyg_1arcmin,fact=5,fun='modal',na.rm=T)
  
  
  # write raster
  terra::writeRaster(r_gdp_adm0_polyg_5arcmin,'data_gis/gdp_adm0_raster_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}


### 4. put data to raster -----------------------------------------------------

source('functions/f_hdi_adm0_data2raster.R')

varNames <- c('gnic' )

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_hdi_adm0_data2raster(inYears = 1990:2023, 
                                         IndexName = varNames[iVar], 
                                         inDataAdm0 = adm0_comb_interpExtrap) 
  
}




### 5. simplify polygon layer ----

if (file.exists('data_gis/gdp_adm0_polyg_simple.gpkg')){
  # load it
  gdp_adm0_polyg_simpl <- st_read('data_gis/gdp_adm0_polyg_simple.gpkg') 
} else { 
  # create it
  
  p <- as.polygons(r_gdp_adm0_polyg_5arcmin)
  as.data.frame(p)
  
  writeVector(p, 'data_gis/gdp_adm0_polyg_simple.gpkg', overwrite=T)
  
  gdp_adm0_polyg_simpl <- st_read('data_gis/gdp_adm0_polyg_simple.gpkg') 
  
  # gdp_adm0_polyg_simpl <- st_as_sf(raster::raster(r_gdp_adm0_polyg_5arcmin))  %>% 
  # 
  #   sf::st_simplify(., preserveTopology = T, dTolerance = 0.1)
  # 
  # 
  # st_write(gdp_adm0_polyg_simpl, 'data_gis/GDL_regions_v7_simpl.gpkg', delete_dsn=T)
  
}



#### 6. put data to gpkg (and slope to raster) -----


source('functions/f_hdi_data2gpkg.R')


varNames <- c('gnic')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_hdi_data2gpkg(inYears = 1990:2023, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap) 
  
}

