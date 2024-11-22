
## prepare spatial data and rasterise the values


library(sf)
library(terra)

library(zoo)
library(purrr)
library(broom)
library(mblm)

library(tidyverse)
library(dplyr) 

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### 1. preparations ------

# 1.1 set time steps ------
timestep <- c(seq(1990, 2021))
step <- c(seq(1,length(timestep)))

### 1.2 load cntry_info

cntry_info <- read_csv("input/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3))

cntry_info_temp <- cntry_info %>% 
  rename(country = country_name) %>% 
  select(country,iso3)

# 1.3 gadm data

GADM_data <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  st_drop_geometry() %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) # kosovo to correct iso3


# 1.4 adm0 and adm1 data

adm0_comb_interpExtrap <- read_csv('results/adm0_comb_interpExtrap.csv')
adm1_ratioAdm1Adm0_interp <- read_csv('results/adm1_ratioAdm1Adm0_interp.csv')



# 1.5 check missing data in adm1 data
NAtemp <- adm1_ratioAdm1Adm0_interp %>% 
  drop_na()

adm1Missing <- adm1_ratioAdm1Adm0_interp %>% 
  filter(!GDLCODE %in% NAtemp$GDLCODE)



## 1.6 check number of subnational areas
admGnic <- adm1_ratioAdm1Adm0_interp %>% 
  select(iso3, GDLCODE, year, gnicRatio) %>% 
  drop_na() %>% 
  summarise(nSub = n_distinct(GDLCODE), 
            nNat = n_distinct(iso3))

adm0Gnic <- adm0_comb_interpExtrap %>% 
  select(iso3, year, gnic) %>% 
  drop_na() %>% 
  summarise(nNat = n_distinct(iso3))



# lifexp missing for some admin areas in TUV

# replace NA ratio with 1; i.e. we use national value for those
adm1_ratioAdm1Adm0_interp[is.na(adm1_ratioAdm1Adm0_interp)] <- 1






#### 2. create / load polygon data -----


if (file.exists('data_gis/GDL_regions_v7.gpkg')){
  # load it
  HDI_GADM_polyg <- read_sf('data_gis/GDL_regions_v7.gpkg') 
} else { 
  # create it
  HDI_polyg <- read_sf('data_gis/GDL Shapefiles V6/shdi2022_World_large.shp') %>% 
    mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
    # some iso_codes missing; let's fill those
    mutate(iso3fill = substr(gdlcode,start = 1, stop=3)) %>% 
    mutate(iso_code = ifelse(is.na(iso_code), iso3fill, iso_code))
  
  HDI_nogeom <- HDI_polyg %>% 
    st_drop_geometry() 
  
  # HDI_nogeom_NA <- HDI_nogeom %>% 
  #   filter(is.na(iso_code))
  
  
  
  HDI_nogeom_dist <- HDI_nogeom %>% 
    distinct(gdlcode)
  
  # check areas for which there is subnat data, but are not in shapefile
  
  GDL_adm1names <- read_csv('input/GDL-Income-index-data.csv') %>% 
    select(Country,	Continent,	ISO_Code,	Level,	GDLCODE,	Region)
  
  HDI_data_filt <- adm1_ratioAdm1Adm0_interp %>% 
    filter(!GDLCODE %in% HDI_nogeom_dist$gdlcode) %>% 
    #filter(!level == 'National') %>% 
    arrange(GDLCODE) 
  
  HDI_data_filt_distinct <- HDI_data_filt %>% 
    distinct(GDLCODE, iso3) %>% 
    
    left_join(GDL_adm1names)
  
  # write_csv(HDI_data_filt_distinct, 'input/HDI_data_filt_distinct.csv')
  # 
  # adm1_GADM_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_1') %>% 
  #   mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) # kosovo to correct iso3
  # 
  # adm1_GADM_polyg_noGeom <- adm1_GADM_polyg %>% 
  #   st_drop_geometry() %>% 
  #   rename(iso3 = GID_0) %>% 
  #   filter(iso3 %in% unique(HDI_data_filt_distinct$iso3))
  # 
  # write_csv(adm1_GADM_polyg_noGeom, 'input/adm1_GADM_polyg_noGeom.csv')
  # 
  
  GADM_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
    mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) # kosovo to correct iso3
  
  # check which countries are not in HDI_polyg
  
  HDI_data_uniqueISO3 <- unique(HDI_nogeom$iso_code) %>% 
    as_tibble() %>% 
    set_names('iso3') %>% 
    mutate(iso3_SHDIdata = iso3)
  
  HDI_polyg_check <- HDI_polyg %>% 
    st_drop_geometry() %>% 
    select(iso_code) %>% 
    distinct(iso_code)%>% 
    rename(iso3 = iso_code) %>% 
    full_join(HDI_data_uniqueISO3) %>% 
    full_join(cntry_info_temp %>% mutate(iso3_adm0Data = iso3),by='iso3') %>% 
    
    # data for French Guyana (GUF) and Mayotte (MYT) are part of France, so let's filter those out
    filter(!iso3 %in% c('GUF', 'MYT'))
  
  # further, some are in shp file but there is no data - let's filter those out
  
  HDI_data_check <- adm1_ratioAdm1Adm0_interp %>% 
    distinct(iso3) %>% 
    mutate(iso3data = iso3) 
  
  HDI_data_poly_check <- HDI_polyg_check %>% 
    left_join(HDI_data_check) %>% 
    mutate(onlyGADM = ifelse(!is.na(iso3data), 1, NA )) %>% 
    mutate(onlyGADM = ifelse(!is.na(iso3_SHDIdata), onlyGADM, NA )) 
  
  
  iso3_onlyInGADM <- HDI_data_poly_check %>% 
    
    filter(is.na(onlyGADM)) %>% 
    select(iso3) %>% 
    drop_na()
  
  
  
  GADM_polyg_sel <- GADM_polyg %>% 
    #st_drop_geometry() %>% 
    rename(iso3 = GID_0, country = COUNTRY) %>% 
    mutate(iso3 = ifelse(iso3 == 'XKO','KSV',iso3)) %>% 
    filter(iso3 %in% iso3_onlyInGADM$iso3) %>% # join the missing countries in HDI polyg; so that only those are selected
    filter(iso3 != 'ALA' & iso3 != 'XCA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed)
    filter(iso3 != 'ESH') %>%  # drop Western Sahara; in GDL part of Morocco
    mutate(gdlcode = paste0(iso3,'t')) %>% 
    mutate(continent = 'GADM') %>% 
    select(gdlcode, continent, iso3, geom) #%>% 
  #set_names(c('gdlcode', 'continent', 'iso_code', 'geometry'))
  
  temp_GADM_polyg_sel <- GADM_polyg_sel %>% 
    st_drop_geometry()
  
  # HDI_polyg_updated_check <- HDI_polyg_updated %>% 
  #   st_drop_geometry()
  # combine sHDI and GADM datasets
  HDI_polyg_comb <- HDI_polyg %>% 
    filter(gdlcode %in% unique(adm1_ratioAdm1Adm0_interp$GDLCODE)) %>%  # choose only those regions we have data for
    select(-iso3fill) %>% 
    rename(geom = geometry, iso3 = iso_code)
  
  HDI_GADM_polyg_temp <- bind_rows(HDI_polyg_comb,GADM_polyg_sel)
  
  GDLids_cntry <- unique(adm1_ratioAdm1Adm0_interp$iso3) %>% 
    as_tibble() %>% 
    rename(iso3 = value) %>% 
    left_join(.,cntry_info)
  
  
  # create unique identified for each admin area in HDI_GADM_polyg
  
  HDI_GADM_polyg <-  HDI_GADM_polyg_temp %>% 
    #st_drop_geometry() %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(admID = ifelse(iso3 %in% GDLids_cntry$iso3, 
                          cntry_id * 10000 + as.numeric( str_split(gdlcode, "r", simplify = TRUE)[ , 2] ),
                          cntry_id)) %>% 
   
    
    left_join(GDL_adm1names %>% select(GDLCODE, Region) %>% rename(gdlcode = GDLCODE))
  
  test_HDI_GADM_polyg <-HDI_GADM_polyg  %>% 
    st_drop_geometry()
  
  write_sf(HDI_GADM_polyg,'data_gis/GDL_regions_v7.gpkg')
}


#  3. create adm0, adm1 raster -----------------------------------------------------

if (file.exists('data_gis/HDI_Adm0Adm1_raster_5arcmin_v7.tif')){
  # load it
  HDI_boundary_raster_5arcmin <- rast('data_gis/HDI_Adm0Adm1_raster_5arcmin_v7.tif')
} else { 
  # create it
  
  #create ref raster
  ref_raster_5arcmin <- raster::raster(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- raster::raster(ncol=360*60, nrow=180*60)
  # rasterise to 1 arc min resolutions
  
  HDI_cntry_raster_1arcmin <-  rast(fasterize::fasterize(HDI_GADM_polyg,ref_raster_1arcmin,field="admID"))
  
  
  # aggregate to 5 arc-min
  HDI_boundary_raster_5arcmin <- terra::aggregate(HDI_cntry_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  
  
  # write raster
  terra::writeRaster(HDI_boundary_raster_5arcmin,'data_gis/HDI_Adm0Adm1_raster_5arcmin_v7.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}




### 4. put data to raster -----------------------------------------------------

source('functions/f_hdi_adm1_data2raster.R')

varNames <- 'gnic' #c('lifexp', 'gnic',  'esch',  'msch' , 'gdi','cci')

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_hdi_adm1_data2raster(inYears = 1990:2021, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm0 = adm0_comb_interpExtrap, 
                                        inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}




### 5. simplify polygon layer ----

if (file.exists('data_gis/GDL_regions_v7_simpl.gpkg')){
  # load it
  HDI_GADM_polyg_simpl <- st_read('data_gis/GDL_regions_v7_simpl.gpkg') %>% 
    rename(admID = layer)
} else { 
  # create it
  
  p <- as.polygons(HDI_boundary_raster_5arcmin)
  as.data.frame(p)
  
  writeVector(p, 'data_gis/GDL_regions_v7_simpl.gpkg', overwrite=T)
  
  HDI_GADM_polyg_simpl <- st_read('data_gis/GDL_regions_v7_simpl.gpkg') %>% 
    rename(admID = layer)
  
  # HDI_GADM_polyg_simpl <- st_as_sf(raster::raster(HDI_boundary_raster_5arcmin))  %>% 
  # 
  #   sf::st_simplify(., preserveTopology = T, dTolerance = 0.1)
  # 
  # 
  # st_write(HDI_GADM_polyg_simpl, 'data_gis/GDL_regions_v7_simpl.gpkg', delete_dsn=T)
  
}



#### 6. put data to gpkg (and slope to raster) -----


source('functions/f_hdi_adm1_data2gpkg.R')

varNames <- 'gnic' #c('lifexp', 'gnic',  'esch',  'msch' , 'gdi', 'cci')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_hdi_adm1_data2gpkg(inYears = 1990:2021, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap, 
                                      inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}




