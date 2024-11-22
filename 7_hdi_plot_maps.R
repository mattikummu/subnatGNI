

#library(raster)
library(terra)
library(sf)
library(dplyr)
library(tmap)
library(tidyverse)
library(scico)
library(rnaturalearth)
library(rmapshaper)


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### load data

sf_gnic <- read_sf('results/vect_gnic_1990_2021.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(log10_2021 = '2021') %>% 
  mutate(log10_2021 = log10(log10_2021)) %>% 
  filter(!iso3 == 'ATA')

sf_gnic_adm1 <- read_sf('results/vect_gnic_1990_2021.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(log10_2021 = '2021') %>% 
  mutate(log10_2021 = log10(log10_2021)) %>% 
  rename(log10_1990 = '1990') %>% 
  mutate(log10_1990 = log10(log10_1990)) %>% 
  filter(!iso3 == 'ATA')

sf_msch <- read_sf('results/vect_msch_1990_2021.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(year_2021 = '2021') %>% 
  filter(!iso3 == 'ATA')

sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
# simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

r_gnic_2021 <- subset(rast('results/rast_gnic_1990_2021.tif'), 32)
r_msch_2021 <- subset(rast('results/rast_msch_1990_2021.tif'), 32)

writeRaster(x = r_gnic_2021,
            filename = "results/r_gnic_2021.tif", gdal="COMPRESS=LZW",overwrite=T)

writeRaster(x = r_msch_2021,
            filename = "results/r_msch_2021.tif", gdal="COMPRESS=LZW",overwrite=T)



# check how many people live in subnational areas we have data for 

r_popCount <- rast('/Users/mkummu/R/GIS_data_common/GlobPOP/GlobPOP_5arcmin_1990_2022_v2.tif')

globPop <- global(subset(r_popCount,32), fun=sum, na.rm=T)

ext_in_x_pop <- exactextractr::exact_extract(x= subset(r_popCount,32),
                                             y= sf_gnic_adm1 %>% filter(admID > 1000), 
                                             fun='sum') %>% 
  as_tibble()

subnatPop <- ext_in_x_pop %>% 
  summarise(totalPop = sum(value))

subnatPop / globPop





### map functions ----


source('functions/f_Plot_sf_abs.R')

source('functions/f_Plot_sf_trend.R')






#### plot maps ----

### gnic

minGnic <- quantile( sf_gnic_adm1$'log10_2021', .01, na.rm=T)
maxGnic <- quantile( sf_gnic_adm1$'log10_2021', .99, na.rm=T) 

log10gnicRange <- seq( plyr::round_any( minGnic,accuracy=0.1,f=floor ), 
                                   plyr::round_any( maxGnic,accuracy=0.1,f=ceiling ) ,
                                   by= 0.05) 



minSlope <- quantile(sf_gnic_adm1$slope, .01, na.rm = T)
maxSlope <- quantile(sf_gnic_adm1$slope, .99, na.rm = T)

slopeRange <- seq(round(minSlope,1),round(maxSlope,1),by=.05) 


p_gnicSlope <- f_Plot_sf_trend(sf_gnic_adm1,'slope',slopeRange)

p_log10gnic_1990 <- f_Plot_sf_abs(sf_gnic_adm1,'log10_1990',log10gnicRange )
p_log10gnic_2021 <- f_Plot_sf_abs(sf_gnic_adm1,'log10_2021',log10gnicRange )



if (dir.exists('figures/figGnic/')) {
  
} else {
  dir.create('figures/figGnic/')  
}

layers <- list(p_gnicSlope, p_log10gnic_1990, p_log10gnic_2021) 
               # p_giniDisp_adm1_1990, p_giniDisp_adm0_1990,
               # p_giniDispSlope_adm1, p_giniDispSlope_adm0)

nameLayers <- c('p_gnicSlope', 'p_log10gnic_1990','p_log10gnic_2021')
                # 'p_giniDisp_adm1_1990', 'p_giniDisp_adm0_1990',
                # 'p_giniDispSlope_adm1', 'p_giniDispSlope_adm0')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figGnic/fig_',nameLayers[i],'.png'),width = 80, units='mm', dpi = 450)
  
}




p_gnic <- tmap_arrange(p_log10gnic_2021, p_gnicSlope,
                       ncol = 2)

tmap_save(p_gnic,filename = paste0('figures/fig1_gnic2021_slope',Sys.Date(),'.pdf'),width = 180, height=60, units='mm')



### msch

minMsch <- quantile( sf_msch$'year_2021', .01, na.rm=T)
maxMsch <- quantile( sf_msch$'year_2021', .99, na.rm=T) 

MschRange <- seq( plyr::round_any( minMsch,accuracy=0.5,f=floor ), 
                       plyr::round_any( maxMsch,accuracy=0.5,f=ceiling ) ,
                       by= 0.5) 



minSlope <- quantile(sf_msch$slope, .01, na.rm = T)
maxSlope <- quantile(sf_msch$slope, .99, na.rm = T)

slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.5,f=floor ), 
                   plyr::round_any( maxSlope,accuracy=0.5,f=ceiling ) ,
                   by= 0.25) 

scico_palette_names()

ACpal <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "vik")


p_AC <- f_Plot_sf_abs(sf_msch,'year_2021',MschRange, colPal = ACpal )


tmap_save(p_AC,filename = paste0('figures/fig_adaptiveCapac2021_',Sys.Date(),'.pdf'),width = 120, height=60, units='mm')



