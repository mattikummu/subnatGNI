

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



### 1. load data ----

sf_gnic <- read_sf('results/vect_gnic_1990_2023.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(log10_2023 = '2021') %>% 
  mutate(log10_2023 = log10(log10_2023)) %>% 
  filter(!iso3 == 'ATA')

sf_gnic_adm1 <- read_sf('results/vect_gnic_1990_2023.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(log10_2023 = '2021') %>% 
  mutate(log10_2023 = log10(log10_2023)) %>% 
  rename(log10_1990 = '1990') %>% 
  mutate(log10_1990 = log10(log10_1990)) %>% 
  filter(!iso3 == 'ATA')


sf_adm0 <- read_sf("data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

r_gnic_2023 <- subset(rast('results/rast_gnic_1990_2023.tif'), 34)


writeRaster(x = r_gnic_2023,
            filename = "results/r_gnic_2023.tif", gdal="COMPRESS=LZW",overwrite=T)




### 2. check how many people live in subnational areas we have data for  ----

r_popCount <- subset(terra::rast("../subnatGini/data_gis/r_pop_GHS_1985_2025_5arcmin.tif"), 6:39)
names(r_popCount) <- paste0('pop', c(1990:2023))

globPop <- global(subset(r_popCount,34), fun=sum, na.rm=T)

ext_in_x_pop <- exactextractr::exact_extract(x= subset(r_popCount,34),
                                             y= sf_gnic_adm1 %>% filter(admID > 1000), 
                                             fun='sum') %>% 
  as_tibble()

subnatPop <- ext_in_x_pop %>% 
  summarise(totalPop = sum(value))

subnatPop / globPop





### 3. map functions ----


source('functions/f_Plot_sf_abs.R')

source('functions/f_Plot_sf_trend.R')






#### 4. plot maps ----

### 4.1 gnic

minGnic <- quantile( sf_gnic_adm1$'log10_2023', .01, na.rm=T)
maxGnic <- quantile( sf_gnic_adm1$'log10_2023', .99, na.rm=T) 

log10gnicRange <- seq( plyr::round_any( minGnic,accuracy=0.1,f=floor ), 
                       plyr::round_any( maxGnic,accuracy=0.1,f=ceiling ) ,
                       by= 0.05) 



minSlope <- quantile(sf_gnic_adm1$slope, .01, na.rm = T)
maxSlope <- quantile(sf_gnic_adm1$slope, .99, na.rm = T)

slopeRange <- seq(round(minSlope,1),round(maxSlope,1),by=.05) 


p_gnicSlope <- f_Plot_sf_trend(sf_gnic_adm1,'slope',slopeRange)

p_log10gnic_1990 <- f_Plot_sf_abs(sf_gnic_adm1,'log10_1990',log10gnicRange )
p_log10gnic_2023 <- f_Plot_sf_abs(sf_gnic_adm1,'log10_2023',log10gnicRange )



if (dir.exists('figures/')) {
  
} else {
  dir.create('figures/')  
}

if (dir.exists('figures/figGnic/')) {
  
} else {
  dir.create('figures/figGnic/')  
}

layers <- list(p_gnicSlope, p_log10gnic_1990, p_log10gnic_2023) 
# p_giniDisp_adm1_1990, p_giniDisp_adm0_1990,
# p_giniDispSlope_adm1, p_giniDispSlope_adm0)

nameLayers <- c('p_gnicSlope', 'p_log10gnic_1990','p_log10gnic_2023')
# 'p_giniDisp_adm1_1990', 'p_giniDisp_adm0_1990',
# 'p_giniDispSlope_adm1', 'p_giniDispSlope_adm0')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  p_fig_legend <- layers[[i]]
  
  tmap_save(p_fig,filename = paste0('figures/figGnic/fig_',nameLayers[i],'.png'),width = 80, units='mm', dpi = 450)
  tmap_save(p_fig_legend,filename = paste0('figures/figGnic/fig_legend_',nameLayers[i],'.png'),width = 80, units='mm', dpi = 450)
}




p_gnic <- tmap_arrange(p_log10gnic_2023, p_gnicSlope,
                       ncol = 2)

tmap_save(p_gnic,filename = paste0('figures/fig1_gnic2023_slope.pdf'),width = 180, height=60, units='mm')


#### 5. range and mean interval of subnational data -----



sf_dataReported_range <- read_csv('input/SHDI-SGDI-Total 8.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, GDLCODE, level, gnic) %>% 
  filter(level == 'Subnat') %>% 
  group_by(iso3) %>% 
  summarise(minYear = min(year), maxYear=max(year)) %>% 
  ungroup() %>% 
  mutate(rangeYear = as.numeric(maxYear) - as.numeric(minYear) + 1)


sf_dataReported_nmbrObs <- read_csv('input/SHDI-SGDI-Total 8.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  filter(level == 'Subnat') %>% 
  select(iso3, country, year, GDLCODE, gnic) %>% 
  distinct(iso3, year, GDLCODE, .keep_all = T) %>% 
  
  pivot_wider(names_from = 'year', values_from = 'gnic') %>% 
  mutate(nmbrObs = rowSums(!is.na(.)) - 3) %>% 
  select(iso3, country, GDLCODE, nmbrObs) %>% 
  group_by(iso3) %>% 
  summarise(nmbrObs = mean(nmbrObs))

sf_dataReported_meanInterval <- read_csv('input/SHDI-SGDI-Total 8.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, GDLCODE, level, gnic) %>% # Filter rows where GDP is not NA
  # filter(GID_nmbr == 1004001) %>% 
  group_by(GDLCODE) %>% 
  arrange(GDLCODE, year) %>%   # Ensure data is sorted by year
  mutate(interval = year - lag(year)) %>%  # Calculate the interval
  select(iso3,GDLCODE, year, interval) %>% # %>%  # Select the relevant columns
  reframe(intervalMean = mean(interval, na.rm = T), iso3 = iso3) %>% 
  ungroup() %>% 
  group_by(iso3) %>% 
  reframe(intervalMean_iso3 = mean(intervalMean, na.rm = T) ) %>% 
  ungroup()

summary(sf_dataReported_meanInterval$intervalMean_iso3)




### 5.1 plot

sf_adm0_gnicDataOrigin <- read_sf('results/polyg_gnic_adm0_1990_2021.gpkg') %>% 
  #st_drop_geometry() %>% 
  select(GID_nmbr, iso3) %>% 
  #left_join(cntry_metaData_gnic_upd) %>% 
  #left_join(sf_dataReported) %>% 
  left_join(sf_dataReported_range %>% distinct(iso3, .keep_all = T))  %>% 
  left_join(sf_dataReported_meanInterval %>% distinct(iso3, .keep_all = T)) %>% 
  
  left_join(sf_dataReported_nmbrObs %>% distinct(iso3, .keep_all = T)) %>% 
  #mutate(intervalMean_iso3 = ifelse(is.nan(intervalMean_iso3)&rangeYear>0, 1, intervalMean_iso3))
  filter(!iso3 == 'AIA')

sf_gnicDataOrigin <- read_sf('results/vect_gnic_1990_2023.gpkg')


pal = scico(8, begin = 0.1, end = 0.9, direction = -1, palette = 'lapaz')
breaksNmbr = c(0,2,5,10,15,20,25,30)
breaksInterval = c(0,1.1,2,3,4,5,6,Inf)

plt_gnicDataNmbrYears <- tm_shape(sf_adm0_gnicDataOrigin, projection = "+proj=robin") +
  tm_polygons(col = "nmbrObs",
              palette = pal,
              breaks = breaksNmbr,
              colorNA = 'white',
              #contrast = c(0.2, 0.7),
              lwd = 0.1
  )+
  tm_shape(sf_gnicDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.05)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)



plt_gnicDataRangeYears <- tm_shape(sf_adm0_gnicDataOrigin, projection = "+proj=robin") +
  tm_fill(col = "rangeYear",
          palette = pal,
          breaks = breaksNmbr,
          colorNA = 'white',
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_gnicDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)

plt_gnicDataIntervalYears <- tm_shape(sf_adm0_gnicDataOrigin, projection = "+proj=robin") +
  tm_fill(col = "intervalMean_iso3",
          palette = pal,
          breaks = breaksInterval,
          colorNA = 'white',
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_gnicDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)





plt_dataOrigin <- tmap_arrange(plt_gnicDataNmbrYears, 
                               plt_gnicDataRangeYears, plt_gnicDataIntervalYears,
                               ncol = 1 )


plt_gnicDataNmbrYears_noLegend <- plt_gnicDataNmbrYears + 
  tm_layout(legend.show=FALSE)

plt_gnicDataRangeYears_noLegend <- plt_gnicDataRangeYears + 
  tm_layout(legend.show=FALSE)

plt_gnicDataIntervalYears_noLegend <- plt_gnicDataIntervalYears + 
  tm_layout(legend.show=FALSE)


tmap_save(plt_gnicDataNmbrYears_noLegend,filename = paste0('figures/fig_','plt_gnicDataNmbrYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)

tmap_save(plt_gnicDataRangeYears_noLegend,filename = paste0('figures/fig_','plt_gnicDataRangeYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)

tmap_save(plt_gnicDataIntervalYears_noLegend,filename = paste0('figures/fig_','plt_gnicDataIntervalYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)


tmap_save(plt_dataOrigin,filename = "figures/plt_gnicDataOrigin.pdf", width = 130, height = 180, units = "mm")




