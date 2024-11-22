
f_hdi_adm0_data2raster <- function(inYears = 1990:2021, 
                                   IndexName = 'gdi', 
                                   inDataAdm0 = adm0_comb_interpExtrap) {
  
  coll_raster = rast()
  
  #ratioName <- paste0(IndexName,'Ratio')
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)] %>% rename(iso3 = iso_code)) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3)) %>% 
    drop_na()
  
  
  
  # check adm0 areas for which we do not have data, and put those to NA in the raster
  idNoData <- adm0_polyg_final %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  r_gdp_adm0_polyg_5arcmin[r_gdp_adm0_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  for (iYear in inYears) {
    
    tempDataAdm0_selYear <- tempDataAdm0 %>% 
      filter(year == iYear)
    
    temp_id <-  as.numeric(tempDataAdm0_selYear$GID_nmbr)
    temp_v <- round(as.numeric(tempDataAdm0_selYear[[IndexName]]))
    
    # reclassify
    temp_raster <- classify(r_gdp_adm0_polyg_5arcmin,
                            cbind(temp_id, temp_v))
    
    terra::add(coll_raster) <- temp_raster
  }
  
  names(coll_raster) <- paste0(IndexName,'_',inYears[1]:inYears[length(inYears)])
  
  ## some countries (western sahara, etc) are given as subnational units for another country; let's add those
  # this means that you need to first run the 3_hdi_prepare_spatial code before you can run this part
  
  coll_raster[is.na(coll_raster)] <- rast('results/rast_gnic_1990_2021.tif')
  
  
  terra::writeRaster(coll_raster,paste0('results/rast_',IndexName,'_adm0_',inYears[1],'_',inYears[length(inYears)],
                                        '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}