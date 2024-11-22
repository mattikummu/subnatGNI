

f_hdi_adm1_data2gpkg <- function(inYears = 1990:2021, IndexName = "gnic", inDataAdm0, inDataAdm1) {
  
  ratioName <- paste0(IndexName,'Ratio')
  
  tempDataAdm0 <- inDataAdm0 %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(admID = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, admID, iso3))
  
  tempDataAdm1_Ratio <- inDataAdm1 %>% 
    select(iso3, year, GDLCODE, !!ratioName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(admID = cntry_id * 10000 + as.numeric( str_split(GDLCODE, "r", simplify = TRUE)[ , 2] ) ) %>% 
    left_join(tempDataAdm0[,1:3]) %>% 
    rename(adm0Value = !!IndexName) %>% 
    mutate(!!as.name(IndexName) := !!as.name(ratioName) * adm0Value) %>% 
    dplyr::select(c(!!IndexName,year, 'admID', iso3))
  
  tempDataAdm0Adm1 <- tempDataAdm0 %>% 
    filter(!iso3 %in% unique(tempDataAdm1_Ratio$iso3)) %>% 
    select(-cntry_id) %>% 
    bind_rows(tempDataAdm1_Ratio) %>% 
    select(-iso3) %>% 
    drop_na() %>% 
    filter(!!IndexName > 0)
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  
  if (IndexName == 'gnic') {
    
    
    tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
      group_by(admID) %>% 
      mutate(time = row_number()) %>% 
      ungroup() %>% 
      select(-year) %>% 
      mutate(log10_gni_pc = log10(!!as.name(IndexName))) %>% 
      drop_na() %>% 
      nest(data = -admID) %>% 
      mutate(
        model = map(data,  ~ mblm(log10_gni_pc ~ time, data = .))
      ) %>% 
      mutate(
        tidy_summary = map(model, tidy)
      ) %>% 
      unnest(tidy_summary) %>% 
      filter(term == 'time') %>% 
      select(admID, estimate, p.value)
  } else {
    
    tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
      group_by(admID) %>% 
      mutate(time = row_number()) %>% 
      ungroup() %>% 
      select(-year) %>% 
      nest(data = -admID) %>% 
      mutate(
        model = map(data,  ~ mblm(!!as.name(IndexName) ~ time, data = .))
      ) %>% 
      mutate(
        tidy_summary = map(model, tidy)
      ) %>% 
      unnest(tidy_summary) %>% 
      filter(term == 'time') %>% 
      select(admID, estimate, p.value)
  }
  
  # # https://stackoverflow.com/questions/32274779/extracting-p-values-from-multiple-linear-regression-lm-inside-of-a-ddply-funct
  # 
  # tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
  #   group_by(admID) %>% 
  #   #nest() %>% 
  #   do({model = lm(gnic~year, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))})   %>% 
  #   filter(term == 'year')
  
  HDI_GADM_polyg_noGeom <- HDI_GADM_polyg %>%
    st_drop_geometry() %>% 
    select(iso3, admID)
  
  tempDataAdm0Adm1_wTrend <- tempDataAdm0Adm1 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0Adm1_trend) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(HDI_GADM_polyg_simpl) %>% 
    left_join(HDI_GADM_polyg_noGeom) %>% 
    select(admID, iso3, slope, everything()) 
  
  
  st_write(tempDataAdm0Adm1_wTrend,paste0('results/vect_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  
  # slope to raster
  
  
  idNoData <- HDI_GADM_polyg %>% 
    st_drop_geometry() %>% 
    select(admID) %>% 
    filter(!admID %in% unique(tempDataAdm0Adm1$admID)) %>% 
    ## REMOVE WST SAHARA as data exist from subnational ata
    filter(!admID == 912) %>% 
    drop_na()
  
  HDI_boundary_raster_5arcmin[HDI_boundary_raster_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  temp_id <-  as.numeric(tempDataAdm0Adm1_trend$admID)
  temp_v <- as.numeric(tempDataAdm0Adm1_trend$estimate)
  
  # reclassify
  slope_raster <- classify(HDI_boundary_raster_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  
  if (IndexName == 'gnic') {
    terra::writeRaster(slope_raster,paste0('results/rast_slope_log10',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  } else {
    terra::writeRaster(slope_raster,paste0('results/rast_slope_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  }
  
}
