

f_hdi_data2gpkg <- function(inYears = 1990:2021, IndexName = 'gnic', 
                            inDataAdm0 = adm0_comb_interpExtrap) {
  
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)] %>% rename(iso3 = iso_code) ) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3)) %>% 
    drop_na()
  
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  
  tempDataAdm0_trend <- tempDataAdm0 %>% 
    group_by(GID_nmbr) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    nest(data = -GID_nmbr) %>% 
    mutate(
      model = map(data,  ~ lm( !!as.name(IndexName)  ~ time, data = .))
    ) %>% 
    mutate(
      tidy_summary = map(model, tidy)
    ) %>% 
    unnest(tidy_summary) %>% 
    filter(term == 'time') %>% 
    select(GID_nmbr, estimate, p.value)
  
  
  # # https://stackoverflow.com/questions/32274779/extracting-p-values-from-multiple-linear-regression-lm-inside-of-a-ddply-funct
  # 
  # tempDataadm0_trend <- tempDataadm0 %>% 
  #   group_by(GID_nmbr) %>% 
  #   #nest() %>% 
  #   do({model = lm(gnic~year, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))})   %>% 
  #   filter(term == 'year')
  
  gdp_adm0_polyg_noGeom <- adm0_polyg_final %>%
    st_drop_geometry() %>% 
    select(iso3, GID_nmbr, Country)
  
  tempDataAdm0_wTrend <- tempDataAdm0 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0_trend) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm0_polyg_simpl) %>% 
    left_join(gdp_adm0_polyg_noGeom) %>% 
    select(GID_nmbr, iso3,  Country, slope, everything()) %>% 
    select(-c(estimate, p.value))
  
  
  
  st_write(tempDataAdm0_wTrend,paste0('results/polyg_',IndexName,'_adm0_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  # only csv
  temp <- tempDataAdm0_wTrend %>% 
    st_drop_geometry() %>% 
    select(-geom)
  
  write_csv(temp, paste0("results/tabulated_",IndexName,"_adm0.csv"))
  
  # slope to raster
  
  
  idNoData <- adm0_polyg_final %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  r_gdp_adm0_polyg_5arcmin[r_gdp_adm0_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  temp_id <-  as.numeric(tempDataAdm0_trend$GID_nmbr)
  temp_v <- as.numeric(tempDataAdm0_trend$estimate)
  
  # reclassify
  slope_raster <- classify(r_gdp_adm0_polyg_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  
  
  terra::writeRaster(slope_raster,paste0('results/rast_slope_log10',IndexName,'_adm0_',
                                         inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}
