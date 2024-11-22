

f_hdi_Adm1_interp <- function(variableName) {
  
  adm0_data_SHDI <- HDI_data_filled %>% 
    filter(level == 'National') %>% 
    select(iso3,year,!!as.name(variableName)) %>% 
    rename(indicAdm0 = !!variableName)
  
  # calculate ratio between adm1 and adm0
  adm1_data_SHDI_ratio <- HDI_data_filled %>% 
    filter(level == 'Subnat') %>% 
    select(iso3,GDLCODE,year,!!variableName) %>% 
    rename(indicAdm1 = !!variableName) %>% 
    left_join(adm0_data_SHDI) %>% 
    mutate(ratioAdm1Adm0 := indicAdm1/indicAdm0)
  
  outVarName <- paste0(as.name(variableName),'Ratio')
  
  # interpolate ratio
  adm1_data_SHDI_interpRatio <- adm1_data_SHDI_ratio %>% 
    select(iso3, GDLCODE, year, ratioAdm1Adm0) %>% 
    # make sure that all years are included
    pivot_wider(names_from = 'year', values_from = 'ratioAdm1Adm0') %>% 
    pivot_longer(-c(iso3, GDLCODE), names_to = 'year', values_to = 'ratioAdm1Adm0') %>% 
    # in lifexp for two adm1 regions no data, we use national data; i.e. ratio = 1
    mutate(ratioAdm1Adm0 = ifelse(iso3 == "TUV" & is.na(ratioAdm1Adm0), 1,  ratioAdm1Adm0) ) %>% 
    # interpolate
    group_by(GDLCODE) %>% 
    #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
    mutate(ratioAdm1Adm0 = na.approx(ratioAdm1Adm0, maxgap = Inf, rule = 2)) %>% 
    ungroup() %>% 
    rename(!!as.name(outVarName) := ratioAdm1Adm0) %>% 
    select(iso3, GDLCODE, year, !!as.name(outVarName))
  
  
  return(adm1_data_SHDI_interpRatio)
}
