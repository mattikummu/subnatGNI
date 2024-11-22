
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


#### 2.1 life exp ----
UN_HDI_le <- UN_HDI_db %>% 
  select(iso3, country, paste0('le_',1990:2021)) %>% 
  filter(nchar(iso3) == 3) %>% # filter out regional values
  filter(!if_all(contains('le'), is.na)) # exclude rows without data

WB_data_lifexp <- read.csv('input/API_SP/API_SP.DYN.LE00.IN_DS2_en_csv_v2_4901681.csv', skip = 4) %>% 
  as_tibble() %>% 
  rename(iso3 = Country.Code) %>% 
  rename(country = Country.Name) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
  select(iso3, country, paste0('X',1990:2021)) %>% 
  set_names('iso3', 'country', paste0('le_',1990:2021)) %>% 
  filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
  filter(!if_all(contains('le_'), is.na))  # exclude rows without data

SHDI_data_le <- SHDI_data %>% 
  select(iso3, country, year, lifexp) %>% 
  pivot_wider( names_from = year, values_from = lifexp) %>% 
  set_names('iso3', 'country', paste0('le_',1990:2021)) %>% 
  filter(!if_all(contains('le_'), is.na))  # exclude rows without data


# combine UN and WB
UN_HDI_le_combWB <- UN_HDI_le %>% 
  bind_rows(WB_data_lifexp %>% filter(!iso3 %in% UN_HDI_le$iso3)) 

UN_HDI_le_combWB_SHDI <- UN_HDI_le_combWB %>% 
  bind_rows(SHDI_data_le %>% filter(!iso3 %in% UN_HDI_le_combWB$iso3)) %>% 
  set_names('iso3', 'country', paste0(1990:2021)) %>% 
  pivot_longer(-c(iso3, country), names_to = 'year', values_to = 'lifexp')


# SHDI_data_le %>% filter(!iso3 %in% UN_HDI_le_combWB$iso3)


#### 2.2 income ----



# GNI per capita, PPP (constant 2017 international $)

UN_HDI_gnipc <- UN_HDI_db %>% 
  select(iso3, country, paste0('gnipc_',1990:2021)) %>% 
  filter(nchar(iso3) == 3) %>% # filter out regional values
  filter(!if_all(contains('gnipc'), is.na)) # exclude rows without data


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

# scale world bank to 2017 international $

# ## PPP converstion from IMF
# PPP_conversion <- read.csv('input/API_PA/API_PA.NUS.PPP_DS2_en_csv_v2_2006097.csv', skip = 4) %>% 
#   as_tibble() %>% 
#   rename(iso3 = Country.Code) %>% 
#   rename(country = Country.Name) %>% 
#   mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
#   select(iso3, country, paste0('X',2017:2021)) %>% 
#   set_names('iso3', 'country', paste0('PPP_',2017:2021)) %>% 
#   filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
#   filter(!if_all(contains('PPP_'), is.na)) %>% 
#   filter(!iso3 %in% UN_HDI_gnipc$iso3)
# 
# inflationWB <- PPP_conversion <- read.csv('input/API_FP/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_2299711.csv', skip = 4) %>% 
#   as_tibble() %>% 
#   rename(iso3 = Country.Code) %>% 
#   rename(country = Country.Name) %>% 
#   mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
#   select(iso3, country, paste0('X',2017:2021)) %>% 
#   set_names('iso3', 'country', paste0('inf_',2017:2021)) %>% 
#   filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
#   filter(!if_all(contains('inf_'), is.na)) %>% 
#   filter(!iso3 %in% UN_HDI_gnipc$iso3)


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


# 2.3 expected years of schooling ----

UN_HDI_eys <- UN_HDI_db %>% 
  select(iso3, country, paste0('eys_',1990:2021)) %>% 
  filter(nchar(iso3) == 3) %>% # filter out regional values
  filter(!if_all(contains('eys'), is.na)) # exclude rows without data

# let's scale denmark value based on mean year of schooling
GRL_eys <- UN_HDI_eys %>% 
  filter(iso3 == 'DNK') %>% 
  mutate(iso3 = 'GRL') %>% 
  mutate(country = 'Greenland') %>% 
  mutate(across(where(is.numeric), ~.*(10.8/12.9)))

WB_data_eys <- read.csv('input/expected-years-of-schooling/Expected years of schooling.csv', skip = 0) %>% 
  as_tibble() %>% 
  filter(Indicator.Code == 'SE.SCH.LIFE') %>% 
  select(Country.Name, Country.Code, Year, Value) %>% 
  pivot_wider(names_from = 'Year', values_from = 'Value') %>% 
  rename(iso3 = Country.Code) %>% 
  rename(country = Country.Name) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
  select(iso3, country, paste0(1990:2021)) %>% 
  set_names('iso3', 'country', paste0('eys_',1990:2021)) %>% 
  filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
  filter(!if_all(contains('eys_'), is.na))  # exclude rows without data

SHDI_data_eys <- SHDI_data %>% 
  select(iso3, country, year, esch) %>% 
  pivot_wider( names_from = year, values_from = esch) %>% 
  set_names('iso3', 'country', paste0('eys_',1990:2021)) %>% 
  filter(!if_all(contains('eys_'), is.na))  # exclude rows without data

# combine UN and WB and SHDI
UN_HDI_eys_combWB <- UN_HDI_eys %>% 
  bind_rows(WB_data_eys %>% filter(!iso3 %in% UN_HDI_eys$iso3)) %>% 
  bind_rows(GRL_eys)

UN_HDI_eys_combWB_SHDI <-UN_HDI_eys_combWB %>% 
  bind_rows(SHDI_data_eys %>% filter(!iso3 %in% UN_HDI_eys_combWB$iso3)) %>% 
  set_names('iso3', 'country', paste0(1990:2021)) %>% 
  pivot_longer(-c(iso3, country), names_to = 'year', values_to = 'esch')


# 2.4 mean years of schooling -----

UN_HDI_mys <- UN_HDI_db %>% 
  select(iso3, country, paste0('mys_',1990:2021)) %>% 
  filter(nchar(iso3) == 3) %>% # filter out regional values
  filter(!if_all(contains('mys'), is.na)) # exclude rows without data

# let's scale south korea value based on expected years of schooling
PRK_mys <- UN_HDI_mys %>% 
  filter(iso3 == 'KOR') %>% 
  mutate(iso3 = 'PRK') %>% 
  mutate(country = 'North Korea') %>% 
  mutate(across(where(is.numeric), ~.*(12.512930/16.52174)))


# http://data.uis.unesco.org/
UNESCO_data_mys <- read.csv('input/UNESCO_mean_years_of_sch.csv', skip = 0) %>%
  as_tibble() %>%
  #filter(NATMON_IND == 'MYS_1T8_AG25T99') %>% 
  select(Country, LOCATION, TIME, Value) %>%
  pivot_wider(names_from = 'TIME', values_from = 'Value') %>%
  rename(iso3 = LOCATION) %>%
  rename(country = Country) %>%
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% # kosovo to correct iso3
  mutate('1997' = NA) %>% 
  select(iso3, country, paste0(1991:2021)) %>%
  set_names('iso3', 'country', paste0('mys_',1991:2021)) %>%
  filter(iso3 %in% cntry_info$iso_code) %>%  # exlude regional rows
  filter(!if_all(contains('mys_'), is.na))  # exclude rows without data


SHDI_data_mys <- SHDI_data %>% 
  select(iso3, country, year, msch) %>% 
  pivot_wider( names_from = year, values_from = msch) %>% 
  set_names('iso3', 'country', paste0('mys_',1990:2021)) %>% 
  filter(!if_all(contains('mys_'), is.na))  # exclude rows without data

# combine UN and WB and SHDI
UN_HDI_mys_combWB <- UN_HDI_mys %>% 
  bind_rows(UNESCO_data_mys %>% filter(!iso3 %in% UN_HDI_mys$iso3)) %>% 
  bind_rows(PRK_mys)

UN_HDI_mys_combWB_SHDI <-UN_HDI_mys_combWB %>% 
  bind_rows(SHDI_data_mys %>% filter(!iso3 %in% UN_HDI_mys_combWB$iso3)) %>% 
  set_names('iso3', 'country', paste0(1990:2021)) %>% 
  pivot_longer(-c(iso3, country), names_to = 'year', values_to = 'msch')




# let's get data for Taiwan from v4 of SHDI
HDI_taiwan <- as_tibble(read_csv ("input/SHDI Complete 4.0 (1).csv")) %>%
  filter(region == 'Taiwan' ) %>%
  mutate(gnic = 1000*gnic) %>%
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, lifexp,  gnic,  esch,  msch) %>% 
  mutate(iso3 = 'TWN') %>% 
  mutate(country = 'Taiwan') %>% 
  # data only until 2018; let's use that for 2019-2021 too
  bind_rows(slice(.,rep(29,3))) %>%
  mutate(year = 1989 + row_number()) %>%
  # all data to NA
  mutate(lifexp = ifelse(year > 2018, NA, lifexp))%>%
  mutate(gnic = ifelse(year > 2018, NA, gnic))%>%
  mutate(esch = ifelse(year > 2018, NA, esch))%>%
  mutate(msch = ifelse(year > 2018, NA, msch)) %>% 
  mutate(year = as.character(year))



#### 2.5 gender development index ----

UN_GDI_db <- read_csv('input/HDR23-24_Composite_indices_complete_time_series.csv') %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% 
  select(iso3, country, region, contains("gdi")) %>% 
  select(-gdi_group_2022, -gdi_2022) %>% 
  filter(!grepl('ZZ',iso3)) %>% 
  mutate(region = ifelse(is.na(region), "ECA", region)) # for european countries NA, so let's use ECA

# add taiwan (similar gender situation as in Singapore, so we use values from there)
# Source: 2021 Gender at Glance in ROC (Taiwan)

UN_GDI_TWN <- UN_GDI_db %>% 
  filter(iso3 == 'SGP') %>% 
  mutate(iso3 = "TWN", country = "Taiwan", region = 'EAP')

UN_GDI_db_twn <- UN_GDI_db %>% 
  bind_rows(UN_GDI_TWN)


UN_GDI_db_regionAvg <- read_csv('input/HDR23-24_Composite_indices_complete_time_series.csv') %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% 
  select(iso3, country, region, contains("gdi")) %>% 
  select(-gdi_group_2022, -gdi_2022) %>% 
  filter(grepl('ZZ',iso3)) %>% #%>%
  mutate(region = substring(iso3,5,10000)) %>% 
  filter(!grepl("HD",region)) %>% 
  select(-iso3, -country) %>% 
  pivot_longer(-region, names_to = 'year', values_to = 'gdi')
  
  
  
  # pivot_longer(-c(iso3, country, region), names_to = 'year', values_to = 'gdi') %>% 
  # mutate(region = ifelse(is.na(region), "OECD", region)) %>% 
  # group_by(region, year) %>% 
  # summarise(gdi = mean(gdi, na.rm=T))

#unique(UN_GDI_db_regionAvg$region)


UN_GDI_db_twn_NA <- UN_GDI_db_twn %>% 
  filter(if_all(gdi_1990:gdi_2021, is.na)) %>% 
  # add countries which are not in UN nor in SGDI, but we have date for other parameters
  add_row(iso3 = 'ABW', country = 'Aruba', region = 'LAC') %>% 
  add_row(iso3 = 'BMU', country = 'Bermuda', region = 'LAC') %>% 
  add_row(iso3 = 'CUW', country = 'Curaçao', region = 'LAC') %>% 
  add_row(iso3 = 'CYM', country = 'Cayman Islands', region = 'LAC') %>% 
  add_row(iso3 = 'GLP', country = 'Guadeloupe', region = 'LAC') %>% 
  add_row(iso3 = 'GRL', country = 'Greenland', region = 'ECA') %>% 
  add_row(iso3 = 'KSV', country = 'Kosovo', region = 'ECA') %>% 
  add_row(iso3 = 'MAC', country = 'Macao, China', region = 'EAP') %>% 
  add_row(iso3 = 'MTQ', country = 'Martinique', region = 'LAC') %>% 
  add_row(iso3 = 'PRI', country = 'Puerto Rico', region = 'LAC') %>% 
  add_row(iso3 = 'REU', country = 'Reunion', region = 'SSA')  %>% 
  pivot_longer(-c(iso3, country, region), names_to = 'year', values_to = 'gdi_org') %>% 
  left_join(UN_GDI_db_regionAvg) %>% 
  select(-gdi_org)

UN_GDI_db_filled <- UN_GDI_db_twn %>% 
  filter(!if_all(gdi_1990:gdi_2021, is.na)) %>% 
  pivot_longer(-c(iso3, country, region), names_to = 'year', values_to = 'gdi') %>% 
  bind_rows(UN_GDI_db_twn_NA)

  

# SGDI national data
SGDI_data <- read_csv('input/GDL-Subnational-GDI-data.csv') %>% 
  rename(iso3 = ISO_Code) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% # kosovo to correct iso3
  filter(Region == 'Total') %>%  # only national data
  select(-c(Continent, Level, GDLCODE, Region)) %>% 
  select(iso3, everything()) %>% 
  set_names(c('iso3', 'country', paste0('gdi_',1990:2021)))


# combine UN and WB and SHDI
UN_gdi_combSGDI <- UN_GDI_db_filled %>% 
  select(-region) %>% 
  pivot_wider(names_from = year, values_from = gdi) %>% 
  bind_rows(SGDI_data %>% filter(!iso3 %in% UN_GDI_db$iso3)) 



UN_gdi_combSGDI_long <-UN_gdi_combSGDI %>% 
  set_names('iso3', 'country', paste0(1990:2021)) %>% 
  pivot_longer(-c(iso3, country), names_to = 'year', values_to = 'gdi')
# 
# 
# 
# 
# UN_GII_db <- read_csv('input/HDR23-24_Composite_indices_complete_time_series.csv') %>% 
#   mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% 
#   select(iso3, country, contains("gii")) %>% 
#   select(-gii_rank_2022, -gii_2022) %>% 
#   filter(!grepl('ZZ',iso3)) 



#### 2.6 subnational corruption -----



WB_CC_combSHDI <- readxl::read_excel('input/P_Data_Extract_From_Worldwide_Governance_Indicators.xlsx',
                                   sheet = 'Data') %>% 
  select(-'Country Name', -'Series Name') %>% 
  rename(iso3 = 'Country Code') %>% 
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% 
  mutate(yr1997 = NA) %>% mutate(yr1999 = NA) %>% mutate(yr2001 = NA) %>% 
  select(iso3, `Series Code`, `1996 [YR1996]` , yr1997,`1998 [YR1998]`,
         yr1999,`2000 [YR2000]`, yr2001, everything()) %>% 
  
  set_names(c('iso3', 'code', paste0(1996:2022))) %>% 
  pivot_longer(-c(iso3, code), names_to = 'year', values_to = 'cci') %>% 
  left_join(cntry_info %>% rename(iso3 = iso_code)) %>% 
  rename(country = country_name) %>% 
  #mutate(cci = round(cci,2, na.rm = T)) %>% 
  select(iso3, country, year, cci) %>% 
  as_tibble()  %>% 
  filter(!is.na(iso3)) %>% 
  distinct(iso3, year, .keep_all = T) # remove dublicates


# SGDI national data
GDL_data <- readxl::read_excel('../GIS_data_common/subnat_corruption/SUBCPI and SUBCCI Dataset.xlsx') %>% 
  rename(iso3 = iso) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO', 'KSV', iso3)) %>% # kosovo to correct iso3
  filter(is.na(region)) %>%  # only national data
  select(iso3, year, CCI) #%>% 
  # select(iso3, everything()) %>% 
  # set_names(c('iso3', 'country', paste0('gdi_',1990:2021)))

# check if GDL has data that is not in worldbank

GDL_data_sel <- GDL_data %>% 
  filter(!iso3 %in% unique(WB_CC_combSHDI$iso3))


## no such data available, we can use WB data



##### 3 combine all ----

adm0_comb <- UN_HDI_le_combWB_SHDI %>% 
  full_join(UN_HDI_gnipc_combWB_SHDI, by=c('iso3', 'year')) %>% 
  full_join(UN_HDI_eys_combWB_SHDI, by=c('iso3', 'year')) %>% 
  full_join(UN_HDI_mys_combWB_SHDI, by=c('iso3', 'year')) %>% 
  full_join(UN_gdi_combSGDI_long, by=c('iso3', 'year')) %>% 
  full_join(WB_CC_combSHDI, by=c('iso3', 'year')) %>% 
  select(iso3, year, lifexp, gnic, esch, msch, gdi, cci) %>% 
  left_join(cntry_info[,1:2] %>% rename(iso3 = iso_code)) %>% 
  rename(country = country_name) %>% 
  select(iso3, country, everything()) %>% 
  bind_rows(HDI_taiwan)



#### 4. some modifications ----

### extrapolation returns negative values for msch for Somalia and Eritrea and Bhutan
# and very high values for esch

# let's use the last available year for previous years for these countries

adm0_comb_sel <- adm0_comb %>% 
  filter(iso3 %in% c('BTN', 'SOM', 'ERI'))

msch_last <- adm0_comb_sel %>% 
  select(iso3, year, msch) %>% 
  drop_na() %>% 
  group_by(iso3) %>% 
  mutate(minYear = min(year)) %>% 
  filter(year == minYear) %>% 
  rename(last.msch = msch) %>% 
  select(-c(year, minYear))

esch_last <- adm0_comb_sel %>% 
  select(iso3, year, esch) %>% 
  drop_na() %>% 
  group_by(iso3) %>% 
  mutate(minYear = min(year)) %>% 
  filter(year == minYear) %>% 
  rename(last.esch = esch) %>% 
  select(-c(year, minYear))

adm0_comb_sel2 <- adm0_comb_sel %>% 
  left_join(msch_last) %>% 
  left_join(esch_last) %>% 
  mutate(esch = ifelse(is.na(esch), last.esch, esch)) %>% 
  mutate(msch = ifelse(is.na(msch), last.msch, msch)) %>% 
  select(-c(last.msch, last.esch))

adm0_comb <- adm0_comb %>% 
  filter(!iso3 %in% c('BTN', 'SOM', 'ERI')) %>% 
  bind_rows(adm0_comb_sel2)


### for some territories data in subnational data, let's add those still

HDI_data <- read_csv('input/SHDI-SGDI-Total 7.0.csv') %>% 
  mutate(iso_code = ifelse(iso_code == 'XKO', 'KSV', iso_code)) %>% # kosovo to correct iso3
  #bind_rows(HDI_taiwan) %>%  # add Taiwan
  rename(iso3 = iso_code) %>% 
  select(iso3, country, year, GDLCODE, level, lifexp,  gnic,  esch,  msch)


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


## filter where GDI is 0 but data for all other indicators

adm0_combSummary_gdi <- adm0_combSummary %>% 
  filter(lifexp > 0 & gnic > 0 & esch > 0 & msch > 0 & gdi == 0) %>% 
  select(iso3, country) 

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
  v_cntryGIS <- terra::simplifyGeom(vect('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0'))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  terra::writeVector(v_cntryGIS, 'data_GIS/v_cntryGISsimplif.gpkg')
  terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
}




# indicators:  lifexp  gnic  esch  msch gdi

lifexp_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'lifexp')
gnic_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'gnic')
esch_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'esch')
msch_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'msch')
gdi_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'gdi')
cci_interpExtrap <- f_interpExtrap_adm0(nameIndic = 'cci', yearRange = 1996:2021)


adm0_comb_interpExtrap <- lifexp_interpExtrap %>% 
  full_join(gnic_interpExtrap) %>% 
  full_join(esch_interpExtrap) %>% 
  full_join(msch_interpExtrap) %>% 
  full_join(gdi_interpExtrap) %>% 
  full_join(cci_interpExtrap)

write_csv(adm0_comb_interpExtrap, 'results/adm0_comb_interpExtrap.csv')





