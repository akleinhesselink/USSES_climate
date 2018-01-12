rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)

df <- readRDS(file = 'data/processed_soil_data/decagon_data_corrected_values.RDS')
station_dat <- read.csv('data/climate/USSES_climate.csv')

# ---------------------------------------------------------------------------------------
station_dat$date <-  as.POSIXct( strptime( station_dat$DATE, '%Y%m%d', tz = 'MST')  ) 

station_dat <- 
  station_dat %>% 
  mutate( TMEAN = ( TMAX + TMIN ) / 2 ) %>% 
  select(date, PRCP, TMEAN)

station_dat$PRCP[ station_dat$PRCP == -9999.0 ] <- 0
station_dat$TMEAN[ station_dat$TMEAN == -9999.0 ] <- NA  ## check if celsius 
station_dat$TMEAN[ station_dat$TMEAN < -4000.0 ] <- NA  ## check if celsius 

station_dat <- 
  station_dat %>% 
  mutate( rainfall = rollapply(PRCP, 2, sum, fill = 0, na.rm = TRUE, align = 'right') ) %>%
  mutate( rainfall = ifelse( rainfall > 0.0 & TMEAN > 3 & !is.na(rainfall), 'rainy', 'not rainy')) %>%
  mutate( rainfall = ifelse( is.na(rainfall), 'not rainy', rainfall))

# create a factor listing each rainy period, including the day before the rain  
station_dat <- 
  station_dat %>% 
  arrange( desc(date) ) %>% 
  mutate( prerain = lag( rainfall, 1) ) %>%
  mutate( prerain = ifelse( prerain == 'rainy' & rainfall == 'not rainy', TRUE, FALSE)) %>%
  arrange( date) %>% 
  mutate( prcp_event = factor( cumsum ( prerain ) )) %>% 
  group_by( prcp_event, prerain) %>% 
  mutate( total_rain = cumsum(PRCP) )

station_dat <- 
  station_dat %>% 
  ungroup() %>% 
  mutate( simple_date = as.Date( date, tz = 'MST')) %>% 
  mutate( year = strftime( simple_date, '%Y', tz = 'MST')) %>%  
  group_by( year ) %>% 
  arrange( year, simple_date ) %>% 
  mutate( ann_cum_PRCP = cumsum(PRCP))

saveRDS( station_dat, 'data/processed_soil_data/daily_station_dat_rainfall.RDS')

# clean-up decagon data -------------------

df <- 
  df %>% 
  filter( stat == 'raw', bad_values == 0 )

df$depth_label <- factor( df$depth , levels = c('air temperature', '5 cm deep', '25 cm deep') , order = TRUE ) 
df$Treatment_label <- factor(df$Treatment, levels = c('Drought', 'Control', 'Irrigation'), order = TRUE)

df <- df %>% 
  mutate ( unique_position = paste0( plot, '.', position))

df$datetime <- df$new_date

station_dat$simple_date <- as.Date( station_dat$date, tz = 'MST')

df <- df %>% left_join( station_dat, by = c('year','simple_date')) 

saveRDS(df, 'data/processed_soil_data/decagon_data_with_station_data.RDS' ) 
