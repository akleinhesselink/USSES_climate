rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

df <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')

clean_df <- 
  df %>% 
  filter(stat=='raw', bad_values == 0, measure == 'VWC') %>% 
  group_by( PrecipGroup, Treatment, plot, depth , unique_position, year, season, month, simple_date, rainfall, PRCP, ann_cum_PRCP ) %>% 
  summarise( avg_VWC = mean(v), n = n()) 

clean_df$PRCP[ clean_df$PRCP == 0 ] <- NA

gg <- ggplot( clean_df, aes( x = simple_date, y = 100*avg_VWC )) + 
  geom_point() + 
  geom_bar(aes(y = PRCP), stat = 'identity', alpha = 0.6 ) + 
  facet_wrap (~ unique_position, ncol = 1 )  

p <- clean_df %>% group_by( plot ) %>% do(p =  gg %+% . + ggtitle(unique( .$Plot)))

pdf( 'figures/plot_soil_moisture_with_rainfall.pdf', height = 8, width = 11)
print( p$p ) 
dev.off()


# compare temperature 

clean_T_df <- 
  df %>% 
  filter( stat == 'raw', bad_values == 0 , measure == 'C', TMEAN > -999) %>% 
  group_by(depth, PrecipGroup, Treatment, plot, unique_position, year, season, month , simple_date, TMEAN ) %>% 
  summarise( avg_C = mean(v), n = n() ) %>% 
  group_by( PrecipGroup, Treatment, plot, unique_position) %>% 
  mutate( scaled_C = scale(avg_C, scale= FALSE) , scaled_TMEAN = scale( TMEAN, scale = FALSE ))

treatment_comparison <- 
  clean_T_df %>% 
  ungroup() %>% 
  filter( depth == 'air temperature') %>% 
  dplyr::select(depth, PrecipGroup, Treatment, year, season, month, simple_date, TMEAN, avg_C ) %>% 
  spread( Treatment, avg_C )  %>% 
  mutate( Drought = Drought - Control, Irrigation = Irrigation - Control ) %>% 
  gather( Treatment , val, Drought:Irrigation )
  
ggplot( treatment_comparison, aes( x = simple_date, y = val, color = Treatment ) ) + 
  geom_point() + 
  facet_grid( PrecipGroup ~ . )

m1 <- lm( data = subset( treatment_comparison , season != 'winter' & PrecipGroup != 3 ), val ~ Treatment*season + factor(PrecipGroup) ) 


# soil temperature 

soil_treatment_comparison <- 
  clean_T_df %>% 
  ungroup() %>% 
  filter( depth == '5 cm deep') %>% 
  dplyr::select(depth, PrecipGroup, Treatment, year, season, month, simple_date, TMEAN, avg_C ) %>% 
  group_by( depth, PrecipGroup, Treatment, year, season, month, simple_date, TMEAN) %>% 
  summarise( avg_C = mean(avg_C)) %>%
  spread( Treatment, avg_C ) %>% 
  mutate( Drought = Drought - Control, Irrigation = Irrigation - Control ) %>% 
  gather( Treatment , val, Drought:Irrigation )

ggplot( soil_treatment_comparison, aes( x = simple_date, y = val, color = Treatment ) ) + 
  geom_point() + 
  facet_grid( PrecipGroup ~ . )

m1 <- lm(data = soil_treatment_comparison, val ~ Treatment*season + factor(PrecipGroup) ) 
summary(m1)

gg_Tp <- ggplot ( clean_T_df, aes( x = simple_date, y = avg_C)) + 
  geom_point(alpha = 0.2) + 
  geom_point( aes( x = simple_date, y = TMEAN), color = 'red', alpha = 0.2 ) + 
  ylim( c(-40, 50 ) )

Tp <- clean_T_df %>% group_by( plot) %>% do( p = gg_Tp %+% . + ggtitle(unique( .$plot)))

gg <- ggplot( clean_T_df, aes( x = TMEAN, y = avg_C)) + 
  geom_point() + 
  geom_abline( aes( intercept = 0 , slope = 1), color = 'red') + 
  facet_wrap( ~ year) + 
  ylab ('observed avg daily temps in the plots') + 
  xlab( 'daily avg temperature observed at the USSES weather station') 

pdf ( 'figures/compare_daily_tmean.pdf', height = 8, width = 8)
print( gg ) 
dev.off()

gg <- clean_T_df %>% do( p =  gg %+% . + ggtitle( paste ( 'plot no.' , .$plot, .$Treatment ))) 

pdf( 'figures/compare_daily_tmean_per_plot.pdf', height = 8, width = 8)
print( gg$p ) 
dev.off()
