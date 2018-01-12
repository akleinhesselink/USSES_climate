################################################################################################## 
#
# Plot daily temperatures for each plot each month.  Good way to check that times are correct. 
#
##################################################################################################

rm(list = ls())
library( dplyr)
library(ggplot2)

temps <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')

df  <- temps %>% 
  filter( bad_values == "0", depth == 'air temperature', !is.na(v) , stat == 'raw') 

test <- df %>% ungroup() %>% filter( f == f[1] )

daily_t <- ggplot(test, aes( x  = hour , y = v , group = simple_date ) ) +  
  geom_line(stat = 'smooth', method = 'loess', alpha = 0.1) + 
  geom_vline(aes(xintercept = 16)) + 
  ylim ( -50, 60 ) + 
  facet_wrap( ~ month )

plots <- df %>% ungroup() %>% 
  group_by (plot , year ) %>% 
  do ( gg = daily_t %+% . + ggtitle ( paste( 'plot = ', unique( .$plot) , 'year = ', unique( .$year) ))) 


# print -----------------------------------------------------------------------------------------------

pdf ( 'figures/check_daily_temperatures.pdf', width = 7 , height = 10)
print( plots$gg ) 
dev.off()



