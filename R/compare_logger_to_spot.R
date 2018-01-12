rm(list = ls()) 

library( ggplot2 ) 
library(tidyr)
library(dplyr)
library(lme4)
library(zoo)

df <- readRDS('data/processed_data/decagon_data_with_station_data.RDS')
df_spot <- readRDS('data/processed_data/spring_spot_measurements.RDS')

# ----------------------------------------------------------------------------- 

daily_VWC <- df %>% 
  filter ( measure == 'VWC', depth == '5 cm deep', bad_values == 0 ) %>% 
  group_by ( Treatment, PrecipGroup, plot, simple_date, depth  ) %>% 
  summarise( dec_VWC = mean( v )*100)

rm(df)

spot_avg <- 
  df_spot %>% 
  group_by( plot, date, Treatment, PrecipGroup) %>% 
  summarise( spot = mean(VWC) )

spot_avg$simple_date <- as.Date( spot_avg$date, tz = 'MST')

spot_avg <- left_join( spot_avg, daily_VWC, by = c('plot', 'simple_date', 'PrecipGroup', 'Treatment'))

m1 <- lm(data = spot_avg, spot ~ dec_VWC )
summary(m1)

m1.1 <- lm(data = subset(spot_avg, PrecipGroup == 1), spot ~ dec_VWC)
summary(m1.1)

m1.3 <- lm(data = subset(spot_avg, PrecipGroup == 3), spot ~ dec_VWC)
summary(m1.3)

m1.4 <- lm(data = subset(spot_avg, PrecipGroup == 4), spot ~ dec_VWC)
summary(m1.4)

m1.6 <- lm(data = subset(spot_avg, PrecipGroup == 6), spot ~ dec_VWC)
summary(m1.6)

ggplot(subset( df_spot, PrecipGroup == 6), aes( x = Treatment, y = VWC, color= Treatment ) ) + 
  geom_point()  + 
  geom_boxplot(fill = NA)

daily_VWC$year <- strftime( daily_VWC$simple_date, '%Y' )
daily_VWC$month <- as.numeric( strftime ( daily_VWC$simple_date, '%m') ) 

ggplot(subset( daily_VWC, PrecipGroup == 6 & month %in% c(4,5,6)), aes( x = Treatment, y = dec_VWC, color = Treatment )) + 
  geom_point() + 
  geom_boxplot(fill = NA)  + 
  facet_wrap( ~ year)  + 
  ggtitle('Precip group 6')

ggplot( subset( spot_avg, PrecipGroup == 6),aes( x = spot, y = dec_VWC)) + geom_point() + facet_wrap( ~ Treatment)

overall_avg <- 
  spot_avg %>% 
  group_by(PrecipGroup, plot, Treatment) %>% 
  summarise( spot_avg = mean(spot, na.rm = TRUE), dec_avg = mean(dec_VWC, na.rm = TRUE))


# plot mean 

pdf( 'figures/avg_spot_v_avg_decagon.pdf', height = 6, width = 6 ) 

print( 
  ggplot( overall_avg, aes( x = spot_avg, y = dec_avg)) + 
    geom_point() + 
    geom_text(aes( label = plot ), vjust = 2) + 
    geom_abline( aes( intercept = 0 , slope = 1 ), linetype = 2) + 
    xlab ( 'spot measurement avg VWC ') + 
    ylab ( 'decagon average VWC on same dates')   
  )

dev.off()
