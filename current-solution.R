library(lubridate)
library(ggplot2)
library(scales)


CrashesByWeek <- function(c){
  
  c$collision_date <- strptime(c$collision_date, format = '%Y-%m-%d %H:%M:%S')
  
  c$hour <- hour(c$collision_date)
  c$day <- weekdays(c$collision_date)
  
  t <- as.data.frame(table(c$day, c$hour))
  
  names(t) <- c('day', 'hour', 'Crashes')
  
  t$hour <- as.numeric(as.character(t$hour))
  t$day <- factor(t$day, ordered = TRUE, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
  
  ggplot(t, aes(day, hour, fill = Crashes)) + 
    geom_raster() + 
    scale_fill_gradientn(values = rescale(c(100, 90, 70, 40, 0)), colours = c("#4A0000", "#f03b20", "#feb24c", "grey90", "white")) + 
    scale_x_discrete(drop=FALSE) + 
    scale_y_continuous(breaks = c(0:23), limits = c(-1,24), labels=c("Unk", 1:23)) +
    xlab('Day of Week') + 
    ylab('Hour (time) of Day') + 
    theme(panel.background = element_rect(fill = 'white'))
}

crashes <- read.csv("./crashes.csv", header=TRUE, stringsAsFactors = FALSE)

crashes$collision_date <- strptime(crashes$collision_date, format = '%m/%d/%Y %H:%M')
crashes$weekday <- weekdays(as.Date(crashes$collision_date))
crashes$hour <- hour(crashes$collision_date)

CrashesByWeek(crashes)
