library(reshape2)
library(tidyverse)
library(lubridate)
library(plotly)

setwd("C:/Users/Declan/Desktop/Data/R/DetectorDash")

vol <- read.csv('Volume.csv', header=FALSE)
time_row <- vol[1,]
time_melt <- melt(time_row, id.vars = 'V1') %>%
  select(-V1)
vol <- vol[-1,]

vol_melt <- melt(vol, id.vars = 'V1') %>%
  select(everything(), volume=value) %>%
  merge(time_melt, by = 'variable') %>%
  mutate(volume = as.numeric(volume))

vol_melt$date <- '2017-05-01'

vol_melt$datetime <- as.POSIXct(paste(vol_melt$date,vol_melt$value,sep=''), format = '%Y-%m-%d %H:%M:%S')

vol_melt$dt_15 <- lubridate::round_date(vol_melt$datetime, "15 minutes") 

vol_melt_15 <- vol_melt %>%
  group_by(V1,dt_15) %>%
  summarise(vol = mean(volume,na.rm=TRUE))

detect <- vol_melt_15 %>%
  filter(V1 == 'I-35E (S864) 3 lanes')

detect$hour <- hour(detect$dt_15)
detect$yday <- 1001


dt_17 <- seq(as.POSIXct('2017-01-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 
             as.POSIXct('2017-12-31 23:45:00', format = '%Y-%m-%d %H:%M:%S'), '15 mins')
detect_df <- data.frame(datetime = dt_17)
detect_df$yday <- yday(detect_df$datetime)

detect_df$hr_min <- paste(hour(detect_df$datetime), minute(detect_df$datetime), sep='_')
detect$hr_min <- paste(hour(detect$dt_15), minute(detect$dt_15), sep='_')

detect_df <- merge(detect_df,select(detect,vol,hr_min))

detect_df$vol <- jitter(detect_df$vol, factor = 1000)
detect_df$hour <- hour(detect_df$datetime)
detect_df$minute <- minute(detect_df$datetime)
detect_df$seconds <- detect_df$hour*60*60 + detect_df$minute*60

detect_vline <- detect_df %>%
  filter(minute == 45) %>%
  group_by(minute,seconds) %>%
  summarise(count = n()) %>%
  mutate(vline = seconds+450,
         labels = vline - 1800)
detect_vline$hour <- 1:24
  

# g <- ggplot(detect_df) +
#   geom_jitter(aes(x = seconds, y=vol, group = seconds),color='grey',alpha=0.05)+
#   geom_boxplot(aes(x = seconds, y=vol, group = seconds), outlier.shape = NA)+
#   geom_vline(xintercept = detect_vline$vline,linetype = "dashed",alpha=0.75, color='grey')+
#   scale_x_continuous(labels = detect_vline$hour, breaks = detect_vline$labels) +
#   theme(panel.background = element_blank())
# 
# ggplotly(g)


f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "lightgrey"
)
f2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "black"
)

ax <- list(
  zeroline = FALSE,
  showline = FALSE,
  showgrid = FALSE,
  title = "Hour",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  tickvals = detect_vline$labels,
  ticktext = detect_vline$hour
)

ay <- list(
  zeroline = FALSE,
  showline = FALSE,
  showgrid = FALSE,
  title = "Volume",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2
)
ylim_min <- min(detect_df$vol)
ylim_max <- max(detect_df$vol)
plot_ly(detect_df,type="box") %>%
  add_boxplot(x=~seconds,y=~vol, boxpoints = 'outliers', jitter=.5, pointpos=0,
              marker = list(color = 'rgb(7,40,89)', opacity = 0.5),
              line = list(color = 'rgb(7,40,89)'),
              name = "All Points") %>%
  add_segments(x = ~detect_vline$vline, xend = ~detect_vline$vline,
               y = ylim_min - ylim_max*0.05, yend = ylim_max + ylim_max*.05,
               line = list(color ='rgb(145,163,176')) %>%
  layout(xaxis=ax, yaxis=ay, showlegend=FALSE)

