### make bar plots like NYC

## get PHL data and break into events using 6 hour MIT for each year

library(dplyr)
library(zoo)
library(tidyr)
library(data.table)
library(lubridate)

Sys.setenv(TZ='EST')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

d <- read.csv("phl_hourly_rain.csv", header=TRUE)

d$DateTime <- ymd_hms(d$DateTime, tz = "EST")

d <- filter(d, Rainfall > 0)

phl_data <- data.frame(
	DateTime = d$DateTime,
	Rainfall = d$Rainfall,
  Year_val =year(d$DateTime)
	#Hour_diff = as.numeric(d$DateTime-lag(d$DateTime), units='hours')
	)

interevent_time = 6


temp <- within(phl_data, {
  timesteps <- c(interevent_time+1, as.numeric(diff(DateTime), units= 'hours'))
  start.idxs <- which(timesteps > interevent_time)
  event <- rep(seq_along(start.idxs), diff(c(start.idxs, length(timesteps)+1)))
  rm(timesteps, start.idxs)
  event_vol_in <- ave(Rainfall,event, FUN=sum)

})

phl_events <- with(temp, {
  list(aggregate(DateTime ~ event, FUN=min),
       aggregate(DateTime ~ event, FUN=max),
       aggregate(event_vol_in ~ event, FUN=max)
  )
})

phl_events <- Reduce(function(...) merge(..., by='event'), phl_events)

names(phl_events) <- c('event', 'rainfall_starttime', 'rainfall_endtime', 'event_volume')

phl_f <- phl_events %>%

mutate(duration_hr = 
  as.numeric(rainfall_endtime-rainfall_starttime, units='hours') + 1)#,
  # ave_int_in_hr = event_volume/duration_hr,
  # year_starttime = year(rainfall_starttime),
  # year_endtime = year(rainfall_endtime))

# year_match <- with(phl_f,{
# match_yr <- which(year_starttime!=year_endtime)
# })

## all events >= 1 inch event volume

phl_f_1inch <- phl_f %>%

group_by(year_endtime) %>%

filter(event_volume >= 1 & event_volume < 1.5) %>%

summarize(no_events = n())

phl_f_1.5inch <- phl_f %>%

group_by(year_endtime) %>%

filter(event_volume >= 1.5 & event_volume < 2) %>%

summarize(no_events = n())

# phl_f_1.6inch <- phl_f %>%

# group_by(year_endtime) %>%

# filter(event_volume >= 1.6 & event_volume < 2) %>%

# summarize(no_events = n())

phl_f_2inch <- phl_f %>%

group_by(year_endtime) %>%

filter(event_volume >= 2) %>%

summarize(no_events = n())

## added 8/10

setwd('D:/20_year_chunks/statistical_tests')

d_events <- read.csv('events_6mit.csv', header = TRUE)

heavy_events <- d_events %>%

group_by(rainfall_starttimeYear) %>%

filter(event_vol_in >= 1.5 & event_vol_in < 2.63) %>%

summarize(no_events = n())

very_heavy_events <- d_events %>%

group_by(rainfall_starttimeYear) %>%

filter(event_vol_in >= 2.63) %>%

summarize(no_events = n())


### make bar plots


library("ggplot2")
library("grid")
library("scales")
library(ggrepel)
library(directlabels)
library(ggalt)
options(warn=-1)


p1 <- ggplot()


p2 <- p1 + geom_bar(data=heavy_events, aes(x=rainfall_starttimeYear, y=no_events, group=1, fill=''), 
  stat='identity', position=position_dodge(), width=0.25)

p3 <- p2 + scale_fill_manual(values='turquoise3', 
  label= '95th percentile (1.5 inches)\n(Heavy Rainfall Events)', name="")

p4 <- p3 + geom_bar(data=very_heavy_events, aes(x=rainfall_starttimeYear, y=no_events, group=1, color=''), 
  stat='identity', fill='tomato', position=position_dodge(), width=0.25)

p5 <- p4 + scale_color_manual(values='tomato', 
  label= '99th percentile (2.63 inches)\n(Very Heavy Rainfall Events)', name="")


# p6 <- p5 + geom_bar(data=phl_f_1.6inch, aes(x=year_endtime, y=no_events, group=1, linetype=''), 
#   stat='identity', fill='orange', position=position_dodge(), width=0.25)

# p7 <- p6 + scale_linetype_manual(values='solid', label= 'Number of events above 1.6 inch', name="")


# p8 <- p5 + geom_bar(data=phl_f_2inch, aes(x=year_endtime, y=no_events, group=1, alpha=''), 
#   stat='identity', fill='seagreen', position=position_dodge(), width=0.25)

# p9 <- p8 + scale_alpha_manual(values=1, label= 'Number of events above 2 inches\n(Extreme Events)', name="")

p10 <- p5 + scale_y_continuous(breaks=seq(0,10,1), limits=c(0,11), expand=c(0,0)) +

scale_x_continuous(breaks=seq(1900,2016,4), minor_breaks=seq(1900,2016,1), limits=c(1899,2018), expand=c(0,0))

p11 <- p10 + labs(x="\nYear", y="Number of Events per Year\n(based on 6 hr inter-event time)")

 windowsFonts(F = windowsFont('Times New Roman'))

# p12 <- p11 + theme(axis.text.x=element_text(size=6, family="F", angle=45, face='bold'),
#                                     axis.text.y=element_text(size=6, family="F", face='bold'),
#                                     #axis.text.x=element_text(size=6, family="F"),
#                                     axis.title.x=element_text(size=8, family="F"),
#                                     axis.title.y=element_text(size=8, family="F"),
#                                     legend.title=element_text(size=8, family="F"),
#                                     legend.text=element_text(size=8, family="F"),
#                                     plot.title=element_text(size=8, hjust = 0.5, face='bold',family="F"),
#                                     #legend.text=element_blank(),
#                                     panel.background = element_rect(fill = "white", colour = NA),
#                                     panel.border = element_rect(fill = NA, color= 'grey20', size=0.4),
#                                     legend.key.size = unit(0, "cm"),
#                                     legend.key = element_rect(colour = NA, fill=NA),
#                                     panel.grid.major = element_line(size=0.2, colour = 'gray80'),
#                                     #panel.grid.major = element_blank(),
#                                     #panel.grid.minor = element_line(size=0.05, colour = 'gray80'),
#                                     axis.ticks = element_line(size=0.2, colour='gray60'),
#                                     panel.grid.minor = element_blank(),
#                                     legend.key.height=unit(0.5,"line"),
#                                     legend.key.width=unit(1,"line"),
#                                     #legend.spacing.y=unit(-0.5, 'cm'))
#                                     legend.margin = unit(0.1,'cm'),
#                                     #legend.position = c(0.950,0.95),
#                                     #legend.justification = c("right","top"),
#                                     legend.box.background = element_rect(fill = "white", colour = NA))

p12 <- p11 + theme(axis.text.x=element_text(size=8, family="F", angle = 90),
                                    axis.text.y=element_text(size=8, family="F"),
                                    axis.title.x=element_text(size=8, family="F"),
                                    axis.title.y=element_text(size=8, family="F"),
                                    legend.title=element_text(size=8, family="F"),
                                    legend.text=element_text(size=8, family="F"),
                                    plot.title=element_text(size=8, hjust = 0.5, face='bold',family="F"),
                                    #legend.text=element_blank(),
                                    panel.background = element_rect(fill = "white", colour = NA),
                                    panel.border = element_rect(fill = NA, color= 'grey20', size=0.4),
                                    legend.key.size = unit(0, "cm"),
                                    legend.key = element_rect(colour = NA, fill=NA),
                                    panel.grid.major = element_line(size=0.2, colour = 'gray80'),
                                    #panel.grid.major = element_blank(),
                                    panel.grid.minor = element_line(size=0.05, colour = 'gray80'),
                                    axis.ticks = element_line(size=0.2, colour='gray60'),
                                    #panel.grid.minor = element_blank(),
                                    legend.key.height=unit(0.5,"line"),
                                    legend.key.width=unit(1,"line"),
                                    #legend.spacing.y=unit(-0.5, 'cm'))
                                    legend.margin = margin(t=0.1, unit ='cm')) 

p13 <- p12 + guides(fill=guide_legend(order=1), color=guide_legend(order=2), alpha=guide_legend(order=3))

setwd("D:/20_year_chunks/bar_plots")

gt <- ggplot_gtable(ggplot_build(p13))
             #gt$layout$clip[gt$layout$name=='panel'] <- 'off'
             grid.draw(gt)
             tiff('phl_events_barplots_heavyVeryheavyevents.tiff', units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()
########################################################################################

