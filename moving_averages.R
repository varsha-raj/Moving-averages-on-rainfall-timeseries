#Reuiqred Packages
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)

#Global environment settings, preferably added to user's Rprofile
Sys.setenv(TZ='EST')
options(error=recover)
options(warn=-1)
options(stringsAsFactors=FALSE)
options(scipen=999)

#Read hourly rainfall time series
d <- read.csv("dummy_hourly_rainfall.csv", header=TRUE)

#Format timestamp to POSIXct time format and specify time zone, as in this 'Eastern Standard Time' is used
d$DateTime <- ymd_hms(d$DateTime, tz = "EST")

#Data filtering get only wet weatger values
d <- d %>%

filter(Rainfall > 0) %>%
select(DateTime, Rainfall)

#Evebt definition criteria (Miminimum inter-event time or MIT)
interevent_time = 6

#Parse the rainfall values into independent rainfall events using the MIT value
d_events <- d %>%

        mutate(timesteps = c(interevent_time+1, as.numeric(diff(DateTime), units= 'hours')),
          event = rep(seq_along(which(timesteps > interevent_time)), 
            diff(c(which(timesteps > interevent_time), length(timesteps) + 1)))) %>%

        group_by(event) %>%

        summarise(rainfall_starttime = first(DateTime),
                  rainfall_endtime = last(DateTime),
                  event_vol_in = sum(Rainfall)) %>% ungroup() %>%
        mutate(duration_hr = 
              as.numeric(rainfall_endtime-rainfall_starttime, units='hours') + 1,
              rainfall_starttimeYear = year(rainfall_starttime)) %>%

        select(everything(), -rainfall_starttime, -rainfall_endtime)

#Extract largest event volumes from within each year
d_max_events <- d_events %>%

arrange(rainfall_starttimeYear) %>%

group_by(rainfall_starttimeYear) %>% 

summarize(largest_event_volume = max(event_vol_in)) %>% na.omit()

#Datafrane for 20-year moving averages

MA20yr_df <- d_max_events %>%

mutate(yr_20_MA = rollmean(largest_event_volume, 20, align='right', fill=NA)) 

#Linear regression of largest event volumes for each year to calculate slope
regress = coef(lm(largest_event_volume ~ rainfall_starttimeYear, data = d_max_events))

## Create plots using ggplot
p1 <- ggplot()

p2 <- p1 + geom_line(data= MA20yr_df, aes(x= rainfall_starttimeYear, y=yr_20_MA, color=''), 

  linetype='solid', size=0.5)  

p3 <-  p2 + geom_line(data= d_max_events, aes(x= rainfall_starttimeYear, y=largest_event_volume, linetype= ''), 

  color='blue', size=0.5) + geom_abline(intercept = regress[[1]], slope = regress[[2]])

p4 <- p3 + scale_color_manual(values=c("red"), 
  label='20-Year Moving Average (1900-2016)', name="")

p5 <- p4 + scale_linetype_manual(values=c("solid"), 
  label='Annual Maximum Precipitation\nEvent Volume (inches)\n(Event definition: 6-hour MIT)', name="")

p6 <- p5 + scale_y_continuous(breaks=seq(0,8.5,0.5), minor_breaks=waiver(), limits=c(0,8.5), expand=c(0,0))+

scale_x_continuous(breaks=seq(1900,2016,10), minor_breaks = waiver(), expand =c(0,0)) + coord_cartesian(xlim=c(1900,2018))

int = round(regress[[1]], 3)
slope = round(regress[[2]], 3)

p7 <- p6 + labs(x='\nYear', y = "") + annotate("text", x = 1990, y = 8, label= sprintf("italic(y) == %0.3f + %0.3f%%.%%italic(x)", int, slope), parse = TRUE,
                                                                                      size =2.5, color='black')


windowsFonts(F = windowsFont('Times New Roman'))

p8 <- p7 + theme(axis.text.x=element_text(size=8, family="F"),
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
                                    legend.margin = unit(0.1,'cm'))


gt <- ggplot_gtable(ggplot_build(p8))
             #gt$layout$clip[gt$layout$name=='panel'] <- 'off'
             grid.draw(gt)
             png('20yrMA_6MIT_dummy.png', units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()








