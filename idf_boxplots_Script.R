
#Boxplots graph code

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

setwd('D:/20_year_chunks/statistical_tests')

#Read hourly rainfall time series
df <- read.csv("max_event_volumes_boxplots.csv", header=TRUE)


p1 <- ggplot(df, aes(year_range,largest_event_volume)) + 
        stat_boxplot(geom="errorbar", width = .25) +
        geom_boxplot(fill= "grey", outlier.shape = 4, outlier.size = 1, outlier.colour = "red", outlier.stroke = 1.5) +
        stat_summary(fun.y = mean, colour="darkred", geom="point", size = 3, aes(
                           shape=''))

p2 <- p1 + labs(x= "\nYear", y = "Largest Event Volumes (inches)\n") +
             #coord_cartesian(ylim = c(0,5000)) +
             annotate("text", x=-Inf, y=8.3, label=" Box hinges = 25 - 75%, Whiskers = (Q1 -1.5*IQR, Q3 + 1.5*IQR)", size=2, hjust=0, vjust=1)

p3 <- p2 + scale_y_continuous(breaks=seq(1,8,0.5), minor_breaks=waiver(), limits=c(1,8.5), expand=c(0,0)) + scale_shape_manual(values = c(rep(18, 6)), name = 'Mean')

windowsFonts(F = windowsFont('Times New Roman'))

p4 <- p3 + theme(axis.text.x=element_text(size=8, family="F"),
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

#png(paste("event_volume_boxplots_mean_12172018","png", sep="."), width=1800, height=1200, res=100);plot(p4);dev.off()
gt <- ggplot_gtable(ggplot_build(p4))
             #gt$layout$clip[gt$layout$name=='panel'] <- 'off'
             grid.draw(gt)
             png('event_volume_boxplots.png', units='in', width=6.5, height=4.5, res=250); plot(gt);dev.off()