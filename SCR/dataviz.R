# Run the code in 'utils_text_curve.R' first

#### Setup ----------------------------------------------------------------------
library(tidyverse)
library(anytime)
library(lubridate)
library(viridis)
library(ggforce)
library(ggdist)
library(ggplot2)
library(gridExtra)
library(ggridges)
library(BBmisc)

starynight <- c('#010406','#175781', '#fecf3c', '#f8e389', '#a0c899')

# Import a short (wide DF) version of the data
babyShort <- read_csv('./DATA/BabyRecords.csv') %>%
  mutate(lengthRaw =  as.numeric(anytime(FinishDate) - anytime(StartDate))/60,
         StartDate = anytime(StartDate),
         FinishDate = anytime(FinishDate),
         StartDateDay = floor_date(StartDate, 'day'),
         FinishDateDay = floor_date(FinishDate, 'day'),
         StartHr = as.numeric(StartDate - StartDateDay)/60/60,
         FinishHr = as.numeric(FinishDate - FinishDateDay)/60/60) %>%
  filter(lengthRaw > 0)

# Import a long time sequenced version of the data
babyLong <- read_csv('./DATA/BabyRecords.csv') %>%
  mutate(lengthRaw =  as.numeric(anytime(FinishDate) - anytime(StartDate))/60,
         StartDate = round_date(anytime(StartDate), "15 mins"),
         FinishDate = round_date(anytime(FinishDate), "15 mins"),
         length = as.numeric(FinishDate - StartDate)/60) %>% 
  mutate(date = map2(StartDate, FinishDate, seq, by =  '15 mins')) %>%
  unnest(date) %>%
  dplyr::select(-StartDate, -FinishDate, -Details) %>%
  mutate(dateDay = ymd_hms(20000101000000) + (date - floor_date(date, 'day')),
         dow = weekdays(date))

#### Sun plot ------------------------------------------------------------------
babyShortToPlot <- babyShort %>%
  mutate(FinishHr = ifelse(FinishHr < StartHr, 24, FinishHr)) %>%
  # need to handle activities that go over from one day to the next
  bind_rows(babyShort %>%
              filter(FinishHr < StartHr) %>%
              mutate(StartHr = 0)) %>%
  mutate(month = month(StartDateDay, label = T)) %>%
  filter(RecordCategory == 'Sleep') 


ybreaks <- pretty(x = babyShortToPlot$StartDateDay, n = 4)
sunPlot <- babyShortToPlot %>%
  ggplot(aes(y=StartDateDay)) +
  annotate("rect", xmin = 0, xmax = 24, 
           ymin = min(babyShortToPlot$StartDateDay), ymax = max(babyShortToPlot$StartDateDay),
           alpha = .8,fill = starynight[3])+
  geom_segment(aes(x=StartHr, xend =FinishHr, yend=StartDateDay), color=starynight[2], 
               size=0.3, alpha=0.9) +
  annotate("segment", x = 0, xend = 24, alpha=0.5,
           y = max(babyShortToPlot$StartDateDay), yend = max(babyShortToPlot$StartDateDay))+
  xlim(0,24) + 
  geom_label(data = data.frame(x = c(0,23,22.5,22,21.5,21), y = ybreaks, label = c('Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan')),
             aes(x = x, y = y, label = label),
             inherit.aes = F,label.size=0.1,fill=alpha('#175781', 0.4), color='white',
             size = 2) +
  coord_polar() +
  scale_x_continuous(breaks =c(0,6,12,18), labels=c('Midnight','Dawn','Noon','Dusk')) +
  theme(legend.position = 'none',
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=8, color='#175781'),
        panel.background = element_rect(fill = "transparent"),
        plot.background=element_rect(fill = "transparent"),
        legend.background = element_rect(fill="transparent"),
        panel.border = element_blank(),
        axis.title = element_blank())
sunPlot

# Create grob for adding to main plot later
insetTop <- ggplotGrob(sunPlot)

#### Mountain plot ------------------------------------------------------------------

babyHills <- babyLong %>%
  mutate(dayNight = ifelse(hour(date) > 18 | hour(date)  < 6, 'Night', 'Day')) %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(RecordCategory %in% c('Sleep')) %>%
  group_by(RecordCategory) %>%
  mutate(lengthRaw = ifelse(month == 'Aug', normalize(lengthRaw, method='range', range=c(0,0.2)),
                            ifelse(month == 'Sep',normalize(lengthRaw, method='range', range=c(0.1,0.3)), 
                                   ifelse(month == 'Oct', normalize(lengthRaw, method='range', range=c(0.2,0.4)),
                                          ifelse(month == 'Nov', normalize(lengthRaw, method='range', range=c(0.3,0.5)), 
                                                 normalize(lengthRaw, method='range', range=c(0.4,0.6))))))) 
babyHillsText1 <- babyHills %>% filter(dayNight == 'Day' & month == 'Sep') %>%
  mutate(group = 'Thula  thul,  thula  baba,  thula  sana,  Thula  thul,  thula  Abbywe are with you...')
babyHillsText2 <- babyHills %>% filter(dayNight == 'Night' & month == 'Dec') %>%
  mutate(group = 'Thula  thul,  thula  baba,  thula  sana,  Thulubab  uzobuya,  ekuseni.  Shhhhhhh.....')
hillsPlot <-  babyHills %>%
  ggplot(aes(x=lengthRaw , linetype=month, fill=dayNight, alpha=dayNight)) +
  geom_density(size=0.25)+
  geom_text(aes(x = x, y = y+0.5, label = letter, angle = angle), inherit.aes=F, size=3,
            data = density_labels(babyHillsText1$lengthRaw, babyHillsText1$group, 0.7))+
  geom_text(aes(x = x, y = y+0.5, label = letter, angle = angle), inherit.aes=F, size=3,
            data = density_labels(babyHillsText2$lengthRaw, babyHillsText2$group, 0.8))+
  #geom_density_ridges(rel_min_height=0, scale=2.5, color=NA)+
  xlim(0,0.6) +
  scale_linetype_manual(values=c(1,1,1,1,1)) +
  scale_alpha_manual(values=c(1,0.5)) +
  scale_fill_manual(values = c(starynight[3], starynight[2])) +
  guides(linetype='none') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.5),
        legend.text = element_text( color='#175781'),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent", color='transparent'),
        plot.background=element_rect(fill = "transparent", color='transparent'),
        legend.background = element_rect(fill="transparent"))
hillsPlot

#Make grob for adding later
insetBottom <- ggplotGrob(hillsPlot)

#### Bird plot and combined --------------------------------------------------------
weekAgg <- babyShort %>%
  mutate(StartDateDay = floor_date(StartDate, 'week')) %>%
  group_by(StartDateDay, RecordCategory) %>%
  summarise(sumLengthRaw = sum(lengthRaw,na.rm = T),
            maxLengthRaw = max(lengthRaw),
            sdLengthRaw = sd(lengthRaw),
            cvLengthRaw = sd(lengthRaw)/mean(lengthRaw),
            medLengthRaw = median(lengthRaw)) 


birds <- weekAgg %>%
  filter(RecordCategory %in% c( 'Sleep')) %>%
  mutate(dodgeTime = days(round(cvLengthRaw*10^1.15)),
         wingStart = StartDateDay - dodgeTime,
         wingEnd = StartDateDay + dodgeTime)

finalPLot <-  birds %>%
  ggplot(aes(x=StartDateDay, y=medLengthRaw, color=maxLengthRaw, fill=maxLengthRaw)) +
  #geom_line() +
  annotation_custom(grob = insetTop, xmin = ymd_hms(20211005000000), 
                    xmax = ymd_hms(20220120000000), 
                    ymin = 40, ymax = 95)+
  annotation_custom(grob = insetBottom, xmin = ymd_hms(20210720000000), 
                    xmax = ymd_hms(20220120000000), 
                    ymin = 8, ymax = 70) +
  geom_point(aes(size=cvLengthRaw), shape = 25, fill='black') +
  geom_curve(aes(x=StartDateDay, xend =wingStart,yend=medLengthRaw),
             curvature = 0.35, ncp = 1000, lineend = 'butt', angle=45) +
  geom_curve(aes(x=StartDateDay, xend =wingEnd, yend=medLengthRaw),
             curvature = -0.35, ncp = 1000, lineend = 'butt', angle=135) +
  scale_size_continuous(range = c(0, 2)) +
  scale_color_gradientn(colors=c(starynight[2], starynight[1]), guide='none') +
  scale_fill_gradientn(colors=c(starynight[2], starynight[1]), guide='none') +
  ylim(15,90) +
  geom_text(data = data.frame(x = ymd_hms(20211010000000), y=90,
                              label = "Sleepscape for Abby"), inherit.aes=F, aes(x=x,y=y,label = label),
            family = 'URWGothic', size=6, color='#175781')+
  geom_text(data = data.frame(x = ymd_hms(20211010000000), y=87,
                              label = "Jan 2022 by Z & S Venter"), inherit.aes=F, aes(x=x,y=y,label = label),
            family = 'URWGothic', size=3, fontface='italic', color='#175781') + 
  
  annotate( geom = "curve", x = ymd_hms(20210815000000), y = 83, xend = ymd_hms(20210808000000), yend = 87, 
            curvature = -.3, arrow = arrow(length = unit(1, "mm")), size=0.4) +
  annotate(geom = "text", x = ymd_hms(20210816000000), y = 83, size=2, 
           label = "X-axis: 10 August \n Y-axis: median sleep duration 88 mins \n Wing width: CV of 0.5", hjust = "left")+
  
  annotate( geom = "curve", x = ymd_hms(20210910000000), y = 47, xend = ymd_hms(20210923000000), yend = 38, 
            curvature = -.3, arrow = arrow(length = unit(1, "mm")), size=0.4) +
  annotate(geom = "text", x = ymd_hms(20210910000000), y = 47, size=2, 
           label = "X-axis: 10 October \n Y-axis: median sleep \nduration 37 mins \n Wing width: CV of 1.25", hjust = "right")+
  
  annotate( geom = "curve", x = ymd_hms(20211011000000), y = 68, xend = ymd_hms(20211023120000), yend = 60, 
            curvature = .3, arrow = arrow(length = unit(1, "mm")), size=0.4) +
  annotate(geom = "text", x = ymd_hms(20211011000000), y = 69, size=2, 
           label = "Density distribution of sleep durations ranging from\n 5 mins (left of mountain) to 7.5 hrs (right of mountain). \nNight-time sleeps (blue) have a wider variation in \nduration compared to day-time sleeps (yellow). \nDay- and night-time sleep durations become more \ndistinct from each other over time.", 
           hjust = "center", vjust = 'bottom')+
  
  annotate( geom = "curve", x = ymd_hms(20211218000000), y = 87, xend = ymd_hms(20211214000000), yend = 86, 
            curvature = .3, arrow = arrow(length = unit(1, "mm")), size=0.4) +
  annotate(geom = "text", x = ymd_hms(20211219000000), y = 87, size=2, 
           label = "Each blue stripe on the sun is a sleep. \nRings within the sun are 24-hr days \nfrom Aug 2021 (center) to Jan 2022 (perimeter).",
           hjust = "left")+
  
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background=element_rect(fill = "transparent"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.15, "cm"),ends = "both")),
        legend.position='none',
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) 
finalPLot

ggsave(file="sleepscape_abby.svg", plot=finalPLot, width=12, height=8)

