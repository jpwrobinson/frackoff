library(tidyverse)
library(ggrepel)
library(funk)
library(here)

setwd(here('frackoff'))

dat<-read.csv(file = 'frack-clean.csv')


t<- dat %>% filter(year > 1989) %>%
  group_by(year,  region) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(region) %>% mutate(qq=cumsum(q)) 


## y axis anchor
anchor <- expand.grid(region = unique(t$region), year = 1989, qq = 0, q = 0)
t <- rbind(anchor, data.frame(t))

labs <- t %>% filter(region != '') %>% group_by(region) %>% summarise(max = max(qq))


pdf(file = 'figs/cumulative-earthquakes.pdf', height=10, width=8)
theme_set(theme_sleek())

ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
  geom_line(size = 0.5, col='grey') +
  geom_line(data = t %>% filter(region == 'LANCASHIRE'), 
            aes(year, qq), col='red') +
  geom_text_repel(data=labs[labs$max > 50 & labs$region !='LANCASHIRE',], aes(2016, max, label = region), segment.color='transparent', force=0.25, size=3) +
  geom_text(data = labs[labs$region == 'LANCASHIRE',], aes(2016, max, label=region), col='red', size=3) +
  labs(y = 'Cumulative number of earthquakes', x ='') + 
  scale_x_continuous(breaks= seq(1990, 2019, 4))

dev.off()


## what are ranks?
ranks<-t %>% group_by(year) %>% mutate(rank = dense_rank(desc(qq))) 
t %>% filter(year == 2018) %>% data.frame()
ranks %>% filter(region == 'LANCASHIRE') %>% data.frame()


#Insert a map as with https://earthquake.usgs.gov/research/induced/overview.php and have it so that whenever you 
#click on the line, it pops up on the map as well 
# https://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny 


### correct so that all counties are the same as Lancashire
area<-read.csv('wiki-counties.csv', stringsAsFactors = FALSE)
area$area_km2<-as.character(area$area_km2)
area$area_km2<-gsub(',', '', area$area_km2)
area$area_km2<-as.numeric(area$area_km2)

## fix some areas
iom<-data.frame(county = 'Isle of Man', population = NA, rank_pop = NA, area_km2=572, area_sqmi = NA, rank_area = NA, people_mk2 = NA, rank_people=NA)

h.w<-area[area$county == 'Herefordshire',4] + area[area$county == 'Worcestershire',4]
hw<-data.frame(county = 'Herefordshire & Worcestershire', population = NA, rank_pop = NA, area_km2=h.w, area_sqmi = NA, rank_area = NA, people_mk2 = NA, rank_people=NA)

area<-rbind(area, iom, hw)

area$area_km2[area$county == 'Lancashire'] ## 3075

area$multiplier<-area$area_km2 / 3075
area$countycaps<-toupper(area$county)

dat$multiplier<-area$multiplier[match(dat$region, area$countycaps)]


t<- dat %>% filter(year > 1989) %>%
  group_by(year,  region, multiplier) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(region, multiplier) %>% mutate(qq=cumsum(q)*multiplier) 


## y axis anchor
anchor <- expand.grid( year = 1989,region = unique(t$region),multiplier=0, qq = 0, q = 0)
t <- rbind(anchor, data.frame(t))

labs <- t %>% filter(region != '') %>% group_by(region) %>% summarise(max = max(qq))

pdf(file = 'figs/cumulative-earthquakesbyarea.pdf', height=10, width=8)
theme_set(theme_sleek())

ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
  geom_line(size = 0.5, col='grey') +
  geom_line(data = t %>% filter(region == 'LANCASHIRE'), 
            aes(year, qq), col='red') +
  geom_text_repel(data=labs[labs$max > 50 & labs$region !='LANCASHIRE',], aes(2016, max, label = region), segment.color='transparent', force=0.25, size=3) +
  geom_text(data = labs[labs$region == 'LANCASHIRE',], aes(2016, max, label=region), col='red', size=3) +
  labs(y = expression(paste('Cumulative number of earthquakes for an area the size of Lancashire (3,075 km'^2, ')')), x ='') + 
  scale_x_continuous(breaks= seq(1990, 2019, 4))

dev.off()

