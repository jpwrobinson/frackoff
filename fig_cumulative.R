library(tidyverse)
library(ggrepel)

dat<-read.csv(file = 'frack-clean.csv')


t<- dat %>% group_by(year, date.ym, region) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(region) %>% mutate(qq=cumsum(q)) 

labs <- t %>% filter(region != '') %>% group_by(region) %>% summarise(max = max(qq))

pdf(file = 'figs/cumulative-earthquakes.pdf', height=10)
theme_set(theme_minimal())
ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
  geom_line(size = 0.5, col='grey') +
  geom_line(data = t %>% filter(region == 'LANCASHIRE'), 
            aes(year, qq), col='red') +
  geom_text_repel(data=labs[labs$max > 50 & labs$region !='LANCASHIRE',], aes(2016, max, label = region), segment.color='transparent', force=0.25, size=3) +
  geom_text(data = labs[labs$region == 'LANCASHIRE',], aes(2016, max, label=region), col='red', size=3) +
  labs(y = 'Cumulative number of earthquakes', x ='') + 
  scale_x_continuous(breaks= seq(1988, 2018, 4))

dev.off()

#Insert a map as with https://earthquake.usgs.gov/research/induced/overview.php and have it so that whenever you 
#click on the line, it pops up on the map as well 
# https://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny 





