library(tidyverse)
library(ggrepel)
library(funk)
theme_set(theme_sleek())

<<<<<<< HEAD
dat<-read.csv(file = 'frack-clean.csv')
=======
>>>>>>> da3a8d28051c7dfb95f30e7cd6a98aad4c8faa88


dat<-read.csv(file = 'frack-clean.csv')


t<- dat %>% filter(year > 1989) %>%
  group_by(year,  region) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(region) %>% mutate(qq=cumsum(q)) 


## y axis anchor
anchor <- expand.grid(region = unique(t$region), year = 1989, qq = 0, q = 0)
t <- rbind(anchor, data.frame(t))

labs <- t %>% filter(region != '') %>% group_by(region) %>% summarise(max = max(qq))

<<<<<<< HEAD
pdf(file = 'figs/cumulative-earthquakes.pdf', height=10)
theme_set(theme_minimal())
=======
pdf(file = 'figs/cumulative-earthquakes.pdf', height=10, width=8)
theme_set(theme_sleek())
>>>>>>> da3a8d28051c7dfb95f30e7cd6a98aad4c8faa88
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

<<<<<<< HEAD
dev.off()

#Insert a map as with https://earthquake.usgs.gov/research/induced/overview.php and have it so that whenever you 
#click on the line, it pops up on the map as well 
# https://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny 





=======
t %>% filter(year == 2018) %>% data.frame()
ranks %>% filter(region == 'LANCASHIRE') %>% data.frame()
>>>>>>> da3a8d28051c7dfb95f30e7cd6a98aad4c8faa88
