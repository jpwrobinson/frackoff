library(tidyverse)
library(ggrepel)

dat<-read.csv(file = 'frack-clean.csv')

t<- dat %>% group_by(year, date.ym) %>%
  summarise(q = length(ML))

ggplot(t, aes(date.ym, q)) + 
  geom_point() + labs(y = 'Number of earthquakes per month')


t<- dat %>% group_by(year, date.ym, locality) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(year) %>%
  summarise(sd = sd(q), q = mean(q))

ggplot(t, aes(year, q)) + 
  geom_pointrange(aes(ymin = q - sd, ymax = q + sd))

t <- dat %>% filter(region == 'LANCASHIRE') %>%
  group_by(year, date.ym) %>%
  summarise(q = length(ML))

ggplot(t, aes(date.ym, q)) + 
  geom_point() + geom_line() + labs(y = 'Number of earthquakes per month', title='Lancashire')

ggplot(dat %>% filter(region == "LANCASHIRE"), aes(date, ML)) + geom_point()
t %>% data.frame()


t<- dat %>% group_by(year, date.ym, region) %>%
  summarise(q = length(ML)) %>% ungroup() %>%
  group_by(region) %>% mutate(qq=cumsum(q)) 

labs <- t %>% filter(region != '') %>% group_by(region) %>% summarise(max = max(qq))

theme_set(theme_minimal())
ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
  geom_line(size = 0.5, col='grey') +
  geom_line(data = t %>% filter(region == 'LANCASHIRE'), 
            aes(year, qq), col='red') +
  geom_text_repel(data=labs[labs$max > 50 & labs$region !='LANCASHIRE',], aes(2016, max, label = region), segment.color='transparent', force=0.25, size=3) +
  geom_text(data = labs[labs$region == 'LANCASHIRE',], aes(2016, max, label=region), col='red', size=3)


unique(dat$locality[dat$region==''])
t %>% filter(qq > 400 & region !='') %>% summarise(unique(region))



## how to subset for a meaningful comparison? (i.e. regions are different areas. Focus on England only? YES.)
## any other locations with fracking and earthquakes?

Blackpool major

per area, per capita...