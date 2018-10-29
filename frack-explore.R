library(tidyverse)

dat<-read.csv('frackoff/frack-dat-28-10-2018.csv')


dat$date<-as.character(dat$date)
dat$month<-str_split_fixed(dat$date, '/', 3)[,2]
dat$year<-str_split_fixed(dat$date, '/', 3)[,3]
dat$date.ym<-with(dat, paste('01', month, year, sep = '-'))
dat$date.ym<-as.Date(dat$date.ym, format='%d-%m-%Y')
dat$date<-as.Date(dat$date, format='%d/%m/%Y')

dat$locality<-str_replace_all(dat$locality, '\xca', '')
dat$region<-str_replace_all(dat$region, '\xca', '')

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

theme_set(theme_minimal())
ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
  geom_line(size = 0.5, col='grey') +
  geom_line(data = t %>% filter(region == 'LANCASHIRE'), 
            aes(year, qq), col='red')

unique(dat$locality[dat$region==''])
t %>% filter(qq > 400 & region !='') %>% summarise(unique(region))


## how to subset for a meaningful comparison? (i.e. regions are different areas. Focus on England only? YES.)
## any other locations with fracking and earthquakes?