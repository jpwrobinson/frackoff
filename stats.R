library(tidyverse)
library(ggrepel)
library(funk)
theme_set(theme_sleek())



dat<-read.csv(file = 'frack-clean.csv')

l<-dat %>% filter(region == 'LANCASHIRE' & year == 2018)
print(paste(dim(l)[1], 'earthquakres in lancs in 2018'))
## lancashire =
## 1,490,500 people
## 3,075 km2
## 484	people per km2

# in 2018, that is 0.012 earthquakes per km^2, or 2.55 earthquakes per 100,000 people