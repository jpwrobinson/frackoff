library(shiny)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(funk)
library(here)
library(rsconnect)
theme_set(theme_bw())

## Deployment info:
## http://shiny.rstudio.com/articles/shinyapps.html

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


#### selects one graph at a time and highlights in red
ui <- fluidPage(
              selectInput("region", "region", choices = unique(t$region)),
              plotOutput("outplot"))
            
            
  
server <- function(input, output) {
  
  output$outplot <- renderPlot({
    ggplot(t %>% filter(region != ''), aes(year, qq, group=region)) + 
      geom_line(size = 0.5, col='grey') +
      geom_line(data = t[t$region == input$region, ],
                aes(year, qq), col='red') +
      geom_text(data = labs[labs$region == input$region,], aes(2016, max, label=region), col='red', size=3) +
      labs(y = 'Cumulative number of earthquakes', x ='') + 
      scale_x_continuous(breaks= seq(1988, 2018, 4))
  })
  

}


shinyApp(ui = ui, server = server)


