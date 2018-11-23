library(shiny)
library(ggplot2)



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


