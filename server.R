library(shiny)
library(dplyr)
library(ggplot2)
mydata <- read.csv("preddata.csv")
library(sqldf)

function(input, output) {
  output$bar <- renderPlot({
  
    df = mydata%>%
      filter(Country == input$Country)
    df = data.frame(y = c(df$X2016, df$X2017), name = c('2016', '2017'))
    ggplot(df, aes(x=factor(name) ,y = y, fill = name)) + geom_bar(stat = "identity")
  }
  
  )
  
}
