library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
eboladat<-read.csv("ebola_data_db_format.csv", as.is = c(3))
eboladat1<-eboladat
eboladat1$Date<-ymd(eboladat$Date)
ui <- fluidPage(
  titlePanel("Ebola Database Explorer", windowTitle = "Ebola Database Explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("countryInput", "Country",choices = c("Liberia", "Guinea", "Sierra Leone", "All Countries"),selected = "Liberia"),
      selectInput("indicatorInput", "Ebola Indicator",choices = levels(eboladat$Indicator))
    ),
    mainPanel(
      h1("Histogram"),
      plotOutput("histplot"),br(),
      h1("Boxplot"),
      plotOutput("boxplot"),br(),
      h1("Trend"),
      plotOutput("trend"),br(),
      h1("Head"),
      tableOutput("head"),br(),
      h1("Tail"),
      tableOutput("tail"),br(),
      h1("Summary"),
      verbatimTextOutput("summary"), width = 5
)
)
)

server <- function(input, output, session) {
  output$trend<- renderPlot({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat1 %>% 
        filter(Indicator == input$indicatorInput, Country == "Guinea"|Country == "Sierra Leone"| Country == "Liberia")
      ggplot(data=filtered,aes(x=Date, y=value, colour=Country)) +geom_line(size = 1)+ labs(title=input$indicatorInput)
      
    }
    else{
      filtered <- eboladat1 %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      ggplot(data=filtered,aes(x=Date, y=value, colour=Country)) +geom_line(size = 1)+ labs(title=input$indicatorInput)
    }  
    
  })
  output$summary<- renderPrint({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat %>% 
        filter(Indicator == input$indicatorInput)
      print(summary(filtered$value))
      #ggplot(filtered, aes(value))+geom_histogram()
      
    }
    else{
      filtered <- eboladat %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      print(summary(filtered$value))
      #ggplot(filtered, aes(value))+geom_histogram()
    }  
  })
  
  
  
  output$head<- renderTable({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat %>% 
        filter(Indicator == input$indicatorInput)
      head(filtered)
      #ggplot(filtered, aes(value))+geom_histogram()
      
    }
    else{
      filtered <- eboladat %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      tail(filtered)
      #ggplot(filtered, aes(value))+geom_histogram()
    }
  })
  output$tail<- renderTable({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat %>% 
        filter(Indicator == input$indicatorInput)
      tail(filtered)
      #ggplot(filtered, aes(value))+geom_histogram()
      
    }
    else{
      filtered <- eboladat %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      tail(filtered)
      #ggplot(filtered, aes(value))+geom_histogram()
    }
  })
  output$histplot <- renderPlot({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat1 %>% 
        filter(Indicator == input$indicatorInput)
      hist(filtered$value, col = "grey", xlab = "Value", main = input$indicatorInput)
      #ggplot(filtered, aes(value))+geom_histogram()
      
    }
    else{
      filtered <- eboladat1 %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      hist(filtered$value, col = "grey", xlab = "Value", main = input$indicatorInput)
      #ggplot(filtered, aes(value))+geom_histogram()
    }
  })
  output$boxplot <- renderPlot({
    if (input$countryInput == "All Countries"){
      filtered <- eboladat1 %>% 
        filter(Indicator == input$indicatorInput)
      boxplot(filtered$value~filtered$Country, col = "grey", main = input$indicatorInput)
      #ggplot(filtered, aes(value))+geom_histogram()
      
    }
    else{
      filtered <- eboladat1 %>% 
        filter(Country == input$countryInput, Indicator == input$indicatorInput)
      boxplot(filtered$value, col = "grey", main = input$indicatorInput)
      #ggplot(filtered, aes(value))+geom_histogram()
    }
  })
    
}

shinyApp(ui = ui, server = server)