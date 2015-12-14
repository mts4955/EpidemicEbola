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
    mainPanel(h1("Head"),
      tableOutput("head"),br(),
      h1("Tail"),
      tableOutput("tail"),br(),
      h1("Histogram"),
      plotOutput("histplot"),br(),
      h1("Boxplot"),
      plotOutput("boxplot")

)
)
)
server <- function(input, output, session) {
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
      head(filtered)
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
      head(filtered)
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
    
}

shinyApp(ui = ui, server = server)