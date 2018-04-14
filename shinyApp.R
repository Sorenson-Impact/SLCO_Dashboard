library(shiny)
library(plyr)
library(tidyverse)
library(googlesheets)
library(shinythemes)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                
                # Application title
                titlePanel("Sorenson SLCO"),
                
                # Sidebar with a slider input for number of bins 
                  # Show a plot of the generated distribution
                plotlyOutput("plot"))

# Define server logic required to draw a histogram
server <- function(input, output) {
  gap <- gs_title("Test Copy of REACH Data Request_Updated_2018")
  myData <- gap %>%
    gs_read()
  ## Wrangling: 
  
  #transpose the data to put observations into rows 
  tData <- t(myData)
  
  #make column names the names of the first row
  colnames(tData) = tData[1, ] # assigns column names from the first row
  tData = tData[-1, ] # removes the first row from the data 
  
  #make the row names the names of the first column 
  rownames(tData) <-tData[ ,1] # assigns column names from the first row
  tData <-  tData[, -1] # removes the first row from the data 
  
  # remove the 'totals' month
  tData <- tData[-c(4, 8, 12, 16, 17), ]
  
  #remove the 'header' columns 
  tData <- tData[ ,-c(1, 11, 17, 30, 35, 43, 51, 61, 67, 73, 79, 86, 97) ]
  
  xaxis <- rownames(tData)
  
  months <- factor(xaxis,levels = c("January", "February", "March",  "April",   "May",  "June", "July", "August",  "September", "October", "November",  "December"))
  
  ## Plot Program Overview: 
  output$plot <- renderPlotly({plot_ly(x = months, y = strtoi(tData[,1]), name = 'Randomized', type = 'scatter', mode = 'lines+markers')  %>%
    #Plot Number of individuals referred to REACH this month
    add_trace(y = strtoi(tData[,2]), name = 'Referred', mode = 'lines+markers') %>%
    #Plot Number of new clients enrolled in REACH this month
    add_trace(y = strtoi(tData[,3]), name = 'New Clients', mode = 'lines+markers') %>%
    #Plot Number of REACH clients actively receiving services
    add_trace(y = strtoi(tData[,4]), name = 'Receiving Services', mode = 'lines+markers') %>%
    #Plot Total number of individuals enrolled in REACH 
    add_trace(y = strtoi(tData[,5]), name = 'Total Enrolled', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,9]), name = 'Total Enrolled', mode = 'lines+markers')
  })
}


# Run the application 
shinyApp(ui = ui, server = server)