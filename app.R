library(shiny)
library(plyr)
library(tidyverse)
library(googlesheets)
library(shinythemes)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("Data Categories",
                           tabPanel("Dashboard",
                                    h3("Dashboard Overview"),
                                    h4("Welcome to the SLCO-REACH DataVis Dashboard"),
                      
                                    p("This dashboard is designed to allow you to explore the data related to the SLCO-REACH project. Click 
                                      on the Category Bar at the top of the screen to see different categories of data. Once you've
                                      found a plot you like, you can use its interactive features to explore your data. Double click a series
                                      on the legend to isolate the plot to that one data series!")
                                    ),
                                    
                           tabPanel("Program Overview",
                                    h3("Program Overview"),
                                    plotlyOutput("programOverviewPlot"),
                                    h3("Client Information"),
                                    h4("Age"),
                                    plotlyOutput("agesLinePlot"),
                                    h4("Race/Ethnicity"),
                                    plotlyOutput("raceLinePlot")
                                    ),
                           
                           tabPanel("Referrals and Randomization",
                                    h3("Referrals and Randomization"),
                                    h4("Randomized into REACH from Jail"),
                                    plotlyOutput("randomizedBarPlot"),
                                    h4("Days Between Randomization and Enrollment"),
                                    plotlyOutput("betweenEnrollmentdBarPlot"),
                                    h4("Contacts Between Randomization and Enrollment"),
                                    plotlyOutput("contactsBetweenEnrollmentdBarPlot"),
                                    h4("Number of REACH Assessments Conducted"),
                                    plotlyOutput("assessmentsBarPlot")
                                    ), 
                           tabPanel("Service Delivery",
                                    h3("Service Delivery"),
                                    h4("Number of Clients by Delivery Type"),
                                    plotlyOutput("serviceDeliveryLinePlot"),
                                    h4("Time Spent on Highest Needs of Client"),
                                    plotlyOutput("highestNeedBarPlot") 
                           ),
                           tabPanel("Employment",
                                   h3("Employment"),
                                   h4("Client Engagement"),
                                   plotlyOutput("employmentLinePlot"), 
                                   h4("Total Percent of Employment"),
                                   plotlyOutput("employmentBarPlot")
                           ),
                           tabPanel("Housing",
                                    h3("Housing"),
                                    h4("Client Numbers"),
                                    plotlyOutput("housingResidentLinePlot"), 
                                    h4("Average Length of Stay"),
                                    plotlyOutput("housingCapacityLinePlotLength"), 
                                    h4("Beds Days Available"),
                                    plotlyOutput("housingCapacityLinePlotBeds"), 
                                    h4("Bed Days Filled"),
                                    plotlyOutput("bedDaysLinePlot")
                           ),
                           tabPanel("SUD Treatment",
                                    h3("SUD Treatment"),
                                    h4("SUD Numbers"),
                                    plotlyOutput("SUDLinePlot"), 
                                    h4("SUD hourly breakdown"),
                                    plotlyOutput("SUDBarPlot"),
                                    h3("UA Treatment"),
                                    h4("UA Numbers"),
                                    plotlyOutput("UALinePlot"),
                                    h4("UA Breakdown"),
                                    plotlyOutput("UASLinePlot")
                                    
                           ),
                           tabPanel("Recidivism",
                                    h3("Recidivism"),
                                    h4("Engagements Number"),
                                    plotlyOutput("engagementsLinePlot"), 
                                    h4("Contacts to disengaged individuals"),
                                    plotlyOutput("engagementsMethodsLinePlot")
                           ),
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                    plotlyOutput("staffingLinePlot")
                           ),
                           tabPanel("Fidelity and Training",
                                    h3("Fidelity and Training"),
                                    plotlyOutput("fidelityScoreLinePlot") 
                           ),
                           tabPanel("Exits",
                                    h3("Exits"),
                                    h4("Number of Exits"),
                                    plotlyOutput("exitLinePlot"),
                                    h4("Overall Attrition"),
                                    plotlyOutput("exitAttritionLinePlot")
                           ),
                           tabPanel("Financial",
                                    h3("Financial Data"),
                                    plotlyOutput("financesLinePlot")
                           )
                           
                           ),
                
               HTML('<center><img src="sorenson_logo.png"></center>')
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  ax <- list(
    title = 'Month',
    zeroline = TRUE,
    showline = TRUE,
    zerolinewidth = 1,
    zerolinecolor = toRGB("white")
  )
  
  
  
  gap <- gs_title("REACH Service Provider Report_Updated_5.2018")
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
  tData <- tData[ ,-c(1, 9, 10, 14, 15, 20, 33, 38, 46, 53, 63, 69, 74, 80, 87, 98) ]
  
  xaxis <- rownames(tData)
  y2 <-  tData[,1]
  
  
  ### Plot Program Overview
  ## plot Number of individuals randomized into REACH this month via line graph
  months <- factor(xaxis,levels = c("January", "February", "March",  "April",   "May",  "June", "July", "August",  "September", "October", "November",  "December"))
  
  ## Plot Program Overview: 
  output$programOverviewPlot <- renderPlotly({programOverviewPlot <- plot_ly(x = months, y = strtoi(tData[,1]), name = 'Randomized', type = 'scatter', mode = 'lines+markers')  %>%
    #Plot Number of individuals referred to REACH this month
    add_trace(y = strtoi(tData[,2]), name = 'Referred', mode = 'lines+markers') %>%
    #Plot Number of new clients enrolled in REACH this month
    add_trace(y = strtoi(tData[,3]), name = 'New Clients', mode = 'lines+markers') %>%
    #Plot Number of REACH clients actively receiving services
    add_trace(y = strtoi(tData[,4]), name = 'Receiving Services', mode = 'lines+markers') %>%
    #Plot Total number of individuals enrolled in REACH 
    add_trace(y = strtoi(tData[,5]), name = 'Total Enrolled', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,9]), name = 'Completed REACH', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = ax)
  })
  
  # Client Information 
  # Plot Client Information as Line Graph
  output$agesLinePlot <- renderPlotly({agesLinePlot <- plot_ly(x = months, y = strtoi(tData[,11]), name = '18-25', type = 'scatter', mode = 'lines+markers')  %>%
    #Plot Number of individuals referred to REACH this month
    add_trace(y = strtoi(tData[,12]), name = '26-35', mode = 'lines+markers') %>%
    #Plot Number of new clients enrolled in REACH this month
    add_trace(y = strtoi(tData[,13]), name = '35-44', mode = 'lines+markers') %>%
    #Plot Number of REACH clients actively receiving services
    add_trace(y = strtoi(tData[,14]), name = '45+', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  #Plot Race as Line Graph
  output$raceLinePlot <- renderPlotly({ raceLinePlot <- plot_ly(x = months, y = strtoi(tData[,15]), name = 'American Indian', type = 'scatter', mode = 'lines+markers')  %>%
    #Plot Number of individuals referred to REACH this month
    add_trace(y = strtoi(tData[,16]), name = 'Asian', mode = 'lines+markers') %>%
    #Plot Number of new clients enrolled in REACH this month
    add_trace(y = strtoi(tData[,17]), name = 'Black/African American', mode = 'lines+markers') %>%
    #Plot Number of REACH clients actively receiving services
    add_trace(y = strtoi(tData[,18]), name = 'Black/African American, White', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,19]), name = 'Pacific Islander', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,20]), name = 'Other: Single race', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,21]), name = 'Other: Two or more races', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,22]), name = 'White', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,23]), name = 'Mexican', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,24]), name = 'Not of Hispanic Origin', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,25]), name = 'Other: Hispanic', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,26]), name = 'Puerto Rican', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  # Referrals and Randomization 
  output$randomizedBarPlot <- renderPlotly({randomizedBarPlot <- plot_ly(x = months, y = strtoi(tData[,27]), type = 'bar', name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Number of Individuals Randomized into REACH', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  output$betweenEnrollmentdBarPlot <- renderPlotly({betweenEnrollmentdBarPlot <- plot_ly(x = months, y = as.double(tData[,28]), type = 'bar', name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Avg. Days from Randomization to Enrollment'), xaxis = list(title = 'Month'))
  })
  
  output$contactsBetweenEnrollmentdBarPlot <- renderPlotly({contactsBetweenEnrollmentdBarPlot <- plot_ly(x = months, y = as.double(tData[,29]), type = 'bar', name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Avg. Contacts from Randomization to Enrollment', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$assessmentsBarPlot <- renderPlotly({assessmentsBarPlot <- plot_ly(x = months, y = strtoi(tData[,30]), type = 'bar', name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Assessments Conducted', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  # Service Delivery
  output$serviceDeliveryLinePlot <- renderPlotly({serviceDeliveryLinePlot <- plot_ly(x = months, y = strtoi(tData[,31]), name = 'Intensive Treatment', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,32]), name = 'Transition', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,33]), name = 'Sustained Recovery', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,34]), name = 'Long-term Recovery', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,35]), name = '200 Hours of Therapy', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,37]), name = 'Completed MRT', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Individuals Receiving', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  output$highestNeedBarPlot <- renderPlotly({ highestNeedBarPlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,36])), type = 'bar', name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = '% of Time Spent On Highest Priority', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  # Employment 
  output$employmentLinePlot <- renderPlotly({employmentLinePlot <- plot_ly(x = months, y = strtoi(tData[,38]), name = 'Completed Assessment', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,39]), name = 'Obtained Employment ', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,40]), name = 'Engaged With REACH Employment', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,41]), name = 'Obtained a Job with DWS', mode = 'lines+markers')%>% #could error with ?
    add_trace(y = strtoi(tData[,42]), name = 'Engaged with Vocational Training', mode = 'lines+markers')%>%
    add_trace(y = strtoi(tData[,44]), name = 'Lost Their Job', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$employmentBarPlot <- renderPlotly({employmentBarPlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,43])), type = 'bar', name = 'REACH Clients') %>%
    layout(yaxis = list(title = '% of REACH Clients Employed', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  
  # Housing
  output$housingResidentLinePlot <- renderPlotly({housingResidentLinePlot <- plot_ly(x = months, y = strtoi(tData[,45]), name = 'Completed Housing Assessments', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,45]), name = 'In Need of Residence', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,46]), name = 'Placed in REACH Recovery Residence', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,47]), name = 'Currently Housed in REACH Recovery', mode = 'lines+markers')%>% #could error with ?
    add_trace(y = strtoi(tData[,49]), name = 'Unique Clients served in REACH Recovery', mode = 'lines+markers')%>%
    layout(yaxis = list(title = 'Number of Clients', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
  })
  output$housingCapacityLinePlotLength <- renderPlotly({ housingCapacityLinePlot <- plot_ly(x = months, y = strtoi(tData[,48]), name = 'Average Length of Stay', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Days', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$housingCapacityLinePlotBeds <- renderPlotly({ housingCapacityLinePlot <- plot_ly(x = months, y = strtoi(tData[,51]), name = 'Beds Available', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Bed Days', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$bedDaysLinePlot <- renderPlotly({bedDaysLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,50])), name = 'In Residence', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = as.numeric(sub("%", "", tData[,51])), name = 'By Transitional', mode = 'lines+markers') %>%
    layout(yaxis = list(title = '% of Bed Days Filled', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  # SUD treatment
  output$SUDLinePlot <- renderPlotly({ SUDLinePlot <- plot_ly(x = months, y = strtoi(tData[,54]), name = 'SUD', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  output$UALinePlot <- renderPlotly({ SUDLinePlot <- plot_ly(x = months, y = strtoi(tData[,55]), name = 'UA', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  output$UASLinePlot <- renderPlotly({UASLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,56])), name = 'Positive', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = as.numeric(sub("%", "", tData[,57])), name = 'No-show', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  output$SUDBarPlot <- renderPlotly({SUDBarPlot <- plot_ly(x = months, y = as.double(sub("%", "", tData[,58]))/100, type = 'bar', name = 'REACH Clients') %>% #divide by 100 as hours are entered as a percentage
    layout(yaxis = list(title = 'Average Number of Hours Per Client', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  # Recidivism 
  output$engagementsLinePlot <- renderPlotly({engagementsLinePlot <- plot_ly(x = months, y = strtoi(tData[,59]), name = 'Post-Incarceration Re-engagements', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,55]), name = 'Successful Re-engagements', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,55]), name = 'Left Unsuccessfully', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Number Completed', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$engagementsMethodsLinePlot <- renderPlotly({engagementsMethodsLinePlot <- plot_ly(x = months, y = as.double(tData[,60]), name = 'Avg. Days Between Jail and Re-enrollment', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = as.double(tData[,61]), name = 'Contact Attempts', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Number', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  #Staffing 
  output$staffingLinePlot <- renderPlotly({staffingLinePlot <- plot_ly(x = months, y = strtoi(tData[,64]), name = 'Case Managers', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,65]), name = 'Mentors', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,66]), name = 'Program Managers', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,67]), name = 'Admission Coordinators', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,68]), name = 'Therapists', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Number on Staff', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  #Fidelity 
  output$fidelityScoreLinePlot <- renderPlotly({fidelityScoreLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,69])), name = 'Staff Trained In Modalities', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = as.numeric(sub("%", "", tData[,70])), name = 'MRI groups with Supervision', mode = 'lines+markers') %>%
    add_trace(y = as.numeric(sub("%", "", tData[,71])), name = 'Clinicians Receiving Fidelity Checks', mode = 'lines+markers') %>%
    add_trace(y = as.numeric(sub("%", "", tData[,72])), name = 'Fidelity Score for MRT', mode = 'lines+markers') %>%
    add_trace(y = as.numeric(sub("%", "", tData[,73])), name = 'Fidelity Score for MI', mode = 'lines+markers') %>%
    add_trace(y = as.numeric(sub("%", "", tData[,74])), name = 'Fidelity Score for TA', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })

  #Exits 
  output$exitLinePlot <- renderPlotly({exitLinePlot <- plot_ly(x = months, y = strtoi(tData[,75]), name = 'Total Unplanned Exits', type = 'scatter', mode = 'lines+markers')  %>%
    add_trace(y = strtoi(tData[,76]), name = 'Jail', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,77]), name = 'Prison', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,78]), name = 'Self Termination', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,79]), name = 'No Contact', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,80]), name = 'Total Terminated by FSH', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,81]), name = 'Deceased', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,82]), name = 'Transfered Programs', mode = 'lines+markers') %>%
    add_trace(y = strtoi(tData[,84]), name = 'Planned Exits', mode = 'lines+markers') %>%
    layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  output$exitAttritionLinePlot <- renderPlotly({exitAttritionLinePlot <- plot_ly(x = months, y = as.numeric(sub("%", "", tData[,83])), name = 'Attrition', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Percent (%)', rangemode = "tozero"), xaxis = list(title = 'Month'))
  })
  
  #Finances
  output$financesLinePlot <- renderPlotly({financesLinePlot <- plot_ly(x = months, y = as.double(tData[,85]), name = 'Finances', type = 'scatter', mode = 'lines+markers')  %>%
    layout(yaxis = list(title = 'Dollars ($)', rangemode = "tozero"), xaxis = list(title = 'Month', rangemode = "tozero"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)