# Load Packages
library(googlesheets)
library(plotly)
library(shiny)
library(shinythemes)
library(siverse) 
library(tidyverse)

# You can download siverse by running the code below. You might not need it but it comes in handy. 
# devtools::install_github("Sorenson-Impact/sorensonimpact")
# devtools::install_github("Sorenson-Impact/siverse")

# Define UI--------------------------------------------------------
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("SLCo HNJ Data Dashboard",
                           tabPanel("Dashboard",
                                    h3("Dashboard Overview"),
                                      p("Welome to the HNJ Dashboard. This dashboard is designed to allow you to explore the data related to the SLCO-HNJ project. Click 
                                      on the Category Bar at the top of the screen to see different categories of data. Once you've
                                      found a plot you like, you can use its interactive features to explore your data. Double click a series
                                      on the legend to isolate the plot to that one data series."),
                                      p("You may also select the unit of time for which you'd like to view the data: months, quarters, or years."),
                                      selectInput(inputId = "timeunit", 
                                                  label = "Time Unit:",
                                                  c("Months" = "months",
                                                    "Quarters" = "quarters",
                                                    "Years" = "years"))
                           ),       
                           tabPanel("Program Overview",
                                    h3("Program Overview"),
                                        plotlyOutput("POplot")
                           ),       
                           tabPanel("Client Demographics",        
                                    h3("Client Demographics"),
                                      h4("Gender"),
                                        plotlyOutput("GenderPlot"),
                                      h4("Age"),
                                        plotlyOutput("AgesLinePlot"),
                                      h4("Race/Ethnicity"),
                                        plotlyOutput("RaceLinePlot")
                           ),       
                           tabPanel("Referrals and Enrollments",
                                    h3("Referrals and Enrollments"),
                                      h4("Eligible per HMIS Data Pull"),
                                        plotlyOutput("HMISplot"),
                                      h4("TRH Location Endeavors"),
                                        plotlyOutput("TRHplot"),
                                      h4("Prescreen Eligibility"),
                                        plotlyOutput("PrescreenPlot")
                           ),       
                           tabPanel("Housing Placement and Services",
                                    h3("Housing Placement and Services"),
                                      h4("Roommate Placement Assessments Conducted"),
                                        plotlyOutput("RPACplot"),
                                      h4("New Placements into Housing"),
                                        plotlyOutput("NPiHplot"),
                                      h4("Awaiting Housing"),
                                        plotlyOutput("AHplot")
                           ),       
                           tabPanel("Behavioral Health",
                                    h3("Behavioral Health"),
                                      h4("Referrals"),
                                        plotlyOutput("ReferralsPlot"), 
                                      h4("HNJ Clinician Statistics"),
                                        plotlyOutput("CSplot"),
                                      h4("Enrolled in Additional Community Services"),
                                        plotlyOutput("EiACSplot")
                           ),         
                           tabPanel("Employment",
                                    h3("Employment"),
                                        plotlyOutput("EmploymentPlot")
                           ),         
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                      h4("Staff to Client Ratio"),
                                        plotlyOutput("StCRplot"),
                                      h4("Staffing Positions"),
                                        plotlyOutput("SPplot")
                           ),       
                           tabPanel("Exits",
                                    h3("Exits"),
                                      h4("Clients that Lost Housing"),
                                        plotlyOutput("CtLHplot"),
                                      h4("Unplanned Exits"),
                                        plotlyOutput("UEtMplot"),
                                      h4("Planned Graduations"),
                                        plotlyOutput("PGplot")
                           )
                ),

                HTML('<center><img src="footer.jpg"></center>')
)
# Define Server --------------------------------------------------------
server <- function(input, output) {
  ax <- list(
    title = 'Month',
    zeroline = TRUE,
    showline = TRUE,
    zerolinewidth = 1,
    zerolinecolor = toRGB("white")
  )
  
  # Wrangling ------------------------------------------------------
  
  sheet_hnj <- gs_title("HNJ Service Provider Report_Updated")
  hnj19_og <- sheet_hnj %>%
    gs_read(ws = "2019-Present", check.names = FALSE) #check.name = false so as not to add on _1 _2 _3 etc after month names as the spreadsheet grows
  hnjpre19_og <- sheet_hnj %>% 
    gs_read(ws= "Pre-2019", check.names = FALSE) #check.name = false so as not to add on _1 _2 _3 etc after month names as the spreadsheet grows
  
  hnj <- bind_cols(hnjpre19_og, hnj19_og) %>% #binds the two spreadsheets together based on row position only. 
    filter_at(.vars = "8881", any_vars(grepl(x = ., pattern = "UCJC") | is.na(.))) %>% # filters out section headings and the rows (ie. metrics) 
    # which are no longer tracked in the updated spreadsheet. Note: the rows still exist in the spread sheet but are hidden. 
    select(-c("3", "Metric", "8881", "YTD Totals Pre-2019"), 
           "Pre 2019" = `Yearly totals to date`) %>% 
    select("Metric" = Metric1, everything()) %>% # the two select calls eliminate duplicate columns after the bind() and renames other columns.
    filter(Metric %ni% c("Client Gender","Client Age", "Client Race", "Client Ethnicity", "Rental Subsidies"))
  
  # This little section transposes the data, puts the months into rows and vars into columns
  hnj <- t(hnj)
  colnames(hnj) <- hnj[1,]
  hnj <- hnj[-1,]
  hnj <- as_tibble(hnj, rownames = "month") %>% 
    mutate_at(.vars = vars(month), 
              .funs = funs(str_replace_all(string = ., pattern = "_.*|(?<=May)1", replacement = ""))) #trims the suffix for making col names unique, preps the vector the mutate below
  
  # This makes month values indicative of what year they fall in  
  hnj <- hnj %>%
    as_tibble() %>%
    mutate(rnum_pre19 = charmatch("Pre 2019", month), # creates anchor values for case when below
           rnum_19 = match("2019", month),
           rnum_20 = match("2020", month),
           rnum_21 = match("2021", month),
           rnum_22 = match("2022", month)) %>%
    mutate_at(.vars = vars(month), # adds year to month
              .funs = funs(case_when(
                # < 5 is arbitrary but its used so as not to conflict with the nov. and dec. in 2018.
                # I use .* in the grepl incase the months become abreviated in the spreadsheet
                row_number() < 5 & grepl("Nov.*|Dec.*|Q4.*", .) ~ paste0(., "'17"), 
                row_number() < rnum_pre19 ~ paste0(., "'18"),
                row_number() == rnum_pre19 ~ paste0(., " Total"),
                row_number() < rnum_19 ~ paste0(., "'19"),
                row_number() == rnum_19 ~ paste0(., " Total"),
                row_number() < rnum_20 ~ paste0(., " '20"),
                row_number() == rnum_20 ~ paste0(., " Total"),
                row_number() < rnum_21 ~ paste0(., "'21"),
                row_number() == rnum_21 ~ paste0(., " Total"),
                TRUE ~ as.character(.)
              ))) %>%
    select(-contains("rnum_")) # discards anchor value columns because they aren't needed
  
  # Seperates the monthly, quarter, and yearly totals into
  # different columns for easy plotting later on.
  # The idea is that we can use these cols for plot x axises 
  hnj <- hnj %>% 
    tibble::add_column(quarter = NA, .before = "month") %>% # quarters
    mutate(quarter = case_when( 
      grepl("Q[[:digit:]]", month) ~ identity(month), 
      TRUE ~ NA_character_)
    ) %>% 
    tibble::add_column(year = NA, .before = "quarter") %>% # years
    mutate(year = case_when(
      grepl("[[:digit:]]{4}|Running Total", month) ~ identity(month), 
      TRUE ~ NA_character_)
    ) %>% 
    mutate(month = case_when( # months
      grepl("[[:digit:]]{4}|Q[[:digit:]]|Running Total", month) ~ NA_character_, 
      TRUE ~ identity(month)
    )
    ) 
  
  # Moves the month, quarter, and year to the end of the df. 
  hnj <- hnj %>% 
    select(-month, month) %>% 
    select(-quarter, quarter) %>% 
    select(-year, year)
  
  # Makes nearly all columns in integer vectors so that it doesnt have to be done on every plot call
  hnj <- hnj %>% 
    mutate_at(.vars = vars(-c("month", "year", "quarter")),
              .funs = funs(as.integer(.)))
  
  # standard cleaning, left towards the end so that months are still capatalized during wrangling
  hnj <- hnj %>% clean_names()
  
  months <- as_factor(hnj$month)
  quarters <- as_factor(hnj$quarter)
  years <- as_factor(hnj$year)
  
  
  timeunit <- reactive({
    if (input$timeunit == "months") {
      months
    } else if (input$timeunit == "quarters") {
      quarters
    } else if (input$timeunit == "years") {
      years
    }
  })
    ### Creating new Columns for Staffing Tab
    # staff <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 7)
    # hnj$Staff <- staff
    # clients <- c(NA, 61, 67, 67, 67, 95, 103, 113, 127, NA)
    # hnj$Clients <- clients
    # PositionsAvailable <- c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    # hnj$PositionsAvailable <- PositionsAvailable
    # PositionsFilled <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 8)
    # hnj$PositionsFilled <- PositionsFilled
    
 ## Graphics --------------------------------------------------------
    ### Program Overview
      #### Program Overview
  output$POplot <- renderPlotly({POplot <- plot_ly(
    x = timeunit(), 
    y = hnj$number_of_new_clients_enrolled_in_hnj_this_month, 
    name = 'New Clients', 
    type = 'scatter', 
    mode = 'lines+markers', 
    connectgaps = TRUE)  %>%
    add_trace(y = hnj$total_number_of_clients_currently_awaiting_housing_placement, 
              name = 'Total Clients', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = hnj$total_number_of_housed_clients_to_date, 
              name = 'Clients Housed', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = hnj$total_number_of_clients_graduated_to_a_permanent_location_to_date, 
              name = 'Clients Permanently Housed', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Individuals', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
    ### Client Demographics --------------------------------------------------------
      #### Gender
      output$GenderPlot <- renderPlotly({GenderPlot <- plot_ly(x = months, y = hnj[ ,5], name = 'Male', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = hnj[ ,6], name = 'Female', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Age 
      output$AgesLinePlot <- renderPlotly({AgesLinePlot <- plot_ly(x = months, y = hnj[ ,9], name = '26-35', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = hnj[ ,10], name = '36-45', mode = 'lines+markers')%>%
        add_trace(y = hnj[ ,11], name = '45+', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Race
      output$RaceLinePlot <- renderPlotly({ RaceLinePlot <- plot_ly(x = months, y = hnj[ ,12], name = 'American Indian', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,13]), name = 'Asian', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,14]), name = 'Pacific Islander', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,15]), name = 'Black/African American', mode = 'lines+markers')%>%
        add_trace(y = strtoi(hnj[,16]), name = 'White', mode = 'lines+markers')%>%
        add_trace(y = strtoi(hnj[,17]), name = 'Other: Two or more races', mode = 'lines+markers')%>%
        add_trace(y = strtoi(hnj[,20]), name = 'Hispanic', mode = 'lines+markers')%>%
        add_trace(y = strtoi(hnj[,21]), name = 'Other: Non-Hispanic', mode = 'lines+markers')%>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
    ### Referrals and Enrollment--------------------------------------------------------
      #### Eligible per HMIS Data Pull
      output$HMISplot <- renderPlotly({HMISplot <- plot_ly(x = months, y = strtoi(hnj[,23]), type = 'bar', name = 'Eligible per HMIS Data Pull') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### TRH Location Endeavors
      output$TRHplot <- renderPlotly({TRHplot <- plot_ly(x = months, y = strtoi(hnj[ ,24]), name = 'Attempted to Locate', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[ ,25]), name = 'Located and Prescreened', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
      #### Prescreen Eligibility
      output$PrescreenPlot <- renderPlotly({PrescreenPlot <- plot_ly(x = months, y = strtoi(hnj[ ,33]), name = 'Eligible after Prescreen', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[ ,34]), name = 'Ineligible after Prescreen', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
    ### Housing Placement and Services --------------------------------------------------------
      #### Roommate Placement Assessments Conducted
      output$RPACplot <- renderPlotly({RPACplot <- plot_ly(x = months, y = strtoi(hnj[,49]), type = 'bar', name = 'Roommate Placement Assessments Conducted')  %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### New Placements into Housing
      output$NPiHplot <- renderPlotly({NPiHplot <- plot_ly(x = months, y = strtoi(hnj[ ,50]), name = 'Total', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[ ,51]), name = 'Single Occupancy Housing', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[ ,52]), name = 'Roommate Housing', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
      #### Awaiting Housing
      output$AHplot <- renderPlotly({AHplot <- plot_ly(x = months, y = strtoi(hnj[ ,53]), name = 'Awaiting Housing Placement', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[ ,54]), name = 'Not Housed within Three Months of Enrollment', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals'), xaxis = list(title = 'Month'))
      })
    ### Behavioral Health --------------------------------------------------------
      #### Referrals
      output$ReferralsPlot <- renderPlotly({ReferralsPlot <- plot_ly(x = months, y = strtoi(hnj[,60]), name = 'Referred to BH Treatment', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,62]), name = 'Referred to BH Treatment after HNJ Assessment ', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### HNJ Clinician Statistics
      output$CSplot <- renderPlotly({CSplot <- plot_ly(x = months, y = strtoi(hnj[,61]), name = 'HNJ Health Assessments Conducted', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,63]), name = 'Met with HNJ Clinician', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Enrolled in Additional Community Services
      output$EiACSplot <- renderPlotly({EiACSplot <- plot_ly(x = months, y = strtoi(hnj[,64]), name = 'Total', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,65]), name = 'SUD', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,66]), name = 'Behavioral', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
    ### Employment
      #### Employment  
      output$EmploymentPlot <- renderPlotly({EmploymentPlot <- plot_ly(x = months, y = strtoi(hnj[,67]), name = 'Employment Assessments Conducted', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,68]), name = 'Enrolled in Employment Services', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,69]), name = 'Participants with Source of Income', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
    ### Staffing
      #### Staff to Client Ratio
      output$StCRplot <- renderPlotly({StCRplot <- plot_ly(x = months, y = strtoi(hnj[,84]), name = 'Staff', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,85]), name = 'Clients', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
      #### Staffing Positions
      output$SPplot <- renderPlotly({SPplot <- plot_ly(x = months, y = strtoi(hnj[,86]), name = 'Positions Available', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,87]), name = 'Positions Filled', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month', ax))
      })
    ### Exits
      #### Clients that Lost Housing this Month
      output$CtLHplot <- renderPlotly({CtLHplot <- plot_ly(x = months, y = strtoi(hnj[,72]), name = 'Total', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,73]), name = 'Eviction', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,74]), name = 'Other', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Unplanned Exits this Month
      output$UEtMplot <- renderPlotly({UEtMplot <- plot_ly(x = months, y = strtoi(hnj[75]), name = 'Total Unplanned Exits', type = 'scatter', mode = 'lines+markers')  %>%
        add_trace(y = strtoi(hnj[,76]), name = 'Jail/Prison', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,77]), name = 'Self-Termination', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,78]), name = 'Left without Contact', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,79]), name = 'Deceased', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,80]), name = 'Transfer to Another Program', mode = 'lines+markers') %>%
        add_trace(y = strtoi(hnj[,81]), name = 'Other', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
      #### Planned Graduations
      output$PGplot <- renderPlotly({PGplot <- plot_ly(x = months, y = strtoi(hnj[,82]), type = 'bar', name = 'Planned Graduations') %>%
        layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), xaxis = list(title = 'Month'))
      })
}
# Run App
shinyApp(ui, server)