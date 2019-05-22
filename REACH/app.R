library(shiny)
library(plyr)
library(tidyverse)
library(googlesheets)
library(shinythemes)
library(plotly)

# THINGS TO DO: 
# CHANGE COLUMN VALUES FOR ESSENTIALLY THE ENTIRE DF 
# FROM CHARACTERS INTO INTEGERS SO THAT PLOTLY WILL PLOT THE Y AXIS CORRECTLY


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("SLCo PFS: REACH Data Dashboard",
                           tabPanel("Dashboard",
                                    h3("Dashboard Overview"),
                                    h4("Welcome to the Salt Lake County PFS-REACH DataVis Dashboard"),
                      
                                    p("This dashboard is designed to allow you to explore the data related to the SLCo-REACH project. Click 
                                      on the Category Bar at the top of the screen to see different categories of data. Once you've
                                      found a plot you like, you can use its interactive features to explore your data. Double click a series
                                      on the legend to isolate the plot to that one data series!"),
                                    p("You may also select the unit of time for which you'd like to view the data: months, quarters, or years."),
                                    selectInput(inputId = "timeunit", 
                                                label = "Time Unit:",
                                                c("Months" = "months",
                                                  "Quarters" = "quarters",
                                                  "Years" = "years")
                                    )
                                    ),
                                    
                           tabPanel("Program Overview",
                                    h3("Program Overview"),
                                    h4("Program Overview Percentages"),
                                    plotlyOutput("enrollmentpercentPlot"),
                                    h4("Program Overview Raw Counts"),
                                    plotlyOutput("programOverviewPlot"),
                                    h4("Failures to Enroll"),
                                    plotlyOutput("enrollmentfailureslinePlot"),
                                    h4("Disengagements"),
                                    plotlyOutput("disengagementslinePlot"),
                                    h3("Client Demographics (of new enrollees)"),
                                    h4("Age"),
                                    plotlyOutput("agesLinePlot"),
                                    h4("Race"),
                                    plotlyOutput("raceLinePlot"),
                                    h4("Ethnicity"),
                                    plotlyOutput("ethnicitylinePlot")
                                    ),
                           
                           tabPanel("Referrals and Randomization",
                                    h3("Referrals and Randomization"),
                                    h4("Randomized into REACH from Jail"),
                                    plotlyOutput("randomizedBarPlot"),
                                    h4("Average Number of Days Between Randomization and Enrollment"),
                                    plotlyOutput("betweenEnrollmentdBarPlot"),
                                    h4("Average Number of Contacts Between Randomization and Enrollment"),
                                    plotlyOutput("contactsBetweenEnrollmentdBarPlot"),
                                    h4("Number of Intake Assessments Conducted"),
                                    plotlyOutput("assessmentsBarPlot")
                                    ), 
                           tabPanel("Service Delivery",
                                    h3("Service Delivery"),
                                    h4("Number of Clients by Delivery Type"),
                                    plotlyOutput("serviceDeliveryLinePlot")
                                  
                           ),
                           tabPanel("Employment",
                                   h3("Employment"),
                                   h4("Percent of All REACH Clients Employed"),
                                   plotlyOutput("employmentBarPlot"),
                                   h4("Number of Clients Who Gained/Lost Employment"),
                                   plotlyOutput("gainedlostemploymentLinePlot"),
                                   h4("Client Engagement"),
                                   plotlyOutput("employmentLinePlot")
                                   
                           ),
                           tabPanel("Housing",
                                    h3("Housing"),
                                    h4("Client Numbers"),
                                    plotlyOutput("housingResidentLinePlot"), 
                                    h4("Average Length of Stay"),
                                    plotlyOutput("housingCapacityLinePlotLength"), 
                                    h4("Bed Days Filled"),
                                    plotlyOutput("bedDaysLinePlot")
                           ),
                           tabPanel("SUD Treatment",
                                    h3("SUD Treatment"),
                                    h4("Number Of UAs Conducted"),
                                    plotlyOutput("UAsLinePlot"), 
                                    h4("UA Percentage Breakdown"),
                                    plotlyOutput("UAPercentLinePlot")
                           ),
                           tabPanel("Recidivism",
                                    h3("Recidivism"),
                                    h4("Re-engagements"),
                                    plotlyOutput("engagementsLinePlot"), 
                                    h4("Average Number Of Direct Contact Attempts To Disengaged Individuals"),
                                    plotlyOutput("engagementcontactattemptsLinePlot")
                           ),
                           tabPanel("Staffing",
                                    h3("Staffing"),
                                    plotlyOutput("staffingLinePlot")
                           ),
                           tabPanel("Fidelity and Training",
                                    h3("Fidelity and Training"),
                                    h4("Fidelity Scores"),
                                    plotlyOutput("fidelityScoreLinePlot"),
                                    h4("Training"),
                                    plotlyOutput("fidelitypercentageLinePlot")
                           ),
                           tabPanel("Financial",
                                    h3("Financial Data"),
                                    p("Currently No Data"),
                                    plotlyOutput("financesLinePlot")
                           )
                           
                           ),
                
               HTML('<center><img src="footer.jpg"></center>')
)


##### Define server logic
server <- function(input, output) { 




# Luke’s Wrangling --------------------------------------------------------
gap <- gs_title("REACH Service Provider Report - Updated 05-10-19")
reach_og <- gap %>%
  gs_read(ws = "Updated Service Report")

# reach <- reach_og %>% as.tibble()
# reach %>% select()
#
# colnames(reach) <- reach[1, ]
# reach <- reach[-c(1,2),]
# reach %>% unite(col = "month", X2:X10, remove = TRUE)

reach <- t(reach_og)
colnames(reach) <- reach[1, ]
reach <- reach[-1, ]

reach <- reach %>%
  as.tibble() %>%
  select("month" = Metric, everything()) %>%
  mutate(rnum_17 = match("2017", month), # creates anchor values for case when below
         rnum_18 = match("2018", month),
         rnum_19 = match("2019", month),
         rnum_20 = match("2020", month),
         rnum_21 = match("2021", month)) %>%
  mutate_at(.vars = vars(month), # adds year to month
            .funs = funs(case_when(
              row_number() < rnum_17 ~ paste0(., "'17"),
              row_number() == rnum_17 ~ paste0(., " Total"),
              row_number() < rnum_18 ~ paste0(., "'18"),
              row_number() == rnum_18 ~ paste0(., " Total"),
              row_number() < rnum_19 ~ paste0(., "'19"),
              row_number() == rnum_19 ~ paste0(., " Total"),
              row_number() < rnum_20 ~ paste0(., " '20"),
              row_number() == rnum_20 ~ paste0(., " Total"),
              row_number() < rnum_21 ~ paste0(., "'21"),
              row_number() == rnum_21 ~ paste0(., " Total"),
              TRUE ~ as.character(.)
            ))) %>%
  select(-contains("rnum_")) # discards anchor value columns because they aren't needed



reach <- reach %>%
  select( # removes section headers
    -c("PROGRAM OVERVIEW", "CLIENT INFORMATION (of new enrollees)", "Age", "Race", "Ethnicity",
       "REFERRALS AND RANDOMIZATION", "SERVICE DELIVERY", "EMPLOYMENT", "HOUSING",
       "SUD TREATMENT", "RECIDIVISM", "STAFFING", "FIDELITY AND TRAINING", "FINANCIAL")
  ) %>%
  janitor::clean_names() # cleans column names

reach <- reach %>% # removes an x that excel inserts before col names begining with a #
  rename_at(.vars = vars(starts_with("x")), 
            .funs = funs(gsub("x", "", .)))


# Seperates the monthly, quarter, and yearly totals into
# different columns for easy plotting later on.
# The idea is that we can use these cols for plot x axises 
reach <- reach %>% 
  tibble::add_column(quarter = NA, .before = "month") %>% 
  mutate(quarter = case_when(
    grepl("Q[[:digit:]]", month) ~ identity(month), 
    TRUE ~ NA_character_)
  ) %>% 
  tibble::add_column(year = NA, .before = "quarter") %>% 
  mutate(year = case_when(
    grepl("[[:digit:]]{4}|Running Total", month) ~ identity(month), 
    TRUE ~ NA_character_)
  ) %>% 
  mutate(month = case_when(
    grepl("[[:digit:]]{4}|Q[[:digit:]]|Running Total", month) ~ NA_character_, 
    TRUE ~ identity(month)
  )
  ) 

# Change column values to integers (except for month, year, quarter) so that they don't have 
# to be changed in every plot call. 
reach <- reach %>%
  mutate_all(.funs = funs(gsub("%", "", .))) %>% 
  mutate_at(.vars = vars(-c("month", "year", "quarter")),
            .funs = funs(as.integer(.)))


months <- as_factor(reach$month)
quarters <- as_factor(reach$quarter)
years <- as_factor(reach$year)

ax <- list(
  title = 'Time Unit',
  zeroline = TRUE,
  showline = TRUE,
  zerolinewidth = 1,
  zerolinecolor = toRGB("white")
)

timeunit <- reactive({
  if (input$timeunit == "months") {
    months
  } else if (input$timeunit == "quarters") {
    quarters
  } else if (input$timeunit == "years") {
    years
  }
})


 # Program Overview --------------------------------------------------------
## Program Overview
### Program Overview Percentages
output$enrollmentpercentPlot <- renderPlotly({plot_ly(
  x = timeunit(), 
  y = as.numeric(sub("%", "", reach$enrollment_percent)) / 100, 
  name = 'Enrollment Percentage', 
  type = 'scatter', 
  mode = 'lines+markers',
  connectgaps = TRUE) %>% 
    add_trace(y = as.numeric(sub("%", "", reach$enrollment_failure_percentage)) / 100,
            name = "Enrollment Failure Percentage",
            type = "scatter",
            mode = "lines+markers",
            connectgaps = TRUE) %>% 
    add_trace(y = as.numeric(sub("%", "", reach$disengagement_percentage)) / 100,
              name = "Disengagement Percentage",
              type = "scatter",
              mode = "lines+markers",
              connectgaps = TRUE) %>% 
  layout(yaxis = list(title = 'Percent', 
                      rangemode = "tozero",
                      tickformat = "%"), 
         xaxis = ax)
})

  
  
### Program Overview Raw Counts 
  output$programOverviewPlot <- renderPlotly({plot_ly(
    #Plot Number of individuals RANDOMIZED in REACH
    x = timeunit(), 
    y = as.integer(reach$number_of_individuals_randomized_into_reach_this_month),
    name = 'Individuals Randomized Into Reach',
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    #Plot Number of individuals ENROLLED in REACH
    add_trace(y = as.integer(reach$number_of_new_clients_enrolled_in_reach_this_month),
              name = 'Clients Enrolled in REACH',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    #Plot Number of individuals ACTIVELY RECIEVING SERVICES
    add_trace(y = as.integer(reach$number_of_reach_clients_actively_receiving_services),
              name = 'Clients Actively Receiving Services',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    #Plot Number of individuals FAILED TO ENROLL after 30 days
    add_trace(y = as.integer(reach$failure_to_enroll_after_30_days),
              name = 'Failed To Enroll After 30 Days',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    #Plot Number of individuals who COMPLETED REACH
    add_trace(y = as.integer(reach$program_completion),
              name = 'Completed REACH Program',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    #Plot Number of individuals who RECEIVED 200 HOURS OF THERAPY  
    add_trace(y = as.integer(reach$percentage_rate_of_clients_who_reached_200_hours_of_therapy),
              name = 'Reached 200 Hours Of Therapy',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"),
           xaxis = ax)
  })
  
### Failures to Enroll
  output$enrollmentfailureslinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$failure_to_enroll_after_30_days, 
    name = 'Enrollment Failures-All Types', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE) %>% 
      add_trace(y = reach$fugitive,
                name = "Fugitive",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$jail_continued_stay,
                name = "Jail-Continued Stay",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$jail_new_stay,
                name = "Jail-New Stay",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$other_county,
                name = "Other County",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$other_treatment_continued_stay,
                name = "Other Treatment-Continued Stay",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$other_treatment_new_or_none,
                name = "Other Treatment-New or None",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      add_trace(y = reach$prison,
                name = "Prison",
                type = "scatter",
                mode = "lines+markers",
                connectgaps = TRUE) %>% 
      layout(yaxis = list(title = 'Percent', 
                          rangemode = "tozero"),
             xaxis = ax)
  })
  
### Disengagements
  output$disengagementslinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$disengagment_post_enrollment,
    name = 'Disengagements-All Types', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
      add_trace(y = reach$exits_to_jail,
                name = 'To Jail', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = reach$exits_to_prison, 
                name = 'To Prison', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y =  reach$medical_leave,
                name = 'Medical Leave', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y =  reach$drop_out,
                name = 'Dropped Out', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = reach$deceased,
                name = 'Deceased', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y =  reach$transfer_to_another_program,
                name = 'Transfered Programs', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = reach$suspended_from_treatment,
                name = 'Suspended From Treatment', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      layout(yaxis = list(title = 'Number of Clients that Exitted', 
                          rangemode = "tozero"), 
             xaxis = list(title = 'Month'))
  })
  
  
## New Enrollee Client Information
### Age  
  output$agesLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    # 18-25
    y = reach$`18_25`, 
    name = '18-25', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE) %>%
    # 26-35
    add_trace(y = reach$`26_35`,
              name = '26-35',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    # 36-45
    add_trace(y = reach$`36_45`,
              name = '36-45',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    # 46+
    add_trace(y = reach$`46`,
              name = '46+',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), 
           xaxis = list(title = ax))
  })
  
### Race
  output$raceLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    # American Indian
    y = reach$american_indian,
    name = 'American Indian', 
    type = 'scatter',
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    # Asian  
      add_trace(y = reach$asian,
                name = 'Asian', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
    # Black/ African American
      add_trace(y = reach$black_african_american,
                name = 'Black/African American', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
    # Black/African American, White
      add_trace(y = reach$black_african_american_white,
                name = 'Black/African American, White', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
    # Pacific Islander
      add_trace(y = reach$native_hawaiian_or_other_pacific_islander,
                name = 'Pacific Islander', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
    # Other Single Race
      add_trace(y = reach$other_single_race,
                name = 'Other Single Race', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
    # Two Or More Races
      add_trace(y = reach$two_or_more_races,
                name = 'Two Or More Races', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
      layout(yaxis = list(title = 'Number of Individuals', tick0 = 0, dtick = 1, range = c(0,3)),  #The range arg. on this is not robust. It ought to be adaptive to the data. 
             xaxis = ax)
  })
    
### Ethnicity
  output$ethnicitylinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    # Mexican 
    y = reach$mexican,
    name = 'Mexican', 
    type = 'scatter',
    mode = 'lines+markers',
    connectgaps = TRUE) %>% 
      add_trace(y = reach$not_of_hispanic_origin,
                name = 'Not of Hispanic Origin', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
      add_trace(y = reach$other_hispanic,
                name = 'Other Hispanic', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = reach$puerto_rican,
                name = 'Puerto Rican', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
      add_trace(y = reach$cuban,
                name = 'Cuban', 
                type = 'scatter',
                mode = 'lines+markers',
                connectgaps = TRUE) %>% 
      layout(yaxis = list(title = 'Number of Individuals', tick0 = 0, dtick = 1), 
             xaxis = ax)
  })
 
# Referrals and Randomization ---------------------------------------------  

  ## Referrals and Randomization 
### Randomized into Reach from Jail  
  output$randomizedBarPlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    # Randomized
    y = reach$number_of_individuals_randomized_into_reach_this_month, 
    name = 'Randomized into REACH',
    type = 'scatter',
    mode = 'lines+marker',
    connectgaps = TRUE) %>%
      # Enrolled
      add_trace(y = reach$number_of_new_clients_enrolled_in_reach_this_month,
                name = 'Clients Enrolled in REACH',
                moade = 'lines+markers',
                connectgaps = TRUE) %>% 
      # Randomized from Jail
      add_trace(y = reach$number_of_individuals_randomized_into_reach_this_month_who_are_in_jail,
                name = 'Randomized into REACH from jail',
                moade = 'lines+markers',
                connectgaps = TRUE) %>% 
    layout(yaxis = list(title = 'Number of Individuals', rangemode = "tozero"), 
           xaxis = ax)
  })



  
 
### Days Between Randomization and Enrollment   
  output$betweenEnrollmentdBarPlot <- renderPlotly({plot_ly(
    x = timeunit(),
    # Btwn randmztn and enrllmnt 
    y = reach$average_number_of_days_between_randomization_and_reach_enrollment,
    name = 'Average # of Days Between Randomization and Enrollment',
    type = 'bar') %>%
    layout(yaxis = list(title = 'Avg. Days from Randomization to Enrollment'), 
           xaxis = ax)
  })
  
  output$contactsBetweenEnrollmentdBarPlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$average_number_of_contacts_between_ucjc_randomization_and_reach_enrollment,
    type = 'bar', 
    name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Avg. Contacts from Randomization to Enrollment', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })

### Contacts Between Randomization and Enrollment  
  output$assessmentsBarPlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$number_of_intake_assessments_conducted_this_month,
    type = 'bar', 
    name = 'Randomized into REACH') %>%
    layout(yaxis = list(title = 'Assessments Conducted', 
                        rangemode = "tozero"), 
           xaxis = list(title = 'Month'))
  })
  

# Service Delivery --------------------------------------------------------
### Number of Clients by Delivery Type
  output$serviceDeliveryLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
      # Intensive treatment
    y = reach$number_of_clients_in_intensive_treatment_phase_this_month, 
    name = 'Intensive Treatment', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE) %>%
      # Transition  
    add_trace(y = reach$number_of_clients_in_transition_phase_this_month,
              name = 'Transition',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
      # Sustained Recovery
    add_trace(y = reach$number_of_clients_in_sustained_recovery_this_month,
              name = 'Sustained Recovery',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
      # Long Term Recovery Mngmnt
    add_trace(y = reach$number_of_clients_in_long_term_recovery_management_this_month,
              name = 'Long-term Recovery',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
      # Completed MRT
    add_trace(y = reach$number_of_individuals_who_completed_mrt_this_month,
              name = 'Completed MRT',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Individuals', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  

  

# Employment --------------------------------------------------------------
### Percent Employed
  output$employmentBarPlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$percent_percent_of_all_reach_clients_employed,
    type = 'bar', 
    name = 'REACH Clients') %>%
      layout(yaxis = list(title = '% of REACH Clients Employed', 
                          rangemode = "tozero"), 
             xaxis = ax)
  })
  
### Gained Lost Employment
  output$gainedlostemploymentLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), y = reach$number_of_clients_who_gained_employment_this_month , 
    name = 'Obtained Employment', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    add_trace(y = reach$number_of_clients_who_lost_a_job_in_the_past_month,
              name = 'Lost Employment', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Individuals', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  
  ### Employment Engagement - 
  output$employmentLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    y = reach$number_of_clients_engaged_in_reach_employment_related_services_this_month,
    name = 'Engaged with REACH employment related services', 
    mode = 'lines+markers',
    type = 'scatter',
    connectgaps = TRUE) %>%
      add_trace(y = reach$number_of_clients_engaged_in_vocational_education_training_programs_not_including_fsh_services_this_month,
                name = 'Engaged in vocational education training program', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>% #could error with ?
      layout(yaxis = list(title = 'Number of Individuals', 
                          rangemode = "tozero"), 
             xaxis = ax)
  })
  

  
  

# Housing -----------------------------------------------------------------
  ##
  ### Client Numbers
  output$housingResidentLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    y = reach$number_of_new_placements_to_the_reach_recovery_residence_this_month,
    name = 'New Placements To Recovery Residence',
    type = 'scatter',
    mode = 'lines+markers',
    connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_returning_clients_to_recovery_residence_this_month,
              name = 'Returning Clients to Recovery Residence',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_unique_clients_served_by_recovery_residence_this_month,
              name = 'Unique Clients Served by Recovery Residence',
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number of Clients', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  
  ### Average Length of Stay
  output$housingCapacityLinePlotLength <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$average_length_of_stay_in_days_in_recovery_residence,
    name = 'Average Length of Stay In Recovery Residence', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE
    )  %>%
    layout(yaxis = list(title = 'Days',
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  
  ### % Of Bed Days Filled
  output$bedDaysLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = as.numeric(sub("%", "", reach$percent_percent_of_bed_days_in_recovery_residence_filled_this_month)) / 100, 
    name = 'Percent Of Bed Days In Recovery Residence Filled',
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
      add_trace(y = as.numeric(sub("%", "", reach$percent_percent_of_bed_days_filled_by_transitional_residence_this_month)) / 100,
                name = 'Percent Of Bed Days Filled By Transitional Residence',
                mode = 'lines+markers'
      ) %>%
      add_trace(y = as.numeric(sub("%", "", reach$percent_percent_of_total_bed_days_filled_this_month)) / 100,
                name = 'Percent Of Total Bed Days Filled',
                mode = 'lines+markers'
      ) %>%  
      layout(yaxis = list(title = '% of Bed Days Filled', 
                          rangemode = "tozero", 
                          tickformat = "%"), 
             xaxis = ax)
  })
  

# SUD treatment -----------------------------------------------------------
  ##
  ### Number Of UAs Conducted
  output$UAsLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    y = reach$number_of_u_as_conducted_this_month,
    name = 'UAs Conducted',
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    layout(yaxis = list(title = 'Number Completed',
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  

  ### UA Percentage Breakdown
  output$UAPercentLinePlot <- renderPlotly({plot_ly(
    x = timeunit(),
    y = as.numeric(sub("%", "", reach$percent_positive_u_as_of_total_number_of_u_as_served_this_month)) / 100,
    name = '% positive UAs (of total number of UAs served this month', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    add_trace(y = as.numeric(sub("%", "", reach$percent_no_show_for_u_as_of_total_number_of_u_as_served_this_month)) / 100, 
              name = '% no-show for UAs (of total number of UAs served this month', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Percent (%)', 
                        rangemode = "tozero",
                        tickformat = "%"), 
           xaxis = ax)
  })
  
  
  
# Recidivism --------------------------------------------------------------  
  ##
  ### Re-engagements 
  output$engagementsLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$number_of_incarcerated_individuals_that_re_engaged_post_release_this_month, 
    name = 'Incarcerated Individuals That Re-Engaged Post-Release', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    add_trace(y = reach$number_of_individuals_who_received_a_direct_contact_attempt, 
              name = 'Individuals Who Received A Direct Contact Attempt', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_successful_reengagements_this_month, 
              name = 'Successful Reengagements', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number Of Clients', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  
  ### Contact Attempts
  output$engagementcontactattemptsLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$average_number_of_direct_contact_attempts_to_disengaged_individuals, 
    name = 'Average Number Of Direct Contact Attempts To Disengaged Individuals',
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE) %>% 
    layout(yaxis = list(title = 'Avg. # Of Direct Contact Attempts', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })


# Staffing ----------------------------------------------------------------
  ##
  ### Staffing 
  output$staffingLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = reach$number_of_case_managers, 
    name = 'Case Managers', 
    type = 'scatter', 
    mode = 'lines+markers',
    connectgaps = TRUE)  %>%
    add_trace(y = reach$number_of_mentor_support, 
              name = 'Mentors', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_program_managers, 
              name = 'Program Managers', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_admissions_coordinator, 
              name = 'Admission Coordinators', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    add_trace(y = reach$number_of_mental_health_therapists_clinicians, 
              name = 'Therapists/Clinicians', 
              mode = 'lines+markers',
              connectgaps = TRUE) %>%
    layout(yaxis = list(title = 'Number on Staff', 
                        rangemode = "tozero"), 
           xaxis = ax)
  })
  

# Fidelity and Training ----------------------------------------------------------------
  ##
  ### Fidelity Scores
  output$fidelityScoreLinePlot <- renderPlotly({plot_ly(
    x = timeunit(), 
    y = as.numeric(sub("%", "", reach$fidelity_score_for_mrt)) / 100,  
    name = 'Fidelity Score for MRT', 
    type = "scatter",
    mode = 'lines+markers',
    connectgaps = TRUE) %>%
      add_trace(y = as.numeric(sub("%", "", reach$fidelity_score_for_mi)) / 100, 
                name = 'Fidelity Score for MI', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = as.numeric(sub("%", "", reach$fidelity_score_for_ta_therapeutic_alliance)) / 100,  
                name = 'Fidelity Score for TA (Therapeutic Alliance)', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      layout(yaxis = list(title = 'Score', 
                          rangemode = "tozero"), 
             xaxis = list(title = 'Month'))
  })
  
  ### Training
  output$fidelitypercentageLinePlot <- renderPlotly({plot_ly(
    x =timeunit(),
    y = as.numeric(sub("%", "", reach$percent_percent_of_staff_trained_in_the_modalities_they_need)) / 100,
    name = 'Staff Trained In The Modalities They Need', 
    type = "scatter",
    mode = 'lines+markers',
    connectgaps = TRUE) %>%
      add_trace(y = as.numeric(sub("%", "", reach$percent_percent_of_mrt_groups_receiving_supervision)) / 100, 
                name = 'MRT Groups Recieving Supervision', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      add_trace(y = as.numeric(sub("%", "", reach$percent_percent_of_clinicians_receiving_fidelity_check)) / 100, 
                name = 'Clinicians Recieving Fidelity Check', 
                mode = 'lines+markers',
                connectgaps = TRUE) %>%
      layout(yaxis = list(title = "Percent (%)",
                          rangemode = "to zero",
                          tickformat = "%"),
             xaxis = ax)
  })

# Financial ---------------------------------------------------------------

#   output$financesLinePlot <- renderPlotly({financesLinePlot <- plot_ly(x = timeunit(), y = as.double(tData[,83]), name = 'Finances', type = 'scatter', mode = 'lines+markers')  %>%
#     layout(yaxis = list(title = 'Dollars ($)', rangemode = "tozero"), xaxis = list(title = 'Month', rangemode = "tozero"))
#   })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)