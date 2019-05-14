library(shiny)
library(plyr)
library(tidyverse)
library(siverse)
library(googlesheets)
library(shinythemes)
library(plotly)



ax <- list(
  title = 'Month',
  zeroline = TRUE,
  showline = TRUE,
  zerolinewidth = 1,
  zerolinecolor = toRGB("white")
)


# Luke’s Wrangling --------------------------------------------------------
gap <- gs_title("REACH Service Provider Report - Updated 02-08-19")
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

# reach %>%
#   gather(key  = "race", value = "race_count", american_indian:white) %>%
#   gather(key  = "ethnicity", value = "ethnicity_count", mexican:cuban)


# Dillon’s wrangling ------------------------------------------------------

# gap <- gs_title("REACH Service Provider Report - Updated 02-08-19")
# myData <- gap %>%
#   gs_read(ws = "Updated Service Report")
# 
# ## Wrangling: 
# 
# #transpose the data to put observations into rows 
# tData <- t(myData)
# 
# #make column names the names of the first row
# colnames(tData) = tData[1, ] # assigns column names from the first row
# tData = tData[-1, ] # removes the first row from the data 
# 
# #make the row names the names of the first column 
# rownames(tData) <-tData[ ,1] # assigns column names from the first row
# tData <-  tData[, -1] # removes the first row from the data 
# 
# # remove the 'totals' month
# tData <- tData[-c(4, 8, 12, 16, 17), ]
# 
# #remove the 'header' columns 
# tData <- tData[ ,-c(1, 9, 10, 14, 15, 20, 33, 38, 46, 53, 63, 69, 74, 80, 87, 98) ]
# 
# xaxis <- rownames(tData)
# y2 <-  tData[,1]



months <- as_factor(reach$month)
quarters <- as_factor(reach$quarter)
years <- as_factor(reach$year)


### Testing Plots
plot_ly(x = years, y = as.numeric(sub("%", "", reach$enrollment_percent)) / 100, 
        name = 'Randomized', 
        type = 'scatter', 
        mode = 'lines+markers',
        connectgaps = TRUE) %>% 
  layout(yaxis = list(title = 'Percent Enrolled', 
                      rangemode = "tozero",
                      tickformat = "%"), 
         xaxis = ax)
  #Plot Number of individuals referred to REACH this month
  add_trace(y = strtoi(tData[,2]), name = 'Referred', mode = 'lines+markers') %>%
  #Plot Number of new clients enrolled in REACH this month
  add_trace(y = strtoi(tData[,3]), name = 'New Clients', mode = 'lines+markers') %>%
  #Plot Number of REACH clients actively receiving services
  add_trace(y = strtoi(tData[,4]), name = 'Receiving Services', mode = 'lines+markers') %>%
  #Plot Total number of individuals enrolled in REACH 
  add_trace(y = strtoi(tData[,5]), name = 'Total Enrolled', mode = 'lines+markers') %>%
  add_trace(y = strtoi(tData[,9]), name = 'Completed REACH', mode = 'lines+markers')%>%
  