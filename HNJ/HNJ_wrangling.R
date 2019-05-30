library(shiny)
library(plyr)
library(tidyverse)
library(siverse)
library(googlesheets)
library(shinythemes)
library(plotly)
# You can download siverse by running the code below. You might not need it but it comes in handy. 
# devtools::install_github("Sorenson-Impact/sorensonimpact")
# devtools::install_github("Sorenson-Impact/siverse")


# Wrangling ------------------------------------------------------

sheet_hnj <- gs_title("HNJ Service Provider Report_Updated")
hnj19_og <- sheet_hnj %>%
  gs_read(ws = "2019-Present", check.names = FALSE) #check.name = false so as not to add on _1 _2 _3 etc after month names as the spreadsheet grows
hnjpre19_og <- gap %>% 
  gs_read(ws= "Pre-2019", check.names = FALSE) #check.name = false so as not to add on _1 _2 _3 etc after month names as the spreadsheet grows

hnj <- bind_cols(hnjpre19_og, hnj19_og) %>% #binds the two spreadsheets together based on row position only. 
  filter_at(.vars = "8881", any_vars(grepl(x = ., pattern = "UCJC") | is.na(.))) %>% # filters out section headings and the rows (ie. metrics) 
  # which are no longer tracked in the updated spreadsheet. Note: the rows still exist in the spread sheet but are hidden. 
  select(-c("3", "Metric", "8881", "YTD Totals Pre-2019"), 
         "Pre 2019 totals" = `Yearly totals to date`) %>% 
  select("Metric" = Metric1, everything()) %>% # the two select calls eliminate duplicate columns after the bind() and renames other columns.
  filter(Metric %ni% c("Client Gender","Client Age", "Client Race", "Client Ethnicity", "Rental Subsidies"))
  

hnj <- t(hnj)
colnames(hnj) <- hnj[1,]
hnj <- hnj[-1,]
hnj <- as_tibble(hnj, rownames = "month")
  
  


# Attempt to use the combined spread sheet. -----------------------------------------------------------
# decided its not worth it as it contains a number of erroneous columns. Better just to build from 2019 and work on incorporating older data later on. 
  gap <- gs_title("HNJ Service Provider Report_Updated")
  myData <- gap %>%
    gs_read(ws = "Do Not Use-Dshbrd Dev.")
  
  ### NOTE:
  # As long as the googlesheets format stays the same, the numeric header removal code should work without alteration. 
  # I spent most of my time attempting to find a solution of using character column names rather than numerical.
  # This is the learning curve I mentioned. Overall the dashboard works well with the updated googlesheet.
  tData <- t(myData) # transposes the data
  tData <- as.data.frame(tData) # transforms the data into a dataframe
  tData <- tData[ ,-c(1, 6, 7, 11, 16, 25, 29, 56, 63, 69, 77, 81, 84, 96)] # removes header columns
  colnames(tData) <- as.character(unlist(tData[2,])) # assigns column names to second row
  tData <- tData[-c(1, 2),] #5, 9, 13, 16, 17, 18, 19, 20, 21, 22), ] # removes quarterly total and duplicate rows
  tData <- select("month" = rownames(), everything()) %>%
    mutate(rnum_pre19 = match("YTD Totals Pre-2019", month), # creates anchor values for case when below
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
    select(-contains("rnum_"))
  xaxis <- rownames(tData) # assigns row names to a vector we can use in our graph
  months <- factor(xaxis,levels = c("October", "November", "December", "January",  "February", "March",  "April", "May", "June",  "July", "August","September"))
  ### Creating new Columns for Staffing Tab
  # staff <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 7)
  # tData$Staff <- staff
  # clients <- c(NA, 61, 67, 67, 67, 95, 103, 113, 127, NA)
  # tData$Clients <- clients
  # PositionsAvailable <- c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  # tData$PositionsAvailable <- PositionsAvailable
  # PositionsFilled <- c(NA, 7, 7, 7, 7, 7, 7, 7, 7, 8)
  # tData$PositionsFilled <- PositionsFilled    
