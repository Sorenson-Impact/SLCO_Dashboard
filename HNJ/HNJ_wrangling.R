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

hnj <- hnj %>% 
  select(-month, month) %>% 
  select(-quarter, quarter) %>% 
  select(-year, year)

