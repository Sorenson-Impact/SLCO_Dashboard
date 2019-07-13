library(tidyverse)
library(siverse)
library(googlesheets)


# REACH -------------------------------------------------------------------


gap <- gs_key("11hR_tcHj_y3MUo79fkyEOgC3WXiRhCjGt38pLT5CdsQ")
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
# reach <- reach %>% 
#   tibble::add_column(quarter = NA, .before = "month") %>% 
#   mutate(quarter = case_when(
#     grepl("Q[[:digit:]]", month) ~ identity(month), 
#     TRUE ~ NA_character_)
#   ) %>% 
#   tibble::add_column(year = NA, .before = "quarter") %>% 
#   mutate(year = case_when(
#     grepl("[[:digit:]]{4}|Running Total", month) ~ identity(month), 
#     TRUE ~ NA_character_)
#   ) %>% 
#   mutate(month = case_when(
#     grepl("[[:digit:]]{4}|Q[[:digit:]]|Running Total", month) ~ NA_character_, 
#     TRUE ~ identity(month)
#   )
#   ) 



# REACH set up for export -------------------------------------------------

#These are the names of the rows in the report table in order. 
str_split("Number of individuals randomized into the REACH program
Number of new clients enrolled
Total number of clients actively receiving services
Attrition-Failures to enroll
Attrition-Disengagements
Average number of days between randomization and REACH enrollment
Number of individuals randomized into REACH still in jail
Average length of stay (in days) in recovery residence
Number of unique clients served by recovery residence this month
Number of clients who gained employment
Percent of all REACH clients employed
Exits to jail or prison
Number of successful reengagements
Number of clients who successfully completed the REACH program", "\n")

#These are the row numbers in the service provider spreadsheet in the order to match the table rownames above
# Note: this vector is one longer than the row names above because rows 19 and 20 are added together for "exits to jail or prison"
c(50, 6, 7, 9, 18, 51, 50, 71, 72, 62, 65, 19, 20, 93, 26) 





## Text Insertions 
#Section 1
# This is the row number for the heading titled "Terminations, Attritions, Refusals"
9 # enrollment failures 
8 # enrollment failures percentage
17 # disengagement percentage
# This is the row number for the heading titled "Referral Metrics"
48
# Section 5
# This is the row numbers for the heading titled "Milestones"
50 # refferrals
6 # enrollemtns





# HNJ Cleaning ---------------------------------------------------------------------

# This tidying is taken from the app file and may not be 
# the best method for automating the monthly report. 
sheet_hnj <- gs_key("1-wKEqtV9pT36Mq2nyGYH4qnGOORGCXmJWv2ALHiOxME")
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

hnj <- clean_names(hnj)


## HNJ set up for export -------------------------------------------------------

#These are the names of the rows in the report table in order. 
str_split(c("Number randomized into HNJ
Number of new placements into housing
Number of new clients enrolled in HNJ this month
Total number of HNJ clients to date
Total number of housed clients to date
Total number of clients graduated to a permanent location to date
Total number of clients currently awaiting housing placement
Number of employment placements
Number of clients referred to substance abuse services
Number of clients referred to behavioral health services
Number identified as eligible per HMIS data pull 
Number of individuals TRH attempted to locate
Number of individuals prescreened 
Number of individuals eligible for prescreens  
Number screened as eligible for HNJ
Number randomized into HNJ"), "\n") 

#These are the row numbers in the service provider spreadsheet in the order to match the table rownames above

c(48, 59, 3, 4, 5, 6, 62, 81, 78, 79, 31, 32, 33, 34, 41, 48)


## Text Insertions 
#Section 1
# This is the row number for the heading titled "Exits"
91 
# This is the row number for the heading titled "Referral Metrics"
48
# Section 5
# This is the row numbers for the heading titled "Milestones"
48
59
  


# # Seperates the monthly, quarter, and yearly totals into
# # different columns for easy plotting later on.
# # The idea is that we can use these cols for plot x axises 
# hnj <- hnj %>% 
#   tibble::add_column(quarter = NA, .before = "month") %>% # quarters
#   mutate(quarter = case_when( 
#     grepl("Q[[:digit:]]", month) ~ identity(month), 
#     TRUE ~ NA_character_)
#   ) %>% 
#   tibble::add_column(year = NA, .before = "quarter") %>% # years
#   mutate(year = case_when(
#     grepl("[[:digit:]]{4}|Running Total", month) ~ identity(month), 
#     TRUE ~ NA_character_)
#   ) %>% 
#   mutate(month = case_when( # months
#     grepl("[[:digit:]]{4}|Q[[:digit:]]|Running Total", month) ~ NA_character_, 
#     TRUE ~ identity(month)
#   )
#   ) 
