library(shiny)
library(plyr)
library(tidyverse)
library(googlesheets)
library(shinythemes)
library(plotly)

gap <- gs_title("HNJ Service Provider Report_Updated")
reach_og <- gap %>%
  gs_read(ws = "Updated Service Report")