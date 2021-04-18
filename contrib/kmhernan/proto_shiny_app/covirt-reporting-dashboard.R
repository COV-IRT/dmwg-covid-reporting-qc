# Purposefully trying to keep it a single file app right now.
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(viridis)
library(zoo)
library(plotly)
library(jsonlite)

case.cols <- c("all_cause", "natural_cause", "septicemia_a40_a41", "malignant_neoplasms_c00_c97", 
               "diabetes_mellitus_e10_e14", "alzheimer_disease_g30", "influenza_and_pneumonia_j09_j18",
               "chronic_lower_respiratory", "other_diseases_of_respiratory", "nephritis_nephrotic_syndrome",
               "symptoms_signs_and_abnormal", "diseases_of_heart_i00_i09", "cerebrovascular_diseases",
               "covid_19_u071_multiple_cause_of_death", "covid_19_u071_underlying_cause_of_death")

# config JSON - ugly right now
# 2014-2018 https://data.cdc.gov/resource/3yf8-kanr.csv?$limit=50000
# 2019-2020 https://data.cdc.gov/resource/muzy-jte6.csv?$limit=10000
# {
#   "weekly_2014_2018": "/path/to/csv",
#   "weekly_2019_2020": "/path/to/csv"
# }
config.file <- Sys.getenv("COVID_CONFIG")
config <- jsonlite::fromJSON(config.file)

# helper to mask nas with a value
# Need to verify this with cassie
cdc.na.mask <- function(x, mask.val=9) {
  x[is.na(x)] <- mask.val
  return(x)
}

# This will ultimately need to handle getting data from http requests
load.cdc_weekly.datasets <- function(){
    # First, get the 2014-2018 data and format columns... these wont always have `row.names=1`
    dat.old <- read.csv(config$weekly_2014_2018, na.strings=c("", "NA"))
    dat.old <- dat.old %>%
      mutate(week_ending_date=date(ymd_hms(weekendingdate)), all_cause=allcause, natural_cause=naturalcause,
             influenza_and_pneumonia_j09_j18=influenza_and_pneumonia_j10,
             covid_19_u071_multiple_cause_of_death=0, covid_19_u071_underlying_cause_of_death=0) %>%
      select(-weekendingdate, -allcause, -naturalcause, -influenza_and_pneumonia_j10)
    
    # Next, get 2019-2020 and combine. these wont always have `row.names=1`
    dat.new <- read.csv(config$weekly_2019_2020, na.strings=c("", "NA"))
    dat.merge <- dat.new %>%
      mutate(week_ending_date=date(mdy(week_ending_date))) %>%
      bind_rows(dat.old) %>%
      arrange(mmwryear, mmwrweek, jurisdiction_of_occurrence) %>%
      mutate_at(case.cols[case.cols != "influenza_and_pneumonia_j09_j18"], cdc.na.mask) %>%
      mutate(influenza_and_pneumonia_j09_j18=cdc.na.mask(influenza_and_pneumonia_j09_j18, mask.val=1))
    return(dat.merge)
}

# also ugly right now and will need to be reactive
cdc.dat <- load.cdc_weekly.datasets()

# basic ui right now
ui <- dashboardPage(
  dashboardHeader(title="COVID Mortality Reporting Explorer"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width=9, plotlyOutput("count_plot")),
      box(
        title="Settings",
        width=3,
        selectInput("sel_state", label = "Jurisdiction:",
                    choices = unique(cdc.dat$jurisdiction_of_occurrence),
                    selected="United States"),
        br(),
        selectInput("sel_col", label = "Select Cause:",
                    choices = case.cols)
      )
    ),
    fluidRow(
      box(width=9, plotlyOutput("freq_plot")),
      box(
        title="Frequency",
        width=3,
        selectInput("sel_state_b", label = "Jurisdiction:",
                    choices = unique(cdc.dat$jurisdiction_of_occurrence),
                    selected="United States"),
        br(),
        selectInput("sel_col_num", label = "Select Cause Numerator:",
                    choices = case.cols[!(case.cols %in% c("all_cause", "natural_cause"))]),
        br(),
        selectInput("sel_col_den", label = "Select Cause Denominator:",
                    choices = c("all_cause", "natural_cause"))
      )
    ),
    fluidRow(
      box(width=9, plotlyOutput("area_plot")),
      box(
        title="Stacked Area Plot",
        width=3,
        selectInput("sel_state_area", label = "Jurisdiction:",
                    choices = unique(cdc.dat$jurisdiction_of_occurrence),
                    selected="United States"),
        br(),
        sliderInput("start_year", label = "Year Filter",
                    min=2014, max=2020, value=c(2014, 2020),
                    step=1, round=TRUE)
      )
    )
  )
)

server <- function(input, output) {
  
  output$count_plot <- renderPlotly({
    cdc.dat %>%
      filter(jurisdiction_of_occurrence == input$sel_state) %>%
      mutate(mmwryear=factor(mmwryear)) %>%
      ggplot(aes(x=mmwrweek, y=!!as.name(input$sel_col), group=mmwryear, color=mmwryear)) +
      geom_line() +
      geom_point() +
      scale_color_viridis(discrete=TRUE) +
      theme_minimal() +
      theme(legend.position="bottom")
  })
  
  output$freq_plot <- renderPlotly({
    cdc.dat %>%
      filter(jurisdiction_of_occurrence == input$sel_state_b) %>%
      mutate(freq=!!as.name(input$sel_col_num)/!!as.name(input$sel_col_den),
             mmwryear=factor(mmwryear)) %>%
      ggplot(aes(x=mmwrweek, y=freq, group=mmwryear, color=mmwryear)) +
      geom_line() +
      geom_point() +
      ylab("Frequency") +
      scale_color_viridis(discrete=TRUE) +
      theme_minimal() +
      theme(legend.position="bottom")
  })
  
  output$area_plot <- renderPlotly({
    sub.cols <- case.cols[!(case.cols %in% c("all_cause", "natural_cause", "covid_19_u071_multiple_cause_of_death"))]
    cdc.dat %>%
      filter(mmwryear >= input$start_year[1] & mmwryear <= input$start_year[2] & jurisdiction_of_occurrence == input$sel_state_area) %>%
      select(jurisdiction_of_occurrence, mmwryear, mmwrweek, week_ending_date, !!sub.cols) %>%
      pivot_longer(c(-jurisdiction_of_occurrence, -mmwryear, -mmwrweek, -week_ending_date), names_to="cause", values_to="counts") %>%
      group_by(week_ending_date, cause) %>%
      summarise(n=sum(counts, na.rm=TRUE)) %>%
      mutate(percentage=n / sum(n)) %>%
      ggplot(aes(x=week_ending_date, y=percentage, fill=cause)) +
      geom_area(alpha=0.7, size=0.5, colour="white") +
      ylab("Frequency") +
      scale_fill_viridis(discrete=TRUE) +
      theme_minimal()
  })
}

shinyApp(ui, server)
