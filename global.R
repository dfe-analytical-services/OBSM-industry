# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(tidyverse))
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinytitle))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(plotly))
shhh(library(DT))
shhh(library(htmltools))
shhh(library(formattable))
shhh(library(gt))
shhh(library(janitor))
shhh(library(devtools))
shhh(library(metathis))
# shhh(library(shinya11y))
# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  r_scripts <- eval(styler::style_dir("R/")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, r_scripts, test_scripts)
  return(script_changes)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

enableBookmarking("url")

site_title <- "DfE FE Outcomes Industry Dashboard"
site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"

source("R/support_links.R")
source("R/read_data.R")



# Read in data ------------------------------------------------------------


# Read in industry data
dfInd <- read_ind_data() %>%
  mutate(NumberSustainedEmployment = suppressWarnings(as.integer(NumberSustainedEmployment))) %>% # Convert columns into numeric values
  mutate(IndustrySection = str_to_sentence(IndustrySection)) %>%
  # Improve formatting for industry variable
  rename(Industry = IndustrySection) %>%
  # Manual override to improve formatting
  mutate(Ethnicity = ifelse(Ethnicity == "Black/African/Caribbean/Black British", "Black/African/ Caribbean/ Black British", Ethnicity))



# Set up list of choices for input selections -----------------------------

# Get list of all options for SSA Tier 1
choicesSSATier1 <- dfInd %>%
  select(SSATier1) %>%
  distinct() %>%
  arrange(SSATier1 != "All", SSATier1) # Ensure 'All' appears at top of list

# Get list of options for provision type
choicesProvision <- dfInd %>%
  select(Provision) %>%
  distinct() %>%
  arrange(Provision != "All", Provision) # Ensure 'All' appears at top of list

# Get list of options for Industry
choicesIndustry <- dfInd %>%
  select(Industry) %>%
  distinct() %>%
  arrange(Industry != "All", Industry) # Ensure 'All' appears at top of list


expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details", id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}
