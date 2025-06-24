# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent
# across users whereas the server and UI files are constantly interacting and
# responsive to user input.
#
# ---------------------------------------------------------
# message("Sourcing global")


# Library calls ----------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(stringr))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(shinytitle))
shhh(library(shinycssloaders))
shhh(library(plyr))

shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(DT))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))
shhh(library(shinytest2))
shhh(library(rstudioapi))
shhh(library(bslib))
shhh(library(dfeshiny))
shhh(library(ggiraph))
# to read in filtered csv:
# shhh(library(readr))
# shhh(library(shinya11y))
# to read in from sql:
shhh(library(dbplyr))
shhh(library(RODBC))
# to read csv in to memory database:
shhh(library(RSQLite))
shhh(library(readr))
shhh(library(arrow))
# using 'is_empty'
shhh(library(sjmisc))
# for combined filter
shhh(library(tidyr))
shhh(library(forcats))
shhh(library(shinyBS))


# Functions --------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate
# numbers:

# This line enables bookmarking such that input choices are shown in the url.
enableBookmarking("url")


# Source scripts ---------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets
# to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")
# Load in the data
source("R/read_data.R")
# Conversion functions for earnings/outcomes to nicely formatted and plotable
source("R/data_parsing.R")
source("R/plotting.R")
source("R/dfe_resources.R")
source("R/datatable.R")

# UI Panels
source("R/ui_panels/0_user_guide.R")
source("R/ui_panels/1_headline_figures.R")
source("R/ui_panels/2_dashboard_panel.R")


# appLoadingCSS ----------------------------------------------------------------
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

vb_theme <- value_box_theme(bg = "#1d70b8")


# Global Variables ------------------------------------------------------------------------------------------------

dashboard_title <- "Longitudinal Education Outcomes - Provider level dashboard"
dashboard_url <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"

sites_list <- c(dashboard_url)
# Update this with your parent
# publication name (e.g. the EES publication)
ees_pub_name <- "LEO Graduate outcomes provider level data"
ees_pub_slug <- "graduate-outcomes-leo-provider-level-data"
# Update with parent publication link
ees_publication <- paste0("https://explore-education-statistics.service.gov.uk/find-statistics/", ees_pub_slug)

feedback_url <- "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-dolPCYMKrpJpVnliJHTqTpUOEVOTENSVjZSWFRLTlQ1MFJWSEZNWU4zNC4u"
repo_url <- "https://dfe-gov-uk.visualstudio.com/HEFE-Higher-Education-Analysis/_git/LEO_provider_dashboard"
team_email <- "he.leo@education.gov.uk"

# The maximum number of filter combinations that will be plotted on the graph. This does not effect the data table.
max_filters <- 40

# TODO: Analytics key
google_analytics_key <- "Z967JJVQQX"


## Default Filter combinations -------------------------------------------------------------------------------------

# This will show on first load, and also when filters are reset
default_tax_year <- "2022/2023"
default_YAG <- 5
default_provider_country <- "Total"
default_provider_geog <- "HEI"
default_cah2_subject_name <- "Total"
default_characteristic_type <- "All graduates"


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
