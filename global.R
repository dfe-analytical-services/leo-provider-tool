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

shhh(library(showtext))
shhh(library(systemfonts))

# Fonts for charts ------------------------------------------------------------
font_add("dejavu", "www/fonts/DejaVuSans.ttf")
register_font(
  "dejavu",
  plain = "www/fonts/DejaVuSans.ttf",
  bold = "www/fonts/DejaVuSans-Bold.ttf",
  italic = "www/fonts/DejaVuSans-Oblique.ttf",
  bolditalic = "www/fonts/DejaVuSans-BoldOblique.ttf"
)
showtext_auto()

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


var_lookup <- get_var_lookup()

# Extract meta data
metadata <- read_meta_data()

choicesTaxYear <- metadata %>%
  filter(filter_name == "tax_year") %>%
  pull(filter_value)

choicesYAG <- metadata %>%
  filter(filter_name == "YAG") %>%
  pull(filter_value)

choicesProviderCountry <- metadata %>%
  filter(filter_name == "provider_country_name") %>%
  pull(filter_value)

# In selections, we want to display the country 'Total' as 'Total (aggregate only)' so we need to name the vector
names(choicesProviderCountry) <- choicesProviderCountry
names(choicesProviderCountry)[choicesProviderCountry == "Total"] <- "Total (aggregate only)"

choicesProviderRegion <- metadata %>%
  filter(filter_name == "provider_region_name") %>%
  filter(filter_value != "Total") %>%
  pull(filter_value)

choicesProviderType <- metadata %>%
  filter(filter_name == "provider_type") %>%
  filter(filter_value != "Total") %>%
  pull(filter_value)

choicesProviderName <- metadata %>%
  filter(filter_name == "provider_name") %>%
  filter(filter_value != "Total") %>%
  pull(filter_value)


choicesProviderGeog <- list(
  "Provider types" = as.list(choicesProviderType),
  "Regions" = as.list(choicesProviderRegion),
  "Providers" = as.list(choicesProviderName)
)

choicesCharType <- metadata %>%
  filter(filter_name == "characteristic_type") %>%
  pull(filter_value)

names(choicesCharType) <- get_var_names(choicesCharType, var_lookup)

choicesCharValue <- metadata %>%
  filter(filter_name == "characteristic_value") %>%
  pull(filter_value)

choicesCharValueTable <- metadata %>%
  filter(
    filter_name %in% choicesCharType,
    filter_value %in% choicesCharValue
  ) %>%
  select(filter_name, filter_value)

choicesSubject <- metadata %>%
  filter(filter_name == "cah2_subject_name") %>%
  pull(filter_value)


# This gives options for colour grouping of graph columns
choicesColGrouping <- metadata %>%
  mutate(filter_name = ifelse(filter_name %in% c("provider_region_name", "provider_type", "provider_name"), "provider_geog", filter_name)) %>%
  filter(!filter_name %in% c(
    "characteristic_type", "sex", "ethnicity", "prior_attainment_code", "POLAR4",
    "home_region_name", "current_region_name", "provider_LAD", "cah3_subject_name", "academic_year", "indicator"
  )) %>%
  pull(filter_name) %>%
  unique()

names(choicesColGrouping) <- sapply(choicesColGrouping, get_var_name, var_lookup)

# This gives options for the indicator to be graphed in the Outcomes tab
choicesOutcomesIndicator <- metadata %>%
  filter(filter_name == "indicator") %>%
  pull(filter_value)

names(choicesOutcomesIndicator) <- sapply(choicesOutcomesIndicator, get_var_name, var_lookup)



# Load CSV into an in memory database  ----------------------------------------------------------------------------

# 1. Create an in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")
read_provider_data(con)


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
