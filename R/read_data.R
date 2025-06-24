# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

# Read Var Lookup -------------------------------------------------------------------------------------------------

var_lookup <- read.csv("data/var_lookup.csv")
# put the variable names in the rownames, so we can easily index them
rownames(var_lookup) <- var_lookup$variable
var_lookup <- select(var_lookup, -variable)


get_var_name <- function(var, expect_name = TRUE) {
  if (var %in% rownames(var_lookup)) {
    var_name <- var_lookup[[var, "name"]]
  } else {
    # browser()
    var_name <- var
    if (expect_name) warning("Variable ", var, " is not in var_lookups.csv")
  }

  return(var_name)
}

get_var_names <- function(vars, ...) {
  sapply(vars, get_var_name, ...)
}

get_var_unit <- function(var, expect_name = TRUE) {
  if (var %in% rownames(var_lookup)) {
    var_unit <- var_lookup[[var, "unit"]]
  } else {
    var_unit <- ""
    if (expect_name) warning("Variable ", var, " is not in var_lookups.csv")
  }
  return(var_unit)
}

get_var_units <- function(vars, ...) {
  sapply(vars, get_var_unit, ...)
}

# Read Metadata ----

metadata_path <- "data/metadata.csv"
metadata <- read.csv(metadata_path, encoding = "UTF-8")

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

names(choicesCharType) <- get_var_names(choicesCharType)

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

names(choicesColGrouping) <- sapply(choicesColGrouping, get_var_name)

# This gives options for the indicator to be graphed in the Outcomes tab
choicesOutcomesIndicator <- metadata %>%
  filter(filter_name == "indicator") %>%
  pull(filter_value)

names(choicesOutcomesIndicator) <- sapply(choicesOutcomesIndicator, get_var_name)


# Connect to database ---------------------------------------------------------------------------------------------

# con <- DBI::dbConnect(odbc::odbc(),
#   driver = "SQL Server",
#   server = "VMT1PR-DHSQL01",
#   database = "EDUDEST-WKG-HE"
# )
#
# LEO_data <- "ProviderDashboard.combined_rounded_with_adjusted_20250320"

# Load CSV into an in memory database  ----------------------------------------------------------------------------

# 1. Create an in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# 2. Read your CSV file into a data frame
# csv_data <- read_csv("data/provider_data_20250528.csv", show_col_types = FALSE)

# save csv as parquet instead:
# file_path <- "data/provider_data.parquet"
# write_parquet(csv_data, file_path)

parquet_data <- read_parquet("data/provider_data.parquet")

# 3. Copy the data frame into the SQLite database
dbWriteTable(con, "LEO_data", parquet_data)

# 4. Remove the CSV version from memory, to save a bit of RAM.
rm(parquet_data)
gc()
