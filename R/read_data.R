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

get_var_lookup <- function() {
  message("Reading in var look-up")
  var_lookup <- read.csv("data/var_lookup.csv")
  # put the variable names in the rownames, so we can easily index them
  rownames(var_lookup) <- var_lookup$variable
  select(var_lookup, -variable)
}

get_var_name <- function(var, var_lookup, expect_name = TRUE) {
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

get_var_unit <- function(var, var_lookup, expect_name = TRUE) {
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
read_meta_data <- function() {
  message("Reading in meta-data")
  metadata_path <- "data/metadata.csv"
  read.csv(metadata_path, encoding = "UTF-8")
}


# Connect to database ---------------------------------------------------------------------------------------------

# con <- DBI::dbConnect(odbc::odbc(),
#   driver = "SQL Server",
#   server = "VMT1PR-DHSQL01",
#   database = "EDUDEST-WKG-HE"
# )
#
# LEO_data <- "ProviderDashboard.combined_rounded_with_adjusted_20250320"


# 2. Read your CSV file into a data frame
# csv_data <- read_csv("data/provider_data_20250528.csv", show_col_types = FALSE)

# save csv as parquet instead:
# file_path <- "data/provider_data.parquet"
# write_parquet(csv_data, file_path)
read_provider_data <- function(con) {
  message("Loading underlying data")
  parquet_data <- read_parquet("data/provider_data.parquet")

  # 3. Copy the data frame into the SQLite database
  dbWriteTable(con, "LEO_data", parquet_data)

  # 4. Remove the CSV version from memory, to save a bit of RAM.
  rm(parquet_data)
  gc()
  message("Data loaded")
}
