label_percent <- scales::label_percent(scale = 1, accuracy = 0.1)
label_pounds <- scales::label_currency(prefix = "£")
label_count <- scales::label_comma()

parseOutcomes <- function(outcomePercent) {
  outcomePercent <- case_when(
    outcomePercent == "low" ~ 0,
    outcomePercent == "z" ~ 0,
    outcomePercent == "c" ~ NA_real_, # Large negative number to suggest something is wrong
    is.na(outcomePercent) ~ NA_real_,
    .default = as.numeric(outcomePercent)
  )

  return(as.numeric(outcomePercent))
}

format_outcome <- function(outcomeVal, naDefault = FALSE) {
  outcomeVal <- case_when(
    outcomeVal == "c" ~ "Suppressed",
    outcomeVal == "low" ~ "Less than 0.05%",
    outcomeVal == "z" ~ "Zero",
    is.na(outcomeVal) ~ "Missing",
    naDefault ~ NA,
    .default = label_percent(as.numeric(outcomeVal))
  )
}

parseEarnings <- function(earnings) {
  earnings <- case_when(
    earnings == "low" ~ 0,
    earnings == "z" ~ 0,
    earnings == "c" ~ NA_real_, # Large negative number to suggest something is wrong
    is.na(earnings) ~ NA_real_,
    .default = as.numeric(earnings)
  )

  return(as.numeric(earnings))
}

format_earnings <- function(earningsVal, adjusted = FALSE, naDefault = FALSE) {
  earningsVal <- case_when(
    earningsVal == "c" ~ "Suppressed",
    earningsVal == "low" ~ "Less than £50",
    earningsVal == "z" ~ "Zero",
    earningsVal == "x" & adjusted ~ "Not Computed",
    is.na(earningsVal) ~ "Missing",
    naDefault ~ NA,
    TRUE ~ label_pounds(as.numeric(earningsVal))
  )
}

format_count <- function(count) {
  count <- case_when(
    count == "c" ~ "Suppressed",
    count == "low" ~ "Low",
    count == "z" ~ "Zero",
    .default = label_count(as.numeric(count))
  )
}
