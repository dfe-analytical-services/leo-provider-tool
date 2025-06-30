generate_column_name <- function(vars) {
  col_names <- sapply(vars, function(var) {
    col_name <- get_var_name(var, var_lookup)

    return(col_name)
  })
}


format_data <- function(data) {
  data %>%
    select(
      tax_year,
      YAG,
      provider_country_name,
      provider_region_name,
      provider_name,
      provider_type,
      cah2_subject_name,
      characteristic_type,
      characteristic_value,
      grads,
      grads_uk, # Should we be reporting grads_uk or grads_matched as graduates used? DW: Using grads_uk as is described in data_guidance.txt for publications.
      grads_matched,
      sust_emp_with_or_without_fs,
      sust_emp_only,
      fs_with_or_without_sust_emp,
      fs_only,
      no_sust_dest,
      activity_not_captured,
      grads_earnings_include,
      earnings_lower:earnings_upper,
      earnings_adjusted_lower:earnings_adjusted_upper
    ) %>%
    mutate(
      across(earnings_lower:earnings_upper, format_earnings),
      across(earnings_adjusted_lower:earnings_adjusted_upper, format_earnings, adjusted = TRUE),
      across(sust_emp_with_or_without_fs:activity_not_captured, format_outcome),
      across(c(grads:grads_matched, grads_earnings_include), format_count),
      across(c(characteristic_type, characteristic_value), get_var_names, var_lookup)
    ) %>%
    rename_with(generate_column_name)
}
