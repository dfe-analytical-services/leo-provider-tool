generate_tooltip_body <- function(varying, combined_varying_filter) {
  values <- strsplit(combined_varying_filter, split = "@")[[1]]
  tooltip <- generate_labels(varying, values, "\n")
  return(tooltip)
}

generate_outcomes_tooltip <- function(varying, combined_varying_filter, indicator, value, sample_size) {
  tooltips <- character(length(combined_varying_filter))
  combined_varying_filter <- as.character(combined_varying_filter)
  for (idx in seq_along(combined_varying_filter)) {
    tooltip <- paste0(
      get_var_name(indicator), ": ", format_outcome(value[[idx]]), "\n",
      "from ", format_count(sample_size[[idx]]), " graduates\n\n"
    )
    tooltip <- paste0(tooltip, generate_tooltip_body(varying, combined_varying_filter[[idx]]))
    tooltips[idx] <- tooltip
  }
  return(tooltips)
}



generate_earnings_tooltip <- function(varying, combined_varying_filter, earnings_list, adjusted, sample_size) {
  tooltips <- character(length(combined_varying_filter))
  combined_varying_filter <- as.character(combined_varying_filter)
  earnings_vars <- names(earnings_list[[1]])
  for (idx in seq_along(combined_varying_filter)) {
    earnings_list_ <- earnings_list[[idx]]
    values <- strsplit(combined_varying_filter[[idx]], split = "@")[[1]]
    tooltip <- if_else(adjusted, "Adjusted by Region\n", "")
    for (var_idx in seq_along(earnings_list_)) {
      tooltip <- paste0(tooltip, get_var_name(earnings_vars[[var_idx]]), ": ", format_earnings(earnings_list_[[var_idx]]), "\n")
    }
    tooltip <- paste0(tooltip, "from ", format_count(sample_size[[idx]]), " graduates\n\n")
    tooltip <- paste0(tooltip, generate_tooltip_body(varying, combined_varying_filter[[idx]]))
    tooltips[idx] <- tooltip
  }
  return(tooltips)
}







generate_label_string <- function(label, value, end_str = "") {
  label_formatted <- get_var_name(label)
  unit <- get_var_unit(label)

  # If the value is in the lookup, use the version from the lookup. Don't throw a warning if it isn't though.
  value <- get_var_name(value, expect_name = FALSE)

  out <- paste0(label_formatted, ": ", value, unit, end_str)
}

generate_labels <- function(labels, values, sep, pre = "") {
  label <- pre
  for (idx in seq_along(values)) {
    if (labels[[idx]] == "provider_geog") {
      labels[[idx]] <- values[[idx + 1]]
    } else if (labels[[idx]] == "provider_geog_type") {
      next
    } else if (labels[[idx]] == "characteristic_type") {
      labels[[idx + 1]] <- values[[idx]]
      next
    }
    if (values[[idx]] %in% c("Total", "All graduates")) {
      next
    }
    label <- paste0(label, generate_label_string(labels[[idx]], values[[idx]]), sep)
  }

  # We'll have a trailing `sep` at the end of this string, remove it.
  label <- substr(label, 1, nchar(label) - nchar(sep))
  if (label == "") {
    label <- "Total"
  }

  return(label)
}


generate_subtitle <- function(constants, temp_data) {
  constants_values <- temp_data[1, constants]

  title <- generate_labels(constants, constants_values, sep = "\n")

  return(title)
}

generate_bar_labels <- function(temp_data, varying) {
  labels <- character(nrow(temp_data))
  for (idx in seq_along(labels)) {
    varying_values <- temp_data[idx, varying]
    labels[idx] <- generate_labels(varying, varying_values, sep = "\n")
  }

  return(labels)
}

plotOutcomes <- function(outcomes_selected, colourGrouping, indicator) {
  # First reconstitute the provider 'geography' in to a single variable.
  outcomes_selected <- outcomes_selected %>%
    mutate(
      provider_geog = case_when(
        provider_name != "Total" ~ provider_name,
        provider_name == "Total" & provider_region_name == "Total" ~ provider_type,
        provider_name == "Total" & provider_region_name != "Total" ~ provider_region_name,
        TRUE ~ "error"
      ),
      provider_geog_type = case_when(
        provider_name != "Total" ~ "provider_name",
        provider_name == "Total" & provider_region_name == "Total" ~ "provider_type",
        provider_name == "Total" & provider_region_name != "Total" ~ "provider_region_name",
        TRUE ~ "error"
      )
    )

  # This makes it possible to separate filters that are varying (to use in x-axis label) from those that are constant (to use in graph title)
  variable_counts <- outcomes_selected %>%
    select(tax_year, provider_country_name, provider_geog, provider_geog_type, YAG, cah2_subject_name, characteristic_type, characteristic_value) %>%
    mutate(across(everything(), ~ n_distinct(.))) %>%
    unique()

  # n.b. need to add a constant (tax_year), for when only one category varying/constant
  varying <- variable_counts %>%
    select(where(~ any(. > 1))) %>%
    colnames()

  # The label generation code relies on the provider geog type being directly after the provider geography.
  # If both vary, this is already the case, but if the user has selected multiple geogs with the same geog type,
  # the geog type does not get added to varying by the above code. Hence we need to make sure it is there.
  if ("provider_geog" %in% varying & !"provider_geog_type" %in% varying) {
    index <- which("provider_geog" == varying)[[1]]
    varying <- append(varying, "provider_geog_type", after = index)
  }

  # Similarly, we need to make sure characteristic type is there whenever characteristic value is
  if ("characteristic_value" %in% varying & !"characteristic_type" %in% varying) {
    index <- which("characteristic_value" == varying)[[1]]
    varying <- append(varying, "characteristic_type", after = index - 1)
  }

  constant <- setdiff(colnames(variable_counts), varying)

  if (length(varying) == 0) {
    varying <- "tax_year"
  }

  temp_data <- outcomes_selected %>%
    # The @ symbol is arbitrary, it's removed before we present anything to the user. It has to be a character that's not in any of the field names
    # TODO: Can I use a list instead of a string?
    unite("combined_varying_filter", varying, sep = "@", remove = FALSE) %>%
    unite("combined_constant_filter", constant, sep = "@", remove = FALSE)

  # This means that a maximum of max_filters bars are plotted
  temp_data <- slice_head(temp_data, n = case_when(
    nrow(temp_data) < max_filters ~ nrow(temp_data),
    TRUE ~ max_filters
  ))

  temp_data <- temp_data %>%
    mutate(
      ind_val = !!sym(indicator),
      text = if_else(is.na(ind_val), "Outcomes Missing. Please report this error using the feedback form linked above", format_outcome(ind_val, naDefault = TRUE))
    )

  xlim_upper <- round_any(max(as.numeric(temp_data$ind_val), na.rm = TRUE), 20, ceiling)

  if (xlim_upper < 0) {
    xlim_upper <- 100
  }

  temp_data <- temp_data %>%
    mutate(
      across(activity_not_captured:fs_only, parseOutcomes),
      combined_varying_filter = forcats::fct_inorder(combined_varying_filter),
      # It doesn't matter what we set noGroup to, it's only used to make sure there is a known constant across temp_data
      noGroup = ""
    )

  bar_labels <- generate_bar_labels(temp_data, varying)
  ggplot(temp_data, aes(
    x = combined_varying_filter,
    y = !!sym(indicator),
    fill = !!sym(colourGrouping),
    group = combined_varying_filter,
    tooltip = generate_outcomes_tooltip(varying, combined_varying_filter, indicator, !!sym(indicator), grads_uk)
  )) +
    geom_col_interactive() +
    # No data text for missing data
    geom_text(aes(y = 0, label = text), hjust = 0, vjust = 0.5, size = 12 / .pt, na.rm = TRUE) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x.bottom = element_text(
        angle = 0, vjust = 0.5,
        margin = margin(r = 12)
      ),
      # We don't need an axis title at the top as it's next to the plot title
      axis.title.x.top = element_blank(),
      # The y axis doesn't need a title as it's just the groups
      axis.title.y = element_blank(),
      axis.line = element_line(size = 0.75),
      legend.position = "none"
    ) +
    scale_y_continuous(
      limits = c(0, xlim_upper),
      labels = label_percent,
      sec.axis = dup_axis()
    ) +
    scale_x_discrete(
      labels = bar_labels
    ) +
    labs(
      title = get_var_name(indicator),
      subtitle = generate_subtitle(constant, temp_data),
      y = get_var_name(indicator)
    ) +
    # xlab("TBC") +
    # ylab(str_wrap("Percent in sustained employment with or without further study", 12)) +
    scale_fill_manual(
      # We don't care which of the colours the pallette uses, so we can just unname it and let ggplot choose
      values = unname(gss_colour_pallette)
    ) +
    coord_flip()
}



plotEarnings <- function(earnings_selected, columnGrouping, fullWidth, adjust, whiskers) {
  # First reconstitute the provider 'geography' in to a single variable. Also track what type of geography it is, so we know what label to use.
  earnings_selected <- earnings_selected %>%
    mutate(
      provider_geog = case_when(
        provider_name != "Total" ~ provider_name,
        provider_name == "Total" & provider_region_name == "Total" ~ provider_type,
        provider_name == "Total" & provider_region_name != "Total" ~ provider_region_name,
        TRUE ~ "error"
      ),
      provider_geog_type = case_when(
        provider_name != "Total" ~ "provider_name",
        provider_name == "Total" & provider_region_name == "Total" ~ "provider_type",
        provider_name == "Total" & provider_region_name != "Total" ~ "provider_region_name",
        TRUE ~ "error"
      )
    )


  if (adjust) {
    # Is there a better way to do this?
    # DW 25/3/25 There probably is, but this does the job in a reasonable way.
    # We just need to make sure we check the adjust variable whenever we need to
    earnings_selected <- earnings_selected %>%
      mutate(
        earnings_lower = earnings_adjusted_lower,
        earnings_LQ = earnings_adjusted_LQ,
        earnings_median = earnings_adjusted_median,
        earnings_UQ = earnings_adjusted_UQ,
        earnings_upper = earnings_adjusted_upper
      )
  }



  # This makes it possible to separate filters that are varying (to use in x-axis label) from those that are constant (to use in graph title)
  variable_counts <- earnings_selected %>%
    select(tax_year, provider_country_name, provider_geog, provider_geog_type, YAG, cah2_subject_name, characteristic_type, characteristic_value) %>%
    mutate(across(everything(), ~ n_distinct(.))) %>%
    unique()

  # n.b. need to add a constant (tax_year), for when only one category varying/constant
  varying <- variable_counts %>%
    select(where(~ any(. > 1))) %>%
    colnames()



  # The label generation code relies on the provider geog type being directly after the provider geography.
  # If both vary, this is already the case, but if the user has selected multiple geogs with the same geog type,
  # the geog type does not get added to varying by the above code. Hence we need to make sure it is there.
  if ("provider_geog" %in% varying & !"provider_geog_type" %in% varying) {
    index <- which("provider_geog" == varying)[[1]]
    varying <- append(varying, "provider_geog_type", after = index)
  }

  # Similarly, we need to make sure characteristic type is there whenever characteristic value is
  if ("characteristic_value" %in% varying & !"characteristic_type" %in% varying) {
    index <- which("characteristic_value" == varying)[[1]]
    varying <- append(varying, "characteristic_type", after = index - 1)
  }

  constant <- setdiff(colnames(variable_counts), varying)

  if (length(varying) == 0) {
    varying <- "tax_year"
  }

  temp_data <- earnings_selected %>%
    unite("combined_varying_filter", varying, sep = "@", remove = FALSE) %>%
    unite("combined_constant_filter", constant, sep = "@", remove = FALSE)

  # This means that a maximum of max_filters bars are plotted
  temp_data <- slice_head(temp_data, n = case_when(
    nrow(temp_data) < max_filters ~ nrow(temp_data),
    TRUE ~ max_filters
  ))

  temp_data <- temp_data %>%
    mutate(row_num = row_number()) %>%
    rowwise() %>%
    mutate(
      earnings_list = list(c(
        earnings_lower = earnings_lower,
        earnings_LQ = earnings_LQ,
        earnings_median = earnings_median,
        earnings_UQ = earnings_UQ,
        earnings_upper = earnings_upper
      ))
    )

  if (!whiskers) {
    temp_data <- temp_data %>%
      mutate(
        earnings_lower = earnings_LQ,
        earnings_upper = earnings_UQ
      )
  }


  earnings_upper_no_na <- na.omit(as.numeric(temp_data$earnings_upper))
  earnings_UQ_no_na <- na.omit(as.numeric(temp_data$earnings_UQ))

  # If fullWidth is not selected:
  # If the earnings_upper is above £200,000, use a max of £200,000. However, if this causes an upper quartile to be withing £10,000 of the xlim, use UQ + £10,000 instead.
  # If fullWidth is selected:
  # Use the maximum earnings_upper
  # In both cases, make sure the xlim is at least £10,000
  xlim_upper <- max(if_else(fullWidth, max(earnings_upper_no_na), max(min(max(earnings_upper_no_na), 200000), earnings_UQ_no_na + 10000)), 10000)


  temp_data <- temp_data %>%
    mutate(
      text = case_when(
        "c" %in% earnings_list & adjust ~ "Adjusted Earnings Suppressed",
        "c" %in% earnings_list & !adjust ~ "Earnings Suppressed",
        "x" %in% earnings_list & adjust ~ "Adjusted Earnings Not Computed (Only computed for individual providers)",
        any(is.na(earnings_list)) ~ "Earnings Missing. Please report this error using the feedback form linked above",
        .default = NA
      ),
      across(earnings_lower:earnings_upper, parseEarnings),
      arrow = ifelse(earnings_upper > xlim_upper, row_num, NA),
      arrow_val = ifelse(earnings_upper > xlim_upper, earnings_upper, NA),
      earnings_upper = ifelse(earnings_upper > xlim_upper, xlim_upper, earnings_upper),
      across(earnings_lower:earnings_upper, ~ replace(., . < 0, NA)),
      combined_varying_filter = forcats::fct_inorder(combined_varying_filter),
    ) %>%
    ungroup() %>%
    mutate(
      across(earnings_lower:earnings_upper, as.numeric),
      noGroup = "",
      # Variable to fill by
      fill = !!sym(columnGrouping),
      # Colour to use as fill. This means we can use gss_colour_pallettte directly in scale_fill_manual
      fill = names(gss_colour_pallette)[dense_rank(fill)],
      # Colour for the median, Can be 'dark' or 'light'
      med_colour = contrast_colour_pallette[fill]
    )

  bar_labels <- generate_bar_labels(temp_data, varying)
  plt <- ggplot(temp_data, aes(
    y = combined_varying_filter,
    xmin = earnings_lower,
    xlower = earnings_LQ,
    xmiddle = earnings_median,
    xupper = earnings_UQ,
    xmax = earnings_upper,
    fill = fill,
    group = combined_varying_filter,
    tooltip = generate_earnings_tooltip(varying, combined_varying_filter, earnings_list, adjust, grads_earnings_include)
  )) +
    # The actual data
    # First plot the contrasting median line
    geom_boxplot_interactive(aes(colour = med_colour), stat = "identity", na.rm = TRUE) +
    # Plot the outline and whiskers on top.
    geom_boxplot_interactive(stat = "identity", colour = "black", fatten = NULL, fill = NA, na.rm = TRUE) +
    # Median line colour mapping
    scale_colour_manual(values = c(light = "white", dark = "black")) +
    # Arrow in case the maximum is off the plot
    geom_segment(aes(x = xlim_upper - 1000, xend = xlim_upper, y = arrow, yend = arrow), arrow = arrow(type = "closed", length = unit(0.15, "inches")), na.rm = TRUE) +
    # Label for the arrow
    geom_text(aes(x = xlim_upper, y = arrow, label = ifelse(!is.na(arrow), label_pounds(arrow_val), NA)), nudge_y = 0.25, na.rm = TRUE) +
    # No data text for missing data
    geom_text(aes(x = 0, label = text), hjust = 0, vjust = 0.5, size = 12 / .pt, na.rm = TRUE) +
    labs(
      x = "Earnings",
      title = "Earnings Distribution",
      subtitle = generate_subtitle(constant, temp_data)
    ) +
    scale_x_continuous(
      labels = label_pounds,
      sec.axis = dup_axis()
    ) +
    # We need clip off so the text labels on the arrows are fully visible
    coord_cartesian(xlim = c(0, xlim_upper), clip = "off") +
    scale_y_discrete(
      na.translate = FALSE,
      labels = bar_labels
    ) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.title.x.bottom = element_text(),
      axis.title.x.top = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_line(size = 0.75),
      legend.position = "none"
    ) +
    scale_fill_manual(
      # Previously defined which colours are being used, so we can simply lookup the colour name in the pallette.
      values = gss_colour_pallette
    )

  plt
}
