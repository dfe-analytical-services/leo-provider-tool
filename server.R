# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your
# app.
#
# Anything you create in the server file won't appear in your app until you call
# it in the UI file. This server script gives an example of a plot and value box
# that updates on slider input. There are many other elements you can add in
# too, and you can play around with their reactivity. The "outputs" section of
# the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {
  # Loading screen -------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts"
  ))


  # Cookies ---------------------------------------------------------------------------------------------------------
  # Cookies server side code from https://dfe-analytical-services.github.io/dfeshiny/articles/implementing-cookies.html


  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })




  # User guide links ------------------------------------------------------------------------------------------------

  observeEvent(input$headlineLink, {
    updateTabsetPanel(session, "navlistPanel", selected = "headline")
  })

  observeEvent(input$dashboardLink, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  observeEvent(input$outcomeLink, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dashboardPanels", selected = "outcomes")
  })

  observeEvent(input$earningsLink, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dashboardPanels", selected = "earnings")
  })

  observeEvent(input$datatableLink, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dashboardPanels", selected = "datatable")
  })


  #  output$cookie_status <- renderText(as.character(input$cookies))


  # Reactivity for characteristic values ----------------------------------------------------------------------------

  # The characteristic value options depend on the characteristic type selected, so we need to update the options
  # whenever the characteristic type is opened or closed

  observeEvent(input$selectCharType, {
    charType <- input$selectCharType
    print(charType)

    charVals <- choicesCharValueTable %>%
      filter(filter_name == charType) %>%
      pull(filter_value)

    print(charVals)
    charVals <- intersect(c(choicesCharValue), c(charVals))
    print(charVals)
    charVals <- c("All graduates", charVals)
    names(charVals) <- get_var_names(charVals)

    updateSelectizeInput(session, "selectCharValue", choices = charVals)

    if (charType == "All graduates") {
      updateSelectizeInput(session, "selectCharValue", selected = "All graduates")
    }
  })

  # Filtering -------------------------------------------------------------------------------------------------



  ## This function is called on to separate the user-selected 'provider Geography'
  ## in to the corresponding provider_country, provider_region_name and provider_name variables in the underlying data.

  disaggGeog <- function(Geography) {
    # First need to separate out the selected Geographies in to Country, Region, Name:
    # if the intersection of the selected Geographies with the allowed values (of Country, Region, Name) is empty, 'empty' is assigned to the variable.
    # Otherwise the intersection is assigned
    # When it comes to using the function and retrieving the data from the sql table:
    # If 'empty' is assigned then the filter in the call to sql will find no values of provider_country (_region, _name) = 'empty'
    # An 'or' statement is used in the sql filter, but actually this alone would return too many row; need to additionally specify the implied values of the lower geographies

    # print(Geography)

    providerRegion <- if (is_empty(intersect(c(choicesProviderRegion), c(Geography)))) {
      "empty"
    } else {
      intersect(c(choicesProviderRegion), c(Geography))
    }

    providerType <- if (is_empty(intersect(c(choicesProviderType), c(Geography)))) {
      "empty"
    } else {
      intersect(c(choicesProviderType), c(Geography))
    }

    providerName <- if (is_empty(intersect(c(choicesProviderName), c(Geography)))) {
      "empty"
    } else {
      intersect(c(choicesProviderName), c(Geography))
    }

    providerCountry <- if (is_empty(intersect(c(choicesProviderCountry), c(Geography)))) {
      "empty"
    } else {
      intersect(c(choicesProviderCountry), c(Geography))
    }

    # Return the outputs as a list
    return(list(region = providerRegion, type = providerType, name = providerName, country = providerCountry))
  }





  generate_colour_choices <- function(data) {
    # This makes it possible to separate filters that are varying (to use in x-axis label) from those that are constant (to use in graph title)
    variable_counts <- data %>%
      select(tax_year, provider_country_name, provider_geog, YAG, cah2_subject_name, characteristic_type, characteristic_value) %>%
      mutate(across(everything(), ~ n_distinct(.))) %>%
      unique()


    # n.b. need to add a constant (tax_year), for when only one category varying. We only want cases where there are 6 or less options, as we only have 6 colours
    varying <- variable_counts %>%
      select(where(~ any(. > 1 & . <= 6))) %>%
      colnames()



    choices <- intersect(varying, choicesColGrouping)



    names(choices) <- get_var_names(choices)

    choices <- append(choices, c(`No Colour Grouping` = "noGroup"), after = 0)

    return(choices)
  }




  # Apply filters ---------------------------------------------------------------------------------------------------





  # This will be run anytime the apply filters button is pressed.
  selected_data_ <- eventReactive(
    input$apply_filters,
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    {
      print(input$selectTaxYear)
      print(input$selectYAG)
      print(input$selectProviderCountry)
      print(input$selectProviderGeography)
      print(input$selectSubject)
      print(input$selectCharValue)

      if (!(is_empty(input$selectTaxYear) | is_empty(input$selectYAG) | is_empty(input$selectProviderCountry) | is_empty(input$selectProviderGeography) |
        is_empty(input$selectSubject) | is_empty(input$selectCharValue))) {
        taxYear <- input$selectTaxYear
        YAG_ <- input$selectYAG
        country <- input$selectProviderCountry
        subject <- input$selectSubject
        charType <- input$selectCharType
        charVal <- input$selectCharValue
        geography <- input$selectProviderGeography

        providerGeographies <- disaggGeog(geography)

        data <- tbl(con, "LEO_data") %>%
          filter(
            tax_year %in% taxYear,
            YAG %in% YAG_,
            provider_country_name %in% country,
            cah2_subject_name %in% subject,
            characteristic_type %in% c("All graduates", charType),
            characteristic_value %in% charVal
          ) %>%
          mutate(provider_geog = case_when(
            provider_name != "Total" ~ provider_name,
            provider_name == "Total" & provider_region_name == "Total" ~ provider_type,
            provider_name == "Total" & provider_region_name != "Total" ~ provider_region_name,
            TRUE ~ "error"
          )) %>%
          filter(
            (provider_region_name %in% providerGeographies$region & provider_name == "Total" |
              provider_type %in% providerGeographies$type & provider_name == "Total" & provider_region_name == "Total" |
              provider_name %in% providerGeographies$name)
          ) %>%
          collect() %>%
          mutate(
            YAG = as.character(YAG),
            characteristic_type = if_else(characteristic_value == "All graduates", "All graduates", characteristic_type)
          )

        choices <- generate_colour_choices(data)
        updateSelectizeInput(inputId = "selectOutcomeColGrouping", choices = choices, selected = "noGroup")
        updateSelectizeInput(inputId = "selectEarningsColGrouping", choices = choices, selected = "noGroup")
        print(data)
        data
      }
    }
  )

  # We want the missing combinations to work even if the selected data is empty, so we user a wrapper reactive for graphs.
  selected_data <- reactive({
    # If there is no data, anything that depends on it will instead show this message.
    validate(need(nrow(selected_data_()) > 0, message = "No data found for any selected combination."))

    selected_data_()
  })

  ## Find missing combinations ---------------------------------------------------------------------------------------

  # The user may have selected filter combinations that don't have any data. We need to identify these so the user can see
  # what data is missing. This uses selected_data_, which does not have the validation check, so it continues to work if
  # there are no combinations. This allows the user to see what the dashboard is actually looking for.

  # Sometimes these missing combinations will be expected, so we shouldn't block use if they exist.
  # For example, the user may want the totals for Scotland, Wales, and the regions of england, which would lead to many missing combinations,
  # while still being a reasonable query.

  missing_combinations <- eventReactive(input$apply_filters,
    ignoreNULL = FALSE,
    ignoreInit = FALSE,
    {
      taxYear <- input$selectTaxYear
      YAG_ <- input$selectYAG
      country <- input$selectProviderCountry
      subject <- input$selectSubject
      charType <- input$selectCharType
      charVal <- input$selectCharValue
      geography <- input$selectProviderGeography

      providerGeographies <- disaggGeog(geography)

      all_combs <- crossing(
        tax_year = taxYear,
        YAG = as.character(YAG_),
        provider_country_name = country,
        cah2_subject_name = subject,
        characteristic_type = charType,
        characteristic_value = charVal,
        provider_geog = geography
      ) %>%
        mutate(characteristic_type = if_else(characteristic_value == "All graduates", "All graduates", characteristic_type))

      if (nrow(selected_data_()) == 0) {
        missing_combs <- all_combs
      } else {
        missing_combs <- anti_join(all_combs, selected_data_(), by = c(
          "tax_year",
          "YAG",
          "provider_country_name",
          "cah2_subject_name",
          "characteristic_type",
          "characteristic_value",
          "provider_geog"
        ))
      }
      if (nrow(missing_combs) > 0) {
        showTab("dashboardPanels", "missingCombs", select = FALSE)
        shinyjs::show(id = "missingCombs")
      } else {
        hideTab("dashboardPanels", "missingCombs")
        shinyjs::hide(id = "missingCombs")
      }

      missing_combs
    }
  )

  output$filterRequirements <- renderText({
    # In order to get missing_combinations to run every time, we need to have a dependency somewhere that isn't hidden.
    # Hence the dependency here, to ensure it is recomputed every time.
    missing_combinations()
    paste0("A maximum of ", max_filters, " filter combinations will be graphed. You have: ", nrow(selected_data()), " filter combinations. (Does not affect data table)")
  })

  output$missingCombs <- renderUI({
    num_missing_combs <- nrow(missing_combinations())
    tagList(
      paste0("There are ", num_missing_combs, " selected combinations that do not appear in the data. "),
      actionLink("missingCombLink", "See missing combinations", style = "color:white;")
    )
  })

  observeEvent(input$missingCombLink, {
    updateTabsetPanel(session, "dashboardPanels", selected = "missingCombs")
  })

  output$missingCombsData <- renderDataTable({
    data <- missing_combinations() %>%
      rename_with(generate_column_name)
    datatable(data,
      options = list(
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE
      )
    )
  })

  # Plots -----------------------------------------------------------------------------------------------------------



  output$colOutcomes <- snapshotPreprocessOutput(
    # if(nrow(outcomes_selected$data) <= length(choicesSubject$filter_value)){
    renderGirafe({
      girafe(
        # ggobj = plotOutcomes(reactiveOutcomesSelected(), input$selectColGrouping),
        ggobj = plotOutcomes(selected_data(), input$selectOutcomeColGrouping, input$selectOutcomeIndicator),
        options = list(opts_sizing(rescale = TRUE, width = 1.0)),
        # width_svg = 5.0,
        # height_svg = 5.0
        # This means the graph can get taller as more options are selected. Still want to figure out left-alignment.
        # The if_else ensures that the adjustment represents the number of bars drawn, not the data in the chart,
        # as bars are limited to length(choicesSubject$filter_value) in plotting.R
        width_svg = 10.0,
        height_svg = 3.0 +
          0.5 * (if_else(nrow(selected_data()) > max_filters, max_filters, nrow(selected_data())))
      )
    }),
    function(value) {
      # Removing elements that cause issues with shinytest comparisons when run on
      # different environments - should add to dfeshiny at some point.
      svg_removed <- gsub(
        "svg_[0-9a-z]{8}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{12}",
        "svg_random_giraph_string",
        value
      )
      font_standardised <- gsub("Arial", "Helvetica", svg_removed)
      cleaned_positions <- gsub(
        "x[0-9]*='[0-9.]*' y[0-9]*='[0-9.]*'",
        "Position", font_standardised
      )
      cleaned_size <- gsub(
        "width='[0-9.]*' height='[0-9.]*'",
        "Size", cleaned_positions
      )
      cleaned_points <- gsub("points='[0-9., ]*'", "points", cleaned_size)
      cleaned_points
    }
    # }
  )


  observeEvent(input$earningsWhiskers, {
    if (input$earningsWhiskers) {
      enable("earningsFullWidth")
    } else {
      disable("earningsFullWidth")
    }
  })

  output$colEarnings <- snapshotPreprocessOutput(
    # if(nrow(outcomes_selected$data) <= length(choicesSubject$filter_value)){
    renderGirafe({
      girafe(
        # ggobj = plotOutcomes(reactiveOutcomesSelected(), input$selectColGrouping),
        ggobj = plotEarnings(selected_data(), input$selectEarningsColGrouping, input$earningsFullWidth, input$earningsAdjust, input$earningsWhiskers),
        options = list(opts_sizing(rescale = TRUE, width = 1.0)),
        # width_svg = 5.0,
        # height_svg = 5.0
        # This means the graph can get taller as more options are selected. Still want to figure out left-alignment.
        # The if_else ensures that the adjustment represents the number of bars drawn, not the data in the chart,
        # as bars are limited to length(choicesSubject$filter_value) in plotting.R
        width_svg = 10.0,
        height_svg = 3.0 +
          0.5 * (if_else(nrow(selected_data()) > max_filters, max_filters, nrow(selected_data())))
      )
    }),
    function(value) {
      # Removing elements that cause issues with shinytest comparisons when run on
      # different environments - should add to dfeshiny at some point.
      svg_removed <- gsub(
        "svg_[0-9a-z]{8}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{4}_[0-9a-z]{12}",
        "svg_random_giraph_string",
        value
      )
      font_standardised <- gsub("Arial", "Helvetica", svg_removed)
      cleaned_positions <- gsub(
        "x[0-9]*='[0-9.]*' y[0-9]*='[0-9.]*'",
        "Position", font_standardised
      )
      cleaned_size <- gsub(
        "width='[0-9.]*' height='[0-9.]*'",
        "Size", cleaned_positions
      )
      cleaned_points <- gsub("points='[0-9., ]*'", "points", cleaned_size)
      cleaned_points
    }
    # }
  )


  # Headline Figures Tab --------------------------------------------------------------------------------------------

  headlineFigureTable <- reactive({
    providerGeographies <- disaggGeog(input$headlineGeog)

    # data <- tbl(con, sql(paste("select * from", LEO_data))) %>%
    data <- tbl(con, "LEO_data") %>%
      filter(
        tax_year == input$headlineTaxYear &
          YAG == input$headlineYAG &
          home_region_code == "Total" &
          current_region_code == "Total" &
          cah2_subject_name == "Total" &
          characteristic_type == "All graduates",
        characteristic_value == "All graduates"
      ) %>%
      filter(provider_region_name %in% providerGeographies$region & provider_name == "Total" |
        provider_type %in% providerGeographies$type & provider_name == "Total" & provider_region_name == "Total" |
        provider_name %in% providerGeographies$name) %>%
      collect()

    if (nrow(data) > 1) {
      data <- data %>% filter(provider_country_name == "Total")
    }

    validate(
      need(nrow(data) > 0, "Data Missing"),
      need(nrow(data) < 2, "Mutiple entries found")
    )


    return(data)
  })

  outcomeHeadlineFigure <- function(figure) {
    headlineFigureTable()[[1, figure]] %>%
      format_outcome()
  }

  earningsHeadlineFigure <- function(figure) {
    headlineFigureTable()[[1, figure]] %>%
      format_earnings()
  }

  output$boxMedianEarnings <- renderText({
    earningsHeadlineFigure("earnings_median")
  })

  output$boxNoSustDest <- renderText({
    outcomeHeadlineFigure("no_sust_dest")
  })

  output$boxSustEmpOnly <- renderText({
    outcomeHeadlineFigure("sust_emp_only")
  })

  output$boxSustEmpWithWithout <- renderText({
    outcomeHeadlineFigure("sust_emp_with_or_without_fs")
  })

  output$boxSustEmpOrFS <- renderText({
    outcomeHeadlineFigure("sust_emp_fs_or_both")
  })

  output$boxFSWithWithout <- renderText({
    outcomeHeadlineFigure("fs_with_or_without_sust_emp")
  })


  # Datatable tab ---------------------------------------------------------------------------------------------------

  # Download the underlying data button
  output$downloadData <- downloadHandler(
    filename = "LEO_providers_underlying_data.csv",
    content = function(file) {
      write.csv(selected_data(), file)
    }
  )

  ## This creates the data table, in line with user filter selections

  output$tabData <- renderDataTable({
    datatable(format_data(selected_data()),
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            # Row numbers
            targets = 0,
            width = "3ch",
            className = "dt-body-left"
          ),
          list(
            # Group labels
            targets = 1:9,
            width = "10ch"
          ),
          list(
            # Outcomes grad counts
            targets = 10:12,
            width = "10ch",
            className = "dt-body-right"
          ),
          list(
            # Outcomes indicators
            targets = 13:18,
            width = "10ch",
            className = "dt-body-right"
          ),
          list(
            # Earnings grad count
            targets = 19,
            width = "10ch",
            className = "dt-body-right"
          ),
          list(
            # Earnings values
            targets = 20:29,
            width = "10ch",
            className = "dt-body-right"
          ),
          list(
            targets = "_all",
            className = "dt-head-center"
          )
        )
      )
    )
  })


  # Stop app -------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
