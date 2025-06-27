dashboard_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Explore the data",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          # h1("Main Dashboard"),
        ),
        column(
          width = 12,

          # Filter Selection Box --------------------------------------------------------------------------------------------

          expandable(
            inputId = "details", label = "Select your filters and then click 'Apply my selections'",
            contents =
              div(
                id = "div_a",
                gov_row(
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectTaxYear",
                      label = "Tax year",
                      choices = choicesTaxYear,
                      selected = default_tax_year,
                      multiple = TRUE,
                      options = list("actions-box" = TRUE),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectYAG",
                      label = "Years after graduation",
                      choices = choicesYAG,
                      selected = default_YAG,
                      multiple = TRUE,
                      options = list("actions-box" = TRUE),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectProviderCountry",
                      label = "Provider country",
                      choices = choicesProviderCountry,
                      selected = default_provider_country,
                      multiple = TRUE,
                      options = list("actions-box" = TRUE),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectProviderGeography",
                      label = "Provider or aggregation",
                      choices = choicesProviderGeog,
                      selected = default_provider_geog,
                      multiple = TRUE,
                      options = list(
                        "actions-box" = TRUE,
                        "live-search" = TRUE
                      ),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  )
                ),
                gov_row(
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectSubject",
                      label = "Subject studied",
                      choices = choicesSubject,
                      selected = default_cah2_subject_name,
                      multiple = TRUE,
                      options = list(
                        "actions-box" = TRUE,
                        "live-search" = TRUE
                      ),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectCharType",
                      label = "Characteristic type",
                      choices = choicesCharType,
                      selected = default_characteristic_type,
                      multiple = TRUE,
                      options = list("max-options" = 1),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    class = "fixed-height",
                    pickerInput(
                      inputId = "selectCharValue",
                      label = "Characteristic value",
                      choices = "All graduates",
                      selected = "All graduates",
                      multiple = TRUE,
                      options = list("actions-box" = TRUE),
                      choicesOpt = NULL,
                      width = "100%",
                      inline = FALSE
                    )
                  ),
                ),
                gov_row(
                  column(
                    width = 6,
                    button_Input("apply_filters", "Apply my selections")
                  )
                ), # this is the gov_row bracket!
                gov_row(
                  column(
                    width = 12,
                    textOutput("filterRequirements")
                  )
                ),
                gov_row(
                  column(
                    width = 12,
                    htmlOutput("missingCombs"),
                  )
                )
              ) # this is the div bracket
          ), # this is the expandable bracket
        ), # this is the column width 12 bracket!
        column(
          width = 12,
          tabsetPanel(
            id = "dashboardPanels",
            tabPanel(

              # Outcomes Tab ----------------------------------------------------------------------------------------------------
              value = "outcomes",
              "Outcomes",
              column(
                width = 12,
                h2("Graduate outcomes across selected groups"),
                p("Hover over each bar for exact values")
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "selectOutcomeIndicator",
                  label = "Select outcome indicator to be graphed:",
                  choices = choicesOutcomesIndicator,
                  selected = "sust_emp_with_or_without_fs",
                  width = "100%"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "selectOutcomeColGrouping",
                  label = "Select variable to be colour-grouped:",
                  choices = c(`No Colour Grouping` = "noGroup"),
                  selected = "noGroup",
                  width = "100%"
                )
              ),
              column(
                width = 12,
                withSpinner(
                  girafeOutput("colOutcomes",
                    width = "100%", height = "100%"
                  ),
                  proxy.height = 400
                )
              )
            ),
            # Earnings Tab ----------------------------------------------------------------------------------------------------

            tabPanel(
              value = "earnings",
              "Earnings",
              column(
                width = 12,
                h2("Earnings Distribution across selected groups"),
                p("Hover over the boxplot for exact values")
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "selectEarningsColGrouping",
                  label = "Select variable to be colour-grouped:",
                  choices = c(`No Colour Grouping` = "noGroup"),
                  selected = "noGroup",
                  width = "100%"
                )
              ),
              column(
                width = 6,
                checkboxInput(
                  inputId = "earningsWhiskers",
                  label = span("Show boxplot whiskers", tipify(icon("circle-question"), title = "Show the maximum and minimum earnings corresponding to each group")),
                  value = FALSE
                ),
                checkboxInput(
                  inputId = "earningsFullWidth",
                  label = span("Show full width of graph", tipify(icon("circle-question"), title = "The maximum earnings does not fit on a reasonable graph scale. This will expand the graph scale to fully show maximum earnings")),
                  value = FALSE
                ),
                checkboxInput(
                  inputId = "earningsAdjust",
                  label = span("Apply regional earnings adjustment", tipify(icon("circle-question"), title = "For individual providers, weight each provider's graduates so that the distribution of graduates from that HE institution is the same as that nationally")),
                  value = FALSE
                ),
              ),
              column(
                width = 12,
                withSpinner(
                  girafeOutput("colEarnings",
                    width = "100%", height = "100%"
                  ),
                  proxy.height = 400
                )
              )
            ),
            tabPanel(

              # Data table --------------------------------------------------------------------------------------------------------
              value = "datatable",
              "Data Table",
              br(),
              column(
                width = 6,
                br(),
                download_button(
                  outputId = "downloadData",
                  button_label = "Download selected data",
                  file_type = "CSV",
                  file_size = "less than 20 MB"
                )
              ),
              column(
                width = 12,
                withSpinner(dataTableOutput("tabData"),
                  proxy.height = 400
                )
              ),
            ),
            tabPanel(
              value = "missingCombs",
              "Missing Combinations",
              column(
                width = 12,
                dataTableOutput("missingCombsData")
              )
            )
          )
        )
      )
      # add box to show user input
    )
  )
}
