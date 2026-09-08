homepage_panel <- function() {
  tabPanel(
    "User guide and information",
    gov_main_layout(
      gov_row(
        column(
          12,
          # h1("User Guide and Information"),
          # br(),
          # br()
        ),

        ## Left panel -------------------------------------------------------

        column(
          width = 12,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold;
                background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  h3(actionLink("headlineLink", "Headline Figures")),
                  p("The headline figures presents summary figures, namely the median earnings, and all of the outcomes proportions.
                    You can filter by Tax Year, Years after graduation, and a specific provider or an overall total (country/region/type of provider)."),
                  h3(actionLink("dashboardLink", "Explore the data")),
                  p(
                    "The 'Explore the data' page contains the main plots of interest. You can filter by:",
                    tags$ul(
                      tags$li("Tax year"),
                      tags$li("Years after graduation"),
                      tags$li("Providers and overall totals (totals by country/region of provider, or type of provider. Select up to six)"),
                      tags$li("Subject studied (CAH2 subject level, select up to six)")
                    ),
                    "You can choose any combination of these filters, to compare between cohorts.
                    You can additionally filter by a single graduate characteristic:",
                    tags$ul(
                      tags$li("Sex"),
                      tags$li("Ethnicity"),
                      tags$li("Income Deprivation Affecting Children Index (IDACI) quintile"),
                      tags$li("GCSE prior attainment quintile")
                    ),
                    "You can choose to select all or any combination of values for your chosen characteristic. The 'Explore the data' page has 3 subsections, which alter the presentation of the data."
                  ),
                  h4(actionLink("outcomeLink", "Outcomes")),
                  p(
                    "Outcomes presents the proportion of graduates in each filter combination that achieved a specific outcome. The outcomes measured are:",
                    tags$ul(
                      tags$li("No sustained destination"),
                      tags$li("Sustained employment only"),
                      tags$li("Sustained employment with and without further study"),
                      tags$li("Sustained employment or further study"),
                      tags$li("Further study with or without sustained employment"),
                      tags$li("Further study only")
                    ),
                    "See ", a("the methodology of the underlying publication", href = "https://explore-education-statistics.service.gov.uk/find-statistics/leo-graduate-and-postgraduate-outcomes"),
                    "for a full definition of each of these outcomes."
                  ),
                  h4(actionLink("earningsLink", "Earnings")),
                  p("Earnings presents a boxplot of graduates earnings. The size of the box shows the range in earnings between the lower and upper quartiles, with the median earnings being the line within the box."),
                  p("The earnings tab additionally has the option to adjust the earnings statistics.
                    The adjusted earnings is calculated by weighting each university’s graduates so that the distribution of graduates
                    from that HE institution is the same as that nationally. This can be toggled on or off with a checkbox.
                    Note that adjusted earnings have been only been computed for individual providers, and not for any aggregations."),
                  h4(actionLink("datatableLink", "Data Table")),
                  p(
                    "Data Table presents the data in a table, for direct browsing. It also provides four download buttons, which can be used to download datasets for further analysis. You can download:",
                    tags$ul(
                      tags$li("the data for the selected filters only"),
                      tags$li("for the selected filters but for all providers"),
                      tags$li("for the selected filters but for all subjects"),
                      tags$li("for the selected filters for all providers and all subjects")
                    )
                  )
                ),
                br()
              )
            )
          ),
        ),
      )
    )
  )
}
