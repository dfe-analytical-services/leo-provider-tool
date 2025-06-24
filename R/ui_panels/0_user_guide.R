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
                    You can filter by Tax Year, Years after graduation, and provider/total"),
                  h3(actionLink("dashboardLink", "Explore the data")),
                  p(
                    "The 'Explore the data' page contains the main plots of interest. You can filter by",
                    tags$ul(
                      tags$li("Tax year"),
                      tags$li("Years after graduation"),
                      tags$li("Provider country (England, Scotland, Wales)"),
                      tags$li("Provider or aggregation"),
                      tags$li("Subject studied (CAH2)")
                    ),
                    "You can choose any combination and amount of these filters, to compare between cohorts.
                    You can additionally filter by a single graduate characteristic, namely",
                    tags$ul(
                      tags$li("Sex"),
                      tags$li("Ethnicity"),
                      tags$li("POLAR4 Quintile"),
                      tags$li("Prior Attainment")
                    ),
                    "choosing any combination of characteristic values. The 'Explore the data' page has 3 subsections, which alter the presentation of the data"
                  ),
                  h4(actionLink("outcomeLink", "Outcomes")),
                  p(
                    "Outcomes presents the proportion of graduates in each filter combination that achieved a specific outcome. The outcomes measured are",
                    tags$ul(
                      tags$li("No sustained destination"),
                      tags$li("Sustained employment only"),
                      tags$li("Sustained employment with and without further study"),
                      tags$li("Sustained employment or further study"),
                      tags$li("Further study with or without sustained employment"),
                      tags$li("Further study only")
                    ),
                    "see ", a("the methodology of the underlying publication", href = "https://explore-education-statistics.service.gov.uk/methodology/graduate-outcomes-leo-provider-level-data-methodology#content-section-2-content-1"),
                    "for a full definition of each of these outcomes."
                  ),
                  h4(actionLink("earningsLink", "Earnings")),
                  p("Earnings presents a boxplot of graduates earnings. It shows the minimum and maximum earnings belonging to the group as boxplot whiskers,
                    the upper and lower quartiles as the central box, and the median as a line inside this box."),
                  p("Due to the nature of the data, the maximum earnings is often very large when compared with the median and the quartiles. In this case, an arrow is plotted with the exact earnings amount above it.
                    The checkbox 'Show full width of graph' disables this behaviour, and shows the maximum earnings on the axis."),
                  p("The earnings tab additionally has the option to adjust the earnings statistics.
                    The adjusted earnings is calculated by weighting each university’s graduates so that the distribution of graduates
                    from that HE institution is the same as that nationally. This can be toggled on or off with a checkbox.
                    Note that adjusted earnings have been only been computed for individual providers, and not for any aggregations."),
                  h4(actionLink("datatableLink", "Data Table")),
                  p("Data Table presents the data in a table, for direct browsing. It also provides a download button, which downloads the data corresponding to the selected filters for further analysis.")
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
