valuebox_panel <- function() {
  tabPanel(
    value = "headline",
    "Headline Figures",

    # Define UI for application that draws a histogram

    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          # h1("Headline Figures"),
        )
      ),
      div(
        class = "govuk-details",
        gov_row(
          column(
            width = 4,
            selectInput(
              inputId = "headlineTaxYear",
              label = "Tax year",
              choices = choicesTaxYear,
              selected = "2022/2023",
              width = "100%",
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "headlineYAG",
              label = "Years after graduation",
              choices = choicesYAG,
              selected = 5,
              width = "100%",
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "headlineGeog",
              label = "Provider or Aggregation",
              choices = choicesProviderGeog,
              selected = "HEI",
              width = "100%",
            )
          ),
        )
      ),
      bslib::layout_column_wrap(
        width = 1 / 3,
        bslib::value_box(
          title = "Median Earnings",
          value = textOutput("boxMedianEarnings"),
          theme = vb_theme
        ),
        bslib::value_box(
          title = "Graduates in sustained employment or further study",
          value = textOutput("boxSustEmpOrFS"),
          theme = vb_theme
        ),
        bslib::value_box(
          title = "Graduates in further study",
          value = textOutput("boxFSWithWithout"),
          theme = vb_theme
        ),
        bslib::value_box(
          title = "Graduates in sustained employment",
          value = textOutput("boxSustEmpWithWithout"),
          theme = vb_theme
        ),
        bslib::value_box(
          title = "Graduates with no sustained destination",
          value = textOutput("boxNoSustDest"),
          theme = vb_theme
        ),
        bslib::value_box(
          title = "Graduates in sustained employment without further study",
          value = textOutput("boxSustEmpOnly"),
          theme = vb_theme
        ),
      )
    )
  )
}
