# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define
# where they are placed. Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to
# decide what goes in. However, every element should meet accessibility
# requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date
# selections, multiple choice dropdowns etc. Use the shiny cheatsheet to explore
# more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to
# use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#
# ---------------------------------------------------------

ui <- function(input, output, session) {
  fluidPage(
    # use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title("LEO Provider Level Dashboard")
    ),
    use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = dashboard_title,
        description = "LEO Provider Level Dashboard",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "stats development",
        rating = "General",
        referrer = "no-referrer"
      ),
    shinyjs::useShinyjs(),
    custom_disconnect_message(
      links = sites_list,
      publication_name = ees_pub_name,
      publication_link = ees_publication
    ),
    # Setting up cookie consent based on a cookie recording the consent:
    # https://book.javascript-for-r.com/shiny-cookies.html
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "additional_style.css"
      )
    ),
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      name = dashboard_title
    ),
    dfeshiny::header(dashboard_title),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is a new service that we are developing.
        If you have any feedback or suggestions for improvements, please submit them using our ",
        external_link(href = feedback_url, link_text = "feedback form", add_warning = TRUE)
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      valuebox_panel(),
      dashboard_panel(),
      tabPanel(
        "Accessibility",
        a11y_panel(
          dashboard_title = dashboard_title,
          dashboard_url = dashboard_url,
          date_tested = "01/01/1900",
          date_prepared = "01/01/1900",
          date_reviewed = "01/01/1900",
          issues_contact = team_email,
          publication_name = ees_pub_name,
          publication_slug = ees_pub_slug,
          non_accessible_components =
            c(
              "Keyboard navigation through the interactive charts is currently limited",
              "Alternative text in interactive charts is limited to titles and axis labels"
            ),
          specific_issues =
            c(
              "Charts have non-accessible components that are inaccessible for keyboard users.",
              "Chart tooltips are not compatible with screen reader use.",
              "Some decorative images are not labelled appropriately as yet.",
              "Some links are not appropriately labelled."
            )
        )
      ),
      # a11y_panel(),
      tabPanel(
        "Support",
        support_panel(
          team_email = team_email,
          repo_name = repo_url,
          form_url = feedback_url,
          publication_name = ees_pub_name,
          publication_slug = ees_pub_slug
        )
      ),
      tabPanel(
        value = "cookies_panel_ui",
        "Cookies",
        dfeshiny::cookies_panel_ui(
          google_analytics_key = google_analytics_key
        )
      )
    ),
    tags$script(
      src = "script.js"
    ),
    #
    #     # Add this JavaScript code to disable the selectize input
    #     js <- "
    # Shiny.addCustomMessageHandler('disableSelectize', function(message) {
    #   $('#' + message.id).selectize()[0].selectize.disable();
    # });
    # "
    #     ,


    shinyGovstyle::footer(TRUE)
  )
}
