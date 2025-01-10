# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

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


ui <- function(input, output, session) {
  fluidPage(
    tags$style(HTML("
    .download-link span {
      color: white;
    }
  ")),
    # use_tota11y(),
    title = tags$head(tags$link(
      rel = "shortcut icon",
      href = "dfefavicon.png"
    )),
    use_shiny_title(),
    tags$html(lang = "en"),
    # Add meta description for search engines
    meta() %>%
      meta_general(
        application_name = "Further Education Outcomes Industry Dashboard",
        description = "Further Education Outcomes Industry Dashboard",
        robots = "index,follow",
        generator = "R-Shiny",
        subject = "education statistics",
        rating = "General",
        referrer = "no-referrer"
      ),
    shinyjs::useShinyjs(),
    dfeshiny::custom_disconnect_message(
      dashboard_title = site_title,
      publication_name = ees_pub_name,
      publication_link = ees_publication
    ),
    # Setting up cookie consent based on a cookie recording the consent:
    dfe_cookies_script(),
    cookies_banner_ui(
      name = site_title
    ),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    # dfeshiny::header(header = site_title),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is in beta phase and we are still reviewing performance and reliability. "
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      industry_by_subject_panel(),
      subject_by_industry_panel(),
      shiny::tabPanel(
        value = "a11y_panel",
        "Accessibility",
        dfeshiny::a11y_panel(
          dashboard_title = site_title,
          dashboard_url = site_primary,
          date_tested = "2nd January 2025",
          date_prepared = "2nd January 2025",
          date_reviewed = "2nd January 2025",
          issues_contact = "FE.OUTCOMESDATA@education.gov.uk",
          publication_slug = "further-education-outcome-based-success-measures",
          publication_name = "Further Education outcomes based success measures"
        )
      ),
      shiny::tabPanel(
        value = "support_panel",
        "Support and feedback",
        support_panel(
          team_email = "FE.OUTCOMESDATA@education.gov.uk",
          form_url = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-YHar1nqsS9Eu7bHka6oC0lUQUlDNzNBVzdGSUE3VVpJMlY1STVTSjNVNC4u",
          repo_name = "https://github.com/dfe-analytical-services/OBSM-industry",
          publication_slug = "further-education-outcome-based-success-measures",
          publication_name = "Further Education outcomes based success measures"
        )
      ),
      shiny::tabPanel(
        value = "cookies_panel_ui",
        "Cookies",
        cookies_panel_ui(google_analytics_key = google_analytics_key)
      )
    ),
    tags$script(
      src = "script.js"
    ),
    footer(full = TRUE)
  )
}
