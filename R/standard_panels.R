a11y_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Accessibility statement"),
          br("This accessibility statement applies to the Further Education Outcomes Industry Dashboard.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
          h2("WCAG 2.1 compliance"),
          br("We follow the reccomendations of the ", a(style = "color:#007fb0", href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(style = "color:#007fb0", href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(style = "color:#007fb0", href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
          )),
          h2("Limitations"),
          br(
            "We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users." # For example, these
            # are known issues that we will continue to monitor and improve:
          ),
          # tags$div(tags$ul(
          #   tags$li("List"),
          #   tags$li("known"),
          #   tags$li("limitations, e.g."),
          #   tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
          # )),
          h2("Feedback"),
          br(
            "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
            a(style = "color:#007fb0", href = "mailto:FE.OUTCOMESDATA@education.gov.uk", "FE.OUTCOMESDATA@education.gov.uk")
          )
        )
      )
    )
  )
}

support_links <- function() {
  tabPanel(
    "Support and feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Give us feedback"),
          "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
          a(
            style = "color:#007fb0",
            href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-YHar1nqsS9Eu7bHka6oC0lUQUlDNzNBVzdGSUE3VVpJMlY1STVTSjNVNC4u",
            "feedback form", .noWS = c("after")
          ), ".", br(),
          br(),
          "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
          a(style = "color:#007fb0", href = "mailto:FE.OUTCOMESDATA.development@education.gov.uk", "FE.OUTCOMESDATA.development@education.gov.uk", .noWS = c("after")), ".",
          br(),
          h2("Find more information on the data"),
          "The data used to produce the dashboard, along with methodological information can be found on ",
          a(style = "color:#007fb0", href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-outcome-based-success-measures", "Explore Education Statistics", .noWS = c("after")),
          ".",
          br(),
          h2("Contact us"),
          "If you have questions about the dashboard or data within it, please contact us at ",
          a(style = "color:#007fb0", href = "mailto:FE.OUTCOMESDATA.development@education.gov.uk", "FE.OUTCOMESDATA.development@education.gov.uk", .noWS = c("after")), br(),
          h2("See the source code"),
          "The source code for this dashboard is available in our ",
          a(style = "color:#007fb0", href = "https://github.com/dfe-analytical-services/OBSM-industry", "GitHub repository", .noWS = c("after")),
          ".",
        ),
        column(
          12,
          h2("Use of cookies"),
          textOutput("cookie_status"),
          actionButton("remove", "Reset cookie consent"),
        )
      )
    )
  )
}
