a11y_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          shinyGovstyle::heading_text(
            "Accessibility statement",
            size = "xl",
            level = 1
          ),
          br(
            "This accessibility statement applies to the Further Education Outcomes Industry Dashboard.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."
          ),
          shinyGovstyle::heading_text(
            "WCAG 2.1 compliance",
            size = "l",
            level = 2
          ),
          br(
            "We follow the reccomendations of the ",
            a(
              style = "color:#007fb0",
              href = "https://www.w3.org/TR/WCAG21/",
              "WCAG 2.1 requirements. ",
              onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"
            ),
            "This application has been checked using the ",
            a(
              style = "color:#007fb0",
              href = "https://github.com/ewenme/shinya11y",
              "Shinya11y tool "
            ),
            ", which did not detect accessibility issues.
             Each page in this application has been audited for accessiblity with the page-snapshot functionality in the ",
            a(
              style = "color:#007fb0",
              href = "https://developers.google.com/web/tools/lighthouse",
              "Google Developer Lighthouse tool"
            ),
            ". Primarily due to the current limitations of the R packages used to create this application, it does not fully pass the accessibility auditing. ",
            "The reasons for this are outlined in the limitations section below.",
            "This app does however follow the following guidelines:"
          ),
          tags$div(
            shinyGovstyle::gov_list(
              list(
                "uses colours that have sufficient contrast",
                "allows you to zoom in up to 300% without the text spilling off the screen",
                "has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users"
              )
            )
          ),
          shinyGovstyle::heading_text(
            "Limitations",
            size = "l",
            level = 2
          ),
          br(
            "We recognise that there are still issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users." # For example, these
          ),
          tags$div(
            shinyGovstyle::gov_list(
              list(
                "some elements fail to have the appropriate aria tags",
                "some table header ids are not assigned correctly",
                "some image elements do not have an alt attributes (note that where this is the case, those images are primarily for presentation)"
              )
            )
          ),
          shinyGovstyle::heading_text(
            "Feedback",
            size = "l",
            level = 2
          ),
          br(
            "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
            a(
              style = "color:#007fb0",
              href = "mailto:FE.OUTCOMESDATA@education.gov.uk",
              "FE.OUTCOMESDATA@education.gov.uk"
            )
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
          shinyGovstyle::heading_text(
            "Give us feedback",
            size = "l",
            level = 2
          ),
          "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
          a(
            style = "color:#007fb0",
            href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-YHar1nqsS9Eu7bHka6oC0lUQUlDNzNBVzdGSUE3VVpJMlY1STVTSjNVNC4u",
            "feedback form",
            .noWS = c("after")
          ),
          ".",
          br(),
          br(),
          "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
          a(
            style = "color:#007fb0",
            href = "mailto:FE.OUTCOMESDATA.development@education.gov.uk",
            "FE.OUTCOMESDATA.development@education.gov.uk",
            .noWS = c("after")
          ),
          ".",
          br(),
          h2("Find more information on the data"),
          "The data used to produce the dashboard, along with methodological information can be found on ",
          a(
            style = "color:#007fb0",
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-outcome-based-success-measures",
            "Explore Education Statistics",
            .noWS = c("after")
          ),
          ".",
          br(),
          h2("Contact us"),
          "If you have questions about the dashboard or data within it, please contact us at ",
          a(
            style = "color:#007fb0",
            href = "mailto:FE.OUTCOMESDATA.development@education.gov.uk",
            "FE.OUTCOMESDATA.development@education.gov.uk",
            .noWS = c("after")
          ),
          br(),
          h2("See the source code"),
          "The source code for this dashboard is available in our ",
          a(
            style = "color:#007fb0",
            href = "https://github.com/dfe-analytical-services/OBSM-industry",
            "GitHub repository",
            .noWS = c("after")
          ),
          ".",
        ),
        column(
          12,
          shinyGovstyle::heading_text(
            "Use of cookies",
            size = "l",
            level = 2
          ),
          textOutput("cookie_status"),
          actionButton("remove", "Reset cookie consent"),
        )
      )
    )
  )
}
